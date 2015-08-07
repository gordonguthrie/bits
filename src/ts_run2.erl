-module(ts_run2).

%% a new runner for time series dev

-export([
	 create_bucket/0,
	 put_data/0,
	 dump/0,
	 query_data/0,
	 bounce_qry/0	]).

-include_lib("riak_kv/src/riak_kv_wm_raw.hrl").

-define(BUCKET,   <<"GeoCheckin">>).
-define(GEOHASH,  "geohash_val").
-define(USER,     "user_").
-define(NOOFRECS, 10).

bounce_qry() ->
    exit(whereis(riak_kv_qry_queue), "adios bandito"),
    ok.

dump() ->
    io:format("Dumping the contents of the bucket ~p from within leveldb~n", [?BUCKET]),
    {Mod, DDL} = create_bucket(),
    {PK, _LK, _Obj} = make_obj(Mod, DDL, 1),
    leveldb_console:dump_bucket(?BUCKET, PK).

create_bucket() ->
    SQL = "CREATE TABLE GeoCheckin " ++
	"(geohash varchar not null, " ++ 
	"user varchar not null, " ++
	"time timestamp not null, " ++ 
	"weather varchar not null, " ++ 
	"temperature varchar, " ++ 
	"PRIMARY KEY((quantum(time, 15, s)), time, user))", 
    Lexed = riak_ql_lexer:get_tokens(SQL),
    {ok, DDL} = riak_ql_parser:parse(Lexed),
    {module, Module} = riak_ql_ddl_compiler:make_helper_mod(DDL, "/tmp"),
    {Module, DDL}.

put_data() ->
    io:format("Writing ~p records~n", [?NOOFRECS]),
    {Mod, DDL} = create_bucket(),
    put2(?NOOFRECS, Mod, DDL).

put2(0, _Mod, _DDL) -> 
    ok;
put2(N, Mod, DDL) when is_integer(N) andalso N > 0 -> 
    {PK, LK, Obj} = make_obj(Mod, DDL, N),
    gg:format("in put2 PK is ~p LK is ~p~n", [PK, LK]),
    RObj = riak_object:new(?BUCKET, PK, term_to_binary(Obj)),
    MD1 = riak_object:get_update_metadata(RObj),
    MD2 = dict:store(?MD_LI_IDX,
		     LK,
		     MD1
		    ),
    RObj2 = riak_object:update_metadata(RObj, MD2),
    riak_client:put(RObj2, {riak_client, [node(), undefined]}),
    put2(N - 1, Mod, DDL).

make_obj(Mod, DDL, N) ->
    N2      = integer_to_list(N),
    Geohash = list_to_binary(?GEOHASH),
    User    = list_to_binary(?USER ++ N2),
    Time    = 10000000 + N,
    Weather = term_to_binary([{some, "random", <<"WeaTH3r">>, N}]),
    Temp    = list_to_binary(N2),
    Obj     = {Geohash, User, Time, Weather, Temp},
    case Mod:validate_obj(Obj) of
	true  -> PK = riak_ql_ddl:get_partition_key(DDL, Obj),
		 LK = riak_ql_ddl:get_local_key(DDL, Obj),
		 {PK, LK, Obj};
	false -> exit("borked object")
    end.

query_data() ->
    Query = "select weather from GeoCheckin where time > 3000 and time < 5000 and user = \"user_1\"",
    Lexed = riak_ql_lexer:get_tokens(Query),
    {ok, SQL} = riak_ql_parser:parse(Lexed),
    {_Mod, DDL} = create_bucket(),
    case riak_ql_ddl:is_query_valid(DDL, SQL) of
	true ->
	    io:format("Executing Query ~p~n", [Query]),
	    {qid, _QID} = riak_kv_qry_queue:put_on_queue(SQL);
	false ->
	    exit('borked query')
    end.
