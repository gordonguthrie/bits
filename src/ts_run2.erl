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
    gg:format("in dump BUCKET is ~p PK is ~p~n", [?BUCKET, PK]),
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
    Obj     = [
	       {"geohash", Geohash}, 
	       {"user", User}, 
	       {"time", Time}, 
	       {"weather", Weather}, 
	       {"temperature", Temp}
	      ],
    Obj2 = eleveldb_ts:encode_record(Obj),
    StrippedObj = list_to_tuple([X || {_, X} <- Obj]),
    case Mod:validate_obj(StrippedObj) of
	true  -> PK = riak_ql_ddl:get_partition_key(DDL, StrippedObj),
		 LK = riak_ql_ddl:get_local_key(DDL, StrippedObj),
		 PK2 = eleveldb_ts:encode_key(PK),
		 LK2 = eleveldb_ts:encode_key(LK),
		 io:format("Writing with PK of ~p (~p)~n", [PK, PK2]),
		 {PK2, LK2, Obj2};
	false -> exit("borked object")
    end.

query_data() ->
%%    Query = "select weather from GeoCheckin where time > 9990000 and time < 11000000 and user = \"user_1\"",
    Query = "select weather from GeoCheckin",
    Lexed = riak_ql_lexer:get_tokens(Query),
    {ok, SQL} = riak_ql_parser:parse(Lexed),
    {_Mod, DDL} = create_bucket(),
    case riak_ql_ddl:is_query_valid(DDL, SQL) of
	true ->
	    io:format("Executing Query ~p~n", [Query]),
	    {qid, QId} = riak_kv_qry_queue:put_on_queue(SQL),
            io:format("Fetching on qid ~p~n", [QId]),
	    fetch_with_patience(QId);
	false ->
	    exit('borked query')
    end.

-define(FETCH_RETRIES, 5).
fetch_with_patience(QId) ->
    fetch_with_patience(QId, ?FETCH_RETRIES).
fetch_with_patience(QId, 0) ->
    io:format("Query results on qid ~p Not available after ~b retries\n", [QId, ?FETCH_RETRIES]),
    [];
fetch_with_patience(QId, N) ->
    case riak_kv_qry_queue:fetch(QId) of
        {error, Why} ->
            io:format("qry status: ~p\n", [Why]),
            timer:sleep(1000),
            fetch_with_patience(QId, N-1);
        Result ->
            Result
    end.
