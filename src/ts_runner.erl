-module(ts_runner).

%% a runner for time series dev

-compile([export_all]).

%% -export([
%% 	 %% setup/0,
%% 	 show/0
%% 	 %% load/0,
%% 	 %% dump/0
%% 	 %% prep_q/0,
%% 	 %% qry/0,
%% 	 %% qry/1,
%% 	 %% get_ddl/0
%% 	 % bounce_qry/0
%% 	]).

%% -include_lib("riak_ql/include/riak_ql_ddl.hrl").
-include_lib("riak_kv/include/riak_kv_index.hrl").
-include_lib("riak_kv/src/riak_kv_wm_raw.hrl").

-define(BUCKET,   <<"TsBucket3">>).
-define(INDEX,    <<"Index_bin">>).
-define(CLIENTID, <<"li_client">>).
-define(GEOHASH,  "geohash_val").
-define(USER,     "user_").
-define(NOOFRECS, 10).

%% bounce_qry() ->
%%     exit(whereis(riak_kv_qry_queue), "adios bandito"),
%%     ok.

%% qry() ->
%%     Qry = prep_q(),
%%     qry(Qry).

%% qry(#riak_kv_li_index_v1{} = Qry) ->
%%     DDL = get_ddl(),
%%     io:format("in qry/1~n- DDL is ~p~n- Qry is ~p~n", [DDL, Qry]),
%%     case riak_ql_ddl:is_query_valid(DDL, Qry) of
%% 	{false, Err} -> exit({invalid_query, Err});
%% 	true         -> qry2(DDL, Qry)
%%     end.

%% qry2(DDL, Qry) ->
%%     Ret = execute_qry(DDL, Qry),
%%     io:format("Ret is ~p~n", [Ret]),
%%     ok.

%% prep_q() ->
%%     Selections  = [["temperature"], ["geohash"]],
%%     Filters     = [
%% 		   {'and',
%% 		    {'=', "geohash", ?GEOHASH},
%% 		    {'and',
%% 		     {'>',  "timestamp", 1},
%% 		     {'=<', "timestamp", 3}
%% 		    }
%% 		   }
%% 		  ],
%%     Operators   = [],
%%     Sorters     = [],
%%     Combinators = [],
%%     Limit       = none,
%%     _Qry = #riak_kv_li_index_v1{bucket      = ?BUCKET,
%% 				selections  = Selections,
%% 				filters     = Filters,
%% 				operators   = Operators,
%% 				sorters     = Sorters,
%% 				combinators = Combinators,
%% 				limit       = Limit}.

%% setup() ->
%%     Client = riak_client:new(node(), ?CLIENTID),
%%     DDL = get_ddl(),
%%     {module, ModName} = riak_ql_ddl_compiler:make_helper_mod(DDL),
%%     BucketProps = [{ddl, 1, DDL}, {verification_module, ModName}],
%%     ok = riak_client:set_bucket(?BUCKET, BucketProps, Client).

%% get_ddl() ->
%%     SQL = "CREATE TABLE GeoCheckin " ++
%% 	"(geohash varchar not null, " ++ 
%% 	"user varchar not null, " ++
%% 	"time timestamp not null, " ++ 
%% 	"weather varchar not null, " ++ 
%% 	"temperature varchar, " ++ 
%% 	"PRIMARY KEY((quantum(time, 15, s)), time, user))", 
%%     {ok, DDL} = make_ddl(SQL),
%%     DDL.

%% dump() ->
%%     io:format("Dumping the contents of the bucket ~p from within leveldb~n", [?BUCKET]),
%%     Mod = riak_ql_ddl:make_module_name(?BUCKET),
%%     DDL = get_ddl(),
%%     {PK, _LK, _Obj} = make_obj(Mod, DDL, 1),
%%     leveldb_console:dump_bucket(?BUCKET, PK).

%% load() ->
%%     io:format("Loading ~p records into the  bucket ~p~n", [?NOOFRECS, ?BUCKET]),
%%     Mod = riak_ql_ddl:make_module_name(?BUCKET),
%%     DDL = get_ddl(),
%%     load(Mod, DDL, ?NOOFRECS).

%% load(_Mod, _DDL, 0) ->
%%     ok;
%% load(Mod, DDL, N) ->
%%     {PK, LK, Obj} = make_obj(Mod, DDL, N),
%%     io:format("making object ~p~n-PK is ~p~n-LK is ~p~n",[N, sext:decode(PK),
%% 							  sext:decode(LK)]),
%%     RObj = riak_object:new(?BUCKET, PK, term_to_binary(Obj)),
%%     MD1 = riak_object:get_update_metadata(RObj),
%%     %% MD2 = dict:store(?MD_INDEX,
%%     %% 		     [{?INDEX, make_index(N)}],
%%     %% 		     MD1
%%     %% 		    ),
%%     MD3 = dict:store(?MD_LI_IDX,
%% 		     LK,
%% 		     MD1
%% 		    ),
%%     RObj2 = riak_object:update_metadata(RObj, MD3),
%%     riak_client:put(RObj2, {riak_client, [node(), undefined]}),
%%     load(Mod, DDL, N - 1).

make_obj(Mod, DDL, N) ->
    N2 = integer_to_list(N),
    Geohash = list_to_binary(?GEOHASH),
    User = list_to_binary(?USER ++ N2),
    Time = 10000000 + N,
    Weather = [{some, "random", <<"WeaTH3r">>, N}],
    Temp = list_to_binary(N2),
    Obj = {Geohash, User, Time, Weather, Temp},
    case Mod:validate_obj(Obj) of
	true  -> PK = riak_ql_ddl:get_partition_key(DDL, Obj),
		 LK = riak_ql_ddl:get_local_key(DDL, Obj),
		 {PK, LK, Obj};
	false -> exit("borked object")
    end.

% make_index(N) -> make_("index", N).

% make_(Prefix, N) when is_integer(N) ->
%    list_to_binary(Prefix ++ "_" ++ integer_to_list(N)).

show() ->
    io:format("Getting bucket properties for the bucket ~p~n", [?BUCKET]),
    Client = riak_client:new(node(), ?CLIENTID),
    _Props = riak_client:get_bucket(?BUCKET, Client).

%% execute_qry(DDL, Q) ->
%%     SubQueries = riak_kv_qry_compiler:compile(DDL, Q),
%%     io:format("in ts_runner:execute_qry SubQueries is ~p~n", [SubQueries]),
%%     bombaste.
    %% Bucket = Q#riak_kv_li_index_v1.bucket,
    %% %% fix these up too
    %% Timeout = {timeout, 10000},
    %% QId = 1,
    %% Me = self(),
    %% CoverageFn = {colocated, riak_kv_qry_coverage_plan},
    %% {ok, _PID} = riak_kv_index_fsm_sup:start_index_fsm(node(), [{raw, QId, Me}, [Bucket, none, Q, Timeout, CoverageFn]]),
    %% rangola.
