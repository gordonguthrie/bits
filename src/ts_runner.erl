-module(ts_runner).

-export([
	 put/0,
	 setup_testing/0
	]).

%% deprecated
-export([
	 load/0
	]).

-include("riak_kv_index.hrl").
-include("riak_kv_ddl.hrl").

-define(BUCKET,   <<"2iBucket">>).
-define(INDEX,    <<"Index_bin">>).
-define(MD_INDEX, <<"index">>).
-define(CLIENTID, <<"li_client">>).

put() ->
    ok.

setup_testing() ->
    Client = riak_client:new(node(), ?CLIENTID),
    DDL = make_ts_testing_ddl(),
    ok = riak_kv_ddl_compiler:make_validation(DDL, true, "/tmp"),
    BucketProps = [{ddl, DDL}],
    ok = riak_client:set_bucket(?BUCKET, BucketProps, Client).

make_ts_testing_ddl() ->
    Geohash     = {"geohash",     binary},
    User        = {"user",        binary},
    Time        = {"time",        timestamp},
    Weather     = {"weather",     any},
    Temperature = {"temperature", binary},
    Fields = [
	      Geohash,
	      User,
	      Time,
	      Weather,
	      Temperature
	     ],
    Colocation = yando,
    _DDL = #ddl{bucket     = ?BUCKET,
		fields     = Fields,
		colocation = Colocation}.

%%
%% obsolete
%%
load() -> load(10).

load(0) -> ok;
load(N) -> RObj = riak_object:new(?BUCKET, make_key(N), make_value(N)),
	   MD1 = riak_object:get_update_metadata(RObj),
	   MD2 = dict:store(?MD_INDEX,
			    [{?INDEX, make_index(N)}],
			    MD1
			   ),
	   gg:format("in load for ~p~n- MD1 is ~p~n- MD2 is ~p~n", [N, dict:to_list(MD1), dict:to_list(MD2)]),
	   RObj2 = riak_object:update_metadata(RObj, MD2),
	   riak_client:put(RObj2, {riak_client, [node(), undefined]}).

make_key(N)   -> make_("key", N).
make_value(N) -> make_("value", N).
make_index(N) -> make_("index", N).

make_(Prefix, N) when is_integer(N) ->
    list_to_binary(Prefix ++ "_" ++ integer_to_list(N)).
