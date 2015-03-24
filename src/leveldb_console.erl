-module(leveldb_console).

-export([
	 run_fold/3,
	 dump_bucket/2,
	 dump_stats/1,
	 get_state_data/1,
	 get_level_reference/1,
	 get_level_reference/2
	]).

run_fold(Bucket, Key, Fun) ->
    Ref = get_vnode_ref(Bucket, Key),
    eleveldb:fold(Ref, Fun, [], []).

dump_bucket(Bucket, Key) when is_binary(Bucket) andalso is_binary(Key) ->
    Ref = get_vnode_ref(Bucket, Key),
    %% ok = dump_stats(Ref),
    DumpFun = fun({K, V}, Acc) when is_binary(K) andalso is_binary(V) ->
		      K2 = sext:decode(K),
		      {V2, K3} = case K2 of
			       {o, K4, SubK} -> 
				   RO = riak_object:from_binary(Bucket, Key, V),
					 SubK1 = sext:decode(SubK),
					 O2 = {o, K4, SubK1},
					 RO2 = riak_object:get_value(RO),
					 {binary_to_term(RO2), O2};
				     Other  ->
					 {V, Other}
				 end,
		      io:format("Key: ~p - Val: ~p~n", [K3, V2]),
		      Acc
	      end,
    eleveldb:fold(Ref, DumpFun, [], []),
    ok.

get_state_data(Pid) ->
    {status, Pid, _Mod, Status} = sys:get_status(Pid),
    Status2 = lists:flatten(Status),
    Status3 = [L || {data, L} <- Status2],
    Status4 = lists:flatten(Status3),
    _State = proplists:get_value("StateData", Status4).

%% Idx is a vnode index (the long number)
get_level_reference(Idx) ->
    {ok, Pid} = riak_core_vnode_manager:get_vnode_pid(Idx, riak_kv_vnode),
    State = get_state_data(Pid),
    ModState = element(4, State),
    case element(3,ModState) of
	riak_kv_eleveldb_backend ->
	    LvlState = element(4, ModState),
	    element(2, LvlState);
	_ ->
	    undefined
    end.

%% If a multi-backend, use the second version with a backend name
%% eg 'eleveldb_multi'
get_level_reference(Idx, _Name0) ->
    {ok, Pid} = riak_core_vnode_manager:get_vnode_pid(Idx, riak_kv_vnode),
    State = get_state_data(Pid),
    ModState = element(4, State),
    case element(3, ModState) of
	riak_kv_multi_backend ->
	    Backends = element(2, element(4, ModState)),
	    SL = [B || {_Name, riak_kv_eleveldb_backend, B} <- Backends],
	    case SL of
		[] -> undefined;
		_ -> element(2, hd(SL))
	    end;
	_ ->
	    undefined
    end.

dump_stats(Ref) ->
    StatsRefs = [
		 <<"leveldb.ROFileOpen">>,
		 <<"leveldb.ROFileClose">>,
		 <<"leveldb.ROFileUnmap">>,
		 <<"leveldb.RWFileOpen">>,
		 <<"leveldb.RWFileClose">>,
		 <<"leveldb.RWFileUnmap">>,
		 <<"leveldb.ApiOpen">>,
		 <<"leveldb.ApiGet">>,
		 <<"leveldb.ApiWrite">>,
		 <<"leveldb.WriteSleep">>,
		 <<"leveldb.WriteWaitImm">>,
		 <<"leveldb.WriteWaitLevel0">>,
		 <<"leveldb.WriteNewMem">>,
		 <<"leveldb.WriteError">>,
		 <<"leveldb.WriteNoWait">>,
		 <<"leveldb.GetMem">>,
		 <<"leveldb.GetImm">>,
		 <<"leveldb.GetVersion">>,
		 <<"leveldb.SearchLevel[0]">>,
		 <<"leveldb.SearchLevel[1]">>,
		 <<"leveldb.SearchLevel[2]">>,
		 <<"leveldb.SearchLevel[3]">>,
		 <<"leveldb.SearchLevel[4]">>,
		 <<"leveldb.SearchLevel[5]">>,
		 <<"leveldb.SearchLevel[6]">>,
		 <<"leveldb.TableCached">>,
		 <<"leveldb.TableOpened">>,
		 <<"leveldb.TableGet">>,
		 <<"leveldb.BGCloseUnmap">>,
		 <<"leveldb.BGCompactImm">>,
		 <<"leveldb.BGNormal">>,
		 <<"leveldb.BGCompactLevel0">>,
		 <<"leveldb.BlockFiltered">>,
		 <<"leveldb.BlockFilterFalse">>,
		 <<"leveldb.BlockCached">>,
		 <<"leveldb.BlockRead">>,
		 <<"leveldb.BlockFilterRead">>,
		 <<"leveldb.BlockValidGet">>,
		 <<"leveldb.Debug[0]">>,
		 <<"leveldb.Debug[1]">>,
		 <<"leveldb.Debug[2]">>,
		 <<"leveldb.Debug[3]">>,
		 <<"leveldb.Debug[4]">>,
		 <<"leveldb.ReadBlockError">>,
		 <<"leveldb.DBIterNew">>,
		 <<"leveldb.DBIterNext">>,
		 <<"leveldb.DBIterPrev">>,
		 <<"leveldb.DBIterSeek">>,
		 <<"leveldb.DBIterSeekFirst">>,
		 <<"leveldb.DBIterSeekLast">>,
		 <<"leveldb.DBIterDelete">>,
		 <<"leveldb.eleveldbDirect">>,
		 <<"leveldb.eleveldbQueued">>,
		 <<"leveldb.eleveldbDequeued">>,
		 <<"leveldb.elevelRefCreate">>,
		 <<"leveldb.elevelRefDelete">>,
		 <<"leveldb.ThrottleGauge">>,
		 <<"leveldb.ThrottleCounter">>,
		 <<"leveldb.ThrottleMicros0">>,
		 <<"leveldb.ThrottleKeys0">>,
		 <<"leveldb.ThrottleBacklog0">>,
		 <<"leveldb.ThrottleCompacts0">>,
		 <<"leveldb.ThrottleMicros1">>,
		 <<"leveldb.ThrottleKeys1">>,
		 <<"leveldb.ThrottleBacklog1">>,
		 <<"leveldb.ThrottleCompacts1">>
		],
    PrintFun = fun(X) ->
		       Text = binary_to_list(X),
		       {ok, S} = eleveldb:status(Ref, X),
		       Stat = list_to_integer(binary_to_list(S)),
		       io:format("~s: ~p~n", [Text, Stat]),
		       ok
	       end,
    [PrintFun(X) || X <- StatsRefs],
    ok.

get_vnode_ref(Bucket, Key) ->
        BucketProps = riak_core_bucket:get_bucket(Bucket),
    DocIdx = riak_core_util:chash_key({Bucket, Key}, BucketProps),
    UpNodes = riak_core_node_watcher:nodes(riak_kv),
    [{{BFN, _}, _}] = riak_core_apl:get_apl_ann(DocIdx, 1, UpNodes),
    _Ref = get_level_reference(BFN).
