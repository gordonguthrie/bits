-module(explore).

-export([
	 mods/0
	 ]).

mods() ->
    Modules = code:all_loaded(),
    Fun = fun({M, _}, Acc) ->
		  M2 = atom_to_list(M),
		  case M2 of
		      "riak_kv_ddl_helper" ++ _Rest -> [M | Acc];
		      _                             -> Acc
		  end
	  end,
    lists:foldl(Fun, [], Modules).
