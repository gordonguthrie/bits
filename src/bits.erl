-module(bits).

-export([
	 print_fn/2,
	 log_terms/1,
	 pp_stacktrace/1
       ]).

-define(LOGFILE, "/tmp/bits.log").

log_terms(Terms) ->
    log_t2(Terms, ?LOGFILE).
    
log_t2(Terms, File) ->
    Str = lists:flatten(io_lib:format("~p.~n", [Terms])),
    _Return = filelib:ensure_dir(File),
    case file:open(File, [append]) of
        {ok, Id} ->
            io:fwrite(Id, "~s~n", [Str]),
            file:close(Id);
        _ ->
            error
    end.

pp_stacktrace(String) when is_list(String) ->
    Stacktrace = re:replace(lists:flatten(io_lib:format("~p", [erlang:get_stacktrace()])), "\n", "~n", [global, {return, list}]),
    io:format(user, "~nStacktrace: " ++ String ++ "~n" ++ Stacktrace ++ "~n", []),
    ok.

print_fn(String, Fn) when is_list(String) andalso is_function (Fn) ->
    {env, Env} = erlang:fun_info(Fn, env),
    io:format("Env is ~p~n", [Env]),
    % [ABS | _Rest] = lists:reverse(Env),
    AST = erl_syntax:abstract(Env),
    io:format("AST is ~p~n", [AST]),
    Snippet = erl_syntax:revert(AST),
    io:format("Snippet is ~p~n", [Snippet]),
    Src = erl_prettypr:format(Snippet),
    io:format("Src is ~p~n", [Src]),
    Src2 = re:replace(Src, "\n", "~n", [global, {return, list}]),
    io:format("printing out Fun: " ++ String ++ "~n~p~n", [Src2]).
