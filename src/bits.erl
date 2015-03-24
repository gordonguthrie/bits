-module(bits).

-export([
	log_terms/1
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
