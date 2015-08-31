-module(gg).

-export([
	 format/1,
	 format/2,
	 format/3
	]).

-define(FILENAME, "/tmp/gg.log").

format(X) -> Str = io_lib:format(X, []),
	     log(Str).

format(X, Y) -> Str = io_lib:format(X, Y),
		log(Str).

format(false, _X, _Y) -> ok;
format(true,  X,   Y) -> format(X, Y).

log(Str) ->
    _FileName = filelib:ensure_dir(?FILENAME),
    [{registered_name, RN}] = erlang:process_info(self(), [registered_name]),
    case file:open(?FILENAME, [append]) of
	{ok, Id} ->
	    io:fwrite(Id, "On ~p as {~p, ~p} at ~p~n- ~s~n",
		      [node(), self(), RN, calendar:now_to_universal_time(now()), Str]),
	    file:close(Id);
	Err  ->
	    {error, Err}
    end.
