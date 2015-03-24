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
    case file:open(?FILENAME, [append]) of
	{ok, Id} -> io:fwrite(Id, "~s~n", [Str]),
		    file:close(Id);
	Err      -> {error, Err}
    end.
	    
	    
    
