-module(maenmpc_svc).
-export([log/2, status_enabled/2]).
-include_lib("maenmpc_db.hrl").

log(To, Log) ->
	{{Y,M,D}, {H,I,S}} = erlang:universaltime(),
	Time = io_lib:format("~w-~2..0w-~2..0wT~2..0w:~2..0w:~2..0wZ",
							[Y, M, D, H, I, S]),
	ok = gen_server:cast(To, Log#dblog{timestamp=Time}).

status_enabled(To, SVCStatus) ->
	ok = gen_server:cast(To, SVCStatus).
