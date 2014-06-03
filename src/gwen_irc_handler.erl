-module(gwen_irc_handler).

-export([connect/2, start/2, loop/1]).

connect(Host, Port) ->
	spawn_link(?MODULE, start, [Host, Port]).

start(Host, Port) ->
	io:format("Connecting to ~s:~B... ", [Host, Port]),
	{ok, Sock} = gen_tcp:connect(Host, Port, [{packet, line}], 5000),
	io:format("Connected~n"),
	gwen_irc:notify(connected),
	loop(Sock).

loop(Sock) ->
	receive
		{tcp, Sock, Data} ->
			io:format("<- ~s", [Data]),
			gen_server:cast(gwen_irc, {line, Data}),
			?MODULE:loop(Sock);
		Msg ->
			io:format("~p~n", [Msg])
	after
		5000 ->
			?MODULE:loop(Sock)
	end.