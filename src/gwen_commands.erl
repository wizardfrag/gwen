-module(gwen_commands).

-export([init/0, cmd_test/4, cmd_commands/4, cmd_restart/4, cmd_version/4]).
-include("irc.hrl").

init() ->
	Commands = application:get_env(commands, gwen, []),
	init_commands(Commands).

init_commands([]) ->
	ok;
init_commands([H|T]) ->
	{C,M,F} = H,
	gwen_irc:register_command(C, M, F),
	init_commands(T).

cmd_test(_User, Channel, _Message, _State) ->
	gwen_irc:privmsg(Channel, "Hello, world!").

cmd_commands(_User, Channel, _Message, State) ->
	Commands = State#state.commands,
	gwen_irc:privmsg(Channel, "Commands: " ++ string:join(proplists:get_keys(Commands), ", ")).

cmd_restart(_, _, _, _) ->
	gen_server:cast(gwen_irc, restart).

cmd_version(_User, Channel, _Message, _State) ->
	{ok, Vsn} = application:get_key(gwen, vsn),
	gwen_irc:privmsg(Channel, io_lib:format("Erlang IRC Bot version: ~s", [Vsn])).