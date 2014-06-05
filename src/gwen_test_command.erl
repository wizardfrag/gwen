-module(gwen_test_command).

-export([call/4]).

call(_User, Channel, _Message, _State) ->
	gwen_irc:privmsg(Channel, "Hello, world!").
