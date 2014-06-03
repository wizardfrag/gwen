-module(gwen_irc_sup).
-behaviour(supervisor).

-export([start_link/1, init/1]).

start_link(Args) ->
	supervisor:start_link(?MODULE, Args).

init(Options) ->
	Restart = {one_for_one, 2, 5},
	C0 = {gwen_irc
	, {gwen_irc, start_link, [Options]}
	, permanent
	, 200
	, worker
	, [gwen_irc]
	},
	{ok, {Restart, [C0]}}.