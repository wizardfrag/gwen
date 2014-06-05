-module(gwen).
-behaviour(application).
-behaviour(supervisor).

-export([start/0, stop/0]).
-export([start/2, stop/1]).
-export([init/1]).

start() ->
	ok = lager:start(),
	application:ensure_all_started(?MODULE).

stop() ->
	application:stop(?MODULE).

start(_Type, _Args) ->
	supervisor:start_link({local, gwen_sup}, ?MODULE, []).

stop(_State) ->
	ok.

init(_) ->
	{ok, Opts} = application:get_env(gwen, irc),
	Restart = {one_for_one, 2, 5},
	C0 = {gwen_irc_sup
	, {gwen_irc_sup, start_link, [Opts]}
	, permanent
	, 200
	, worker
	, [gwen_irc_sup]
	},
	C1 = {gwen_db
	, {gwen_db, start, []}
	, permanent
	, 200
	, worker
	, [gwen_db]
	},
	{ok, {Restart, [C1, C0]}}.