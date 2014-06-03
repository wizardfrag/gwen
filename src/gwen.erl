-module(gwen).
-behaviour(application).
-behaviour(supervisor).

-export([start/0, stop/0, squery/2, equery/3]).
-export([start/2, stop/1]).
-export([init/1]).

start() ->
	ok = lager:start(),
	application:ensure_all_started(?MODULE),
	{ok, Opts} = application:get_env(gwen, irc),
	gwen_irc_sup:start_link(Opts).

stop() ->
	application:stop(?MODULE).

start(_Type, _Args) ->
	supervisor:start_link({local, gwen_sup}, ?MODULE, []).

stop(_State) ->
	ok.

init([]) ->
	{ok, Pools} = application:get_env(gwen, pools),
	PoolSpecs = lists:map(fun({Name, SizeArgs, WorkerArgs}) ->
		PoolArgs = [{name, {local, Name}},
			{worker_module, gwen_db_worker}] ++ SizeArgs,
		poolboy:child_spec(Name, PoolArgs, WorkerArgs)
	end, Pools),
	{ok, {{one_for_one, 10, 10}, PoolSpecs}}.

squery(PoolName, Sql) ->
	poolboy:transaction(PoolName, fun(Worker) ->
		gen_server:call(Worker, {squery, Sql})
	end).

equery(PoolName, Stmt, Params) ->
	poolboy:transaction(PoolName, fun(Worker) ->
		gen_server:call(Worker, {equery, Stmt, Params})
	end).