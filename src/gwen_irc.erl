-module(gwen_irc).
-behaviour(gen_server).

-export([start_link/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	terminate/2, code_change/3]).

-export([notify/1]).

-record(state, {connection, conn_options}).

start_link(Args) ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, Args, [{debug, [trace, log]}]).

init(Args) ->
	Hostname = proplists:get_value(hostname, Args),
	Port     = proplists:get_value(port, Args),
	Pid      = gwen_irc_handler:connect(Hostname, Port),
	{ok, #state{connection=Pid, conn_options=Args}}.

handle_call({msg, Target, Msg}, _From, #state{connection=Conn}=State) ->
	RawMsg = "PRIVMSG " ++ Target ++ ":" ++ Msg,
	{reply, send(Conn, RawMsg), State};
handle_call(Request, _From, State) ->
	io:format("Call: ~p~n", [Request]),
	{reply, ok, State}.

handle_cast(Msg, State) ->
	lager:info("Cast: ~p~n", [Msg]),
	{noreply, State}.

handle_info(Info, State) ->
	lager:info("Info: ~p~n", [Info]),
	{noreply, State}.

terminate(_Reason, #state{connection=Conn}) ->
	ok = send(Conn, "QUIT"),
	ok = gen_tcp:close(Conn),
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

send(Sock, Msg) ->
	lager:info("-> ~s~n", [Msg]),
	gen_tcp:send(Sock, Msg ++ "\r\n").

notify(Msg) ->
	gen_server:call(?MODULE, Msg).