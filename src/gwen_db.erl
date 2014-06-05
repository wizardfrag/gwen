-module(gwen_db).
-behaviour(gen_server).

-export([start/0, start/1, stop/0]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	terminate/2, code_change/3]).

-export([put/3]).

-record(state, {conn}).

start() ->
	start([]).

start(Args) ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [Args], []).

stop() ->
	lager:info("Stopping Riak DB handler..."),
	gen_server:cast(?MODULE, shutdown).


init(_) ->
	process_flag(trap_exit, true),
	lager:info("Connecting to Riak..."),
	{ok, Pid} = riakc_pb_socket:start_link("127.0.0.1", 8087),
	lager:info("Connected to Riak!"),
	State = #state{conn=Pid},
	pong = riakc_pb_socket:ping(Pid),
	{ok, State}.


handle_call({put, Object}, _From, #state{conn=Conn}=State) ->
	Result = riakc_pb_socket:put(Conn, Object),
	{reply, Result, State};
handle_call(Message, From, State) ->
	lager:info("Call: ~p from ~p with state ~p", [Message, From, State]),
	{reply, ok, State}.

handle_cast(shutdown, State) ->
	{stop, normal, State};
handle_cast(Message, State) ->
	lager:info("Cast: ~p with state ~p", [Message, State]),
	{noreply, State}.

handle_info(Message, State) ->
	io:format("Info: ~p with state ~p", [Message, State]),
	{noreply, State}.

terminate(_Reason, _State) ->
	io:format("Terminating...").

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

put(Bucket, Key, Val) when is_binary(Val) ->
	Object = riakc_obj:new(Bucket, Key, Val),
	gen_server:call(?MODULE, {put, Object});
put(Bucket, Key, Val) ->
	put(Bucket, Key, term_to_binary(Val, [compressed])).