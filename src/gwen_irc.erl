-module(gwen_irc).
-behaviour(gen_server).

-export([start_link/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	terminate/2, code_change/3]).

-export([raw/1, nick/1, privmsg/2, notice/2, register_command/3, unregister_command/1, quit/0]).

-include("irc.hrl").
-define(CMD_CHAR, $$).

start_link(Args) ->
	gen_server:start_link({local, gwen_irc}, ?MODULE, Args, []).

init(Args) ->
	process_flag(trap_exit, true),
	Hostname  = proplists:get_value(hostname, Args),
	Port      = proplists:get_value(port, Args),
	Nickname  = proplists:get_value(nickname, Args, "Gwen"),
	Realname  = proplists:get_value(realname, Args, "Gwen Bot"),
	Password  = proplists:get_value(password, Args, ""),
	lager:info("Connecting to IRC on ~s/~p...", [Hostname, Port]),
	{ok, Conn} = gen_tcp:connect(Hostname, Port, [{packet, line}, {active, once}, {recbuf, 512}], 5000),
	lager:info("Connected to IRC"),
	gwen_commands:init(),
	irc_register(Nickname, Realname, Password),
	{ok, #state{connection=Conn, conn_options=Args}}.

handle_call(Request, _From, State) ->
	io:format("Call: ~p | State: ~p~n", [Request, State]),
	{reply, ok, State}.

handle_cast({raw, Msg}, #state{connection=Conn}=State) ->
	send(Conn, Msg),
	{noreply, State};
handle_cast({my_nick, Nickname}, State) ->
	{noreply, State#state{nickname=Nickname}};
handle_cast({command, User, Channel, Command, Message}, State) ->
	case proplists:get_value(Command, State#state.commands) of
		{M,F} ->
			spawn(M, F, [User, Channel, Message, State]);
		_ ->
			privmsg(Channel, "No such command: " ++ Command)
	end,
	{noreply, State};
handle_cast({add_command, C,M,F}, State) ->
	NewCommands = State#state.commands ++ [{C,{M,F}}],
	{noreply, State#state{commands=NewCommands}};
handle_cast({remove_command, Command}, State) ->
	NewCommands = proplists:delete(Command, State#state.commands),
	{noreply, State#state{commands=NewCommands}};
handle_cast(restart, State) ->
	{stop, "Requested", State};
handle_cast(Msg, State) ->
	io:format("Cast: ~p~n", [Msg]),
	{noreply, State}.


handle_info({tcp, Conn, RawLine}, State) ->
	Line = string:tokens(strip_newline(RawLine), " "),
	lager:info("<- ~p", [Line]),
	gwen_db:put(<<"lines">>, undefined, [{date, calendar:local_time()}, {line, list_to_binary(strip_newline(RawLine))}]),
	{ok, NewState} = parse_line(Line, State),
	inet:setopts(Conn, [{active, once}]),
	{noreply, NewState};
handle_info({tcp_closed, _Conn}, State) ->
	{stop, "Connection Closed!", State#state{connection=undefined}};
handle_info(Info, State) ->
	io:format("Info: ~p~n", [Info]),
	{noreply, State}.

irc_register(Nickname, Realname) ->
	raw(io_lib:format("USER ~s . . :~s", [Nickname, Realname])),
	raw(io_lib:format("NICK ~s", [Nickname])).

irc_register(Nickname, Realname, "") ->
	irc_register(Nickname, Realname);
irc_register(Nickname, Realname, Password) ->
	raw("PASSWORD " ++ Password),
	irc_register(Nickname, Realname).

parse_line(["PING"|Token], State) ->
	raw(io_lib:format("PONG ~s", [Token])),
	lager:info("Ping: ~p", [Token]),
	{ok, State};
parse_line([RawUser, "NICK", [$:|NewNick]], State) ->
	User = parse_user(RawUser),
	{ok, NewState} = handle_nick(User, NewNick, State),
	{ok, NewState};
parse_line([RawUser, "PRIVMSG", Target | RawMessage], State) ->
	User = parse_user(RawUser),
	Message = strip_colon(string:join(RawMessage, " ")),
	handle_privmsg(User, Target, Message),
	{ok, State};
parse_line([_Server,"433",_,_Nickname|_], State) ->
	Newnick = State#state.nickname ++ "1",
	raw("NICK " ++ Newnick);
parse_line([_Server,"001",Nickname|_Rest], State) ->
	raw("JOIN #rartm,#rartm.dev"),
	{ok, State#state{nickname=Nickname}};
parse_line(_Line, State) ->
	{ok, State}.

parse_user(User) ->
	[Nickname, Ident, Host] = string:tokens(strip_colon(User), "!@"),
	#gwen_user{nickname=Nickname, ident=Ident, host=Host}.

handle_nick(User, NewNick, State) when User#gwen_user.nickname =:= State#state.nickname ->
	{ok, State#state{nickname=NewNick}};
handle_nick(_User, _NewNick, State) ->
	{ok, State}.

handle_privmsg(User, _Target, [1,$V,$E,$R,$S,$I,$O,$N,1]) ->
	{ok, Vsn} = application:get_key(gwen, vsn),
	notice(User#gwen_user.nickname, io_lib:format("\1VERSION Erlang IRC Bot ~s - Copyright 2014\1", [Vsn]));
handle_privmsg(User, [$#|_]=Channel, [?CMD_CHAR|Message]) ->
	[Command|Rest] = string:tokens(Message, [$\s]),
	gen_server:cast(?MODULE, {command, User, Channel, Command, Rest});
handle_privmsg(_User, [$#|_]=_Channel, _Message) ->
	ok;
	% lager:info("User: ~p Channel: ~p Message: ~p", [lager:pr(User, ?MODULE), Channel, Message]);
handle_privmsg(User, _Me, Message) ->
	raw(io_lib:format("NOTICE ~s :~s", [User#gwen_user.nickname, Message])).

nick(Nickname) ->
	raw(io_lib:format("NICK ~s", [Nickname])).

notice(_Target, []) ->
	ok;
notice(Target, Message) ->
	Messages = string:tokens(Message, "\r\n"),
	send_multiline("NOTICE", Target, Messages).

privmsg(_Target, []) ->
	ok;
privmsg(Target, Message) ->
	Messages = string:tokens(io_lib:format("~s", [Message]), "\r\n"),
	send_multiline("PRIVMSG", Target, Messages).

send_multiline(_Type, _Target, []) ->
	ok;
send_multiline(Type, Target, [H|T]) ->
	raw(io_lib:format("~s ~s :~s", [Type, Target, H])),
	send_multiline(Type, Target, T).

terminate(_Reason, #state{connection=_Conn}) ->
	ok = raw("QUIT"),
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

send(Sock, Msg) ->
	lager:info("-> ~s~n", [Msg]),
	gen_tcp:send(Sock, Msg ++ "\r\n").

raw(Msg) ->
	gen_server:cast(?MODULE, {raw, Msg}).

strip_newline(Line) ->
	string:strip(string:strip(Line, right, $\n), right, $\r).

%% We do it using this and not string:strip because of
%% people that send emoticons and shiznit
strip_colon([$:|Rest]) ->
	Rest;
strip_colon(Word) ->
	Word.

register_command(Command, Module, Func) ->
	gen_server:cast(?MODULE, {add_command, Command, Module, Func}),
	lager:info("Registered new command: ~s:~s/4 is available as ~s", [Module, Func, Command]).

unregister_command(Command) ->
	gen_server:cast(?MODULE, {remove_command, Command}),
	lager:info("Unregistered command: ~s", [Command]).

quit() ->
	raw("QUIT :Goodbye and thanks for all the fish").