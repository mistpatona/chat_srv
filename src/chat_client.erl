%% @author sergey
%% @doc @todo Add description to chat_client.


-module(chat_client).
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% ====================================================================
%% API functions
%% ====================================================================
-export([login/1,write/3,logout/1,history/2,users/1]).
-export([notify_message/3,start_link/0]).

-export([get_pid/1]).

start_link() ->
	gen_server:start_link(?MODULE, [], []).

stop(Pid) ->
	gen_server:cast(Pid,stop).

notify_message(Conn,From,Body) ->
	gen_server:cast(Conn,{notify_of_msg,From,Body}).

login(Name) -> 
	{ok,PMy} = ?MODULE:start_link(),
	{ok,P}=chat_login:login(PMy, Name),
	set_pid(PMy,P),
	{ok,PMy}.

logout(Pid) ->
	P=get_pid(Pid),
	io:format("client logout: local:~p, remote:~p~n",[Pid,P]),
	Result=chat_cli:logout(P),
	stop(Pid),
	Result.

write(Pid,To,Body) -> 
	P=get_pid(Pid),
	chat_cli:send(P, To, Body).

history(Pid,Friend) ->
	P=get_pid(Pid),
	H=chat_cli:get_history(P,Friend),
	[lists:flatten(io_lib:format(" ~p->~p: ~p",[F,T,B]))
		%("m: " ++ F ++ "->" ++ T ++ ": " ++ B)
		|| {_Msg,F,T,B} <- H ].

users(Pid) ->
	P=get_pid(Pid),
	{Onl,All}=chat_cli:get_users(P),
	%Ofl = All -- Onl
	Pretty=[
	 case lists:member(X0, Onl) of
	 	true -> "*" ++ pretty_string(X0);
		_    -> pretty_string(X0)
	 end
	||  X0 <-All],
	{{Onl,All},Pretty}.

get_pid(P) ->
	gen_server:call(P,get_remote_pid).

set_pid(P,Pid) ->
	gen_server:cast(P,{set_remote_pid,Pid}).

%% ====================================================================
%% Behavioural functions
%% ====================================================================
-record(state, {}).

%% init/1
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:init-1">gen_server:init/1</a>
-spec init(Args :: term()) -> Result when
	Result :: {ok, State}
			| {ok, State, Timeout}
			| {ok, State, hibernate}
			| {stop, Reason :: term()}
			| ignore,
	State :: term(),
	Timeout :: non_neg_integer() | infinity.
%% ====================================================================
init([]) ->
    {ok, #state{}}.


%% handle_call/3
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:handle_call-3">gen_server:handle_call/3</a>
-spec handle_call(Request :: term(), From :: {pid(), Tag :: term()}, State :: term()) -> Result when
	Result :: {reply, Reply, NewState}
			| {reply, Reply, NewState, Timeout}
			| {reply, Reply, NewState, hibernate}
			| {noreply, NewState}
			| {noreply, NewState, Timeout}
			| {noreply, NewState, hibernate}
			| {stop, Reason, Reply, NewState}
			| {stop, Reason, NewState},
	Reply :: term(),
	NewState :: term(),
	Timeout :: non_neg_integer() | infinity,
	Reason :: term().
%% ====================================================================
handle_call( get_remote_pid,_F,State) ->
	{reply,State,State};


handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.


%% handle_cast/2
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:handle_cast-2">gen_server:handle_cast/2</a>
-spec handle_cast(Request :: term(), State :: term()) -> Result when
	Result :: {noreply, NewState}
			| {noreply, NewState, Timeout}
			| {noreply, NewState, hibernate}
			| {stop, Reason :: term(), NewState},
	NewState :: term(),
	Timeout :: non_neg_integer() | infinity.
%% ====================================================================
handle_cast({notify_of_msg,From,Body}, State) ->
	io:format("~p: ~p~n",[From,Body]),
    {noreply, State};

handle_cast({set_remote_pid,Pid},_State) ->
	{noreply,Pid};

handle_cast(stop,State) ->
	{stop,normal,State};

handle_cast(_Msg, State) ->
    {noreply, State}.


%% handle_info/2
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:handle_info-2">gen_server:handle_info/2</a>
-spec handle_info(Info :: timeout | term(), State :: term()) -> Result when
	Result :: {noreply, NewState}
			| {noreply, NewState, Timeout}
			| {noreply, NewState, hibernate}
			| {stop, Reason :: term(), NewState},
	NewState :: term(),
	Timeout :: non_neg_integer() | infinity.
%% ====================================================================
handle_info(_Info, State) ->
    {noreply, State}.


%% terminate/2
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:terminate-2">gen_server:terminate/2</a>
-spec terminate(Reason, State :: term()) -> Any :: term() when
	Reason :: normal
			| shutdown
			| {shutdown, term()}
			| term().
%% ====================================================================
terminate(_Reason, _State) ->
    ok.


%% code_change/3
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:code_change-3">gen_server:code_change/3</a>
-spec code_change(OldVsn, State :: term(), Extra :: term()) -> Result when
	Result :: {ok, NewState :: term()} | {error, Reason :: term()},
	OldVsn :: Vsn | {down, Vsn},
	Vsn :: term().
%% ====================================================================
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%% ====================================================================
%% Internal functions
%% ====================================================================

pretty_string(X) -> 
	lists:flatten (io_lib:format("~s",[X])).
