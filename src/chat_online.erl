%% @author sergey
%% @doc @todo Add description to chat_online.


-module(chat_online).
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% ====================================================================
%% API functions
%% ====================================================================
-export([start_link/0]).

-export([register/3,unregister/2,
		 get_user_list/1,
		 get_pids_by_name/2]).


start_link() ->
	gen_server:start_link({local,?MODULE},?MODULE, [], []).

register(P,Pid,Login) ->
	gen_server:cast(P,{register,Pid,Login}).

unregister(P,Pid) ->
	gen_server:cast(P,{unregister,Pid}).

get_pids_by_name(P,Login) ->
	L = get_online_list(P),
	[Pid || {Pid,User} <- L , User =:= Login].

get_online_list(P) ->
	gen_server:call(P,get_online_list).


get_user_list(P) ->
	gen_server:call(P,get_user_list).

%% ====================================================================
%% Behavioural functions
%% ====================================================================
-record(state, {online,all}).

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
    {ok, #state{online=[],all=sets:new()}}. 
	% "online" is a list [{Pid,Login}], "all" is a set of logins

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
handle_call(get_online_list, _From, #state{online=L}=State) ->
	{reply, L, State};

handle_call(get_offline_list, _From, #state{all=L}=State) ->
	{reply, L, State};

handle_call(get_user_list, _From, #state{online=On,all=All}=State) ->
	{reply, {lists:usort([L||{_Pid,L}<-On]),lists:sort(sets:to_list(All))}, State};

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

handle_cast({register,Pid,Login},#state{online=Lonline,all=Sall}=State) ->
	{noreply,State#state{	online= [{Pid,Login}|Lonline],
							all   = sets:add_element(Login,Sall) 
				   		}};

handle_cast({unregister,Pid},#state{online=L}=State) ->
	io:format("chat_online: unregistering ~p~n",[Pid]),
	{noreply,State#state{online=
							 lists:filter(fun({X,_})-> X=/=Pid end,L)
						}};

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


%% =================================================
%% =============== store TESTS here ================
%% =================================================

% tests moved from separate file
% to comply with rebar3 eunit command



-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.


-ifdef(TEST).

start_stop_test() ->
	{ok,P} = start(),
	stop(P).


reg_unreg_test() ->
	{ok,P} = start(),
	chat_online:register(P, aa, a),
	chat_online:register(P, bb, b),
	chat_online:register(P, bbb, b),
	chat_online:register(P, aaa, a),
	
	Pa = chat_online:get_pids_by_name(P,a),
	?assertEqual(length(Pa),2),
	?assertEqual(Pa -- [aa,aaa],[]),
	
	chat_online:unregister(P, ccc), % nonexisting
	chat_online:unregister(P, aaa),
	
	{U,_} = chat_online:get_user_list(P),
	?assertEqual(U,[a,b]), % usernames are uniq'ed and sorted
	chat_online:unregister(P, aa),
	{Ub,_} = chat_online:get_user_list(P),
	?assertEqual(Ub,[b]),
	P_no_a = chat_online:get_pids_by_name(P,a),
	?assertEqual(P_no_a,[]),
	stop(P).

%% ====================================================================
%% Internal functions for tests
%% ====================================================================

start() -> 
	gen_server:start_link(chat_online, [], []).

stop(P) ->
	gen_server:stop(P).

-endif.