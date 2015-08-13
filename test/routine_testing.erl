%% @author sergey
%% @doc @todo Add description to routine_testing.


-module(routine_testing).

-include("../src/debug_print.hrl").
-include_lib("eunit/include/eunit.hrl").

%% ====================================================================
%% API functions
%% ====================================================================
-export([mailing/0,setup/0,teardown/0]).

-export([arabmail/0,bigmailing/1,biglogins/1]).

setup() ->
	application:start(chat_srv).

teardown() ->
	application:stop(chat_srv).

mailing() ->
 	%application:start(chat_srv),
	{ok,T}=chat_client:login("tema"),
	{ok,V}=chat_client:login("vova"),
	chat_client:write(T,"vova","hello Vovan"),
	chat_client:write(T,"vova","hello Vovan 2"),
	chat_client:write(V,"tema","hello Tema"),
	{ok,_V2}=chat_client:login("vova"), % one more login, but no new username
	arab(),
	U = chat_client:users(T), 
	{Uon,Uall} = chat_client:users_bare(T),
	?assertEqual(length(Uon),2), % 2 users online
	?assertEqual(length(Uall),3), % users total
	H = chat_client:history(V, "tema"),
	chat_client:logout(T),

	UV = chat_client:users(V),
	

	?assertEqual(length(H),3), % 3 messages
	?assertEqual(length(U),3), % 3 usernames
	?assertEqual(length(UV),3). % 3 usernames
	 
	%application:stop(chat_srv).

bigmailing(N) ->
	{ok,T}=chat_client:login("tema"),
	To = "vova2",
	[onemail(T,To,K) || K <- lists:seq(1,N) ].
	
onemail(Client,To,K) ->	
	Body = 	io_lib:format("auto mail Number ~b to ~s",[K,To]),				 	
	chat_client:write(Client,To,Body).

biglogins(N) ->
	Name = "arab2",
	{ok,A0}=chat_client:login(Name),
	?printf("testing ~b logins (of ~s)",[N,Name]),
	L = [ onelogin(Name) || _K <- lists:seq(1,N) ], 
	?printf("all logins done",[]),
	[ onelogout(K) || K <- L ],	
	?printf("all logouts done",[]),
	
	{ok,_Msg_id}=chat_client:write(A0,Name,"test mail to myself"),
	chat_client:logout(A0).	

onelogin(Name) -> 
	{ok,A}=chat_client:login(Name),
	A.

onelogout(Client) ->
	chat_client:logout(Client).

arab() ->
	{ok,A}=chat_client:login("arab"),
	chat_client:logout(A).	

arabmail() ->
	{ok,A}=chat_client:login("arab"),
	unlink(A),
	chat_client:write(A,"vova","hello Vovan -- arabmail()"),
	chat_client:logout(A).
%% ====================================================================
%% Internal functions
%% ====================================================================


