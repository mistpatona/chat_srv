%% @author sergey
%% @doc @todo Add description to routine_testing.


-module(routine_testing).

-include_lib("eunit/include/eunit.hrl").

%% ====================================================================
%% API functions
%% ====================================================================
-export([mailing/0,setup/0,teardown/0]).

-export([arabmail/0]).

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
	%io:format("bkah-blah!~n"),
	chat_client:logout(T),

	UV = chat_client:users(V),
	

	?assertEqual(length(H),3), % 3 messages
	?assertEqual(length(U),3), % 3 usernames
	?assertEqual(length(UV),3). % 3 usernames
	 
	%application:stop(chat_srv).


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


