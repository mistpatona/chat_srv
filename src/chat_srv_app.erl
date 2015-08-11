%% @author sergey
%% @doc @todo Add description to chat_sup.


-module(chat_app). %also serves as chat_sup -- main supervisor
-behaviour(supervisor).  
-behaviour(application).
-export([init/1]).

%% ====================================================================
%% API functions
%% ====================================================================
-export([start_link/0,
		 start/2,stop/1]).

start(_StartType, _StartArgs) ->
	start_link().

stop(_State) ->
	ok.

start_link() ->
	supervisor:start_link({local,?MODULE},?MODULE, []).



%% ====================================================================
%% Behavioural functions
%% ====================================================================

%% init/1
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/supervisor.html#Module:init-1">supervisor:init/1</a>
-spec init(Args :: term()) -> Result when
	Result :: {ok, {SupervisionPolicy, [ChildSpec]}} | ignore,
	SupervisionPolicy :: {RestartStrategy, MaxR :: non_neg_integer(), MaxT :: pos_integer()},
	RestartStrategy :: one_for_all
					 | one_for_one
					 | rest_for_one
					 | simple_one_for_one,
	ChildSpec :: {Id :: term(), StartFunc, RestartPolicy, Type :: worker | supervisor, Modules},
	StartFunc :: {M :: module(), F :: atom(), A :: [term()] | undefined},
	RestartPolicy :: permanent
				   | transient
				   | temporary,
	Modules :: [module()] | dynamic.
%% ====================================================================
init([]) ->
    Sup_flags  = #{strategy => rest_for_one},
	Stub_sup   = #{id => stub_sup, % runs chat_storage and chat_online
				   start => {chat_stub_sup,start_link,[]},
				   type  => supervisor
				  },
	Msg_srv    = #{id => msg_srv,
				   start => {chat_msg_srv,start_link,[]}
				   },
	Cli_sup    = #{id => cli_sup,
				   start => {chat_cli_sup,start_link,[]},
				   type  => supervisor},
	Login      = #{id => login,
				   start => {chat_login,start_link,[]}
				  },
	{ok,{Sup_flags, [Stub_sup,Msg_srv,Cli_sup,Login]}}.
	
%% ====================================================================
%% Internal functions
%% ====================================================================


