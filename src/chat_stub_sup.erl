%% @author sergey
%% @doc @todo Add description to chat_stub_sup.


-module(chat_stub_sup).
-behaviour(supervisor).
-export([init/1]).

%% ====================================================================
%% API functions
%% ====================================================================

-export([start_link/0]).

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
	Sup_flags  = #{strategy => one_for_one},
	Storage_child = #{id => storage,
				   start => {chat_storage,start_link,[]}
				  },
	Online_child = #{id => online,
				   start => {chat_online,start_link,[]}
				  },
    {ok,{Sup_flags, [Storage_child,Online_child]}}.

%% ====================================================================
%% Internal functions
%% ====================================================================


