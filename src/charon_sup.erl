
-module(charon_sup).

-behaviour(supervisor).

%% API
-export([start_link/0, start_storage/1, delete_storage/1]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).


-spec start_storage(atom()) -> {ok, pid()} | {error, term()}.
start_storage(Type) ->
    SpecProxy = {Type, {charon_storage, start_link, [Type]},
                 temporary, 2000, worker, [charon_storage]},
    case supervisor:start_child(?MODULE, SpecProxy) of
        {ok, Pid} -> {ok, Pid};
        {error, Error} -> {error, Error}
    end.


delete_storage(Type) ->
    supervisor:terminate_child(?MODULE, Type).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    CharonManager = ?CHILD(charon_manager, worker),
    {ok, { {one_for_one, 5, 10}, [CharonManager]} }.

