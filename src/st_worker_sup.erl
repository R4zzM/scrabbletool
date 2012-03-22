%%%-------------------------------------------------------------------
%%% @author Rasmus Mattsson <rasmus@rasmus-desktop>
%%% @copyright (C) 2012, Rasmus Mattsson
%%% @doc
%%%
%%% @end
%%% Created : 21 Feb 2012 by Rasmus Mattsson <rasmus@rasmus-desktop>
%%%-------------------------------------------------------------------
-module(st_worker_sup).

-behaviour(supervisor).

%% API
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(DbRef) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, [DbRef]).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart frequency and child
%% specifications.
%%
%% @spec init(Args) -> {ok, {SupFlags, [ChildSpec]}} |
%%                     ignore |
%%                     {error, Reason}
%% @end
%%--------------------------------------------------------------------
init([DbRef]) ->
    RestartStrategy = simple_one_for_one,
    MaxRestarts = 4,
    MaxSecondsBetweenRestarts = 3600,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    Worker = {st_worker, {st_worker, start_link, [DbRef]},
	      temporary, 2000, worker, [st_worker]},

    {ok, {SupFlags, [Worker]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
