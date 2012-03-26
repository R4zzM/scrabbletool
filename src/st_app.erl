%%%-------------------------------------------------------------------
%%% @author Rasmus Mattsson <rasmus@rasmus-desktop>
%%% @copyright (C) 2012, Rasmus Mattsson
%%% @doc
%%%
%%% @end
%%% Created : 21 Feb 2012 by Rasmus Mattsson <rasmus@rasmus-desktop>
%%%-------------------------------------------------------------------
-module(st_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

-export([lookup_hook/1, lookup_hook_tree/1, count_words/0]).

%% -export([behaviour_info/1]).

-record(state, {timestamp_start}).

%%%===================================================================
%%% Callback behaviours
%%%===================================================================

%% behaviour_info(callbacks) ->
%% 	[{handle_lookup_hook_response, 1},
%% 	{handle_lookup_hook_tree_response, 1},
%% 	{handle_count_words_response, 1}];
%% behaviour_info(_Other) ->
%% 	undefined.

%%%===================================================================
%%% Application callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called whenever an application is started using
%% application:start/[1,2], and should start the processes of the
%% application. If the application is structured according to the OTP
%% design principles as a supervision tree, this means starting the
%% top supervisor of the tree.
%%
%% @spec start(StartType, StartArgs) -> {ok, Pid} |
%%                                      {ok, Pid, State} |
%%                                      {error, Reason}
%%      StartType = normal | {takeover, Node} | {failover, Node}
%%      StartArgs = term()
%% @end
%%
%%--------------------------------------------------------------------
start(normal, File) ->
	AbsolutePath = code:priv_dir(scrabbletool) ++ "/testdict.txt", 
	{ok, DbRef, WordsProcessed, WordsInDb} = st_database:new(saol_wordlist, AbsolutePath, flat),
	case st_sup:start_link(DbRef) of
		{ok, Pid} ->
			{ok, Pid, #state{timestamp_start = now()}};
		Error ->
			Error
	end.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called whenever an application has stopped. It
%% is intended to be the opposite of Module:start/2 and should do
%% any necessary cleaning up. The return value is ignored.
%%
%% @spec stop(State) -> void()
%% @end
%%--------------------------------------------------------------------
stop(State) ->
	Uptime = diff_in_gregorian_seconds(State#state.timestamp_start, now()),
	io:format("Scrabbletool server stopped. Was alive for ~w seconds~n", [Uptime]),
	ok.

%%%===================================================================
%%% API
%%%===================================================================

lookup_hook(Word) ->
	st_worker:st_worker_lookup_hook(Word).

lookup_hook_tree(Word) ->
	st_worker:st_worker_lookup_hook_tree(Word).	

count_words() ->
	st_worker:st_worker_count_words().


%%%===================================================================
%%% Internal functions
%%%===================================================================

% Timestamp2 is more recent than Timestamp1 => Timestamp2 - Timestamp1 > 0
diff_in_gregorian_seconds(NowTime1, NowTime2) ->
	GregorianSeconds1 = calendar:datetime_to_gregorian_seconds(calendar:now_to_datetime(NowTime1)),
	GregorianSeconds2 = calendar:datetime_to_gregorian_seconds(calendar:now_to_datetime(NowTime2)),
	GregorianSeconds2 - GregorianSeconds1.
