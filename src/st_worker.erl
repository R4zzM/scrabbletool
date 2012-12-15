%%%-------------------------------------------------------------------
%%% @author  <erasmat@EV78E7D1AF959E>
%%% @copyright (C) 2012, 
%%% @doc
%%%
%%% @end
%%% Created : 21 Feb 2012 by  <erasmat@EV78E7D1AF959E>
%%%-------------------------------------------------------------------
-module(st_worker).

-behaviour(gen_server).

%% API
-export([start_link/1, st_worker_lookup_hook/1, st_worker_lookup_hook_tree/1, st_worker_count_words/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
		 terminate/2, code_change/3]).

-record(state, {db_ref}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(Db) ->
	gen_server:start_link(?MODULE, [Db], []).

st_worker_lookup_hook(Word) ->
	{ok, Pid} = supervisor:start_child(st_worker_sup, []),
	gen_server:call(Pid, {lookup_hook_request, Word}).

st_worker_lookup_hook_tree(Word) ->
	{ok, Pid} = supervisor:start_child(st_worker_sup, []),
	gen_server:call(Pid, {lookup_hook_tree_request, Word}).

st_worker_count_words() ->
	{ok, Pid} = supervisor:start_child(st_worker_sup, []),
	gen_server:call(Pid, {count_words_request}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([Db]) ->
	{ok, #state{db_ref = Db}}.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call(Request, _From, State) ->
	case Request of
		{lookup_hook_request, Word} ->
			Response = lookup_hook(State#state.db_ref, Word),
			{stop, normal, Response, State};
		
		{lookup_hook_tree_request, Word} ->
			Response = lookup_hook_tree(State#state.db_ref, Word),
			{stop, normal, Response, State};
		
		{count_words_request} ->
			Response = count_words(State#state.db_ref),
			{stop, normal, Response, State}
	end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(Msg, State) ->  
	case Msg of 
		stop ->
			{stop, normal, State}
	end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
	{noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
	ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

lookup_hook(Db, Word) ->
	st_database:lookup(Db, Word).

lookup_hook_tree(Db, Word) ->
	MochijsonObj = do_deep_lookup(Db, Word),
	st_mochijson2:encode(MochijsonObj).

do_deep_lookup(Db, Word) -> 
	case st_database:lookup(Db, Word) of
		[] ->
			{struct, [{Word, <<"null">>}]}; % Word doesn't exist in dict
		[{Word, []}] ->
			{struct, [{Word, []}]}; % Word is a leaf
		[{Word, Children}] ->
			{struct, [{Word, lists:map(fun(Elem) -> 
				                 do_deep_lookup(Db, Elem) end, Children)}]}
	end.

count_words(Db) ->
	st_database:count_words(Db).