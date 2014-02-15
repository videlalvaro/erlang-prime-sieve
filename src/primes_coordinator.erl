%%%-------------------------------------------------------------------
%%% @author Alvaro Videla <avidela@avidela.local>
%%% @copyright (C) 2014, Alvaro Videla
%%% @doc
%%%
%%% @end
%%% Created : 15 Feb 2014 by Alvaro Videla <avidela@avidela.local>
%%%-------------------------------------------------------------------
-module(primes_coordinator).

-behaviour(gen_server).

%% API
-export([start_link/1]).

-export([run/1, delete/2, collect/1, keys/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(START, 3).

-record(state, {tree, n}).

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
start_link(N) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [N], []).

run(Pid) ->
    gen_server:call(Pid, {run}).

delete(Pid, K) ->
    gen_server:cast(Pid, {delete, K}).

collect(Pid) ->
    gen_server:call(Pid, {collect}).

keys(Pid) ->
    gen_server:call(Pid, {keys}).


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
init([N]) ->
    Tree = create_processes(N, gb_trees:empty()),
    {ok, #state{tree = Tree, n = N}}.

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
handle_call({run}, _From, #state{tree = Tree, n = N} = State) ->
    call_nodes(N, Tree),
    Reply = ok,
    {reply, Reply, State};

handle_call({collect}, _From, #state{tree = Tree} = State) ->
    Values = collect_numbers(Tree),
    Reply = {ok, lists:reverse(Values)},
    {reply, Reply, State};

handle_call({keys}, _From, #state{tree = Tree} = State) ->
    Reply = {ok, gb_trees:keys(Tree)},
    {reply, Reply, State};

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

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
handle_cast({delete, K}, #state{tree = Tree} = State) ->
    Tree2 = gb_trees:delete(K, Tree),
    {noreply, State#state{tree = Tree2}};
handle_cast(_Msg, State) ->
    {noreply, State}.

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

create_processes(N, Tree) ->
    create_processes(1, N, Tree).

create_processes(N, N, Tree) ->
    insert_node(N, Tree);
create_processes(Curr, N, Tree) ->
    Tree2 = insert_node(Curr, Tree),
    create_processes(Curr+1, N, Tree2).

insert_node(K, Tree) ->
    {ok, Pid} = primes_node:start_link(K, self()),
    gb_trees:insert(K, Pid, Tree).

call_nodes(N, Tree) ->
    SQRT = math:sqrt(N*2),
    iterate_nodes(?START, round(SQRT), gb_trees:iterator(Tree)).

iterate_nodes(Curr, Max, Iter) ->
    case gb_trees:next(Iter) of
        none ->
            ok;
        {_Key, Pid, Iter2} ->
            primes_node:maybe_filter(Pid, Curr, Max),
            iterate_nodes(Curr, Max, Iter2)
    end.


collect_numbers(Tree) ->
    collect_numbers(gb_trees:iterator(Tree), []).

collect_numbers(Iter, Acc) ->
    case gb_trees:next(Iter) of
        none ->
            Acc;
        {_Key, Pid, Iter2} ->
            {ok, Value} = primes_node:value(Pid),
            collect_numbers(Iter2, [Value*2+1|Acc])
    end.
