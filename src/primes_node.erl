%%%-------------------------------------------------------------------
%%% @author Alvaro Videla <avidela@avidela.local>
%%% @copyright (C) 2014, Alvaro Videla
%%% @doc
%%%
%%% @end
%%% Created : 15 Feb 2014 by Alvaro Videla <avidela@avidela.local>
%%%-------------------------------------------------------------------
-module(primes_node).

-behaviour(gen_server).

%% API
-export([start_link/2]).

-export([maybe_filter/3, value/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {k, coord}).

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
start_link(K, Cid) ->
    gen_server:start_link(?MODULE, [K, Cid], []).

maybe_filter(Pid, Curr, Max) ->
    gen_server:cast(Pid, {maybe_filter, Curr, Max}).

value(Pid) ->
    gen_server:call(Pid, {value}).

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
init([K, Cid]) ->
    {ok, #state{k = K, coord = Cid}}.

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
handle_call({value}, _From, #state{k = K} = State) ->
    {reply, {ok, K}, State};

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
handle_cast({maybe_filter, N, Max}, #state{k = K, coord = Cid} = State) ->
    case internal_maybe_filter(K, N, Max) of
        true ->
            primes_coordinator:delete(Cid, K),
            {stop, normal, State};
        false ->
            {noreply, State}
    end;

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

internal_maybe_filter(K, N, Max) when N < Max->
    case K > N of
        true ->
            Residue = N div 2,
            case K rem N =:= Residue of
                true ->
                    true;
                false ->
                    internal_maybe_filter(K, N+2, Max)
            end;
        false ->
            false
    end;

internal_maybe_filter(K, N, _Max) ->
    case K > N of
        true ->
            Residue = N div 2,
            case K rem N =:= Residue of
                true ->
                    true;
                false ->
                    false
            end;
        false ->
            false
    end.
