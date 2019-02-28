%%%-------------------------------------------------------------------
%%% @author wangyida
%%% @copyright (C) 2019, <SKYMOONS>
%%% @doc
%%%
%%% @end
%%% Created : 25. 二月 2019 18:13
%%%-------------------------------------------------------------------
-module(ekcp_listener_srv).
-include("ekcp.hrl").

-behaviour(gen_server).

%% API
-export([start_link/1, start/1]).

%% gen_server callbacks
-export([init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {handle_module, port, socket, opts, ref}).

%%%===================================================================
%%% API
%%%===================================================================
start([#{port:=Port}] = Args) ->
    supervisor:start_child(ekcp_sup
        , {{?MODULE, Port}, {?MODULE, start_link, Args}
            , permanent, 60000, worker, [?MODULE]}).

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link(term()) ->
    {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link(Args) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, Args, []).

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
-spec(init(Args :: term()) ->
    {ok, State :: #state{}} | {ok, State :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term()} | ignore).
init(Opts) ->
    #{port:=Port, handle_module:= HandleModule, ref:= Ref} = Opts,
%%    {ok, Socket} = gen_udp:open(Port, [binary, {active, true}]),
    {ok, Socket} = HandleModule:open(Port),
    ets:insert(?TAB, {{listener, Ref}, self()}),
    {ok, #state{handle_module = HandleModule, port = Port, socket = Socket, ref = Ref, opts = Opts}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #state{}) ->
    {reply, Reply :: term(), NewState :: #state{}} |
    {reply, Reply :: term(), NewState :: #state{}, timeout() | hibernate} |
    {noreply, NewState :: #state{}} |
    {noreply, NewState :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term(), Reply :: term(), NewState :: #state{}} |
    {stop, Reason :: term(), NewState :: #state{}}).
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_cast(Request :: term(), State :: #state{}) ->
    {noreply, NewState :: #state{}} |
    {noreply, NewState :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: #state{}}).
handle_cast(_Request, State) ->
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
-spec(handle_info(Info :: timeout() | term(), State :: #state{}) ->
    {noreply, NewState :: #state{}} |
    {noreply, NewState :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: #state{}}).
%% 单个包体最好不要太大，否则每次input了并不一定可以recv_data成功
%% 如果消息被kcp分包了，每次都会去尝试解包但是必须等到最后一个包到了才能解完
handle_info({udp, Socket, IP, InPortNo, Binary}, #state{socket = Socket, handle_module = HandleModule, ref = Ref, opts = Opts} = State) ->
    case HandleModule:decode_packet(Binary) of
        {reg, From, _Binary2} ->
            HandleModule:reg(Ref, From, Socket, IP, InPortNo, Opts);
        {normal_msg, From, Binary2} ->
            HandleModule:route(Ref, From, Socket, IP, InPortNo, Binary2);
        {heart, From, _Binary2} ->
            HandleModule:heart(Ref, From, Socket, IP, InPortNo);
        {disconnect, From, _Binary2} ->
            HandleModule:disconnect(Ref, From, Socket, IP, InPortNo);
        _ ->
            pass
    end,
    {noreply, State};
handle_info({send_udp_msg, Tar, Msg}, #state{handle_module = HandleModule, socket = Socket, ref = Ref} = State) ->
    HandleModule:send(Ref, Socket, Tar, Msg),
    {noreply, State};
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
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: #state{}) ->
    term()).
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
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #state{},
    Extra :: term()) ->
    {ok, NewState :: #state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
