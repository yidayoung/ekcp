%%%-------------------------------------------------------------------
%%% @author wangyida
%%% @copyright (C) 2019, <SKYMOONS>
%%% @doc
%%%
%%% @end
%%% Created : 23. 二月 2019 15:53
%%%-------------------------------------------------------------------
-module(ekcp_conn_srv).


-behaviour(gen_server).

%% API
-export([start_link/1, start/1, rereg/2, stop/1]).

%% gen_server callbacks
-export([init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {kcp, mark, opts, ref, handle_module, last_tick}).

-define(INTERVAL, 20).
-define(HEART_TIME_OUT, 60).
-define(HEART_CHECK_INTERVAL, 1000 * 10).

%%%===================================================================
%%% API
%%%===================================================================

start(#{mark:=Mark, ref:=Ref} = Args) ->
    {ok, Child} = supervisor:start_child(ekcp_conn_sup
        , {{?MODULE, Ref, Mark}, {?MODULE, start_link, [Args]}
            , temporary, 60000, worker, [?MODULE]}),
    Child ! start_init,
    {ok, Child}.

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link(Args :: term()) ->
    {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link(Opts) ->
    gen_server:start_link(?MODULE, Opts, []).


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
    process_flag(trap_exit, true),
    {ok, #state{opts = Opts}}.

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
handle_cast(stop, State) ->
    {stop, normal, State};
handle_cast({rerege,Opts}, #state{kcp=Kcp} = _State) ->
    ekcp:release(Kcp),
    {ok, NewState} = init_state(Opts),
    {noreply, NewState};
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
handle_info(start_init, #state{opts = Opts} = _State) ->
    {ok, NewState} = init_state(Opts),
    erlang:send_after(?INTERVAL, self(), update),
    erlang:send_after(?HEART_CHECK_INTERVAL, self(), heart_check),
    {noreply, NewState};
handle_info(Info, State) ->
    case catch (do_handle_info(Info, State)) of
        {noreply, NewState} ->
            {noreply, NewState};
        Err ->
            io:format("Err:~p~n", [Err]),
            {noreply, State}
    end.

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
terminate(_Reason, #state{kcp = Kcp, ref = Ref, mark = Mark} = _State) ->
    ekcp:release(Kcp),
    ekcp_lib:clear(Ref, Mark, self()),
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

%% 这里获得的值必须不能超过int32，否则c逻辑那边会出错
millis() ->
    {_MegaSecs, Secs, MicroSecs} = erlang:timestamp(),
    (Secs * 1000 + MicroSecs div 1000) rem 36000000.
nowsec() ->
    {M, S, _} = os:timestamp(),
    M * 1000000 + S.


%% 从udp层解包后得到的kcp包体
do_handle_info({from_udp_msg, From, Binary}, #state{kcp = Kcp, handle_module = HandleModule} = State) ->
    io:format("got msg from udp:~p~n", [Binary]),
    ekcp:input(Kcp, Binary),
    flush(Kcp, From, HandleModule),
    {noreply, State#state{last_tick = nowsec()}};
do_handle_info(update, #state{kcp = Kcp} = State) ->
    ekcp:update(Kcp, millis()),
    erlang:send_after(?INTERVAL, self(), update),
    {noreply, State};
do_handle_info(heart_beat, State) ->
    {noreply, State#state{last_tick = nowsec()}};
do_handle_info(heart_check, #state{last_tick = LastTick} = State) ->
    NowSec = nowsec(),
    if
        NowSec - LastTick > ?HEART_TIME_OUT ->
            gen_server:cast(self(), stop);
        true ->
            erlang:send_after(?HEART_CHECK_INTERVAL, self(), heart_check)
    end,
    {noreply, State};
%% 这里的Msg并不一定和ekcp:send的入参一样，如果单条消息很长，会被拆分成多个包，而一个kcp_msg只是一个包
%% 回复的ack包也是走这条逻辑
do_handle_info({kcp_msg, Msg}, #state{ref = Ref, mark = Mark, handle_module = HandleModule} = State) ->
    io:format("receive kcp_msg:~p~n",[Msg]),
    Listener = ekcp_lib:listener(Ref),
    MsgBody = HandleModule:encode_packet(Mark, Msg),
    Listener ! {send_udp_msg, Mark, MsgBody},
    {noreply, State};
do_handle_info({send_msg, Msg}, #state{kcp = Kcp} = State) ->
    io:format("send msg ~p~n", [Msg]),
    ekcp:send(Kcp, Msg),
    ekcp:update(Kcp, millis()),
    {noreply, State}.

rereg(Pid, Opts) ->
    gen_server:cast(Pid, {rerege, Opts}).

init_state(Opts) ->
    Mark = maps:get(mark, Opts),
    io:format("Mark:~p~n",[Mark]),
    {ok, Kcp} = ekcp:create(Mark, self()),

    SndWnd = maps:get(sndwnd, Opts, 32),
    RcvWnd = maps:get(rcvwnd, Opts, 32),
    ekcp:wndsize(Kcp, SndWnd, RcvWnd),

    NoDelay = maps:get(nodelay, Opts, 0),
    Interval = maps:get(interval, Opts, 10),
    Resend = maps:get(resend, Opts, 0),
    Nc = maps:get(nc, Opts, 0),
    ekcp:nodelay(Kcp, NoDelay, Interval, Resend, Nc),

    HandleModule = maps:get(handle_module, Opts),
    Ref = maps:get(ref, Opts),
    {ok, #state{kcp = Kcp, mark = Mark, handle_module = HandleModule, ref = Ref, last_tick = nowsec(), opts = Opts}}.

flush(Kcp, From, HandleModule) ->
    Msg = ekcp:recv_data(Kcp),
    case Msg of
        nil ->
            pass;
        _ ->
            HandleModule:route_to_server(From, Msg),
            flush(Kcp, From, HandleModule)
    end.

stop(Pid) ->
    gen_server:cast(Pid, stop).