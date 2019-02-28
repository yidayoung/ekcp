%%%-------------------------------------------------------------------
%%% @author wangyida
%%% @copyright (C) 2019, <SKYMOONS>
%%% @doc
%%%
%%% @end
%%% Created : 28. 二月 2019 13:42
%%%-------------------------------------------------------------------
-module(ekcp_test_server).


-behaviour(gen_server).

%% API
-export([start_link/1]).

%% gen_server callbacks
-export([init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3]).
-define(SERVER, ?MODULE).

-record(state, {kcp, socket, port, tar}).

-define(INTERVAL, 100).
-define(HEART_INTERVAL, 1000 * 10).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link(Args :: term()) ->
    {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link([Port,_] = Args) ->
    gen_server:start_link({local, erlang:list_to_atom("srv_" ++ erlang:integer_to_list(Port))}, ?MODULE, Args, []).

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
init([Port, Tar]) ->
    process_flag(trap_exit, true),
    erlang:send_after(?INTERVAL, self(), update),
    {ok, Socket} = gen_udp:open(Port, [binary, {active, true}]),
    {ok, Kcp} = ekcp:create(Port, self()),
    ekcp:wndsize(Kcp, 128, 128),
    ekcp:nodelay(Kcp, 0, 10, 0, 0),
    erlang:send_after(?HEART_INTERVAL, self(), heart),
    {ok, #state{kcp = Kcp, socket = Socket, port = Port, tar = Tar}}.

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
terminate(_Reason, #state{kcp = Kcp} = _State) ->
    ekcp:release(Kcp),
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

millis() ->
    {_MegaSecs, Secs, MicroSecs} = erlang:timestamp(),
    (Secs * 1000 + MicroSecs div 1000) rem 36000000.

do_handle_info({udp, Socket, IP, InPortNo, Binary}, #state{kcp = Kcp, socket = Socket} = State) ->
    io:format("receive Binary from udp ~p:~p, ~p~n", [IP, InPortNo, Binary]),
    case Binary of
        <<1:8, _:32, Binary2/binary>> ->
            ekcp:input(Kcp, Binary2),
            flush(Kcp);
        _ ->
            pass
    end,
    {noreply, State};
do_handle_info({kcp_msg, Msg}, #state{socket = Socket, port = Port, tar = Tar} = State) ->
    io:format("kcp want send msg ~p, Tar:~p~n", [Msg, Tar]),
    gen_udp:send(Socket, "127.0.0.1", Tar, <<1:8, Port:32, Msg/binary>>),
    {noreply, State};
do_handle_info(update, #state{kcp = Kcp} = State) ->
    ekcp:update(Kcp, millis()),
    erlang:send_after(?INTERVAL, self(), update),
    {noreply, State};
do_handle_info({send_msg, Msg}, #state{kcp = Kcp} = State) ->
    io:format("send msg ~p~n", [Msg]),
    ekcp:send(Kcp, Msg),
    {noreply, State};
do_handle_info(init, #state{socket = Socket, port = Port, tar = Tar} = State) ->
    io:format("send reg ~p~n", [Port]),
    gen_udp:send(Socket, "127.0.0.1", Tar, <<0:8, Port:32>>),
    {noreply, State};
do_handle_info(reload, #state{socket = Socket, port = Port, kcp = Kcp, tar = Tar} = State) ->
    io:format("reload ~p~n", [Port]),
    ekcp:release(Kcp),
    {ok, NewKcp} = ekcp:create(Port, self()),
    gen_udp:send(Socket, "127.0.0.1", Tar, <<0:8, Port:32>>),
    {noreply, State#state{kcp = NewKcp}};
do_handle_info(heart, #state{socket = Socket, port = Port, tar = Tar} = State) ->
    gen_udp:send(Socket, "127.0.0.1", Tar, <<5:8, Port:32>>),
    erlang:send_after(?HEART_INTERVAL, self(), heart),
    {noreply, State}.

flush(Kcp) ->
    Msg = ekcp:recv_data(Kcp),
    case Msg of
        nil ->
            pass;
        _ ->
            io:format("receive msg from kcp ~p~n", [Msg]),
            flush(Kcp)
    end.