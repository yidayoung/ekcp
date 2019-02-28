%%%-------------------------------------------------------------------
%%% @author wangyida
%%% @copyright (C) 2019, <SKYMOONS>
%%% @doc
%%%
%%% @end
%%% Created : 25. 二月 2019 18:36
%%%-------------------------------------------------------------------
-module(ekcp_handle).
-include("ekcp.hrl").
-behavior(kcp_handle).
%% API
-export([reg/6, route/6, send/4, decode_packet/1, encode_packet/2,
    route_to_server/2, heart/5, open/1, disconnect/5]).

open(Port) ->
    gen_udp:open(Port, [binary, {active, true}]).

%% 收到玩家的udp连接请求,将RoleID和IP端口进行绑定，并创建进程
%% 如果已经存在玩家的信息则进行更新
reg(Ref, From, Socket, IP, InPortNo, Opts) ->
    Opts2 = Opts#{mark => From, ref => Ref},
    case ekcp_lib:worker(Ref, From) of
        Worker when is_pid(Worker) ->
            ekcp_conn_srv:rereg(Worker, Opts2),
            gen_udp:send(Socket, IP, InPortNo, <<?ACT_LOGIN_AGAIN:8>>),
            {OldIP, OldPort} = ekcp_lib:get_address(Ref, From),
            gen_udp:send(Socket, OldIP, OldPort, <<?ACT_LOGIN_OTHER:8>>);
        _ ->
            {ok, GwPid} = ekcp_conn_srv:start(Opts2),
            ets:insert(?TAB, {{connection, Ref, From}, #kcp_conn{ref = Ref, key = From, ip = IP, port = InPortNo, worker = GwPid}}),
            gen_udp:send(Socket, IP, InPortNo, <<?ACT_LOGIN:8>>)
    end,
    ok.


%% 收到的kcp包，转发给handle处理进行进行解包
route(Ref, From, Socket, IP, InPortNo, Binary) ->
    case ekcp_lib:worker(Ref, From) of
        Worker when is_pid(Worker) ->
            case ekcp_lib:get_address(Ref, From) of
                {IP, InPortNo} ->
                    Worker ! {from_udp_msg, From, Binary};
                _ ->
                    gen_udp:send(Socket, IP, InPortNo, <<?ACT_ALREADY_LOGIN:8>>)
            end;
        _ ->
            gen_udp:send(Socket, IP, InPortNo, <<?ACT_NOT_REG:8>>)
    end,
    ok.

%% 收到玩家的回复消息，将消息发送给UDP对端
send(Ref, Socket, Tar, Msg) ->
    case ekcp_lib:get_address(Ref, Tar) of
        {IP, Port} ->
            gen_udp:send(Socket, IP, Port, Msg);
        _ ->
            io:format("want send msg by udp to ~p, buf cant find address~n", [Tar])
    end,
    ok.

%% udp包解包到kcp包
decode_packet(<<0:8, From:32, Msg/binary>>) ->
    {reg, From, Msg};
decode_packet(<<1:8, From:32, Msg/binary>>) ->
    {normal_msg, From, Msg};
decode_packet(<<5:8, From:32, Msg/binary>>) ->
    {heart, From, Msg}.

%% kcp包打包成udp发送包
encode_packet(Tar, Msg) ->
    <<1:8, Tar:32, Msg/binary>>.

route_to_server(From, Msg) ->
    io:format("RoleID ~p got msg:~p~n", [From, Msg]).

heart(Ref, From, Socket, IP, InPortNo) ->
    case ekcp_lib:worker(Ref, From) of
        Worker when is_pid(Worker) ->
            case ekcp_lib:get_address(Ref, From) of
                {IP, InPortNo} ->
                    NowSec = ekcp_lib:nowsec(),
                    gen_udp:send(Socket, IP, InPortNo, <<?ACT_HEART:8, NowSec:32>>),
                    Worker ! heart_beat;
                _ ->
                    gen_udp:send(Socket, IP, InPortNo, <<?ACT_ALREADY_LOGIN:8>>)
            end;
        _ ->
            gen_udp:send(Socket, IP, InPortNo, <<?ACT_NOT_REG:8>>)
    end,
    ok.

disconnect(Ref, From, Socket, IP, InPortNo) ->
    case ekcp_lib:worker(Ref, From) of
        Worker when is_pid(Worker) ->
            case ekcp_lib:get_address(Ref, From) of
                {IP, InPortNo} ->
                    ekcp_conn_srv:stop(Worker),
                    gen_udp:send(Socket, IP, InPortNo, <<?ACT_OFF:8>>);
                _ ->
                    gen_udp:send(Socket, IP, InPortNo, <<?ACT_ALREADY_LOGIN:8>>)
            end;
        _ ->
            gen_udp:send(Socket, IP, InPortNo, <<?ACT_NOT_REG:8>>)
    end.