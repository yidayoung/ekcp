%%%-------------------------------------------------------------------
%%% @author wangyida
%%% @copyright (C) 2019, <SKYMOONS>
%%% @doc
%%%
%%% @end
%%% Created : 28. 二月 2019 11:11
%%%-------------------------------------------------------------------
-module(kcp_handle).
-type socket() :: port().

%% API
-export([]).

-callback open(Port :: integer()) ->
    {ok, Socket} | {error, Reason} when
    Socket :: socket(),
    Reason :: inet:posix().

-callback reg(Ref, From, Socket, IP, InPortNo, Opts) ->
    ok when
    Ref :: term(),
    From :: term(),
    Socket :: socket(),
    IP :: list(),
    InPortNo :: number(),
    Opts :: map().

-callback route(Ref, From, Socket, IP, InPortNo, Binary) ->
    ok when
    Ref :: term(),
    From :: term(),
    Socket :: socket(),
    IP :: list(),
    InPortNo :: number(),
    Binary :: binary().

-callback send(Ref, Socket, Tar, Msg) ->
    ok when
    Ref :: term(),
    Socket :: socket(),
    Tar :: term(),
    Msg :: binary().

-callback decode_packet(Packet) ->
    {reg, From, Msg}|
    {normal_msg, From, Msg}|
    {heart, From, Msg}|
    {disconnect, From, Msg} when
    Packet :: binary().

-callback encode_packet(Tar, Msg) ->
    Packet when
    Tar :: term(),
    Msg :: binary(),
    Packet :: binary().

-callback heart(Ref, From, Socket, IP, InPortNo) ->
    ok when
    Ref :: term(),
    From :: term(),
    Socket :: term(),
    IP :: list(),
    InPortNo :: integer().

-callback route_to_server(From, Msg) ->
    ok when
    From :: term(),
    Msg :: binary().

-callback disconnect(Ref, From, Socket, IP, InPortNo) ->
    ok when
    Ref :: term(),
    From :: term(),
    Socket :: socket(),
    IP :: list(),
    InPortNo :: integer().
