%%%-------------------------------------------------------------------
%%% @author wangyida
%%% @copyright (C) 2019, <SKYMOONS>
%%% @doc
%%%
%%% @end
%%% Created : 26. 二月 2019 11:45
%%%-------------------------------------------------------------------
-module(ekcp_lib).
-include("ekcp.hrl").

%% API
-export([worker/2, get_address/2, clear/3, nowsec/0, listener/1]).

worker(Ref, RoleID) ->
    case ets:lookup(?TAB, {connection, Ref, RoleID}) of
        [{_,#kcp_conn{worker = Worker}}] ->
            Worker;
        _ ->
            undefined
    end.

get_address(Ref, RoleID) ->
    case ets:lookup(?TAB, {connection, Ref, RoleID}) of
        [{_, #kcp_conn{ip = IP, port = Port}}] ->
            {IP, Port};
        _ ->
            undefined
    end.

listener(Ref) ->
    case ets:lookup(?TAB, {listener, Ref}) of
        [{_,Listener}] when is_pid(Listener) ->
            Listener;
        _ ->
            undefined
    end.

clear(Ref, RoleID, Pid) ->
    case worker(Ref, RoleID) of
        Pid ->
            ets:delete(?TAB, {connection, Ref, RoleID});
        _ ->
            pass
    end.

nowsec() ->
    {M, S, _} = os:timestamp(),
    M * 1000000 + S.
