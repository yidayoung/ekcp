%%%-------------------------------------------------------------------
%%% @author wangyida
%%% @copyright (C) 2019, <SKYMOONS>
%%% @doc
%%%
%%% @end
%%% Created : 22. 二月 2019 18:18
%%%-------------------------------------------------------------------
-module(ekcp).
-on_load(init/0).

-type kcp_res() :: binary().  %% Kcp create by eckp:create

%% API
-export([init/0, create/2, recv_data/1, send/2, update/2, check/2, input/2, flush/1, wndsize/3, nodelay/5, release/1, set_mtu/2]).
-export([start_listener/1]).

init() ->
    erlang:load_nif("./priv/ekcp", 0).

-spec create(ID::integer(), Pid::pid()) -> {ok, kcp_res()}|create_err.
create(_ID, _Pid) ->
    erlang:nif_error({module, ?MODULE}, {line, ?LINE}).

-spec recv_data(kcp_res()) -> binary()|nil|res_null.
recv_data(_Kcp) ->
    erlang:nif_error({module, ?MODULE}, {line, ?LINE}).

-spec send(kcp_res(), binary()) -> integer()|res_null.
send(_Kcp, _Msg) ->
    erlang:nif_error({module, ?MODULE}, {line, ?LINE}).

-spec update(kcp_res(), integer()) -> ok|res_null.
update(_Kcp, _Current) ->
    erlang:nif_error({module, ?MODULE}, {line, ?LINE}).

-spec check(kcp_res(), integer()) -> integer()|res_null.
check(_Kcp, _Current) ->
    erlang:nif_error({module, ?MODULE}, {line, ?LINE}).

-spec input(kcp_res(), binary()) -> integer()|res_null.
input(_Kcp, _Msg) ->
    erlang:nif_error({module, ?MODULE}, {line, ?LINE}).

-spec flush(kcp_res()) -> ok|res_null.
flush(_Kcp) ->
    erlang:nif_error({module, ?MODULE}, {line, ?LINE}).

-spec wndsize(kcp_res(), integer(), integer()) -> ok|res_null.
wndsize(_Kcp, _SndWnd, _RcvWnd) ->
    erlang:nif_error({module, ?MODULE}, {line, ?LINE}).

-spec nodelay(kcp_res(), integer(), integer(), integer(), integer()) -> ok|res_null.
nodelay(_Kcp, _NoDelay, _InterVal, _ReSend, _Nc) ->
    erlang:nif_error({module, ?MODULE}, {line, ?LINE}).

-spec release(kcp_res()) -> ok|res_null.
release(_Kcp) ->
    erlang:nif_error({module, ?MODULE}, {line, ?LINE}).

-spec set_mtu(kcp_res(), integer()) -> ok|res_null.
set_mtu(_Kcp, _Mtu) ->
    erlang:nif_error({module, ?MODULE}, {line, ?LINE}).

%%-------------------------------
start_listener(Args) ->
    ekcp_listener_srv:start(Args).
