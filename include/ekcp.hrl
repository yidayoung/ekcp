%%%-------------------------------------------------------------------
%%% @author wangyida
%%% @copyright (C) 2019, <SKYMOONS>
%%% @doc
%%%
%%% @end
%%% Created : 26. 二月 2019 11:22
%%%-------------------------------------------------------------------

-record(kcp_conn, {
    ref,                %% 连接标识，可以理解为信道，用来区分不同端口的连接
    key,                %% 单个连接内的链接标识
    ip,                 %% 链接对端IP
    port,               %% 对端端口
    worker              %% 链接的处理进程
}).

-define(TAB, ets_kcp).

-define(ACT_REG,                0).
-define(ACT_MSG,                1).
-define(ACT_OFF,                2).
-define(ACT_LOGIN,              3).
-define(ACT_LOGIN_AGAIN,        4).
-define(ACT_HEART,              5).
-define(ACT_NOT_REG,            6).
-define(ACT_ALREADY_LOGIN,      7).
-define(ACT_LOGIN_OTHER,        8).

