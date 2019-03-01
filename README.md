# 简介
kcp的erlang实现版本，使用的erlang的nif完成，windows下的编译可能不支持。
kcp的源地址 https://github.com/skywind3000/kcp
关于kcp的基础部分不再做介绍，查看原地址的文档即可。

# 上层包装介绍
这里基于udp简单实现了一份链接维护逻辑，并额外加入了一些应用层的打包，如心跳等。
但是总体都是由ekcp_handle模块来统一实现。
ekcp_handle是一个kcp_handle的模板实现，你可以仿照ekcp_handle自定义自己的处理模块。

# 基础使用
1. 服务器启动udp端口监听
    ```erlang
    ekcp:start_listener(
    [#{port=>9000,handle_module=>ekcp_handle, ref=>9000, sndwnd=>128, rcvwnd=>128,
    nodelay => 0, interval=>10, resend =>0, nc =>0}]).
    ```
    - port udp服务要监听的端口
    - handle_module 一个基于kcp_handle模板实现的处理模块
    - ref 用来区分不同的监听端口，类型没有要求，不一定非得是数字
    其他参数对应kcp的 ikcp_nodelay 和 ikcp_wndsize

2. 客户端发起连接请求
    ```erlang
    {ok, Socket} = gen_udp:open(Port, [binary, {active, true}]),
    {ok, Kcp} = ekcp:create(Port, self()),
    ekcp:wndsize(Kcp, 128, 128),
    ekcp:nodelay(Kcp, 0, 10, 0, 0),
    gen_udp:send(Socket, "127.0.0.1", Tar, <<0:8, Port:32>>),
    ```
    - 本地打开一个UDP端口
    - 创建kcp，这里要注意conv也就是ekcp:create的第一个参数必须前后端相同，否则接收到消息也不会处理
    - 设置工作模式
    - 单纯用udp层发起一个建立连接的请求  
    
    上面的行为会导致服务端拉起一个ekcp_conn_srv来专门处理这个“连接”，以后从这个本地端口和IP发往服务器的消息
    都会由对应的ekcp_conn_srv来处理。
    ekcp_conn_srv通过服务端的监听端口，和第一条udp消息中的Port字段，也就是后32位来区分，你可以把Port替换成用户标识

3. 交互  
    连接建立后，就可以通过kcp来实现稳定可靠的消息交互了
    - 客户端
        ```erlang
        ekcp:send(Kcp, Msg)
        ...
        do_handle_info({kcp_msg, Msg}, #state{socket = Socket, port = Port, tar = Tar} = State) ->
            io:format("kcp want send msg ~p, Tar:~p~n", [Msg, Tar]),
            gen_udp:send(Socket, "127.0.0.1", Tar, <<1:8, Port:32, Msg/binary>>),
            {noreply, State};
        ```
        因为UDP是无连接的，所以每条消息必须带上用户标识，使用ekcp.send来完成消息的kcp打包  
        打包后的消息会通过{kcp_msg, Msg} 发送给接收进程，接收进行再加上应用层的打包，就可以通过UDP进行发送
    - 服务端
        服务端的UDP消息都是由监听进程来统一处理，监听进程会调用handle_module指定的处理模块提供的解包方法来做应用层的解包
        解包后通过当前Ref和用户标识将解包后的消息转发给对应的ekcp_conn_srv来处理
        ```erlang
        ekcp:input(Kcp, Binary),
        flush(Kcp, From, HandleModule)
        ...
        flush(Kcp, From, HandleModule) ->
            Msg = ekcp:recv_data(Kcp),
            case Msg of
                nil ->
                    pass;
                _ ->
                    HandleModule:route_to_server(From, Msg),
                    flush(Kcp, From, HandleModule)
            end.
  
        ```  
        ekcp_conn_srv在收到消息后调用ekpc:input将消息入栈，之后不断调用ekcp:recv_data来将消息解包取出  
        取出后调用HandleModule:route_to_server(From, Msg)将消息体路由给要处理的进程

4. 测试  
    `./rebar3 shell` 启动命令行
   ```erlang
   ekcp:start_listener([#{port=>9000,handle_module=>ekcp_handle, ref=>9000, sndwnd=>128, rcvwnd=>128}]).
   ```
   启动服务器
   
   ```erlang
   ekcp_test_server:start_link([9003,9000]).
   srv_9003!init.
   ```
   初始化客户端
   之后调用 `srv_9003!{send_msg, <<"abc">>}.`来完成消息的发送
   
   服务端想主动发送消息可以调用
   `ekcp_lib:worker(9000,9003)!{send_msg, <<"abc">>}`
   
# API
ekcp模块提供了KCP的基础API，大多数和KCP本身没什么区别，这里只介绍必须有所改动的API

- ekcp:create
    ```erlang
    -spec create(ID::integer(), Pid::pid()) -> {ok, kcp_res()}|create_err.
    create(_ID, _Pid) ->
        erlang:nif_error({module, ?MODULE}, {line, ?LINE}).
    ```
    create函数要求传对话描述和一个PID，对话描述就是ikcp的对话描述，这里传入的Pid是为了kcp->output的回调用，
    kcp消息都是通过回调返回的，这里的实现方式是在回调触发的时候，通过发送消息，发送给注册时指定的Pid来完成
    如果你想让其他进程来处理kcp消息，可以替换上面例子中的self调用。
- ekcp:recv_data  
    这里不像ikcp必须要求提供长度，但是实际上长度也是有上限的，也就是单条消息的长度是有限制的，定义在ekcp.c中的
    RECV_BUF_LEN，现在的值是1Kb，这里不要随意的扩展这个定义，而应该专注于让应用层能确保每个包都很小，单个包体大了后在kcp分包，udp分包
    flsuh的多余recv_data操作都会造成性能的下降  
    recv_data返回的数据必然是可靠保序的，如果3包先到了2包还没到，那么recv_data不会返回3包的消息，必须等2包到了才会返回。  
    
    因此有多种情况都会造成udp层是通的，但是消息却无法传输接收，比如客户端的kcp逻辑已经重启过了，但是服务器接收的kcp逻辑还是老的，
    或者客户端的kcp逻辑是老的，服务器的是新的，都会造成这种状况。kcp逻辑的重新注册是很廉价的，客户端什么时候该进行“重启”是很重要的话题
    

- ekcp:update  
    update的调用时机很有说法，但是建议就是简单的循环调用即可，erlang多进程的支持可以让update很廉价，不要尝试发送消息后立刻调用update,
    这并不一定能优化性能，如果一个时间片内有多次小包的发送，这样频繁的调用update反而会造成性能下降，当然也不是不允许。最好在理解update和check之间要如何配合后
    在根据自己项目的需求，进行自定义 。  
    
# 使用
仿照ekcp_handle实现一份你自己的处理模块，注意最好套用kcp_handle行为检查，定义完成后调用start_listener就可以启动服务器监听


# 重要
nif出错会直接导致虚拟机crash，这些内容都没有线上项目验证，如果要使用，请自己进行完整的测试！