# windows 下可以将kcp库编译成dll，也可以在windows下进行调试

# 基础准备
- vs任意版本
- erlang环境


# 编译步骤
 1. 在vs中新建Visual C++ 的空项目
 2. 将eckp.c, ikcp.c ikcp.h 分别复制到源文件和头文件目录
 3. 修改项目属性，注意是项目不是解决方案，在项目上右键->属性
 4. 首先将最上面的配置选择改成所有配置和所有平台
 5. 在配置属性->常规->项目默认值->配置类型 中将项目类型改成dll
 6. 在配置属性->VC++目录->包含目录  中添加你的erlang，inculde目录，
 比如你的erlang安装在D:\Program Files\erl8.0，那么include目录对应就是D:\Program Files\erl8.0\erts-8.0\include。
 注意是添加，不是覆盖
 7. 修改当前配置，选择为Release，X64（看你自己当前环境，如果是32位操作系统就选X86）
 8. 在项目上右键点击生成，或者F6，输出窗口那会显示产生的dll所在目录
 
# 使用
 将编译好的dll拷贝到priv目录中就可以和linux下一样进行调用了
 
# 注意点
- 选择erlang inculde目录的时候要选你将来运行erlang的版本，虽然说不一定会出问题，但是最好保持一致
- 如果dll要分享给别人使用，同样要注意操作系统是否是X64，还有erlang版本是否一致

 