#include "erl_nif.h"
#include "ikcp.h"
#include <string.h>

#define RECV_BUF_LEN (1024 * 1024) /* 单条消息接收缓冲区大小 */

struct Callback {
    ErlNifPid current_pid;
};

typedef struct _res_t {
    ikcpcb *kcp;
} res_t;

static ErlNifResourceType *KCP_RESOURCE;

#define GET_RESOURCE(env, argc, argv) res_t *res; \
    if(!argc || !enif_get_resource(env, argv[0], KCP_RESOURCE, (void**)&res)) \
        return enif_make_atom(env, "arg_err"); \
    if(!res->kcp) return enif_make_atom(env, "res_null");

#define LOG() printf("file=%s,func=%s,line=%d\n",__FILE__,__FUNCTION__,__LINE__);

int kcp_output_callback(const char *buf, int len, ikcpcb *kcp, void *arg);

//发送动作由C发送消息给erlang PID PID调用gen_udp来完成或者定义你自己的发送函数
int kcp_output_callback(const char *buf, int len, ikcpcb *kcp, void *arg) {
    struct Callback* c = (struct Callback*)arg;
    ErlNifBinary nif_binary;
	enif_alloc_binary(len, &nif_binary);
	memcpy(nif_binary.data, (unsigned char *)buf, len);
    ErlNifEnv* msg_env = enif_alloc_env();
    enif_send(NULL, &(c->current_pid), msg_env, enif_make_tuple(msg_env, 2,
        enif_make_atom(msg_env, "kcp_msg"),
        enif_make_binary(msg_env, &nif_binary)));
    enif_free_env(msg_env);
    return 0;
}

static
ERL_NIF_TERM ekcp_create(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    unsigned int conv = 0;
    if(!enif_get_uint(env, argv[0], &conv)){
		return enif_make_badarg(env);
	}

	struct Callback* c = enif_alloc(sizeof(struct Callback));
	if(!enif_get_local_pid(env, argv[1], &(c -> current_pid))) {
		return enif_make_badarg(env);
	}

    ikcpcb* kcp = ikcp_create(conv, (void*)c);
    if (kcp == NULL) {
            return enif_make_atom(env, "kcp_create_fail");
        }
    kcp->output = kcp_output_callback;
    res_t *res = (res_t*)enif_alloc_resource(KCP_RESOURCE, sizeof(res_t));
    res->kcp = kcp;
    return enif_make_tuple(env, 2, enif_make_atom(env, "ok"), enif_make_resource(env, res));
}


static
ERL_NIF_TERM ekcp_recv(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    GET_RESOURCE(env, argc, argv);
    char *buf = (char *)enif_priv_data(env);
    memset(buf, 0, RECV_BUF_LEN);
    int32_t hr = ikcp_recv(res->kcp, buf, RECV_BUF_LEN);
    if (hr <= 0) {
            return enif_make_atom(env, "nil");
        }
	ErlNifBinary nif_binary;
	enif_alloc_binary(hr, &nif_binary);
	memcpy(nif_binary.data, (unsigned char *)buf, hr);
    return enif_make_binary(env, &nif_binary);
}

static
ERL_NIF_TERM ekcp_send(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    GET_RESOURCE(env, argc, argv);
    ErlNifBinary buf;
    if (!enif_inspect_binary(env, argv[1], &buf)) {
        return enif_make_badarg(env);
    }
    int32_t hr = ikcp_send(res->kcp, (const char *)buf.data, buf.size);
    return enif_make_int(env, hr);
}


static
ERL_NIF_TERM ekcp_update(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    GET_RESOURCE(env, argc, argv);
    IUINT32 current;
    if(!enif_get_uint(env, argv[1], &current)){
		return enif_make_badarg(env);
	}
    ikcp_update(res->kcp, current);
    return enif_make_atom(env, "ok");
}

static
ERL_NIF_TERM ekcp_check(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    GET_RESOURCE(env, argc, argv);
    IUINT32 current;
    if(!enif_get_uint(env, argv[1], &current)){
		return enif_make_badarg(env);
	}
    IUINT32 hr = ikcp_check(res->kcp, current);
    return enif_make_int(env, hr);
}

static
ERL_NIF_TERM ekcp_input(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    GET_RESOURCE(env, argc, argv);
    ErlNifBinary buf;

    if (!enif_inspect_binary(env, argv[1], &buf)) {
        return enif_make_badarg(env);
    }
    int32_t hr = ikcp_input(res->kcp, (const char *)buf.data, buf.size);
    return enif_make_int(env, hr);
}

static
ERL_NIF_TERM ekcp_flush(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    GET_RESOURCE(env, argc, argv);
    ikcp_flush(res->kcp);
    return enif_make_atom(env, "ok");
}

static
ERL_NIF_TERM ekcp_wndsize(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    GET_RESOURCE(env, argc, argv);
    ikcp_flush(res->kcp);
    int32_t sndwnd;
    int32_t rcvwnd;
	if(!enif_get_int(env, argv[1], &sndwnd)){
		return enif_make_badarg(env);
	}
	if(!enif_get_int(env, argv[2], &rcvwnd)){
		return enif_make_badarg(env);
	}
    ikcp_wndsize(res->kcp, sndwnd, rcvwnd);
    return enif_make_atom(env, "ok");
}

static
ERL_NIF_TERM ekcp_nodelay(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    GET_RESOURCE(env, argc, argv);
    ikcp_flush(res->kcp);
    int32_t nodelay,interval,resend,nc;
	if(!enif_get_int(env, argv[1], &nodelay)){
		return enif_make_badarg(env);
	}
	if(!enif_get_int(env, argv[2], &interval)){
		return enif_make_badarg(env);
	}
	if(!enif_get_int(env, argv[3], &resend)){
		return enif_make_badarg(env);
	}
	if(!enif_get_int(env, argv[4], &nc)){
		return enif_make_badarg(env);
	}
    int32_t hr = ikcp_nodelay(res->kcp, nodelay, interval, resend, nc);
    return enif_make_int(env, hr);
}

static
ERL_NIF_TERM ekcp_release(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    GET_RESOURCE(env, argc, argv);
	ikcp_release(res->kcp);
    return enif_make_atom(env, "ok");
}

static
ERL_NIF_TERM ekcp_set_mtu(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    GET_RESOURCE(env, argc, argv);
    int mtu;
    if(!enif_get_int(env, argv[1], &mtu)){
    		return enif_make_badarg(env);
    	}
	ikcp_setmtu(res->kcp, mtu);
    return enif_make_atom(env, "ok");
}

static void nifs_kcp_resource_cleanup(ErlNifEnv* env, void* arg){
    res_t *res = (res_t*)arg;
    if ( NULL != res->kcp) {
    ikcp_release(res->kcp);
    res->kcp = NULL;}
}

/*
 * 所有NIF函数列表定义
 */
static ErlNifFunc nif_funcs[] = {
      {"create", 2, ekcp_create}
    , {"recv_data", 1, ekcp_recv}
    , {"send", 2, ekcp_send}
    , {"update", 2, ekcp_update}
    , {"check", 2, ekcp_check}
    , {"input", 2, ekcp_input}
    , {"flush", 1, ekcp_flush}
    , {"wndsize", 3, ekcp_wndsize}
    , {"nodelay", 5, ekcp_nodelay}
    , {"release", 1, ekcp_release}
    , {"set_mtu", 2, ekcp_set_mtu}
};

static int nif_load(ErlNifEnv * env, void ** priv_data, ERL_NIF_TERM load_info) {
    //ikcp_malloc_hook = enif_alloc;
    //ikcp_free_hook = enif_free;
    KCP_RESOURCE = enif_open_resource_type(env,"ekcp", "kcp_resource", &nifs_kcp_resource_cleanup, ERL_NIF_RT_CREATE, NULL);
    ikcp_allocator(enif_alloc, enif_free);
    *priv_data = enif_alloc(RECV_BUF_LEN);
    //这里不调用了，后面每次使用前都会set
    //memset(buf, 0, RECV_BUF_LEN);
    return 0;
}

static void nif_unload(ErlNifEnv * env, void * priv_data) {
    if(NULL != priv_data){
            enif_free(priv_data);
    		priv_data = NULL;
        }
}

/*
 * 执行初始化NIF库的宏
 */
ERL_NIF_INIT(ekcp, nif_funcs, nif_load, NULL, NULL, nif_unload)