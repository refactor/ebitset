#include <erl_nif.h>
#include <errno.h>

#include "cbitset/bitset.h"
#include "mylog.h"

static ERL_NIF_TERM ATOM_OK;
static ERL_NIF_TERM ATOM_TRUE;
static ERL_NIF_TERM ATOM_FALSE;

typedef struct cbs_context_s {
    bitset_t * b;
} cbs_context_t;

void cbs_dtor(ErlNifEnv *env, void *obj) {
    cbs_context_t *ctx = (cbs_context_t*)obj;
    if (ctx) {
        bitset_free(ctx->b);
        ctx->b = NULL;
    }
}

static ERL_NIF_TERM new(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    ErlNifResourceType* res_type = (ErlNifResourceType*)enif_priv_data(env);
    cbs_context_t *res = (cbs_context_t*)enif_alloc_resource(res_type, sizeof(*res));
    res->b = bitset_create();

    ERL_NIF_TERM ret = enif_make_resource(env, res);
    enif_release_resource(res);
    return ret;
}

static ERL_NIF_TERM count(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    cbs_context_t *res = NULL;
    ErlNifResourceType* res_type = (ErlNifResourceType*)enif_priv_data(env);
    if (argc != 1 || !enif_get_resource(env, argv[0], res_type, (void**)&res))
        return enif_make_badarg(env);
    const size_t cnt = bitset_count(res->b);
    LOG("cnt: %u", cnt);
    return enif_make_uint64(env, cnt);
}

static int nifload(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info) {
    *priv_data = enif_open_resource_type(env, NULL,
                                         "cbitset",
                                         cbs_dtor,
                                         ERL_NIF_RT_CREATE|ERL_NIF_RT_TAKEOVER,
                                         NULL);
    if (*priv_data == NULL)
        return ENOMEM;

    ATOM_OK = enif_make_atom(env, "ok");
    ATOM_TRUE = enif_make_atom(env, "true");
    ATOM_FALSE = enif_make_atom(env, "false");

    LOG("nifload...priv_data(%p)-> %p", priv_data, *priv_data);
    return 0;
}

static ErlNifFunc nif_funcs[] = {
    {"new",  0, new},
    {"count",  1, count},
};
    
ERL_NIF_INIT(ebitset, nif_funcs, nifload, NULL,NULL,NULL)
