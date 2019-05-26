#include <erl_nif.h>
#include <limits.h>
#include <errno.h>

#include "cbitset/bitset.h"
#include "mylog.h"

static ERL_NIF_TERM ATOM_OK;
static ERL_NIF_TERM ATOM_TRUE;
static ERL_NIF_TERM ATOM_FALSE;

typedef struct cbs_context_s {
    ErlNifRWLock* rwlock;
    bitset_t * b;
} cbs_context_t;

void cbs_dtor(ErlNifEnv *env, void *obj) {
    cbs_context_t *ctx = (cbs_context_t*)obj;
    if (ctx) {
        if (ctx->b) {
            bitset_free(ctx->b);
            ctx->b = NULL;
        }
        if (ctx->rwlock) {
            enif_rwlock_destroy(ctx->rwlock);
            ctx->rwlock = NULL;
        }
    }
}

static ERL_NIF_TERM new(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    ErlNifResourceType* res_type = (ErlNifResourceType*)enif_priv_data(env);
    cbs_context_t *res = (cbs_context_t*)enif_alloc_resource(res_type, sizeof(*res));
    res->rwlock = enif_rwlock_create("bitset-rwlock");
    res->b = bitset_create();

    ERL_NIF_TERM ret = enif_make_resource(env, res);
    enif_release_resource(res);
    return ret;
}

static ERL_NIF_TERM new_from_rawbinary(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    ErlNifResourceType* res_type = (ErlNifResourceType*)enif_priv_data(env);
    cbs_context_t *res = (cbs_context_t*)enif_alloc_resource(res_type, sizeof(*res));
    res->rwlock = enif_rwlock_create("bitset-rwlock");
    res->b = bitset_create();

    ErlNifBinary bin;
    if (argc != 1 ||
            !enif_inspect_binary(env, argv[0], &bin)) {
        return enif_make_badarg(env);
    }
    const int32_t num = bin.size / sizeof(uint32_t);
    if (num > TILE_SIZE * TILE_SIZE) {
        return enif_raise_exception(env, enif_make_string(env, "overflow tile", ERL_NIF_LATIN1));
    }

    bitset_set_list(res->b, (uint32_t*)bin.data, bin.size/(sizeof(uint32_t)));

    ERL_NIF_TERM ret = enif_make_resource(env, res);
    enif_release_resource(res);
    return ret;
}

static ERL_NIF_TERM copy(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    ErlNifResourceType* res_type = (ErlNifResourceType*)enif_priv_data(env);
    cbs_context_t *src = NULL;
    if (argc != 1 ||
            !enif_get_resource(env, argv[0], res_type, (void**)&src))
        return enif_make_badarg(env);
    
    enif_rwlock_rlock(src->rwlock);
    bitset_t* newbitset = bitset_copy(src->b);
    enif_rwlock_runlock(src->rwlock);

    cbs_context_t *res = (cbs_context_t*)enif_alloc_resource(res_type, sizeof(*res));
    res->b = newbitset;
    res->rwlock = enif_rwlock_create("bitset-rwlock");

    ERL_NIF_TERM ret = enif_make_resource(env, res);
    enif_release_resource(res);
    return ret;
}

static ERL_NIF_TERM set_union(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    ErlNifResourceType* res_type = (ErlNifResourceType*)enif_priv_data(env);
    cbs_context_t *s1 = NULL;
    cbs_context_t *s2 = NULL;
    if (argc != 2 ||
            !enif_get_resource(env, argv[0], res_type, (void**)&s1) ||
            !enif_get_resource(env, argv[1], res_type, (void**)&s2))
        return enif_make_badarg(env);

    enif_rwlock_rlock(s1->rwlock);
    bitset_t *newbitset = bitset_copy(s1->b);
    enif_rwlock_runlock(s1->rwlock);

    enif_rwlock_rlock(s2->rwlock);
    bitset_inplace_union(newbitset, s2->b);
    enif_rwlock_runlock(s2->rwlock);

    cbs_context_t *res = (cbs_context_t*)enif_alloc_resource(res_type, sizeof(*res));
    res->b = newbitset;
    res->rwlock = enif_rwlock_create("bitset-rwlock");
    ERL_NIF_TERM ret = enif_make_resource(env, res);
    enif_release_resource(res);
    return ret;
}

static ERL_NIF_TERM union_count(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    ErlNifResourceType* res_type = (ErlNifResourceType*)enif_priv_data(env);
    cbs_context_t *s1 = NULL;
    cbs_context_t *s2 = NULL;
    if (argc != 2 ||
            !enif_get_resource(env, argv[0], res_type, (void**)&s1) ||
            !enif_get_resource(env, argv[1], res_type, (void**)&s2))
        return enif_make_badarg(env);

    enif_rwlock_rlock(s1->rwlock);
    enif_rwlock_rlock(s2->rwlock);
    uint32_t uc = bitset_union_count(s1->b, s2->b);
    enif_rwlock_runlock(s1->rwlock);
    enif_rwlock_runlock(s2->rwlock);
    return enif_make_uint(env, uc);
}

static ERL_NIF_TERM intersection(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    ErlNifResourceType* res_type = (ErlNifResourceType*)enif_priv_data(env);
    cbs_context_t *s1 = NULL;
    cbs_context_t *s2 = NULL;
    if (argc != 2 ||
            !enif_get_resource(env, argv[0], res_type, (void**)&s1) ||
            !enif_get_resource(env, argv[1], res_type, (void**)&s2))
        return enif_make_badarg(env);

    enif_rwlock_rlock(s1->rwlock);
    bitset_t* newbitset = bitset_copy(s1->b);
    enif_rwlock_runlock(s1->rwlock);

    enif_rwlock_rlock(s2->rwlock);
    bitset_inplace_intersection(newbitset, s2->b);
    enif_rwlock_runlock(s2->rwlock);

    cbs_context_t *res = (cbs_context_t*)enif_alloc_resource(res_type, sizeof(*res));
    res->rwlock = enif_rwlock_create("bitset-rwlock");
    res->b = newbitset;
    ERL_NIF_TERM ret = enif_make_resource(env, res);
    enif_release_resource(res);
    return ret;
}

static ERL_NIF_TERM intersects(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    ErlNifResourceType* res_type = (ErlNifResourceType*)enif_priv_data(env);
    cbs_context_t *s1 = NULL;
    cbs_context_t *s2 = NULL;
    if (argc != 2 ||
            !enif_get_resource(env, argv[0], res_type, (void**)&s1) ||
            !enif_get_resource(env, argv[1], res_type, (void**)&s2))
        return enif_make_badarg(env);

    enif_rwlock_rlock(s1->rwlock);
    enif_rwlock_rlock(s2->rwlock);
    bool ret = bitset_intersects(s1->b, s2->b);
    enif_rwlock_runlock(s1->rwlock);
    enif_rwlock_runlock(s2->rwlock);

    if (ret) {
        return ATOM_TRUE;
    }
    return ATOM_FALSE;
}

static ERL_NIF_TERM difference(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    ErlNifResourceType* res_type = (ErlNifResourceType*)enif_priv_data(env);
    cbs_context_t *s1 = NULL;
    cbs_context_t *s2 = NULL;
    if (argc != 2 ||
            !enif_get_resource(env, argv[0], res_type, (void**)&s1) ||
            !enif_get_resource(env, argv[1], res_type, (void**)&s2))
        return enif_make_badarg(env);

    enif_rwlock_rlock(s1->rwlock);
    bitset_t* newbitset = bitset_copy(s1->b);
    enif_rwlock_runlock(s1->rwlock);

    enif_rwlock_rlock(s2->rwlock);
    bitset_inplace_difference(newbitset, s2->b);
    enif_rwlock_runlock(s2->rwlock);

    cbs_context_t *res = (cbs_context_t*)enif_alloc_resource(res_type, sizeof(*res));
    res->rwlock = enif_rwlock_create("bitset-rwlock");
    res->b = newbitset;
    ERL_NIF_TERM ret = enif_make_resource(env, res);
    enif_release_resource(res);
    return ret;
}

static ERL_NIF_TERM count(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    cbs_context_t *res = NULL;
    ErlNifResourceType* res_type = (ErlNifResourceType*)enif_priv_data(env);
    if (argc != 1 || !enif_get_resource(env, argv[0], res_type, (void**)&res))
        return enif_make_badarg(env);

    enif_rwlock_rlock(res->rwlock);
    const int32_t cnt = bitset_precount(res->b);
    enif_rwlock_runlock(res->rwlock);

    return enif_make_int(env, cnt);
}

static ERL_NIF_TERM minimum(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    cbs_context_t *res = NULL;
    ErlNifResourceType* res_type = (ErlNifResourceType*)enif_priv_data(env);
    if (argc != 1 || !enif_get_resource(env, argv[0], res_type, (void**)&res))
        return enif_make_badarg(env);

    enif_rwlock_rlock(res->rwlock);
    const int32_t minimum = bitset_minimum(res->b);
    enif_rwlock_runlock(res->rwlock);

    return enif_make_int(env, minimum);
}

static ERL_NIF_TERM set_by_rawbinary(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    cbs_context_t *res = NULL;
    ErlNifBinary bin;
    ErlNifResourceType* res_type = (ErlNifResourceType*)enif_priv_data(env);
    if (argc != 2 ||
            !enif_get_resource(env, argv[0], res_type, (void**)&res) ||
            !enif_inspect_binary(env, argv[1], &bin))
        return enif_make_badarg(env);

    const int32_t num = bin.size / sizeof(uint32_t);
    if (num > TILE_SIZE * TILE_SIZE) {
        return enif_raise_exception(env, enif_make_string(env, "overflow tile", ERL_NIF_LATIN1));
    }

    enif_rwlock_rwlock(res->rwlock);
    bitset_set_list(res->b, (uint32_t*)bin.data, bin.size/(sizeof(uint32_t)));
    enif_rwlock_rwunlock(res->rwlock);

    return argv[0];
}

static ERL_NIF_TERM set(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    cbs_context_t *res = NULL;
    uint32_t i = 0;
    ErlNifResourceType* res_type = (ErlNifResourceType*)enif_priv_data(env);
    if (argc != 2 ||
            !enif_get_resource(env, argv[0], res_type, (void**)&res) ||
            !enif_get_uint(env, argv[1], &i))
        return enif_make_badarg(env);

    if (i > TILE_SIZE * TILE_SIZE) {
        return enif_raise_exception(env, enif_make_string(env, "overflow tile", ERL_NIF_LATIN1));
    }

    enif_rwlock_rwlock(res->rwlock);
    bitset_set(res->b, i);
    enif_rwlock_rwunlock(res->rwlock);

    return argv[0];
}

static ERL_NIF_TERM unset(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    cbs_context_t *res = NULL;
    uint32_t i = 0;
    ErlNifResourceType* res_type = (ErlNifResourceType*)enif_priv_data(env);
    if (argc != 2 ||
            !enif_get_resource(env, argv[0], res_type, (void**)&res) ||
            !enif_get_uint(env, argv[1], &i))
        return enif_make_badarg(env);

    if (i > TILE_SIZE * TILE_SIZE) {
        return enif_raise_exception(env, enif_make_string(env, "overflow tile", ERL_NIF_LATIN1));
    }

    enif_rwlock_rwlock(res->rwlock);
    bitset_unset(res->b, i);
    enif_rwlock_rwunlock(res->rwlock);

    return argv[0];
}

static ERL_NIF_TERM maximum(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    cbs_context_t *res = NULL;
    ErlNifResourceType* res_type = (ErlNifResourceType*)enif_priv_data(env);
    if (argc != 1 || !enif_get_resource(env, argv[0], res_type, (void**)&res))
        return enif_make_badarg(env);
    const int maximum = bitset_maximum(res->b);
    return enif_make_int(env, maximum);
}

static ERL_NIF_TERM get(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    cbs_context_t *res = NULL;
    ErlNifUInt64 i = 0;
    ErlNifResourceType* res_type = (ErlNifResourceType*)enif_priv_data(env);
    if (argc != 2 ||
            !enif_get_resource(env, argv[0], res_type, (void**)&res) ||
            !enif_get_uint64(env, argv[1], &i))
        return enif_make_badarg(env);

    enif_rwlock_rlock(res->rwlock);
    bool ret = bitset_get(res->b, i);
    enif_rwlock_runlock(res->rwlock);

    if (ret) 
        return ATOM_TRUE;
    return ATOM_FALSE;
}

static ERL_NIF_TERM tilesize(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    return enif_make_uint(env, TILE_SIZE);
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
    {"new",   0, new},
    {"from_binary",   1, new_from_rawbinary},
    {"copy",  1, copy},
    {"union", 2, set_union},
    {"union_count", 2, union_count},
    {"intersection", 2, intersection},
    {"intersects", 2, intersects},
    {"difference", 2, difference},
    {"set_by_rawbinary", 2, set_by_rawbinary},
    {"set",   2, set},
    {"unset",   2, unset},
    {"get",   2, get},
    {"count",  1, count},
    {"minimum",  1, minimum},
    {"maximum",  1, maximum},
    {"tilesize", 0, tilesize}
};

ERL_NIF_INIT(ebitset, nif_funcs, nifload, NULL,NULL,NULL)
