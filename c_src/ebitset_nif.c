#include <erl_nif.h>
#include <limits.h>
#include <errno.h>

#include <stdint.h>

#include <unistd.h>

#include "cbitset/bitset.h"
#include "mylog.h"

#define ENIF(name) static ERL_NIF_TERM name(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])

#define RW_LOCK(fun, res) \
    while (enif_rwlock_tryrwlock(res->rwlock) == EBUSY) {\
        if (enif_consume_timeslice(env, 25)) {\
            WARN("the rwlock are locked by a writer: %s", #fun);\
            return enif_schedule_nif(env, #fun, 0, fun, argc, argv);\
        }\
        LOG("trying to exausted.... %s", #fun);\
    }
#define RW_UNLOCK(res) enif_rwlock_rwunlock(res->rwlock)

#define R_LOCK(fun, res) \
    while (enif_rwlock_tryrlock(res->rwlock) == EBUSY) {\
        if (enif_consume_timeslice(env, 25)) {\
            return enif_schedule_nif(env, #fun, 0, fun, argc, argv);\
        }\
    }
#define R_UNLOCK(res) enif_rwlock_runlock(res->rwlock);

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

ENIF(new) {
    ErlNifResourceType* res_type = (ErlNifResourceType*)enif_priv_data(env);
    cbs_context_t *res = (cbs_context_t*)enif_alloc_resource(res_type, sizeof(*res));
    res->rwlock = enif_rwlock_create("bitset-rwlock");
    res->b = bitset_create();

    ERL_NIF_TERM ret = enif_make_resource(env, res);
    enif_release_resource(res);
    return ret;
}

ENIF(new_from_rawbinary) {
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
    if (num > BITILE_BITSIZE) {
        return enif_raise_exception(env, enif_make_string(env, "overflow tile", ERL_NIF_LATIN1));
    }

    bitset_set_list(res->b, (uint32_t*)bin.data, bin.size/(sizeof(uint32_t)));

    ERL_NIF_TERM ret = enif_make_resource(env, res);
    enif_release_resource(res);
    return ret;
}

ENIF(copy) {
    ErlNifResourceType* res_type = (ErlNifResourceType*)enif_priv_data(env);
    cbs_context_t *src = NULL;
    if (argc != 1 ||
            !enif_get_resource(env, argv[0], res_type, (void**)&src))
        return enif_make_badarg(env);
    
    R_LOCK(copy, src);
    bitset_t* newbitset = bitset_copy(src->b);
    R_UNLOCK(src);

    cbs_context_t *res = (cbs_context_t*)enif_alloc_resource(res_type, sizeof(*res));
    res->b = newbitset;
    res->rwlock = enif_rwlock_create("bitset-rwlock");

    ERL_NIF_TERM ret = enif_make_resource(env, res);
    enif_release_resource(res);
    return ret;
}

ENIF(set_union) {
    ErlNifResourceType* res_type = (ErlNifResourceType*)enif_priv_data(env);
    cbs_context_t *s1 = NULL;
    cbs_context_t *s2 = NULL;
    if (argc != 2 ||
            !enif_get_resource(env, argv[0], res_type, (void**)&s1) ||
            !enif_get_resource(env, argv[1], res_type, (void**)&s2))
        return enif_make_badarg(env);

    R_LOCK(set_union, s1);
    bitset_t *newbitset = bitset_copy(s1->b);
    R_UNLOCK(s1);

    if (s1 != s2) {
        R_LOCK(set_union, s2);
        bitset_inplace_union(newbitset, s2->b);
        //sleep(3);
        R_UNLOCK(s2);
    }

    cbs_context_t *res = (cbs_context_t*)enif_alloc_resource(res_type, sizeof(*res));
    res->b = newbitset;
    res->rwlock = enif_rwlock_create("bitset-rwlock");
    ERL_NIF_TERM ret = enif_make_resource(env, res);
    enif_release_resource(res);
    return ret;
}

ENIF(union_count) {
    ErlNifResourceType* res_type = (ErlNifResourceType*)enif_priv_data(env);
    cbs_context_t *s1 = NULL;
    cbs_context_t *s2 = NULL;
    if (argc != 2 ||
            !enif_get_resource(env, argv[0], res_type, (void**)&s1) ||
            !enif_get_resource(env, argv[1], res_type, (void**)&s2))
        return enif_make_badarg(env);

    if (s1 == s2) {
        WARN("two same bitset, just count one");
        return enif_make_uint(env, bitset_count(s1->b));
    }

    cbs_context_t *res1 = NULL, *res2 = NULL;
    if ((uintptr_t)s1->rwlock < (uintptr_t)s2->rwlock) {
        res1 = s1;
        res2 = s2;
    } 
    else {
        res1 = s2;
        res2 = s1;
    }
    // TODO: we have to use ERL_NIF_THR_DIRTY_CPU_SCHEDULER 
    enif_rwlock_rlock(res1->rwlock);
    enif_rwlock_rlock(res2->rwlock);
    uint32_t uc = bitset_union_count(s1->b, s2->b);
    //sleep(3);
    enif_rwlock_runlock(res2->rwlock);
    enif_rwlock_runlock(res1->rwlock);
    return enif_make_uint(env, uc);
}

ENIF(intersection) {
    ErlNifResourceType* res_type = (ErlNifResourceType*)enif_priv_data(env);
    cbs_context_t *s1 = NULL;
    cbs_context_t *s2 = NULL;
    if (argc != 2 ||
            !enif_get_resource(env, argv[0], res_type, (void**)&s1) ||
            !enif_get_resource(env, argv[1], res_type, (void**)&s2))
        return enif_make_badarg(env);

    R_LOCK(intersection, s1);
    bitset_t* newbitset = bitset_copy(s1->b);
    R_UNLOCK(s1);

    if (s1 != s2) {
        R_LOCK(intersection, s2);
        bitset_inplace_intersection(newbitset, s2->b);
        R_UNLOCK(s2);
    }

    cbs_context_t *res = (cbs_context_t*)enif_alloc_resource(res_type, sizeof(*res));
    res->rwlock = enif_rwlock_create("bitset-rwlock");
    res->b = newbitset;
    ERL_NIF_TERM ret = enif_make_resource(env, res);
    enif_release_resource(res);
    return ret;
}

ENIF(intersects) {
    ErlNifResourceType* res_type = (ErlNifResourceType*)enif_priv_data(env);
    cbs_context_t *s1 = NULL;
    cbs_context_t *s2 = NULL;
    if (argc != 2 ||
            !enif_get_resource(env, argv[0], res_type, (void**)&s1) ||
            !enif_get_resource(env, argv[1], res_type, (void**)&s2))
        return enif_make_badarg(env);

    if (s1 == s2) {
        WARN("is it corrent to intersects two same bitsets?");
        return ATOM_TRUE;
    }

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

ENIF(difference) {
    ErlNifResourceType* res_type = (ErlNifResourceType*)enif_priv_data(env);
    cbs_context_t *s1 = NULL;
    cbs_context_t *s2 = NULL;
    if (argc != 2 ||
            !enif_get_resource(env, argv[0], res_type, (void**)&s1) ||
            !enif_get_resource(env, argv[1], res_type, (void**)&s2))
        return enif_make_badarg(env);

    if (s1 == s2) {
        cbs_context_t *res = (cbs_context_t*)enif_alloc_resource(res_type, sizeof(*res));
        res->rwlock = enif_rwlock_create("bitset-rwlock");
        res->b = bitset_create();
        ERL_NIF_TERM ret = enif_make_resource(env, res);
        enif_release_resource(res);
        return ret;
    }

    R_LOCK(difference, s1);
    bitset_t* newbitset = bitset_copy(s1->b);
    R_UNLOCK(s1);

    R_LOCK(difference, s2);
    bitset_inplace_difference(newbitset, s2->b);
    R_UNLOCK(s2);

    cbs_context_t *res = (cbs_context_t*)enif_alloc_resource(res_type, sizeof(*res));
    res->rwlock = enif_rwlock_create("bitset-rwlock");
    res->b = newbitset;
    ERL_NIF_TERM ret = enif_make_resource(env, res);
    enif_release_resource(res);
    return ret;
}

ENIF(difference_count) {
    ErlNifResourceType* res_type = (ErlNifResourceType*)enif_priv_data(env);
    cbs_context_t *s1 = NULL;
    cbs_context_t *s2 = NULL;
    if (argc != 2 ||
            !enif_get_resource(env, argv[0], res_type, (void**)&s1) ||
            !enif_get_resource(env, argv[1], res_type, (void**)&s2))
        return enif_make_badarg(env);

    if (s1 == s2) {
        WARN("two same bitset, count 0");
        return enif_make_uint(env, 0);
    }

    cbs_context_t *res1 = NULL, *res2 = NULL;
    if ((uintptr_t)s1->rwlock < (uintptr_t)s2->rwlock) {
        res1 = s1;
        res2 = s2;
    } 
    else {
        res1 = s2;
        res2 = s1;
    }
    // TODO: we have to use ERL_NIF_THR_DIRTY_CPU_SCHEDULER 
    enif_rwlock_rlock(res1->rwlock);
    enif_rwlock_rlock(res2->rwlock);
    uint32_t uc = bitset_difference_count(s1->b, s2->b);
    enif_rwlock_runlock(res2->rwlock);
    enif_rwlock_runlock(res1->rwlock);
    return enif_make_uint(env, uc);
}

ENIF(symmetric_difference) {
    ErlNifResourceType* res_type = (ErlNifResourceType*)enif_priv_data(env);
    cbs_context_t *s1 = NULL;
    cbs_context_t *s2 = NULL;
    if (argc != 2 ||
            !enif_get_resource(env, argv[0], res_type, (void**)&s1) ||
            !enif_get_resource(env, argv[1], res_type, (void**)&s2))
        return enif_make_badarg(env);

    if (s1 == s2) {
        cbs_context_t *res = (cbs_context_t*)enif_alloc_resource(res_type, sizeof(*res));
        res->rwlock = enif_rwlock_create("bitset-rwlock");
        res->b = bitset_create();
        ERL_NIF_TERM ret = enif_make_resource(env, res);
        enif_release_resource(res);
        return ret;
    }

    R_LOCK(symmetric_difference, s1);
    bitset_t* newbitset = bitset_copy(s1->b);
    R_UNLOCK(s1);

    R_LOCK(symmetric_difference, s2);
    bitset_inplace_symmetric_difference(newbitset, s2->b);
    R_UNLOCK(s2);

    cbs_context_t *res = (cbs_context_t*)enif_alloc_resource(res_type, sizeof(*res));
    res->rwlock = enif_rwlock_create("bitset-rwlock");
    res->b = newbitset;
    ERL_NIF_TERM ret = enif_make_resource(env, res);
    enif_release_resource(res);
    return ret;
}

ENIF(symmetric_difference_count) {
    ErlNifResourceType* res_type = (ErlNifResourceType*)enif_priv_data(env);
    cbs_context_t *s1 = NULL;
    cbs_context_t *s2 = NULL;
    if (argc != 2 ||
            !enif_get_resource(env, argv[0], res_type, (void**)&s1) ||
            !enif_get_resource(env, argv[1], res_type, (void**)&s2))
        return enif_make_badarg(env);

    if (s1 == s2) {
        WARN("two same bitset, count 0");
        return enif_make_uint(env, 0);
    }

    cbs_context_t *res1 = NULL, *res2 = NULL;
    if ((uintptr_t)s1->rwlock < (uintptr_t)s2->rwlock) {
        res1 = s1;
        res2 = s2;
    } 
    else {
        res1 = s2;
        res2 = s1;
    }
    // TODO: we have to use ERL_NIF_THR_DIRTY_CPU_SCHEDULER 
    enif_rwlock_rlock(res1->rwlock);
    enif_rwlock_rlock(res2->rwlock);
    uint32_t uc = bitset_symmetric_difference_count(s1->b, s2->b);
    enif_rwlock_runlock(res2->rwlock);
    enif_rwlock_runlock(res1->rwlock);
    return enif_make_uint(env, uc);
}

ENIF(count) {
    cbs_context_t *res = NULL;
    ErlNifResourceType* res_type = (ErlNifResourceType*)enif_priv_data(env);
    if (argc != 1 || !enif_get_resource(env, argv[0], res_type, (void**)&res))
        return enif_make_badarg(env);

    R_LOCK( count, res );
    const int32_t cnt = bitset_precount(res->b);
    R_UNLOCK(res);

    return enif_make_int(env, cnt);
}

ENIF(minimum) {
    cbs_context_t *res = NULL;
    ErlNifResourceType* res_type = (ErlNifResourceType*)enif_priv_data(env);
    if (argc != 1 || !enif_get_resource(env, argv[0], res_type, (void**)&res))
        return enif_make_badarg(env);

    R_LOCK( minimum, res );
    const int32_t minimum = bitset_minimum(res->b);
    R_UNLOCK(res);

    return enif_make_int(env, minimum);
}

ENIF(set_by_rawbinary) {
    cbs_context_t *res = NULL;
    ErlNifBinary bin;
    ErlNifResourceType* res_type = (ErlNifResourceType*)enif_priv_data(env);
    if (argc != 2 ||
            !enif_get_resource(env, argv[0], res_type, (void**)&res) ||
            !enif_inspect_binary(env, argv[1], &bin))
        return enif_make_badarg(env);

    const int32_t num = bin.size / sizeof(uint32_t);
    if (num > BITILE_BITSIZE) {
        return enif_raise_exception(env, enif_make_string(env, "overflow tile", ERL_NIF_LATIN1));
    }

    RW_LOCK( set_by_rawbinary, res );
    bitset_set_list(res->b, (uint32_t*)bin.data, bin.size/(sizeof(uint32_t)));
    RW_UNLOCK(res);

    return argv[0];
}

ENIF(set) {
    cbs_context_t *res = NULL;
    uint32_t i = 0;
    ErlNifResourceType* res_type = (ErlNifResourceType*)enif_priv_data(env);
    if (argc != 2 ||
            !enif_get_resource(env, argv[0], res_type, (void**)&res) ||
            !enif_get_uint(env, argv[1], &i))
        return enif_make_badarg(env);

    if (i > BITILE_BITSIZE) {
        return enif_raise_exception(env, enif_make_string(env, "overflow tile", ERL_NIF_LATIN1));
    }

    RW_LOCK( set, res );
    bitset_set(res->b, i);
    RW_UNLOCK(res);
    return argv[0];
}

ENIF(unset) {
    cbs_context_t *res = NULL;
    uint32_t i = 0;
    ErlNifResourceType* res_type = (ErlNifResourceType*)enif_priv_data(env);
    if (argc != 2 ||
            !enif_get_resource(env, argv[0], res_type, (void**)&res) ||
            !enif_get_uint(env, argv[1], &i))
        return enif_make_badarg(env);

    if (i > BITILE_BITSIZE) {
        return enif_raise_exception(env, enif_make_string(env, "overflow tile", ERL_NIF_LATIN1));
    }

    RW_LOCK( unset, res );
    bitset_unset(res->b, i);
    RW_UNLOCK(res);
    return argv[0];
}

ENIF(maximum) {
    cbs_context_t *res = NULL;
    ErlNifResourceType* res_type = (ErlNifResourceType*)enif_priv_data(env);
    if (argc != 1 || !enif_get_resource(env, argv[0], res_type, (void**)&res))
        return enif_make_badarg(env);

    R_LOCK(maximum, res);
    const int maximum = bitset_maximum(res->b);
    R_UNLOCK(res);
    return enif_make_int(env, maximum);
}

ENIF(get) {
    cbs_context_t *res = NULL;
    ErlNifUInt64 i = 0;
    ErlNifResourceType* res_type = (ErlNifResourceType*)enif_priv_data(env);
    if (argc != 2 ||
            !enif_get_resource(env, argv[0], res_type, (void**)&res) ||
            !enif_get_uint64(env, argv[1], &i))
        return enif_make_badarg(env);

    R_LOCK( get, res );
    bool ret = bitset_get(res->b, i);
    R_UNLOCK(res);

    if (ret) 
        return ATOM_TRUE;
    return ATOM_FALSE;
}

ENIF(tilesize) {
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
    {"union_count", 2, union_count, ERL_NIF_THR_DIRTY_CPU_SCHEDULER},
    {"intersection", 2, intersection},
    {"intersects", 2, intersects},
    {"difference", 2, difference},
    {"difference_count", 2, difference_count},
    {"symmetric_difference", 2, symmetric_difference},
    {"symmetric_difference_count", 2, symmetric_difference_count},
    {"set_by_rawbinary", 2, set_by_rawbinary},
    {"unset",   2, unset},
    {"set",   2, set},
    {"get",   2, get},
    {"count",  1, count},
    {"minimum",  1, minimum},
    {"maximum",  1, maximum},
    {"tilesize", 0, tilesize}
};

ERL_NIF_INIT(ebitset, nif_funcs, nifload, NULL,NULL,NULL)
