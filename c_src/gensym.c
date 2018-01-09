/*
 * gensym.c --- LFE `gensym' hack.
 *
 * Copyright (c) 2018 Paul Ward <asmodai@gmail.com>
 *
 * Author:     Paul Ward <asmodai@gmail.com>
 * Maintainer: Paul Ward <asmodai@gmail.com>
 * Created:    09 Jan 2018 00:27:52
 */
/* {{{ License: */
/*
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License
 * as published by the Free Software Foundation; either version 3
 * of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, see <http://www.gnu.org/licenses/>.
 */
/* }}} */
/* {{{ Commentary: */
/*
 *
 */
/* }}} */

/**
 * @file gensym.c
 * @author Paul Ward
 * @brief LFE `gensym' hack.
 */

#include <stdint.h>
#include <limits.h>

#include "erl_nif.h"

#if __WORDSIZE == 64
# define INTEGER_TYPE    uint64_t
# define INTEGER_MAX     UINT64_MAX
#else
# define INTEGER_TYPE    uint32_t
# define INTEGER_MAX     UINT32_MAX
#endif

/**
 * @brief Gensym counter.
 *
 * This is meant to mirror CL's *GENSYM-COUNTER*, except it is read-only.
 */
typedef struct {
  ErlNifMutex  *lock;                   /*!< Mutex. */
  INTEGER_TYPE  value;                  /*!< Gensym counter value. */
} gensym_t;

/**
 * @brief Counter instance.
 */
static gensym_t *gensym_counter = NULL;

/**
 * @brief Initialise the gensym counter.
 * @returns A newly-initialised gensym counter.
 */
gensym_t *
gensym_create(void)
{
  gensym_t *ret = NULL;

  if ((ret = (gensym_t *)enif_alloc(sizeof *ret)) == NULL) {
    return NULL;
  }

  ret->lock  = NULL;
  ret->value = 0;

  if ((ret->lock = enif_mutex_create("gensym_lock")) != NULL) {
    return ret;
  }

  if (ret->lock != NULL) {
    enif_mutex_destroy(ret->lock);
  }

  if (ret != NULL) {
    enif_free(ret);
  }

  return NULL;
}

/**
 * @brief Destroy a gensym counter.
 * @param counter The counter to destroy.
 */
void
gensym_destroy(gensym_t *counter)
{
  ErlNifMutex *lock = NULL;

  enif_mutex_lock(counter->lock);
  lock          = counter->lock;
  counter->lock = NULL;
  enif_mutex_unlock(counter->lock);

  enif_mutex_destroy(lock);
  enif_free(counter);
}

/**
 * @brief Increment the gensym counter.
 * @returns The value of the incremented counter.
 * @note Wraps around if an increment would cause an overflow.
 */
INTEGER_TYPE
gensym_incr(void)
{
  INTEGER_TYPE ret = 0;

  enif_mutex_lock(gensym_counter->lock);

  if (gensym_counter->value == INTEGER_MAX) {
    /*
     * On Common Lisp, if *GENSYM-COUNTER* reaches MOST-POSITIVE-FIXNUM, the
     * value is promoted to bignum.  I should really implement this, but
     * I know nothing of how Erlang represents bignums internally (GMP?), so
     * for now we simply wrap.
     */
    gensym_counter->value = 0;
  } else {
    gensym_counter->value++;
  }

  ret = gensym_counter->value;

  enif_mutex_unlock(gensym_counter->lock);

  return ret;
}

/**
 * @brief Action to take when the nif is loaded.
 * @param env The environment (unused).
 * @param priv Private data (unused).
 * @param load_info Don't know, don't care, don't use.
 */
static
int
load(ErlNifEnv *env, void **priv, ERL_NIF_TERM load_info)
{
  if (gensym_counter == NULL) {
    gensym_counter = (gensym_t *)enif_alloc(sizeof *gensym_counter);
    if (gensym_counter == NULL) {
      return -1;
    }

    if ((gensym_counter = gensym_create()) == NULL) {
      return -1;
    }
  }

  return 0;
}

/**
 * @brief Action to take when the nif is unloaded.
 * @param env The environment (unused).
 * @param priv Private data (unused).
 */
static
void
unload(ErlNifEnv *env, void *priv)
{
  gensym_destroy(gensym_counter);
}

/**
 * @brief Get and increment the gensym counter.
 * @param env The environment.
 * @param argc Count of arguments.
 * @param argv Array of arguments.
 * @returns The contents of the gensym counter, as an Erlang term.
 * @note Ignores its arguments.
 */
static
ERL_NIF_TERM
gensym_counter_nif(ErlNifEnv          *env,
                   int                 argc,
                   const ERL_NIF_TERM  argv[])
{
  INTEGER_TYPE cnt = 0;

  cnt = gensym_incr();

  return enif_make_int(env, cnt);
}

/**
 * @brief NIF function(s) we export.
 */
static ErlNifFunc nif_funcs[] = {
  {"gensym_counter", 0, gensym_counter_nif}
};

/*
 * Initialise the NIF.
 */
ERL_NIF_INIT(lfe_gensym, nif_funcs, &load, NULL, NULL, &unload);

/* gensym.c ends here. */
