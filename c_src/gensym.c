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
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
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

/*
 * Erlang will crash when the atom table reaches its maximal count, which will
 * happen before uint32_t or uint64_t will run out of bits.
 */
#define INTEGER_TYPE    uint16_t
#define INTEGER_MAX     UINT16_MAX

/* Prototypes, so gcc shuts up. */
INTEGER_TYPE gensym_incr(void);

/**
 * @brief Gensym counter.
 *
 * This is meant to mirror CL's *GENSYM-COUNTER*, except it is read-only.
 */
typedef struct {
  INTEGER_TYPE  value;                  /*!< Gensym counter value. */
} gensym_t;

/**
 * @brief Counter instance.
 */
static gensym_t *gensym_counter = NULL;

/**
 * @brief Increment the gensym counter.
 * @returns The value of the incremented counter.
 * @note Wraps around if an increment would cause an overflow.
 */
INTEGER_TYPE
gensym_incr(void)
{
  if (gensym_counter->value == INTEGER_MAX) {
    /*
     * On Common Lisp, if *GENSYM-COUNTER* reaches MOST-POSITIVE-FIXNUM, the
     * value is promoted to bignum.  I should really implement this, but
     * I know nothing of how Erlang represents bignums internally (GMP?), so
     * for now we simply wrap.
     */
    gensym_counter->value = 0;
  }
  
  return ++gensym_counter->value;
}

/**
 * @brief Action to take when the nif is loaded.
 * @param env The environment (unused).
 * @param priv Private data (unused).
 * @param load_info Second argument to erlang:load_nif/2.
 */
static
int
load(ErlNifEnv     *env,
     void         **priv,
     ERL_NIF_TERM   load_info)
{
  if (gensym_counter == NULL) {
    gensym_counter = (gensym_t *)enif_alloc(sizeof *gensym_counter);
    if (gensym_counter == NULL) {
      return -1;
    }

    gensym_counter->value = 1;
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
  if (gensym_counter != NULL) {
    enif_free(gensym_counter);
    gensym_counter = NULL;
  }
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
  INTEGER_TYPE cnt = gensym_incr();

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
