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
#include "erl_nif.h"

/* Prototypes, so gcc shuts up. */
unsigned int gensym_incr(void);

/**
 * @brief Gensym counter.
 *
 * This is meant to mirror CL's *GENSYM-COUNTER*, except it is read-only.
 */
typedef struct {
  unsigned int max;                     /*!< Maximum number of atoms. */
  unsigned int value;                   /*!< Gensym counter value. */
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
unsigned int
gensym_incr(void)
{
  if (gensym_counter->value == gensym_counter->max) {
    gensym_counter->value = 0;
  }
  
  return ++gensym_counter->value;
}

/**
 * @brief Action to take when the nif is loaded.
 * @param env The environment (unused).
 * @param priv Private data (unused).
 * @param load_info The atom_tab size limit obtained from erlang:system_info.
 */
static
int
load(ErlNifEnv     *env,
     void         **priv,
     ERL_NIF_TERM   load_info)
{
  unsigned int limit = 0;

  if (!enif_is_number(env, load_info)) {
    return -1;
  }

  if (!enif_get_uint(env, load_info, &limit)) {
    return -1;
  }

  if (gensym_counter == NULL) {
    gensym_counter = (gensym_t *)enif_alloc(sizeof *gensym_counter);
    if (gensym_counter == NULL) {
      return -1;
    }

    gensym_counter->max   = (unsigned int)(limit * 0.10);
    gensym_counter->value = 0;
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
  return enif_make_int(env, gensym_incr());
}

/**
 * @brief Return the maximum value that the gensym counter can contain.
 * @param env The environment.
 * @param argc Count of arguments.
 * @param argv Array of arguments.
 * @returns The maximum gensym counter value.
 * @note Ignores its arguments.
 */
static
ERL_NIF_TERM
most_positive_gensym(ErlNifEnv          *env,
                     int                 argc,
                     const ERL_NIF_TERM  argv[])
{
  return enif_make_int(env, gensym_counter->max);
}

/**
 * @brief NIF function(s) we export.
 */
static ErlNifFunc nif_funcs[] = {
  { "gensym_counter",       0, gensym_counter_nif   },
  { "most_positive_gensym", 0, most_positive_gensym }
};

/*
 * Initialise the NIF.
 */
ERL_NIF_INIT(lfe_gensym, nif_funcs, &load, NULL, NULL, &unload);

/* gensym.c ends here. */
