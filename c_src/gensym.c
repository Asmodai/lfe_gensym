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

#include <stdio.h>
#include <stdint.h>
#include <string.h>
#include "erl_nif.h"

/**
 * @brief Gensym counter.
 *
 * This is meant to mirror CL's *GENSYM-COUNTER*, except it is read-only.
 */
typedef struct {
  unsigned int max;                     /*!< Maximum number of atoms. */
  unsigned int created;                 /*!< Number of gensyms created. */
  unsigned int value;                   /*!< Gensym counter value. */
} gensym_t;

/**
 * @brief Counter instance.
 */
static gensym_t gensym_counter = { 0 };

/**
 * @brief Prefix for symbol names.
 */
static const char *const gensym_prefix = "sym_";

/**
 * @brief Compute and return the length of a printed representation of a number.
 * @param num The number.
 * @returns A length.
 */
static
size_t
numlen(unsigned int num)
{
  if (num >= 100000) {
    if (num >= 10000000) {
      if (num >= 1000000000) return 10;
      if (num >= 100000000)  return 9;
      return 8;
    }

    if (num >= 1000000) return 7;
    return 6;
  } else {
    if (num >= 1000) {
      if (num >= 10000) return 5;
      return 4;
    } else {
      if (num >= 100) return 3;
      if (num >= 10)  return 2;
      return 1;
    }
  }
}

/**
 * @brief Create and return an Erlang atom.
 * @param env The environment.
 * @param atom The name of the atom.
 * @returns An existing atom of the same name or a freshly created atom.
 */
static
ERL_NIF_TERM
mk_atom(ErlNifEnv *env, const char *atom)
{
  ERL_NIF_TERM ret = 0;

  if (!enif_make_existing_atom(env, atom, &ret, ERL_NIF_LATIN1)) {
    return enif_make_atom(env, atom);
  }

  return ret;
}

/**
 * @brief Increment the gensym counter.
 * @returns The value of the incremented counter.
 * @note Wraps around if an increment would cause an overflow.
 */
static
unsigned int
gensym_incr(void)
{
  if (gensym_counter.value == gensym_counter.max) {
    gensym_counter.value = 0;
  }

  if (gensym_counter.created < gensym_counter.max) {
    gensym_counter.created++;
  }
  
  return ++gensym_counter.value;
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

  gensym_counter.max     = (unsigned int)(limit * 0.10);
  gensym_counter.created = 0;
  gensym_counter.value   = 1;
  
  return 0;
}

/**
 * @brief Return gensym system information.
 * @param env The environment.
 * @param argc Count of arguments.
 * @param argv Array of arguments.
 * @returns A tuple with 2 elements.
 *
 * The tuple returned is in the form:
 *
 *   @c { @c limit @c , @c created @c }
 */
static
ERL_NIF_TERM
gensym_system_info(ErlNifEnv          *env,
                   int                 argc,
                   const ERL_NIF_TERM  argv[])
{
  return enif_make_tuple2(env,
                          enif_make_int(env, gensym_counter.max),
                          enif_make_int(env, gensym_counter.created));
}

/**
 * @brief Return the value that will be used for the next gensym.
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
  return enif_make_int(env, gensym_counter.value);
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
gensym_limit(ErlNifEnv          *env,
             int                 argc,
             const ERL_NIF_TERM  argv[])
{
  return enif_make_int(env, gensym_counter.max);
}

/**
 * @brief Generate and return a fresh symbol.
 * @param env The environment.
 * @param argc Count of arguments.
 * @param argv Array of arguments.
 * @returns An Erlang atom, or a tuple of two atoms if there is an error.
 * @note Ignores its arguments.
 */
static
ERL_NIF_TERM
gensym(ErlNifEnv          *env,
       int                 argc,
       const ERL_NIF_TERM  argv[])
{
  static size_t  prefixlen = 0;
  size_t         len       = 0;
  char          *buf       = NULL;

  if (prefixlen == 0) {
    prefixlen = strlen(gensym_prefix);
  }

  len = prefixlen + numlen(gensym_counter.value) + 1;

  if ((buf = enif_alloc(sizeof(char) * len)) == NULL) {
    return enif_make_tuple2(env,
                            mk_atom(env, "error"),
                            mk_atom(env, "out_of_memory"));
  }

  snprintf(buf, len, "%s%d", gensym_prefix, gensym_counter.value);
  gensym_incr();
  return mk_atom(env, buf);
}

/**
 * @brief NIF function(s) we export.
 */
static ErlNifFunc nif_funcs[] = {
  { "gensym_counter", 0, gensym_counter_nif },
  { "gensym_limit",   0, gensym_limit       },
  { "system_info",    0, gensym_system_info },
  { "gensym",         0, gensym             }
};

/*
 * Initialise the NIF.
 */
ERL_NIF_INIT(lfe_gensym, nif_funcs, &load, NULL, NULL, NULL);

/* gensym.c ends here. */
