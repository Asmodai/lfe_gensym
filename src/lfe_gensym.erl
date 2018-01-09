%% -*- Mode: Erlang -*-
%%
%% lfe_gensym.erl --- `gensym' hacks for LFE.
%%
%% Copyright (c) 2018 Paul Ward <asmodai@gmail.com>
%%
%% Author:     Paul Ward <asmodai@gmail.com>
%% Maintainer: Paul Ward <asmodai@gmail.com>
%% Created:    09 Jan 2018 02:35:12
%%
%%{{{ License:
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%
%%}}}
%%{{{ Commentary:
%%
%%}}}

-module(lfe_gensym).

-ifdef(TEST).
-compile(export_all).
-endif.

-export([gensym_counter/0,
         gensym/1,
         gensym/0]).

-on_load(init/0).

-define(APPNAME, lfe_gensym).
-define(LIBNAME, lfe_gensym).

init() ->
    SoName = case code:priv_dir(?APPNAME) of
                 {error, bad_name} ->
                     case filelib:is_dir(filename:join(["..", priv])) of
                         true -> filename:join(["..", priv, ?LIBNAME]);
                         _    -> filename:join([priv, ?LIBNAME])
                     end;
                 Dir               ->
                     filename:join(Dir, ?LIBNAME)
             end,
    erlang:load_nif(SoName, 0).

not_loaded(Line) ->
    exit({not_loaded, [{module, ?MODULE}, {line, Line}]}).

%%----------------------------------------------------------------------------
%% gensym_counter() -> pos_number()
%%
%% Description: Returns the current `gensym' count.
%%
%% For more information on what this is, see:
%% http://www.lispworks.com/documentation/HyperSpec/Body/v_gensym.htm
%%----------------------------------------------------------------------------
gensym_counter() ->
    not_loaded(?LINE).

%%----------------------------------------------------------------------------
%% gensym() -> atom()
%%
%% Description: Returns a fresh symbol.
%%
%% For information on how GENSYM works, see:
%% http://www.lispworks.com/documentation/HyperSpec/Body/f_gensym.htm
%%----------------------------------------------------------------------------
gensym() ->
    gensym("G").

%%----------------------------------------------------------------------------
%% gensym(Symbolprefix) -> atom()
%%   SymbolPrefix - string()
%%
%% Description: Returns a fresh symbol with the given prefix.
%%
%% For information on how GENSYM works, see:
%% http://www.lispworks.com/documentation/HyperSpec/Body/f_gensym.htm
%%----------------------------------------------------------------------------
gensym(Prefix) ->
    list_to_atom(
      lists:flatten(
        io_lib:format("~s~w", [Prefix, gensym_counter()]))).

%% lfe_gensym.erl ends here.
