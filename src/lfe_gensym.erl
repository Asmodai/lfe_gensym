%% -*- Mode: Erlang -*-
%%
%% lfe_gensym.erl --- `gensym' hacks for LFE.
%%
%% Copyright (c) 2018 Paul Ward <pward@alertlogic.com>
%%
%% Author:     Paul Ward <pward@alertlogic.com>
%% Maintainer: Paul Ward <pward@alertlogic.com>
%% Created:    09 Jan 2018 02:35:12
%%
%%{{{ License:
%%
%% This program is free software; you can redistribute it and/or
%% modify it under the terms of the GNU General Public License
%% as published by the Free Software Foundation; either version 3
%% of the License, or (at your option) any later version.
%%
%% This program is distributed in the hope that it will be useful,
%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%% GNU General Public License for more details.
%%
%% You should have received a copy of the GNU General Public License
%% along with this program; if not, see <http://www.gnu.org/licenses/>.
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
