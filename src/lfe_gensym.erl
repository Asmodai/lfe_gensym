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

-export([system_info/0,
         most_positive_gensym/0,
         gensym_counter/0,
         gensym/0]).

-on_load(init/0).

-define(APPNAME, lfe_gensym).
-define(LIBNAME, lfe_gensym).

%%----------------------------------------------------------------------------
%% atom_tab_limit() -> pos_number()
%%
%% Description: Returns the atom limit for the Erlang VM.
%%----------------------------------------------------------------------------
atom_tab_limit() ->
    try erlang:system_info(atom_count) of
        N -> N
    catch
        error:badarg -> atom_tab_limit_helper();
        _:Error      -> {error, caught, Error}
    end.

%%----------------------------------------------------------------------------
%% atom_tab_limit_helper() -> pos_number()
%%
%% Description: Returns the atom limit for the Erlang VM.
%% This is called should `erlang:system_info(atom_count)' throw an error.
%%----------------------------------------------------------------------------
atom_tab_limit_helper() ->
    Info      = erlang:system_info(info),
    Chunks    = binary:split(Info, <<"=">>, [global]),
    [TabInfo] = [X || <<"index_table:atom_tab", X/binary>> <- Chunks],
    Lines     = binary:split(TabInfo, <<"\n">>, [global]),
    Chunks2   = [binary:split(L, <<": ">>) || L <- Lines, L =/= <<>>],
    PropList  = [list_to_tuple(X) || X <- Chunks2],
    binary_to_integer(proplists:get_value(<<"limit">>, PropList)).

%%----------------------------------------------------------------------------
%% Description: Initialise the C module.
%%----------------------------------------------------------------------------
init() ->
    SoName  = case code:priv_dir(?APPNAME) of
                  {error, bad_name} ->
                      case filelib:is_dir(filename:join(["..", priv])) of
                          true -> filename:join(["..", priv, ?LIBNAME]);
                          _    -> filename:join([priv, ?LIBNAME])
                      end;
                  Dir               ->
                      filename:join(Dir, ?LIBNAME)
              end,
    AtomTab = atom_tab_limit(),
    ok      = erlang:load_nif(SoName, AtomTab).

%%----------------------------------------------------------------------------
%% Trigger an exit if the C module is not loaded.
%%----------------------------------------------------------------------------
not_loaded(Line) ->
    exit({not_loaded, [{module, ?MODULE}, {line, Line}]}).

%%----------------------------------------------------------------------------
%% system_info() -> tuple()
%%
%% Description: Returns a tuple containing various bits of 'system'
%% information.
%%
%% As of now, the value returned is {limit, count}, where:
%%    o `limit' is the maximum gensym counter value,
%%    o `count' is the current gensym counter value.
%%----------------------------------------------------------------------------
system_info() ->
    not_loaded(?LINE).

%%----------------------------------------------------------------------------
%% most_positive_gensym() -> pos_number()
%%
%% Description: Returns the highest number the gensym counter can reach before
%% it wraps around.
%%----------------------------------------------------------------------------
most_positive_gensym() ->
    not_loaded(?LINE).

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
    list_to_atom(
      lists:flatten(
        io_lib:format("sym_~w", [gensym_counter()]))).

%% lfe_gensym.erl ends here.
