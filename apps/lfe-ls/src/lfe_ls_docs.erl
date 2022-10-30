%% Copyright (c) 2008-2020 Robert Virding
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

%% File    : lfe_shell.erl
%% Author  : Robert Virding
%% Purpose : A simple Lisp Flavoured Erlang shell.

%% We keep three environments: the current environment; the saved
%% environment which contains the environment from before a slurp; and
%% the base environment which contains the predefined shell variables,
%% functions and macros. The base environment is used when we need to
%% revert to an "empty" environment. The save environment is used to
%% store the current environment when we 'slurp' a file which we can
%% revert back to when we do an 'unslurp'.

%%
%% Modified lfe_shell module to only handle documentation
%%

-module(lfe_ls_docs).

-export([h/1,h/2,h/3]).

-import(orddict, [store/3,find/2]).
-import(lists, [reverse/1,foreach/2]).

-define(NATIVE_FORMAT,<<"application/erlang+html">>).
-define(LFE_FORMAT,   <<"text/markdown">>).

-define(CURR_DOC_VERSION, {1,0,0}).

%% The Docs v1 record.
-record(docs_v1, {anno,
                  beam_language,
                  format,
                  module_doc,
                  metadata,
                  docs = []
                 }).

%% Coloured strings for the LFE banner, red, green, yellow and blue.
-define(RED(Str), "\e[31m" ++ Str ++ "\e[0m").
-define(GRN(Str), "\e[1;32m" ++ Str ++ "\e[0m").
-define(YLW(Str), "\e[1;33m" ++ Str ++ "\e[0m").
-define(BLU(Str), "\e[1;34m" ++ Str ++ "\e[0m").


%% doc(Mod) -> ok | {error,Error}.
%% doc(Mod, Func) -> ok | {error,Error}.
%% doc(Mod, Func, Arity) -> ok | {error,Error}.
%%  Print out documentation of a module/macro/function. Always try to
%%  find the file and use it as this is the only way to get hold of
%%  the chunks. This may get a later version than is loaded.

%% doc(?Q(What)) -> doc(What);                     %Be kind if they quote it
%% doc(What) ->
%%     [Mod|F] = lfe_lib:split_name(What),

h(Mod) ->
    case lfe_docs:get_module_docs(Mod) of
        {ok,#docs_v1{}=Docs} ->
            Ret = get_module_doc(Mod, Docs),
            format_doc(Ret);
        Error -> Error
    end.

h(Mod, Func) ->
    case lfe_docs:get_module_docs(Mod) of
        {ok,#docs_v1{}=Docs} ->
            Ret = get_macro_doc(Mod, Func, Docs),
            format_doc(Ret);
        Error -> Error
    end.

h(Mod, Func, Arity) ->
    case lfe_docs:get_module_docs(Mod) of
        {ok,#docs_v1{}=Docs} ->
            Ret = get_function_doc(Mod, Func, Arity, Docs),
            format_doc(Ret);
        Error -> Error
    end.

format_doc({error,_}=Error) -> Error;
format_doc(Docs) -> list_to_binary(Docs).
    %% {match,Lines} = re:run(Docs, "(.+\n|\n)",
    %%                        [unicode,global,{capture,all_but_first,binary}]),
%%    Pfun = fun (Line) ->
           %%         io:put_chars(Line),
           %%         1                            %One line
           %% end,
    %paged_output(Pfun, Lines),
    %% io:format(?RED("~*c")++"\n", [60,$_]),
    %% lists:foreach(fun (Line) ->
    %%                   io:put_chars(Line)
    %%                   end,
    %%              Lines),
    %% ok.


get_module_doc(Mod, #docs_v1{format = ?NATIVE_FORMAT}=Docs) ->
    shell_docs:render(Mod, Docs);
get_module_doc(Mod, #docs_v1{format = ?LFE_FORMAT}=Docs) ->
    lfe_shell_docs:render(Mod, Docs);
get_module_doc(_Mod, #docs_v1{format = Enc}) ->
    {error, {unknown_format, Enc}}.

get_macro_doc(Mod, Name, #docs_v1{format = ?NATIVE_FORMAT}=Docs) ->
    shell_docs:render(Mod, Name, Docs);
get_macro_doc(Mod, Name, #docs_v1{format = ?LFE_FORMAT}=Docs) ->
    lfe_shell_docs:render(Mod, Name, Docs);
get_macro_doc(_Mod, _Name, #docs_v1{format = Enc}) ->
    {error, {unknown_format, Enc}}.

get_function_doc(Mod, Name, Arity, #docs_v1{format = ?NATIVE_FORMAT}=Docs) ->
    shell_docs:render(Mod, Name, Arity, Docs);
get_function_doc(Mod, Name, Arity, #docs_v1{format = ?LFE_FORMAT}=Docs) ->
    lfe_shell_docs:render(Mod, Name, Arity, Docs);
get_function_doc(_Mod, _Name, _Arity, #docs_v1{format = Enc}) ->
    {error, {unknown_format, Enc}}.

%% paged_output(Pfun, Items) ->
%%     %% How many rows per "page", just set it to 30 for now.
%%     Limit = 30,
%%     paged_output(Pfun, 0, Limit, Items).

%% paged_output(Pfun, Curr, Limit, Items) when Curr >= Limit ->
%%     case more() of
%%         more -> paged_output(Pfun, 0, Limit, Items);
%%         less -> ok
%%     end;
%% paged_output(Pfun, Curr, Limit, [I|Is]) ->
%%     Lines = Pfun(I),
%%     paged_output(Pfun, Curr + Lines, Limit, Is);
%% paged_output(_, _, _, []) -> ok.

%% more() ->
%%     case io:get_line('More (y/n)? ') of
%%         "y\n" -> more;
%%         "c\n" -> more;
%%         "n\n" -> less;
%%         "q\n" -> less;
%%         _ -> more()
%%     end.
