-module(template_tests).

-define(PROPER_NO_IMPORTS, true). %% I hate imported functions!
-include_lib("proper/include/proper.hrl").
-compile(export_all).

%% Generators ==========================================================
printable_char() ->
    proper_types:elements(lists:seq($ , $~)).

string() ->
    proper_types:list(printable_char()).

%% Properties ==========================================================
%% Test that no substitutions leave the string intact
prop_string_empty_list() ->
    ?FORALL(S, string(), proper:equals(template:string(S, []), S)).

%% Internals ============================================================
format_failure(Template, Substs, Expected, Result) ->
    io:format(
      "~nTemplate: ~p~n"
      "Substs  : ~p~n"
      "Expected: ~p~n"
      "Got     : ~p~n",
      [Template, Substs, Expected, Result]).
