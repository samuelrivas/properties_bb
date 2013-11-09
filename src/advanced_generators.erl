%%% @doc
-module(advanced_generators).

-compile([export_all]).

%%%_* Includes =========================================================
-define(PROPER_NO_IMPORTS, true). %% I hate imported functions!
-include_lib("proper/include/proper.hrl").

%%%_* Properties =======================================================
%%% Don't use recursive generators for this yet, work only with ?LET and
%%% ?SUCHTHAT

%% The pair generator, from the slides
pair(G) -> ?LET(X, G, {X, X}).

%% Generate a list and a prefix of that list
list_and_prefix(G) ->
  ?LET(
     L, proper_types:list(G),
     ?LET(
        Pos, proper_types:choose(0, length(L)),
        {L, lists:sublist(L, 1, Pos)})).

%% The lengthy list from the slides
lengthy_list(Length, G) ->
  ?SUCHTHAT(L1, proper_types:list(G), length(L1) >= Length).

%% This doesn't work
prop_lengthy() ->
  ?FORALL(
     L, lengthy_list(10, proper_types:int()),
     length(L) >= 10).

%% A gb_set
gb_set(G) ->
  ?LET(L, proper_types:list(G), gb_sets:from_list(L)).

%% A list of Gs without duplicates (use remove_dups)
unique_elements(G) ->
  ?LET(L, proper_types:list(G), remove_dups(L)).

%% A better implementation of a sentence, now avoiding duplicated, leading and
%% trailing white spaces
word() ->
  proper_types:non_empty(proper_types:list(basic_generators:small_letter())).

sentence() ->
  ?LET(
     Words, proper_types:list(word()),
     string:join(Words, " ")).

%% A better lengthy list that can generate lists of arbitrarily many Gs:
better_lengthy_list(Length, G) ->
  ?LET(
     {Prefix, L}, {proper_types:vector(Length, G), proper_types:list(G)},
     Prefix ++ L).

prop_better_lengthy() ->
  ?FORALL(
     L, better_lengthy_list(100, proper_types:int()),
     length(L) >= 100).

%% A tuple of three different Gs
trio(G) ->
  ?SUCHTHAT({X, Y, Z}, {G, G, G}, X /= Y andalso Y /= Z andalso X /= Z).

%%%_* Internals ========================================================
remove_dups(L) ->
  FoldF = fun(X, {Acc, Seen}) ->
              case gb_sets:is_element(X, Seen) of
                true  -> {Acc , Seen};
                false -> {[X|Acc], gb_sets:add(X, Seen)}
              end
          end,
  {Reversed, _} = lists:foldl(FoldF, {[], gb_sets:new()}, L),
  lists:reverse(Reversed).

%%%_* Emacs ============================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
