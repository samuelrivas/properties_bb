%%% @doc
-module(properties_1).

%%%_* Exports ==========================================================
-export([]).

%%%_* Includes =========================================================
-define(PROPER_NO_IMPORTS, true). %% I hate imported functions!
-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").

%%%_* Properties =======================================================

prop_sum_1() ->
  ?FORALL(X, proper_types:pos_integer(), X >= 0).

prop_sum_2() ->
  ?FORALL({X, Y},
          {proper_types:pos_integer(),proper_types:pos_integer()},
          X + Y =:= Y + X).

prop_sum_3() ->
  ?FORALL(X, proper_types:pos_integer(),
          ?FORALL(Y, proper_types:pos_integer(),
                  X + Y =:= Y + X)).

%% Exercise: Write the basic properties to verify
%% * ++ is commutative
%% * ++ is associative
%% * [] is the identity element.
%%
%% Hint, instead of =:= you may use proper:equals/2 for better reporting
int_list() -> proper_types:list(proper_types:integer()).

prop_concat_commutative() ->
  ?FORALL(
     {L1, L2}, {int_list(), int_list()},
     proper:equals(L1 ++ L2, L2 ++ L1)).

prop_concat_associative() ->
  ?FORALL(
     {L1, L2, L3}, {int_list(), int_list(), int_list()},
     proper:equals((L1 ++ L2) ++ L3, L1 ++ (L2 ++ L3))).

prop_concat_identity() ->
  ?FORALL(L, int_list(), proper:equals(L, L ++ [])).

%% For the operation op/2, verify the same properties (one doesn't hold)
prop_op_commutative() ->
  ?FORALL(
     {L1, L2}, {int_list(), int_list()},
     proper:equals(op(L1, L2), op(L2, L1))).

prop_op_associative() -> % This property doesn't hold!
  ?FORALL(
     {L1, L2, L3}, {int_list(), int_list(), int_list()},
     proper:equals(op(op(L1, L2), L3), op(L1, op(L2, L3)))).

%% But this would've passed!
op_associative_test() ->
  ?assertEqual(op(op([1], [2]), [3]), op([1], op([2], [3]))).

prop_op_identity() ->
  ?FORALL(L, int_list(), proper:equals(L, op(L, []))).

%%%_* Private Functions ================================================
op(A, B) when A < B -> A ++ B;
op(A, B)            -> B ++ A.

%%%_* Emacs ============================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
