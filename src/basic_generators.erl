%%% @doc
-module(basic_generators).

-compile([export_all]).

%%% For this exercise you should use only the types exported by
%%% proper_types. Don't bother about ?LET, ?SIZED, ?SUCHTHAT, etc.

%% A dice throw (i.e. an integer in [1, 6])
die_throw() ->
  proper_types:choose(1, 6).

%% Two dice throws, e.g. {1, 6}, {2, 4}, ...
two_throws() ->
  {die_throw(), die_throw()}.

%% Generate things like a, b, ..., z
small_letter() ->
  proper_types:choose($a, $z).

%% Generate things like A, B, ..., Z
big_letter() ->
  proper_types:choose($A, $Z).

%% Either a big or a small letter
letter() ->
  proper_types:oneof([big_letter(), small_letter()]).

%% Generate strings like "askfjKKEp"
printable_string() ->
  proper_types:list(letter()).

%% A sentence. This should generate a lower case string with words (random lists
%% of letters) of around 5 letters.
%% Don't worry about duplicated spaces (for now :))
sentence() ->
  Spacey =
    proper_types:frequency(
      [{1, space()},
       {5, small_letter()}]),
  proper_types:list(Spacey).

space() -> $ .

%% A printable string starting with capital letter, like "Askfjkkep"
'PrintableString'() ->
  [big_letter() | proper_types:list(small_letter())].

%% A gb_set (use integers as elements)
%% Hint, this may be hard
gb_set() ->
  %% This doesn't work! gb_sets:from_list needs an actual list, not a proper
  %% generator!
  %% We'll see how to implement this later
  gb_sets:from_list(proper_types:list(proper_types:integer())).

%%%_* Emacs ============================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
