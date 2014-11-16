-module(envloader_tests).
-include_lib("eunit/include/eunit.hrl").

envloader_test_() ->
  {setup, fun setup/0, fun(_Setup) -> {inparallel, tests()} end}.

tests() ->
  [{"Sets single quoted values", single_quoted_()},
   {"Sets double quoted values", double_quoted_()},
   {"Sets unquoted values", unquoted_()}].

setup() ->
  EnvPath = filename:join([fixture_path(), "dotenv-test"]),
  envloader:load(EnvPath).

single_quoted_() ->
  ?_assertEqual("works", os:getenv("SINGLE_QUOTED")).

double_quoted_() ->
  ?_assertEqual("works", os:getenv("DOUBLE_QUOTED")).

unquoted_() ->
  ?_assertEqual("works", os:getenv("UNQUOTED")).

%% Rebar copies eunit tests into a custom directory
%% Test for the existence of a rebar-like directory before returning
%% a path used for loading fixtures.
fixture_path() ->
  Alt1 = filename:join(["./test", "fixtures"]),
  % Possible execution from rebar
  Alt2 = filename:join(["../test", "fixtures"]),
  case filelib:is_dir(Alt1) of
    true -> Alt1;
    false ->
      case filelib:is_dir(Alt2) of
        true -> Alt2;
        false -> erlang:throw(fixture_path_not_found)
      end
  end.
