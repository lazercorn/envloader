-module(envloader).

%% API
-export([autoload/0, load/1]).

%% Regex for parsing a .env file. Based on the regex from the original
%% dotenv library for Ruby.
-define(PATTERN, "^" ++
                 "(?:export\\s+)?" ++       % optional export
                 "([\\w\\.]+)" ++           % key
                 "(?:\\s*=\\s*|:\\s+?)" ++  % separator
                 "(" ++                     % optional value begin
                  "'(?:\'|[^'])*'" ++      % single quoted value
                  "|" ++                    % or
                  "\"(?:\"|[^\"])*\"" ++    % double quoted value
                  "|" ++                    % or
                  "[^#\\n]+" ++             % unquoted value
                 ")?" ++                    % value end
                 "(?:\\s*\\#.*)?" ++        % optional comment
                 "$").

%% ====================================================================
%% API Functions
%% ====================================================================

autoload() ->
  {ok, Cwd} = file:get_cwd(),
  Path = filename:join([Cwd, ".env"]),
  case filelib:is_file(Path) of
    true -> load(Path);
    false -> {error, not_found}
  end.

load(Dotenv) ->
  case file:read_file(Dotenv) of
    {ok, File} ->
      Lines = binary:split(File, <<"\n">>, [global, trim]),
      [process_line(Line) || Line <- Lines],
      ok;
    {error, enoent} ->
      ErrorMsg = io_lib:format("Unable to load file ~s", [Dotenv]),
      erlang:error(lists:flatten(ErrorMsg))
  end.

%% ====================================================================
%% Private Functions
%% ====================================================================

process_line(Line) ->
  case re:run(Line, ?PATTERN, [global, {capture, all, list}]) of
    {match, Match} ->
      [set_env(VarName, Value) || [_All, VarName, Value] <- Match];
    nomatch ->
      ErrorMsg = io_lib:format("Unable to process ~p", [Line]),
      erlang:error(lists:flatten(ErrorMsg))
  end.

set_env(VarName, RawValue) ->
  % Remove quotes and double quotes from the matching value
  Value = re:replace(RawValue, "['\"]", "", [global, {return, list}]),
  os:putenv(VarName, Value).
