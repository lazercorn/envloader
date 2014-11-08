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
                  "'(?:\\'|[^'])*'" ++      % single quoted value
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

autoload() -> autoload(find_dotenv()).

autoload({ok, Path}) -> load(Path);
autoload(Error={error, _Reason}) -> Error.

load(Dotenv) ->
  {ok, Raw} = file:read_file(Dotenv),
  {match, Match} = re:run(Raw, ?PATTERN, [global, {capture, all, list}]),
  [os:putenv(VarName, Value) || [_All, VarName, Value] <- Match],
  ok.

%% ====================================================================
%% Private Functions
%% ====================================================================

find_dotenv() ->
  {ok, Cwd} = file:get_cwd(),
  Path = filename:join([Cwd, ".env"]),
  Exists = filelib:is_file(Path),
  find_dotenv(Path, Exists).

find_dotenv(Path, true) -> {ok, Path};
find_dotenv(_Path, false) -> {error, not_found}.
