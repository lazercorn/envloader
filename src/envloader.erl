-module(envloader).

%% Application callbacks
-export([load/1]).

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

load(Dotenv) ->
  {ok, Raw} = file:read_file(Dotenv),
  {match, Match} = re:run(Raw, ?PATTERN, [global, {capture, all, list}]),
  [os:putenv(VarName, Value) || [_All, VarName, Value] <- Match].
