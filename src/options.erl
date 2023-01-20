%%%-------------------------------------------------------------------
%%% @author alex
%%% @copyright (C) 2023, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 05. Jan 2023 20:19
%%%-------------------------------------------------------------------
-module(options).
-author("alex").
-include_lib("fsm.hrl").

%% API
-export([get_options/1, print_usage/0]).

options_list() ->
  [
    {outfile, $o,        "outfile", string, "Output file name"},
    {file,    undefined, undefined, string, "Input file"}
  ].

get_options(Args) ->
  {ok,{Config, _}} = getopt:parse(options_list(), Args),
  parse_config(Config, #options{}).


print_usage() ->
  getopt:usage(options_list(), "fsm"),
  true.



parse_config([{file, File} | T], Config) ->
  parse_config(T, Config#options{infile = File});
parse_config([{outfile, File} | T], Config) ->
  parse_config(T, Config#options{outfile = File});
parse_config([_ | T], Config) ->
  parse_config(T, Config);
parse_config([], Config) -> Config.


