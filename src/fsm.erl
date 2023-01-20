-module(fsm).
-include_lib("fsm.hrl").

%% API exports
-export([main/1]).

%%====================================================================
%% API functions
%%====================================================================

%% escript Entry point
main(Args) ->
    Options = options:get_options(Args),

    Options#options.infile /= undefined orelse options:print_usage() andalso erlang:halt(0),
    file_path_valid(Options#options.infile) orelse ?FATAL("Given file has wrong extension. Only pas files accespted"),
    file_exists(Options#options.infile) orelse ?FATAL("Given file is not accessible"),

    Infile = Options#options.infile,
    Outfile = case Options#options.outfile of
                  undefined -> default_outfile(Options#options.infile);
                  Any -> Any
              end,

    reader:start_link(Infile),
    writer:start_link(Outfile),

    {ok, {Pid, _}} = parser:start_link(reader, writer),
    erlang:link(Pid),
    parser:parse(),

    receive
        _ -> ok
    end,

    erlang:halt(0).

%%====================================================================
%% Internal functions
%%====================================================================

file_path_valid(undefined) ->
    ?ERR("File path is not defined"),
    false;
file_path_valid(FileName) ->
    filename:extension(string:lowercase(FileName)) == ".pas".


file_exists(FileName) ->
    filelib:is_regular(FileName).


default_outfile(FileName) ->
    string:replace(string:lowercase(FileName), ".pas", "-out.pas").
