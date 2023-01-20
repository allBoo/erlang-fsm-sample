%%%-------------------------------------------------------------------
%%% @author alex
%%% @copyright (C) 2023, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 05. Jan 2023 23:46
%%%-------------------------------------------------------------------
-module(parser).
-author("alex").

-behaviour(gen_statem).
-include_lib("fsm.hrl").

%% API
-export([start_link/2, parse/0, stop/0]).

%% gen_statem callbacks
-export([init/1, handle_event/3, terminate/3, code_change/4, callback_mode/0]).
% state functions
-export([initial/3, done/3, read_head/3, read_body/3, start_loop/3, read_loop_condition/3,
  read_loop_body_header/3, read_loop_body/3, end_loop/3]).

-define(SERVER, ?MODULE).

-record(loop, {
  depth = 0     :: integer(),
  label = ""    :: string(),
  type = inline :: normal|inline
}).

-record(parser_state, {
  reader          :: module(),
  writer          :: module(),
  header = ""     :: string(),
  body = ""       :: string(),
  char            :: char(),
  word = ""       :: string(),
  c_word = ""     :: string(),
  condition = ""  :: string(),
  loops_data      :: queue:queue(),
  loops_count = 0 :: integer(),
  block_depth = 0 :: integer()
}).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Creates a gen_statem process which calls Module:init/1 to
%% initialize. To ensure a synchronized start-up procedure, this
%% function does not return until Module:init/1 has returned.
start_link(ReaderModule, WriterModule) ->
  gen_statem:start_monitor({local, ?SERVER}, ?MODULE, [ReaderModule, WriterModule], []).


parse() ->
  gen_statem:cast(?SERVER, start).

stop() ->
  gen_statem:stop({local, ?SERVER}).


%%%===================================================================
%%% gen_statem callbacks
%%%===================================================================

%% @private
%% @doc Whenever a gen_statem is started using gen_statem:start/[3,4] or
%% gen_statem:start_link/[3,4], this function is called by the new
%% process to initialize.
init([ReaderModule, WriterModule]) ->
    ?DBG("Parser started"),
    {ok, initial, #parser_state{reader = ReaderModule, writer = WriterModule, loops_data = queue:new()}}.

%% @private
%% @doc This function is called by a gen_statem when it needs to find out
%% the callback mode of the callback module.
callback_mode() ->
  state_functions.

%% @private
%% @doc If callback_mode is handle_event_function, then whenever a
%% gen_statem receives an event from call/2, cast/2, or as a normal
%% process message, this function is called.
handle_event(A, B, Data) ->
  %% Ignore all other events
  ?DBG("Unknown event ~p ~p ~p", [A, B, Data]),
  {keep_state, Data}.

%% @private
%% @doc This function is called by a gen_statem when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_statem terminates with
%% Reason. The return value is ignored.
terminate(_Reason, _StateName, _State = #parser_state{}) ->
  ?DBG("TERMINATE PARSER"),
  ok.

%% @private
%% @doc Convert process state when code is changed
code_change(_OldVsn, StateName, State = #parser_state{}, _Extra) ->
  {ok, StateName, State}.

%%%===================================================================
%%% State functions
%%%===================================================================


%% @private
%% @doc There should be one instance of this function for each possible
%% state name.  If callback_mode is state_functions, one of these
%% functions is called when gen_statem receives and event from
%% call/2, cast/2, or as a normal process message.
initial(cast, start, State = #parser_state{}) ->
  ?DBG("Initial state call"),
  {next_state, read_head, State#parser_state{char = reader:get_char()}, 0}.


read_head(_, _, State = #parser_state{word = Word, c_word = "begin", char = {ok, Char, empty}}) ->
  {next_state, read_body, State#parser_state{body = Word ++ Char, word = "", c_word = "", char = reader:get_char()}, 0};

read_head(_, _, State = #parser_state{header = Header, word = Word, char = {ok, Char, empty}}) ->
  {next_state, read_head, State#parser_state{header = Header ++ Word ++ Char, word = "", c_word = "", char = reader:get_char()}, 0};

read_head(_, _, State = #parser_state{word = Word, c_word = CWord, char = {ok, Char, Code}}) ->
  {next_state, read_head, State#parser_state{word = Word ++ Char, c_word = CWord ++ Code, char = reader:get_char()}, 0};

read_head(_, _, State = #parser_state{char = eof}) ->
  {next_state, done, State, 0}.



read_body(_, _, State = #parser_state{c_word = "while", char = {ok, Char, empty}}) ->
  {next_state, start_loop, State#parser_state{word = "", c_word = "", char = Char}, 0};

read_body(_, _, State = #parser_state{body = Body, word = Word, char = {ok, Char, empty}}) ->
  {next_state, read_body, State#parser_state{body = Body ++ Word ++ Char, word = "", c_word = "", char = reader:get_char()}, 0};

read_body(_, _, State = #parser_state{word = Word, c_word = CWord, char = {ok, Char, Code}}) ->
  {next_state, read_body, State#parser_state{word = Word ++ Char, c_word = CWord ++ Code, char = reader:get_char()}, 0};

read_body(_, _, State = #parser_state{char = eof}) ->
  {next_state, done, State, 0}.



start_loop(_, _, State = #parser_state{header = Header, body = Body, loops_count = LoopsCount, block_depth = BlockDepth, loops_data = LoopsData}) ->
  LabelName = label_name(LoopsCount + 1),

  LoopDepth = case queue:out_r(LoopsData) of {empty, _} -> 1; {{value, #loop{depth = CurrentLoopDepth}}, _} -> CurrentLoopDepth + 1 end,
  LoopInfo = #loop{label = LabelName, depth = LoopDepth},

  ?DBG("Start loop ~p with depth ~p", [LabelName, LoopDepth]),

  {next_state, read_loop_condition, State#parser_state{
    word = "",
    c_word = "",
    condition = "",
    header = Header ++ "label " ++ LabelName ++ ";\n",
    body = Body ++ LabelName ++ ": ",
    loops_count = LoopsCount + 1,
    block_depth = BlockDepth + 1,
    loops_data = queue:in(LoopInfo, LoopsData),
    char = reader:get_char()
  }, 0}.


end_loop(_, _, State = #parser_state{body = Body, loops_data = LoopsData, block_depth = BlockDepth}) ->
  {{value, #loop{label = LoopLabel, depth = LoopDepth}}, NewLoopsData} = queue:out_r(LoopsData),

  %% when it is a first-level loop - close it and back to the body parser
  %% otherwise - if the next-level loop is of the inline type - close it too, else go to the body parser
  NextState = case LoopDepth == 1 of
                true -> read_body;
                false ->
                  {{value, #loop{type = NextLoopType}}, _} = queue:out_r(NewLoopsData),
                  case NextLoopType of normal -> read_loop_body; inline -> end_loop end
              end,

  ?DBG("End loop ~p with depth ~p and go to the ~p", [LoopLabel, LoopDepth, NextState]),

  {next_state, NextState, State#parser_state{
    word = "",
    c_word = "",
    body = Body ++ "goto " ++ LoopLabel ++ "; end;\n",
    loops_data = NewLoopsData,
    block_depth = BlockDepth - 1,
    char = reader:get_char()
  }, 0}.


read_loop_condition(_, _, State = #parser_state{body = Body, condition = Condition, c_word = "do", char = {ok, _, empty}}) ->
  ?DBG("Got loop condition ~p", [Condition]),

  {next_state, read_loop_body_header, State#parser_state{
    word = "",
    c_word = "",
    body = Body ++ "if " ++ Condition ++ " then begin\n",
    char = reader:get_char()
  }, 0};

read_loop_condition(_, _, State = #parser_state{condition = Condition, word = Word, char = {ok, Char, empty}}) ->
  {next_state, read_loop_condition, State#parser_state{condition = Condition ++ Word ++ Char, word = "", c_word = "", char = reader:get_char()}, 0};

read_loop_condition(_, _, State = #parser_state{word = Word, c_word = CWord, char = {ok, Char, Code}}) ->
  {next_state, read_loop_condition, State#parser_state{word = Word ++ Char, c_word = CWord ++ Code, char = reader:get_char()}, 0}.



read_loop_body_header(_, _, State = #parser_state{body = Body, word = "", char = {ok, Char, empty}}) ->   %% ignore any empty symbols
  {next_state, read_loop_body_header, State#parser_state{body = Body ++ Char, word = "", c_word = "", char = reader:get_char()}, 0};

read_loop_body_header(_, _, State = #parser_state{loops_data = LoopsData, c_word = "begin", char = {ok, _, empty}}) ->   %% go to the full-body mode if there is a begin keyword
  ?DBG("Loop type changed to the normal"),
  {{value, ActiveLoop = #loop{}}, LoopsDataTmp} = queue:out_r(LoopsData),
  NewLoopsData = queue:in(ActiveLoop#loop{type = normal}, LoopsDataTmp),
  {next_state, read_loop_body, State#parser_state{loops_data = NewLoopsData, word = "", c_word = "", char = reader:get_char()}, 0};

read_loop_body_header(_, _, State = #parser_state{c_word = "while", char = {ok, Char, empty}}) ->   %% new inner loop
  {next_state, start_loop, State#parser_state{word = "", c_word = "", char = Char}, 0};

read_loop_body_header(_, _, State = #parser_state{body = Body, word = Word, char = {ok, Char, empty}}) ->
  {next_state, read_loop_body_header, State#parser_state{body = Body ++ Word ++ Char, word = "", c_word = "", char = reader:get_char()}, 0};

read_loop_body_header(_, _, State = #parser_state{body = Body, word = Word, char = {ok, Char, ";"}}) ->   %% the inline-body mode
  {next_state, end_loop, State#parser_state{body = Body ++ Word ++ Char ++ "\n", word = "", c_word = "", char = Char}, 0};

read_loop_body_header(_, _, State = #parser_state{word = Word, c_word = CWord, char = {ok, Char, Code}}) ->
  {next_state, read_loop_body_header, State#parser_state{word = Word ++ Char, c_word = CWord ++ Code, char = reader:get_char()}, 0};

read_loop_body_header(_, _, State = #parser_state{char = eof}) ->
  {next_state, done, State, 0}.


read_loop_body(_, _, State = #parser_state{c_word = "while", char = {ok, Char, empty}}) ->   %% new inner loop
  {next_state, start_loop, State#parser_state{word = "", c_word = "", char = Char}, 0};

%% increase Block Counter by the "begin" keyword
read_loop_body(_, _, State = #parser_state{block_depth = BlockDepth, word = Word, body = Body, c_word = "begin", char = {ok, Char, empty}}) ->
  ?DBG("Found begin block inside the loop"),
  {next_state, read_loop_body, State#parser_state{block_depth = BlockDepth + 1, body = Body ++ Word ++ Char, word = "", c_word = "", char = reader:get_char()}, 0};

%% the "end" keyword may indicate the end of block or the end of loop
%% when the Block Counter is equals to the Loop Index this means the end of loop
read_loop_body(_, _, State = #parser_state{block_depth = BlockDepth, loops_data = LoopsData, word = Word, body = Body, c_word = "end;", char = {ok, Char, empty}}) ->
  {{value, #loop{depth = LoopDepth}}, _} = queue:out_r(LoopsData),
  case BlockDepth == LoopDepth of
    %% the end of the loop
    true -> {next_state, end_loop, State#parser_state{block_depth = BlockDepth, word = "", c_word = "", char = Char}, 0};
    %% the end of the block
    false ->
      ?DBG("Found end of block inside the loop"),
      {next_state, read_loop_body, State#parser_state{block_depth = BlockDepth - 1, body = Body ++ Word ++ Char, word = "", c_word = "", char = reader:get_char()}, 0}
  end;

read_loop_body(_, _, State = #parser_state{body = Body, word = Word, char = {ok, Char, empty}}) ->
  {next_state, read_loop_body, State#parser_state{body = Body ++ Word ++ Char, word = "", c_word = "", char = reader:get_char()}, 0};

read_loop_body(_, _, State = #parser_state{word = Word, c_word = CWord, char = {ok, Char, Code}}) ->
  {next_state, read_loop_body, State#parser_state{word = Word ++ Char, c_word = CWord ++ Code, char = reader:get_char()}, 0};

read_loop_body(_, _, State = #parser_state{char = eof}) ->
  {next_state, done, State, 0}.



done(_EventType, _EventContent, #parser_state{header = Header, body = Body}) ->
  ?DBG("End of file reached"),
  writer:write(Header ++ Body),
  stop.


label_name(LoopCntr) -> "wh" ++ integer_to_list(LoopCntr).

