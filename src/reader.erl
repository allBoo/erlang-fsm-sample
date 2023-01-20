%%%-------------------------------------------------------------------
%%% @author alex
%%% @copyright (C) 2023, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 05. Jan 2023 23:13
%%%-------------------------------------------------------------------
-module(reader).
-author("alex").

-behaviour(gen_server).
-include_lib("fsm.hrl").

%% API
-export([start_link/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
  code_change/3]).
-export([get_char/0]).

-define(SERVER, ?MODULE).

-record(reader_state, {file :: string()}).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Spawns the server and registers the local name (unique)
-spec(start_link(Infile :: string()) ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link(Infile) ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [Infile], []).


get_char() ->
  gen_server:call(?SERVER, get_char).


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @private
%% @doc Initializes the server
-spec(init(Args :: term()) ->
  {ok, State :: #reader_state{}} | {ok, State :: #reader_state{}, timeout() | hibernate} |
  {stop, Reason :: term()} | ignore).
init([Infile]) ->
  ?DBG("Reader started for file \"~s\"", [Infile]),

  {ok, Fd} = file:open(Infile, [read, raw]),
  {ok, #reader_state{file = Fd}}.

%% @private
%% @doc Handling call messages
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #reader_state{}) ->
  {reply, Reply :: term(), NewState :: #reader_state{}} |
  {reply, Reply :: term(), NewState :: #reader_state{}, timeout() | hibernate} |
  {noreply, NewState :: #reader_state{}} |
  {noreply, NewState :: #reader_state{}, timeout() | hibernate} |
  {stop, Reason :: term(), Reply :: term(), NewState :: #reader_state{}} |
  {stop, Reason :: term(), NewState :: #reader_state{}}).

handle_call(get_char, _From, State = #reader_state{file = Fd}) ->
  Char = file:read(Fd, 1),
  {reply, get_char_with_code(Char), State};
handle_call(_Request, _From, State = #reader_state{}) ->
  {reply, ok, State}.

%% @private
%% @doc Handling cast messages
-spec(handle_cast(Request :: term(), State :: #reader_state{}) ->
  {noreply, NewState :: #reader_state{}} |
  {noreply, NewState :: #reader_state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #reader_state{}}).
handle_cast(_Request, State = #reader_state{}) ->
  {noreply, State}.

%% @private
%% @doc Handling all non call/cast messages
-spec(handle_info(Info :: timeout() | term(), State :: #reader_state{}) ->
  {noreply, NewState :: #reader_state{}} |
  {noreply, NewState :: #reader_state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #reader_state{}}).
handle_info(_Info, State = #reader_state{}) ->
  {noreply, State}.

%% @private
%% @doc This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: #reader_state{}) -> term()).
terminate(_Reason, _State = #reader_state{file = Fd}) ->
  file:close(Fd),
  ok.

%% @private
%% @doc Convert process state when code is changed
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #reader_state{},
    Extra :: term()) ->
  {ok, NewState :: #reader_state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State = #reader_state{}, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

get_char_with_code(eof) ->
  eof;
get_char_with_code({ok, " "}) ->
  {ok, " ", empty};
get_char_with_code({ok, "\n"}) ->
  {ok, "\n", empty};
get_char_with_code({ok, "\r"}) ->
  {ok, "\r", empty};
get_char_with_code({ok, Char}) when is_list(Char) ->
  {ok, Char, string:lowercase(Char)};
get_char_with_code(AnyChar) ->
  {ok, AnyChar, AnyChar}.
