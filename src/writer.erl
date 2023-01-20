%%%-------------------------------------------------------------------
%%% @author alex
%%% @copyright (C) 2023, <COMPANY>
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(writer).

-behaviour(gen_server).
-include_lib("fsm.hrl").

-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
  code_change/3, write/1]).

-define(SERVER, ?MODULE).

-record(writer_state, {file :: string()}).

%%%===================================================================
%%% API
%%%===================================================================

start_link(Outfile) ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [Outfile], []).


write(Data) ->
  gen_server:call(?SERVER, {write, Data}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([Outfile]) ->
    ?DBG("Writer started for file \"~s\"", [Outfile]),
    {ok, #writer_state{file = Outfile}}.

handle_call({write, Data}, _From, State = #writer_state{file = Outfile}) ->
  {ok, Fd} = file:open(Outfile, [write, raw]),
  file:write(Fd, Data),
  file:close(Fd),
  {reply, ok, State};

handle_call(_Request, _From, State = #writer_state{}) ->
  {reply, ok, State}.

handle_cast(_Request, State = #writer_state{}) ->
  {noreply, State}.

handle_info(_Info, State = #writer_state{}) ->
  {noreply, State}.

terminate(_Reason, _State = #writer_state{}) ->
  ?DBG("TERMINATE WRITER"),
  ok.

code_change(_OldVsn, State = #writer_state{}, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
