%%% -*- coding: utf-8 -*-
%%% Author contact: alboo@list.ru


-define(DBG(F, A), io:format("DEBUG: " ++ F ++ "~n", A)).
-define(DBG(F), io:format("DEBUG: " ++ F ++ "~n")).
-define(LOG(F, A), io:format("INFO: " ++ F ++ "~n", A)).
-define(LOG(F), io:format("INFO: " ++ F ++ "~n")).
-define(WARN(F, A), io:format("WARNING: " ++ F ++ "~n", A)).
-define(WARN(F), io:format("WARNING: " ++ F ++ "~n")).
-define(ERR(F, A), io:format("ERROR: " ++ F ++ "~n", A)).
-define(ERR(F), io:format("ERROR: " ++ F ++ "~n")).


-define(FATAL(F, A), ?ERR(F, A) == ok andalso erlang:halt(0)).
-define(FATAL(F), ?ERR(F) == ok andalso erlang:halt(0)).
