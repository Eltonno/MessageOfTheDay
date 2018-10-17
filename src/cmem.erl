%%%-------------------------------------------------------------------
%%% @author Elton
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 23. Sep 2018 20:33
%%%-------------------------------------------------------------------
-module(cmem).
-author("Elton").

%% API
-export([initCMEM/2,delCMEM/1,updateClient/4,getClientNNr/2,listCMEM/1,lengthCMEM/1]).

%%TODO: Alle Funktionen implementieren

initCMEM(RemTime,Datei) ->
  util:logging(Datei, "CMEM initialisiert\nRemTime is: " ++ util:to_String(RemTime) ++ "\n"),
  {[], RemTime}.

delCMEM(CMEM) ->
  ok.

updateClient(CMEM,ClientID,NNr,Datei) ->
  ok.

getClientNNr(CMEM,ClientID) ->
  {CMEMlist, _} = CMEM,
  Exists = list:member(ClientID, CMEMlist),
  case Exists of
    false ->
      1;
    true ->
      {ClientID, NNr, ClientTimestamp} = list:keyfind(ClientID, CMEMlist),
      ok
  end
.

listCMEM(CMEM) ->
  ok.

lengthCMEM(CMEM) ->
  ok.