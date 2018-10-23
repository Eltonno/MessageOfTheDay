%%%-------------------------------------------------------------------
%%% @author Florian Weiß, Ditmar Lange
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

%% Erstellt CMEM und gibt leeres CMEM zurück
initCMEM(RemTime,Datei) ->
  util:logging(Datei, "CMEM>>> initialisiert mit Lebenszeit " ++ util:to_String(RemTime) ++ " Sekunden\n"),
  %% CMEM besteht aus einer Liste von Tupeln, 
  %% bestehend aus Clients, 
  %% deren NNr, 
  %% und dem Timestamp der letzten Anfrage dieses Clients.
  {[], RemTime}.

%% Löscht CMEM und gibt ok zurück.
delCMEM(_CMEM) ->
  ok.


%% Speichert die letzte NNr, 
%% die der Client bekommen hat
updateClient(CMEM,ClientID,NNr,Datei) ->
  {CMEMlist, RemTime} = CMEM,
  %% Überprüfen ob ein Client mit dieser ClientID schon im CMEM (CMEMlist) ist.
  Member = member(ClientID,CMEMlist),
  case Member of
    true ->
      util:logging(Datei, "CMEM>>> Client " ++ util:to_String(ClientID) ++ " in CMEM eingefügt " ++ util:to_String(vsutil:now2UTC(erlang:timestamp())) ++ "\n"),
      %% Wenn ja, 
      %% seine NNr und seinen Timestamp aktualisieren
      CMEMlist_Tmp_1 = keystore3Tupel(ClientID, CMEMlist, {ClientID, NNr, erlang:timestamp()}),
      %% Alle Clients werden auf einen Timeout überprüft.
      %% Sollte einer vorhanden sein, 
      %% so wird der Client aus dem CMEM entfernt.
      deleteClients({CMEMlist_Tmp_1,RemTime},Datei);
    false ->
      util:logging(Datei, "CMEM>>> Client " ++ util:to_String(ClientID) ++ " wird aktualisiert\n"),
      %% Ansonsten den Client im CMEM speichern (keystore3Tupel), 
      %% zusammen mit der NNr und dem Timestamp.
      CMEMlist_Tmp_1 = keystore3Tupel(ClientID, CMEMlist, {ClientID, NNr, erlang:timestamp()}),
      %% Alle Clients werden auf einen Timeout überprüft.
      %% Sollte einer vorhanden sein, 
      %% so wird der Client aus dem CMEM entfernt.
      deleteClients({CMEMlist_Tmp_1,RemTime},Datei)
  end.

%% Gibt die NNr der Nachricht zurück, 
%% die der Client als nächstes bekommen sollte.
getClientNNr(CMEM,ClientID) ->
  {CMEMlist, RemTime} = CMEM,
  Client = keyfind3Tupel(ClientID, CMEMlist),
  case Client of
    false ->
      %% Wenn der Client nicht im CMEM vermerkt ist, 
      %% wird 1 zurückgegeben.
      1;
    _Otherwise ->
      %% Nach jeder Anfrage beim CMEM eines beliebigen Clients werden alle Clients auf den vordefinierten Timeout geprüft und gegebenenfalls entfernt.
      {_, CNNr, CTs} = Client,
      {_,Sec,_} = vsutil:diffTS(erlang:timestamp(),CTs),
      Delete = Sec >= RemTime,
      case Delete of
        false ->
          %% Ansonsten gebe die gespeicherte NNr des Clients + 1 zurück.
          CNNr + 1;
        true ->
          1
      end
  end
.

%% Iteriert durch die Clientliste des CMEMs und speichert alle ClientIDs und dessen zuletzt bekommene NNr in einer Liste solcher Tupel.
listCMEM(CMEM) ->
  {CMEMList, _} = CMEM,
  %% Die Liste wird daraufhin zurückgegeben.
  getClients(CMEMList).


%% Die dem CMEM bekannten Clients werden gezählt, 
%% und die Anzahl dann zurückgegeben.
lengthCMEM(CMEM) ->
  {CMEMList, _} = CMEM,
  listLength(CMEMList).

getClients([]) ->
  [];
getClients(L) ->
  getClients(L,[]).

getClients([],A) ->
  A;
getClients([{ID,NNr,_}|T],A) ->
  getClients(T,append(A,{ID,NNr})).

deleteClients({CMEMlist, RemTime},Datei) ->
  deleteClients(CMEMlist,[],RemTime,Datei).

deleteClients([],A, RemTime,_Datei) ->
  {A,RemTime};
deleteClients(CMEMlist, A, RemTime,Datei) ->
  [{CID,CNNr,CTs}|T] = CMEMlist,
  {_,Sec,_} = vsutil:diffTS(erlang:timestamp(),CTs),
  Delete = Sec >= RemTime,
  case Delete of
    true ->
      util:logging(Datei, "CMEM>>> Client " ++ util:to_String(CID) ++ " in CMEM gelöscht\n"),
      deleteClients(T,A,RemTime,Datei);
    false ->
      deleteClients(T,append(A,{CID,CNNr,CTs}),RemTime,Datei)
  end.

keyfind3Tupel(_,[]) ->
  false;
keyfind3Tupel(Key, Tuplelist) ->
  [Head|Rest] = Tuplelist,
  {K,_,_} = Head,
  if K == Key -> Head;
    true -> keyfind3Tupel(Key,Rest)
  end.

keystore3Tupel(_,[],Tupel) ->
  [Tupel];
keystore3Tupel(Key,[Head|Rest],Tupel) ->
  {K,_,_}= Head,
  if K == Key -> append(Tupel, Rest);
    true -> keystore3Tupel(Key,Rest,[Head],Tupel)
  end.

keystore3Tupel(_,[],Front,Tupel) ->
  append(Front, Tupel);
keystore3Tupel(Key,[Head|Rest],Front,Tupel) ->
  {K,_,_} = Head,
  if K == Key -> append(append(Front,Tupel),Rest);
    true -> keystore3Tupel(Key,Rest,append(Front,Head),Tupel)
  end.

append([], []) ->
  [];
append([],[H|T]) ->
  [H|T];
append([],Elem) ->
  [Elem];
append([H|T], []) ->
  [H|T];
append(Elem, []) ->
  [Elem];
append([H1|T1],[H2|T2]) ->
  append([H1|T1] ++ [H2], T2);
append([H|T],Elem) ->
  [H|T] ++ [Elem];
append(L,[H|T]) ->
  append([L] ++ [H], T);
append(E1,E2) ->
  [E1] ++ [E2].

listLength([]) ->
  0;
listLength(L) ->
  listLength(L,0).

listLength([],A) ->
  A;
listLength([_|T],A) ->
  listLength(T,A+1).

member(_,[]) ->
  false;
member(Elem, [H|T]) ->
  if
    H == Elem -> true;
    true -> member(Elem, T)
  end.
