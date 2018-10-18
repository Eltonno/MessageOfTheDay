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

initCMEM(RemTime,Datei) ->
  util:logging(Datei, "CMEM initialisiert\nRemTime is: " ++ util:to_String(RemTime) ++ "\n"),
  {[], RemTime}.

delCMEM(_CMEM) ->
  ok.

updateClient(CMEM,ClientID,NNr,Datei) ->
  {CMEMlist, RemTime} = CMEM,
  CMEMlist_Tmp_1 = keystore(ClientID, CMEMlist, {ClientID, NNr, erlang:timestamp()}),
  util:logging(Datei, "CMEM>> Client " ++ util:to_String(ClientID) ++ " upgedated\n"),
  deleteClients({CMEMlist_Tmp_1,RemTime}).

getClientNNr(CMEM,ClientID) ->
  {CMEMlist, RemTime} = CMEM,
  Client = keyfind3Tupel(ClientID, CMEMlist),
  case Client of
    false ->
      1;
    _Otherwise ->
      {_, CNNr, CTs} = Client,
      {_,Sec,_} = vsutil:diffTS(erlang:timestamp(),CTs),
      Delete = Sec >= RemTime,
      case Delete of
        true ->
          1;
        false -> CNNr + 1
      end
  end
.

listCMEM(CMEM) ->
  {CMEMList, _} = CMEM,
  getClients(CMEMList).

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

deleteClients({CMEMlist, RemTime}) ->
  deleteClients(CMEMlist,[],RemTime).

deleteClients([],A, RemTime) ->
  {A,RemTime};
deleteClients(CMEMlist, A, RemTime) ->
  [{CID,CNNr,CTs}|T] = CMEMlist,
  {_,Sec,_} = vsutil:diffTS(erlang:timestamp(),CTs),
  Delete = Sec >= RemTime,
  case Delete of
    true ->
      deleteClients(T,A,RemTime);
    false ->
      deleteClients(T,append(A,{CID,CNNr,CTs}),RemTime)
  end.

keyfind3Tupel(_,[]) ->
  false;
keyfind3Tupel(Key, Tuplelist) ->
  [Head|Rest] = Tuplelist,
  {K,_,_} = Head,
  if K == Key -> Head;
    true -> keyfind3Tupel(Key,Rest)
  end
.

keystore(_,[],Tupel) ->
  [Tupel];
keystore(Key,[Head|Rest],Tupel) ->
  {K,_}= Head,
  if K == Key -> append(Tupel, Rest);
    true -> keystore(Key,Rest,[Head],Tupel)
  end
.

keystore(_,[],Front,Tupel) ->
  append(Front, Tupel);
keystore(Key,[Head|Rest],Front,Tupel) ->
  {K,_} = Head,
  if K == Key -> append(append(Front,Tupel),Rest);
    true -> keystore(Key,Rest,append(Front,Head),Tupel)
  end
.

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
  [E1] ++ [E2]
.

listLength([]) ->
  0;
listLength(L) ->
  length(L,0).

listLength([],A) ->
  A;
listLength([_|T],A) ->
  length(T,A+1).