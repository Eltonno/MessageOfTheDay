%%%-------------------------------------------------------------------
%%% @author Florian Weiß, Ditmar Lange
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 23. Sep 2018 20:33
%%%-------------------------------------------------------------------
-module(dlq).
-author("Elton").

%% API
-export([initDLQ/2,delDLQ/1,expectedNr/1,push2DLQ/3,deliverMSG/4,listDLQ/1,lengthDLQ/1]).

%% Initialisiert die DLQ
initDLQ(Size,Datei) ->
  util:logging(Datei, "dlq>>> initialisiert mit Kapazität " ++ util:to_String(Size) ++ "\n"),
  %% Die DLQ enthält eine sortierte Liste, 
  %% in die die empfangenen Nachrichten geschrieben werden.
  %% Erstellt eine Queue DLQ mit Grösse size.
  [[], Size].

%% delDLQ(Queue) Löscht die DLQ und gibt ok zurück.
delDLQ(_) ->
  ok.

%% Liefert die NNr. 
%% der nächsten Nachricht, 
%% die in die DLQ geschrieben werden darf.
expectedNr([[],_]) ->
%% Wenn die DLQ leer ist, 
%% 1 zurückgeben
  1;
expectedNr(Queue) ->
  [DLQ, _] = Queue,
  %% Ansonsten solange durch die Nachrichten iterieren, 
  %% bis man bei der letzten angekommen ist (last(DLQ)).
  [NNr,_Msg,_TSclientout,_TShbqin,_TSdlqin] = last(DLQ),
  %% Dessen NNr + 1 zurückgeben.
  NNr + 1.

%% Speichert eine Nachricht in der DLQ und fügt einen Eingangszeitstempel an.
push2DLQ([NNr,Msg,Tsclientout,Tshbqin],Queue,Datei) ->
  [DLQ,Size] = Queue,
  Len = lengthDLQ(Queue),
  if
    Len < Size ->
      util:logging(Datei, "dlq>>> Nachricht " ++ util:to_String(NNr) ++ " in DLQ eingefügt.\n"),
      [append(DLQ,[[NNr,Msg,Tsclientout,Tshbqin,erlang:timestamp()]]),Size];
    true ->
      %% Wenn die DLQ voll ist, wird die älteste Nachricht aus der DLQ entfernt.
      [[DNNr,_Msg,_Tsclientout,_Tshbqin,_Tsdlqin]|T] = DLQ,
      util:logging(Datei, "dlq>>> Nachricht " ++ util:to_String(DNNr) ++ " aus DLQ gelöscht.\n"),
      util:logging(Datei, "dlq>>> Nachricht " ++ util:to_String(NNr) ++ " in DLQ eingefügt.\n"),
      [append(T,[[NNr,Msg,Tsclientout,Tshbqin,erlang:timestamp()]]),Size]
  end.

%% Sendet eine Nachricht an den Leser.
deliverMSG(MSGNr,ClientPID,Queue,Datei) ->
  [DLQ, _] = Queue,
  %% Überprüft ob in der DLQ eine Nachricht mit NNr gleich MSGNr vorhanden ist (keyfind(MSGNr, DLQ)).
  Bool = keyfind(MSGNr,DLQ),
  Expect = expectedNr(Queue),
  TSdlqout = erlang:timestamp(),

  case Bool of
    false ->
      %% Falls diese Nachricht nicht vorhanden ist, wird die erste Nachricht der DLQ genommen.
      NNNr = findNext(MSGNr,Expect,DLQ),
      %util:logging(Datei, "dlq>>> Nachricht " ++ util:to_String(NNNr) ++ " ist die nächste in der DLQ vorhandene Nachricht nach " ++ util:to_String(MSGNr) ++ ".\n"),
      [NNr,Msg,TSclientout,TShbqin,TSdlqin] = keyfind(NNNr,DLQ),
      [LNNr,_,_,_,_] = last(DLQ),
      %% Überprüft ob weitere Nachrichten in der DLQ vorhanden sind, 
      %% indem die nächste von der DLQ erwartete NNr (Expect) mit der NNr der Nachricht (MSGNr) verglichen wird.
      if
        LNNr < MSGNr ->
          ClientPID ! {reply, [-1, nokA, 0,0,0,0], true},
          util:logging(Datei, "dlq>>> Nachricht " ++ util:to_String(NNr) ++ " an Client " ++ util:to_String(ClientPID) ++ " ausgeliefert.\n"),
          -1;
        (Expect - 1) > MSGNr ->
          %% Fügt der Nachricht einen Zeitstempel TSdlqout an.
          %% Ansonsten ((Expect - 1) > MSGNr) ist ‘Terminated’ ‘false’.
          %% Sendet die Nachricht an den Client.
          ClientPID ! {reply,[NNr,Msg ++ util:to_String(vsutil:now2string(TSdlqout)),TSclientout,TShbqin,TSdlqin,TSdlqout], false},
          util:logging(Datei, "dlq>>> Nachricht " ++ util:to_String(NNr) ++ " an Client " ++ util:to_String(ClientPID) ++ " ausgeliefert.\n"),
          %% Gibt die gesendete NNr zurück.
          NNr;
        (Expect - 1) == MSGNr ->
          %% Ist die erwartete NNr (Expect) um 1 größer als die dieser Nachricht (MSGNr), 
          %% so wird der Parameter ‘Terminated’ auf ‘true’ gesetzt, 
          %% als Zeichen dafür, 
          %% dass dies die letzte Nachricht ist.
          ClientPID ! {reply, [NNr,Msg ++ util:to_String(vsutil:now2string(TSdlqout)),TSclientout,TShbqin,TSdlqin,erlang:timestamp()], true},
          util:logging(Datei, "dlq>>> Nachricht " ++ util:to_String(NNr) ++ " an Client " ++ util:to_String(ClientPID) ++ " ausgeliefert.\n"),
          NNr
      end;
    _Otherwise ->
      [NNr,Msg,TSclientout,TShbqin,TSdlqin] = Bool,
      if
        (Expect - 1) > MSGNr ->
          ClientPID ! {reply, [NNr,Msg ++ util:to_String(vsutil:now2string(TSdlqout)),TSclientout,TShbqin,TSdlqin,TSdlqout], false},
          util:logging(Datei, "dlq>>> Nachricht " ++ util:to_String(NNr) ++ " an Client " ++ util:to_String(ClientPID) ++ " ausgeliefert.\n"),
          NNr;
        (Expect - 1) == MSGNr ->
          ClientPID ! {reply, [NNr,Msg ++ util:to_String(vsutil:now2string(TSdlqout)),TSclientout,TShbqin,TSdlqin,TSdlqout], true},
          util:logging(Datei, "dlq>>> Nachricht " ++ util:to_String(NNr) ++ " an Client " ++ util:to_String(ClientPID) ++ " ausgeliefert.\n"),
          NNr
      end
  end
.

%% Erzeugt eine Liste der in der DLQ vorhandenen Nachrichtnummern und gibt diese zurück.
listDLQ(Queue) ->
  [DLQ,_] = Queue,
  getNNrs(DLQ).

%% Zählt die Anzahl der Nachrichten, 
%% die in der DLQ sind, 
%% und gibt diese zurück.
lengthDLQ(Queue) ->
  [DLQ,_] = Queue,
  listLength(DLQ).

getNNrs([]) ->
  [];
getNNrs(L) ->
  getNNrs(L,[]).

getNNrs([],A) ->
  A;
getNNrs([[NNr,_,_,_,_]|T],A) ->
  getNNrs(T,append(A,NNr)).

last([]) ->
  [-1, nokA, 0,0,0,0];
last([Head|[]]) ->
  Head;
last([_|Tail]) ->
  last(Tail).

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

keyfind(_,[]) ->
  false;
keyfind(Key, Tuplelist) ->
  [Head|Rest] = Tuplelist,
  [NNr,_Msg,_TSclientout,_TShbqin,_TSdlqin] = Head,
  if NNr == Key -> Head;
    true -> keyfind(Key,Rest)
  end.

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
member(Elem, [[NNr,_Msg,_TSclientout,_TShbqin,_TSdlqin]|T]) ->
  if
    NNr == Elem -> true;
    true -> member(Elem, T)
  end.


findNext(INNr,ExpNr,DLQ) ->
  Member = member(INNr,DLQ),
  if
    INNr >= ExpNr ->
      -1;
    true ->
      case Member of
        true ->
          INNr;
        false ->
          findNext(INNr+1,ExpNr,DLQ)
      end
  end.
