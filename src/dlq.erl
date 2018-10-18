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

initDLQ(Size,Datei) ->
  util:logging(Datei, "dlq>>> initialisiert mit Kapazität " ++ Size ++ "\n"),
  [[], Size].

delDLQ(_) ->
  ok.

expectedNr([]) ->
  1;
expectedNr(Queue) ->
  [DLQ, _] = Queue,
  [NNr,_,_,_] = last(DLQ),
  NNr + 1.

%%TODO Testen und Loggen einfügen
push2DLQ([NNr,Msg,Tsclientout,Tshbqin],Queue,Datei) ->
  [DLQ,Size] = Queue,
  Len = lengthDLQ(Queue),
  if
    Len < Size ->
      [append(DLQ,[[NNr,Msg,Tsclientout,Tshbqin,erlang:timestamp()]]),Size];
    true ->
      [_|T] = DLQ,
      [append(T,[[NNr,Msg,Tsclientout,Tshbqin,erlang:timestamp()]]),Size]
  end.

deliverMSG(MSGNr,ClientPID,Queue,Datei) ->
  [DLQ, _] = Queue,
  Bool = keyfind(MSGNr,DLQ),
  Expect = expectedNr(Queue),
  TSdlqout = erlang:timestamp(),

  case Bool of
    false ->
      [NNr,Msg,TSclientout,TShbqin,TSdlqin] = first(DLQ),
      [LNNr, _, _,_,_,_] = last(DLQ),
      if
        LNNr < MSGNr ->
          ClientPID ! {reply, [-1, nokA, 0,0,0,0], false};
        (Expect - 1) > MSGNr ->
          ClientPID ! {reply,[NNr,Msg ++ util:to_String(vsutil:now2string(TSdlqout)),TSclientout,TShbqin,TSdlqin,TSdlqout], false},
          NNr;
        (Expect - 1) == MSGNr ->
          ClientPID ! {reply, [NNr,Msg ++ util:to_String(vsutil:now2string(TSdlqout)),TSclientout,TShbqin,TSdlqin,erlang:timestamp()], true},
          NNr
      end;
    _Otherwise ->
      [NNr,Msg,TSclientout,TShbqin,TSdlqin] = Bool,
      if
        (Expect - 1) > MSGNr ->
          ClientPID ! {reply, [NNr,Msg ++ util:to_String(vsutil:now2string(TSdlqout)),TSclientout,TShbqin,TSdlqin,TSdlqout], false},
          NNr;
        (Expect - 1) == MSGNr ->
          ClientPID ! {reply, [NNr,Msg ++ util:to_String(vsutil:now2string(TSdlqout)),TSclientout,TShbqin,TSdlqin,TSdlqout], true},
          NNr
      end
  end
.

listDLQ(Queue) ->
  [DLQ,_] = Queue,
  getNNrs(DLQ)
.

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
  last(Tail)
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

first([Head|_]) ->
  Head
.

last([]) ->
  nil;
last([Head|[]]) ->
  Head;
last([_|Tail]) ->
  last(Tail)
.

keyfind(_,[]) ->
  false;
keyfind(Key, Tuplelist) ->
  [Head|Rest] = Tuplelist,
  {K,_} = Head,
  if K == Key -> Head;
    true -> keyfind(Key,Rest)
  end
.

listLength([]) ->
  0;
listLength(L) ->
  listLength(L,0).

listLength([],A) ->
  A;
listLength([_|T],A) ->
  listLength(T,A+1).