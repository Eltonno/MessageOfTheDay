%%%-------------------------------------------------------------------
%%% @author Elton
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
  [NNr,_,_,_] = list:last(DLQ),
  NNr + 1.

push2DLQ([NNr,Msg,Tsclientout,Tshbqin],Queue,Datei) ->
  [DLQ,Size] = Queue,
  [list:append(DLQ,[[NNr,Msg,Tsclientout,Tshbqin,erlang:timestamp()]]),Size].

deliverMSG(MSGNr,ClientPID,Queue,Datei) ->
  [DLQ, _] = Queue,
  Bool = list:keyfind(MSGNr,DLQ),
  Expect = expectedNr(Queue),
  case Bool of
    false ->
      [NNr,Msg,TSclientout,TShbqin,TSdlqin] = list:first(DLQ),
      if
        (Expect - 1) > MSGNr ->
          ClientPID ! {reply, list:append([NNr,Msg,TSclientout,TShbqin,TSdlqin],erlang:timestamp()), false},
          NNr;
        (Expect - 1) == MSGNr ->
          ClientPID ! {reply, list:append([NNr,Msg,TSclientout,TShbqin,TSdlqin],erlang:timestamp()), true},
          NNr
      end;
    Otherwise ->
      [NNr,Msg,TSclientout,TShbqin,TSdlqin] = Bool,
      if
        (Expect - 1) > MSGNr ->
          ClientPID ! {reply, list:append(Bool,erlang:timestamp()), false},
          NNr;
        (Expect - 1) == MSGNr ->
          ClientPID ! {reply, list:append(Bool,erlang:timestamp()), true},
          NNr
      end
  end
.

listDLQ(Queue) ->
  getNNrs(Queue)
.

lengthDLQ(Queue) ->
  list:length(Queue).

getNNrs([]) ->
  [];
getNNrs(L) ->
  getNNrs(L,[]).

getNNrs([],A) ->
  A;
getNNrs([[NNr,_,_,_,_],T],A) ->
  getNNrs(T,append(A,NNr)).