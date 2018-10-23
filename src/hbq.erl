%%%-------------------------------------------------------------------
%%% @author Florian Weiß, Ditmar Lange
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 23. Sep 2018 20:33
%%%-------------------------------------------------------------------
-module(hbq).
-author("Elton").

%% API
-export([startHBQ/0,startHBQ/1]).

-define(RECHNER_NAME, erlang:node()).
-define(LOGFILE, "HB-DLQ" ++ atom_to_list(?RECHNER_NAME) ++".log").

startHBQ() ->
  startHBQ("server.cfg").

startHBQ(File) ->
  util:logging(?LOGFILE, "HBQ>>> gestartet\n"),
  {ok, ConfigListe} = file:consult(File),
  {ok, HBQname} = vsutil:get_config_value(hbqname, ConfigListe),
  {ok, DlqLimit} = vsutil:get_config_value(dlqlimit, ConfigListe),
  erlang:register(HBQname, self()),
  receive
    {SPID,{request,initHBQ}} ->
      util:logging(?LOGFILE, "HBQ>>> init empfangen\n"),
      DLQ = dlq:initDLQ(DlqLimit,?LOGFILE),
      SPID ! {reply,ok},
      Lim = DlqLimit * 2 / 3,
      loop(Lim, [], DLQ)
  end
.
%%TODO Msg anpassen mit HBQin und now2String
loop(Limit,HBQ,DLQ) ->
  util:logging(?LOGFILE, "HBQ>>> wartet auf Nachricht\n"),
  receive
    {SPID, {request,pushHBQ,[NNr,Msg,TSclientout]}} ->
      util:logging(?LOGFILE, "HBQ>>> Speichern der Nachricht " ++ util:to_String(NNr) ++ " in der HBQ eingeleitet\n"),
      TShbqin = erlang:timestamp(),
      DLQNNr = dlq:expectedNr(DLQ),
      SortedHBQ = bubblesort(HBQ),
      LenghtHBQ = length(HBQ),
      if
        NNr < DLQNNr ->
          util:logging(?LOGFILE, "HBQ>>> Nachricht " ++ util:to_String(NNr) ++ " verworfen, da sie nicht mehr erwartet wird.\n"),
          loop(Limit,SortedHBQ,DLQ);

        NNr == DLQNNr ->
          NewDLQ = dlq:push2DLQ([NNr,Msg ++ util:to_String(vsutil:now2string(TShbqin)),TSclientout,TShbqin],DLQ,?LOGFILE),
          util:logging(?LOGFILE, "HBQ>>> Nachricht " ++ util:to_String(NNr) ++" direkt an die DLQ weitergeleitet\n"),
          SPID ! {reply, ok},
          if
            LenghtHBQ > 0 ->
              %util:logging(?LOGFILE, "HBQ>>> Content(" ++ util:to_String(listLength(SortedHBQ)) ++ "): " ++ util:to_String(getNNrs(SortedHBQ)) ++ "\n"),
              {HBQ_Tmp_1, DLQ_Tmp_1} = checkEqual(SortedHBQ,NewDLQ),
              loop(Limit,HBQ_Tmp_1,DLQ_Tmp_1);
            true ->
              loop(Limit,SortedHBQ,NewDLQ)
          end;
        true ->
          NewHBQ = append(HBQ,[[NNr,Msg ++ util:to_String(vsutil:now2string(TShbqin)),TSclientout,TShbqin]]),
          util:logging(?LOGFILE, "HBQ>>> Nachricht " ++ util:to_String(NNr) ++ " in der HBQ gespeichert aber noch nicht an DLQ ausgeliefert, da Nachrichten fehlen.\n"),
          %util:logging(?LOGFILE, "HBQ>>> aktuell " ++ util:to_String(NewHBQ) ++ "\n"),
          %SortedHBQ_Tmp_1 = bubblesort(NewHBQ),
          %util:logging(?LOGFILE, "HBQ>>> presort " ++ util:to_String(NewHBQ) ++ "\nHBQ>>> postsort" ++ util:to_String(SortedHBQ_Tmp_1) ++ "\n"),
          Length = listLength(NewHBQ),
          [MinNNr,_,_,_] = first(NewHBQ),
          util:logging(?LOGFILE, "HBQ>>> lenght " ++ util:to_String(Length) ++ "\nDLQ>>> limit " ++ util:to_String(Limit) ++ "\n"),
          if
            Length >= Limit ->
              NewDLQ = dlq:push2DLQ([MinNNr-1, "***Fehlernachricht fuer Nachrichten " ++ util:to_String(DLQNNr) ++ " bis " ++ util:to_String(MinNNr-1) ++ " um " ++ vsutil:now2stringD(erlang:timestamp()) ++ "|." ,{0,0,0}, {0,0,0}], DLQ, ?LOGFILE),
              {HBQ_Tmp_1, DLQ_Tmp_1} = checkEqual(NewHBQ,NewDLQ),
              SPID ! {reply, ok},
              loop(Limit,HBQ_Tmp_1,DLQ_Tmp_1);
            true ->
              SPID ! {reply, ok},
              loop(Limit,NewHBQ, DLQ)
          end
      end;
    {SPID, {request,deliverMSG,NNr,ToClient}} ->
       INNr = dlq:deliverMSG(NNr,ToClient,DLQ,?LOGFILE),
      SPID ! {reply, INNr},
      loop(Limit,HBQ,DLQ);
%%TODO Test die Funktion listDLQ
    {SPID, {request, listDLQ}} ->
      DLQlist = dlq:listDLQ(DLQ),
      util:logging(?LOGFILE, "dlq>>> Content(" ++ util:to_String(dlq:lengthDLQ(DLQ)) ++ "): " ++ util:to_String(DLQlist) ++ "\n"),
      SPID ! {reply, ok},
      loop(Limit,HBQ,DLQ);
    {SPID, {request, listHBQ}} ->
      util:logging(?LOGFILE, "HBQ>>> Content(" ++ util:to_String(listLength(HBQ)) ++ "): " ++ util:to_String(getNNrs(HBQ)) ++ "\n"),
      SPID ! {reply, ok},
      loop(Limit,HBQ,DLQ);
    {SPID, {request,dellHBQ}} ->
      dlq:delDLQ(DLQ),
      SPID ! {reply, ok},
      [FNNr,_FMsg,_FTSclientout,_FTShbqin] = first(HBQ),
      util:logging(?LOGFILE, "HBQ>>> Downtime: " ++ vsutil:now2string(erlang:timestamp()) ++ " von HBQ und DLQ " ++ util:to_String(self()) ++ "; Anzahl Restnachrichten HBQ:" ++ util:to_String(listLength(HBQ)) ++ "; DLQ letzte NNr:" ++ util:to_String(FNNr - 1) ++ "\n")
  end
.

checkEqual([],DLQ) ->
  {[],DLQ};
checkEqual([[NNr,Msg,TSclientout,TShbqin]],DLQ) ->
  DLQNNr = dlq:expectedNr(DLQ),
  if
    NNr == DLQNNr ->
      NewDLQ = dlq:push2DLQ([NNr,Msg,TSclientout,TShbqin],DLQ,?LOGFILE),
      {[[NNr,Msg,TSclientout,TShbqin]],NewDLQ};
    true ->
      {[[NNr,Msg,TSclientout,TShbqin]],DLQ}
  end;
checkEqual([[NNr,Msg,TSclientout,TShbqin]|T],DLQ) ->
  DLQNNr = dlq:expectedNr(DLQ),
  if
    NNr == DLQNNr ->
      NewDLQ = dlq:push2DLQ([NNr,Msg,TSclientout,TShbqin],DLQ,?LOGFILE),
      checkEqual([[NNr,Msg,TSclientout,TShbqin]],T,NewDLQ);
    true ->
      {append([[NNr,Msg,TSclientout,TShbqin]],T),DLQ}
  end.

checkEqual(Head,[],DLQ) ->
  {Head,DLQ};
checkEqual(Head, [[NNr,Msg,TSclientout,TShbqin]|T],DLQ) ->
  DLQNNr = dlq:expectedNr(DLQ),
  if
    NNr == DLQNNr ->
      NewDLQ = dlq:push2DLQ([NNr,Msg,TSclientout,TShbqin],DLQ,?LOGFILE),
      checkEqual(append(Head,[[NNr,Msg,TSclientout,TShbqin]]),T,NewDLQ);
    true ->
      {append(append(Head,[[NNr,Msg,TSclientout,TShbqin]]),T),DLQ}
  end.

getNNrs([]) ->
  [];
getNNrs(L) ->
  getNNrs(L,[]).

getNNrs([[]],A) ->
  A;
getNNrs([[NNr,_,_,_]],A) ->
  append(A,NNr);
getNNrs([[NNr,_,_,_]|T],A) ->
  getNNrs(T,append(A,NNr)).

reverse(L) ->
  reverse(L,[]).

reverse([], R) ->
  R;
reverse([H|T], R) ->
  reverse(T, append([H],R)).

%Auf die HBQ zugeschnittener Bubblesort!
bubblesort(L) when length(L) =< 1 ->
  L;
bubblesort(L) ->
  util:logging(?LOGFILE, "UNSORTED HBQ >>> " ++ util:to_String(L) ++ "\n"),
  bubblesort(L, [], true).

bubblesort([], L, true) ->
  util:logging(?LOGFILE, "ALMOST SORTED HBQ >>> " ++ util:to_String(L) ++ "\n"),
  reverse(L);
bubblesort([], L, false) ->
  bubblesort(reverse(L), [], true);
bubblesort([[NNrP,MsgP,TSclientoutP,TShbqinP],[NNr,Msg,TSclientout,TShbqin]|T], Sorted, _) when NNrP > NNr ->
  bubblesort([[NNrP,MsgP,TSclientoutP,TShbqinP]|T], [[NNr,Msg,TSclientout,TShbqin]|Sorted], false);
bubblesort([H|T], Sorted, Halt) ->
  bubblesort(T, [H|Sorted], Halt).

%bubblesort([[NNrP,MsgP,TSclientoutP,TShbqinP]], Sorted, _) ->
%  bubblesort([], [[NNrP,MsgP,TSclientoutP,TShbqinP]|Sorted], false);
%bubblesort([[NNrP,MsgP,TSclientoutP,TShbqinP],[NNr,Msg,TSclientout,TShbqin]], Sorted, _) when NNrP > NNr ->
%  bubblesort([], [[NNrP,MsgP,TSclientoutP,TShbqinP],[NNr,Msg,TSclientout,TShbqin]|Sorted], false);
%bubblesort([[NNrP,MsgP,TSclientoutP,TShbqinP],[NNr,Msg,TSclientout,TShbqin]], Sorted, _) when NNrP =< NNr ->
%  bubblesort([], [[NNr,Msg,TSclientout,TShbqin],[NNrP,MsgP,TSclientoutP,TShbqinP]|Sorted], false);
%bubblesort([[NNrP,MsgP,TSclientoutP,TShbqinP],[NNr,Msg,TSclientout,TShbqin]|T], Sorted, _) when NNrP > NNr ->
%  bubblesort([[NNrP,MsgP,TSclientoutP,TShbqinP]|T], [[NNr,Msg,TSclientout,TShbqin]|Sorted], false);
%bubblesort([H|T], Sorted, Halt) ->
%  bubblesort(T, [H|Sorted], Halt).

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

first([H|_])->
  H;
first(L) ->
  L.

listLength([]) ->
  0;
listLength(L) ->
  listLength(L,0).

listLength([],A) ->
  A;
listLength([_|T],A) ->
  listLength(T,A+1).