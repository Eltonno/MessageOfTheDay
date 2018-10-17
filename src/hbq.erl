%%%-------------------------------------------------------------------
%%% @author Elton
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
  {ok, ConfigListe} = file:consult(File),
  {ok, HBQname} = vsutil:get_config_value(hbqname, ConfigListe),
  {ok, DlqLimit} = vsutil:get_config_value(dlqlimit, ConfigListe),
  receive
    {SPID,{request,initHBQ}} ->
      erlang:register(HBQname, self()),
      dlq:initDLQ(DlqLimit,?LOGFILE),
      SPID ! {reply,ok},
      Lim = DlqLimit * 2 / 3,
      loop(Lim, [], [])
  end
.

loop(Limit,HBQ,DLQ) ->
  receive
    {SPID, {request,pushHBQ,[NNr,Msg,TSclientout]}} ->
      TShbqin = erlang:timestamp(),
      DLQNNr = dlq:expectedNr(DLQ),
      SortedHBQ = list:bubblesort(HBQ),
      if
        NNr < DLQNNr ->
          loop(Limit,SortedHBQ,DLQ);
        NNr == DLQNNr ->
          NewDLQ = dlq:push2DLQ([NNr,Msg,TSclientout,TShbqin],DLQ,?LOGFILE),
          {HBQ_Tmp_1, DLQ_Tmp_1} = checkEqual(SortedHBQ,NewDLQ),
          loop(Limit,HBQ_Tmp_1,DLQ_Tmp_1);
        true ->
          NewHBQ = append(HBQ,[[NNr,Msg,TSclientout,TShbqin]]),
          Length = list:length(NewHBQ),
          [MinNNr,_,_,_] = list:first(NewHBQ),
          if
            Length >= Limit ->
              NewDLQ = dlq:push2DLQ([MinNNr-1, "***Fehlernachricht fuer Nachrichten " ++ util:to_String(DLQNNr) ++ " bis " ++ util:to_String(MinNNr-1) ++ " um " ++ vsutil:now2stringD(erlang:timestamp()) ++ "|.", {0,0,0}, {0,0,0}], DLQ, ?LOGFILE),
              {HBQ_Tmp_1, DLQ_Tmp_1} = checkEqual(NewHBQ,NewDLQ)
          end,
          loop(Limit,append(HBQ_Tmp_1,[[NNr,Msg,TSclientout,TShbqin]]),DLQ_Tmp_1)
      end;
    {SPID, {request,deliverMSG,NNr,ToClient}} ->
      ok;
    {SPID, listDLQ} ->
      dlq:listDLQ(DLQ);
    {SPID, listHBQ} ->
      util:logging(?LOGFILE, "HBQ>>> Content(" ++ list:length(HBQ) ++ "): " ++ util:to_String(getNNrs(HBQ)) ++ "\n");
    {SPID, {request,dellHBQ}} ->
      dlq:delDLQ(DLQ),
      SPID ! {reply, ok},
      ok
  end
.

checkEqual([[NNr,Msg,TSclientout,TShbqin]|T],DLQ) ->
  DLQNNr = dlq:expectedNr(DLQ),
  if
    NNr == DLQNNr ->
      NewDLQ = dlq:push2DLQ([NNr,Msg,TSclientout,TShbqin],DLQ,?LOGFILE),
      checkEqual(T,NewDLQ);
    true ->
      {[[NNr,Msg,TSclientout,TShbqin]|T],DLQ}
  end.

getNNrs([]) ->
  [];
getNNrs(L) ->
  getNNrs(L,[]).

getNNrs([],A) ->
  A;
getNNrs([[NNr,_,_,_],T],A) ->
  getNNrs(T,append(A,NNr)).