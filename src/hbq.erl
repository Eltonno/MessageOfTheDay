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

%% Liest die Config-Datei aus
startHBQ(File) ->
  util:logging(?LOGFILE, "HBQ>>> gestartet\n"),
  {ok, ConfigListe} = file:consult(File),
  {ok, HBQname} = vsutil:get_config_value(hbqname, ConfigListe),
  {ok, DlqLimit} = vsutil:get_config_value(dlqlimit, ConfigListe),
  erlang:register(HBQname, self()),
  %% Dann wird auf den initHBQ-Request gewartet.
  receive
    %% Initialisiert die HBQ und die DLQ
    {SPID,{request,initHBQ}} ->
      util:logging(?LOGFILE, "HBQ>>> init empfangen\n"),
      %% Erstellt eine Queue HBQ und eine Queue DLQ durch initDLQ
      DLQ = dlq:initDLQ(DlqLimit,?LOGFILE),
      %% Gibt ok zurück
      SPID ! {reply,ok},
      %% Außerdem wird berechnet und gespeichert, 
      %% wie viel 2/3 von 
      %% dem DLQ-Limit sind.
      Lim = DlqLimit * 2 / 3,
      loop(Lim, [], DLQ)
  end
.
%%TODO Msg anpassen mit HBQin und now2String
loop(Limit,HBQ,DLQ) ->
  util:logging(?LOGFILE, "HBQ>>> wartet auf Nachricht\n"),
  receive
    %% Fügt der HBQ eine Nachricht ein
    {SPID, {request,pushHBQ,[NNr,Msg,TSclientout]}} ->
      util:logging(?LOGFILE, "HBQ>>> Speichern der Nachricht " ++ util:to_String(NNr) ++ " in der HBQ eingeleitet\n"),
      %% Aktuellen Timestamp speichern (Eingangszeitstempel für die HBQ)
      TShbqin = erlang:timestamp(),
      %% Bei der DLQ die als nächstes erwartete Nachrichtnr. anfragen.
      DLQNNr = dlq:expectedNr(DLQ),
      %% Die HBQ sortieren.
      SortedHBQ = bubblesort(HBQ),
      LenghtHBQ = length(HBQ),
      if
        %% Falls die empfangene NNr kleiner als die von der DLQ erwartete ist, 
        %% wird die Nachricht verworfen.
        NNr < DLQNNr ->
          util:logging(?LOGFILE, "HBQ>>> Nachricht " ++ util:to_String(NNr) ++ " verworfen, da sie nicht mehr erwartet wird.\n"),
          loop(Limit,SortedHBQ,DLQ);
        %% Sollte die NNr der erwarteten NNr entsprechen, 
        %%so wird die Nachricht direkt an die DLQ weitergeleitet (dlq:push2DLQ).
        NNr == DLQNNr ->
          NewDLQ = dlq:push2DLQ([NNr,Msg ++ util:to_String(vsutil:now2string(TShbqin)),TSclientout,TShbqin],DLQ,?LOGFILE),
          util:logging(?LOGFILE, "HBQ>>> Nachricht " ++ util:to_String(NNr) ++" direkt an die DLQ weitergeleitet\n"),
          %% Daraufhin wird ein ok zurückgegeben, 
          %% und überprüft, 
          %% ob die auf die NNr folgenden Nachrichten schon in der HBQ sind (checkEqual).
          SPID ! {reply, ok},
          if
            LenghtHBQ > 0 ->
              {HBQ_Tmp_1, DLQ_Tmp_1} = checkEqual(SortedHBQ,NewDLQ),
              loop(Limit,HBQ_Tmp_1,DLQ_Tmp_1);
            true ->
              loop(Limit,SortedHBQ,NewDLQ)
          end;
        true ->
          NewHBQ = append(HBQ,[[NNr,Msg ++ util:to_String(vsutil:now2string(TShbqin)),TSclientout,TShbqin]]),
          util:logging(?LOGFILE, "HBQ>>> Nachricht " ++ util:to_String(NNr) ++ " in der HBQ gespeichert aber noch nicht an DLQ ausgeliefert, da Nachrichten fehlen.\n"),
          Length = listLength(NewHBQ),
          [MinNNr,_,_,_] = first(NewHBQ),
          if
            %% Wenn in der HBQ mehr als 2/3-tel an 
            %% Nachrichten enthalten sind, 
            %% als durch die vorgegebene maximale Anzahl an Nachrichten in der DLQ stehen können, 
            %% dann wird, 
            %% sofern eine Lücke besteht, 
            %% diese Lücke zwischen DLQ und HBQ mit genau einer Fehlernachricht geschlossen, 
            %% etwa: "***Fehlernachricht fuer Nachrichtennummern 11 bis 17 um 16.05 18:01:30,580”, indem diese Fehlernachricht in die DLQ eingetragen wird (push2DLQ) und als Nachrichten-ID die größte fehlende ID der Lücke erhält.
            Length >= Limit ->
              %% Ist die empfangene NNr größer als die erwartete, so wird die Nachricht an die HBQ angefügt.
              NewDLQ = dlq:push2DLQ([MinNNr-1, "***Fehlernachricht fuer Nachrichten " ++ util:to_String(DLQNNr) ++ " bis " ++ util:to_String(MinNNr-1) ++ " um " ++ vsutil:now2stringD(erlang:timestamp()) ++ "|." ,{0,0,0}, {0,0,0}], DLQ, ?LOGFILE),
              {HBQ_Tmp_1, DLQ_Tmp_1} = checkEqual(NewHBQ,NewDLQ),
              SPID ! {reply, ok},
              loop(Limit,HBQ_Tmp_1,DLQ_Tmp_1);
            true ->
              SPID ! {reply, ok},
              loop(Limit,NewHBQ, DLQ)
          end
      end;
    
    %% Beauftragt die DLQ (durch dlq:deliverMSG) damit, 
    %% dem Client seine ungelesenen Nachrichten zu senden.
    {SPID, {request,deliverMSG,NNr,ToClient}} ->
       INNr = dlq:deliverMSG(NNr,ToClient,DLQ,?LOGFILE),
      %% Bekommt von der DLQ die tatsächlich gesendete NNr zurück, 
      %% und sendet diese als Rückgabe an den Server.
      SPID ! {reply, INNr},
      loop(Limit,HBQ,DLQ);
    
    %% Sendet der DLQ den listDLQ Befehl, 
    %% damit dessen Nachrichtennummmern geloggt werden.
    {SPID, {request, listDLQ}} ->
      DLQlist = dlq:listDLQ(DLQ),
      util:logging(?LOGFILE, "dlq>>> Content(" ++ util:to_String(dlq:lengthDLQ(DLQ)) ++ "): " ++ util:to_String(DLQlist) ++ "\n"),
      %% Bei Erfolg bekommt der Server ein ok.
      SPID ! {reply, ok},
      loop(Limit,HBQ,DLQ);
    
    %% Loggt die Länge der HBQ, 
    %% und die vorhandenen Nachrichtnummern.
    {SPID, {request, listHBQ}} ->
      util:logging(?LOGFILE, "HBQ>>> Content(" ++ util:to_String(listLength(HBQ)) ++ "): " ++ util:to_String(getNNrs(HBQ)) ++ "\n"),
      %% Der Server bekommt ein ok als Rückgabe.
      SPID ! {reply, ok},
      loop(Limit,HBQ,DLQ);
    
    %% Terminiert die HBQ.
    {SPID, {request,dellHBQ}} ->
      %% Löscht die DLQ mittels dlq:delDLQ.
      dlq:delDLQ(DLQ),
      %% Gibt ok als Rückgabewert zurück.
      SPID ! {reply, ok},
      util:logging(?LOGFILE, "HBQ>>> Downtime: " ++ vsutil:now2string(erlang:timestamp()) ++ " von HBQ und DLQ " ++ util:to_String(self()) ++ "; Anzahl Restnachrichten HBQ:" ++ util:to_String(listLength(HBQ)) ++ "; DLQ letzte NNr:" ++ util:to_String(dlq:expectedNr(DLQ) - 1) ++ "\n")
  end
.

checkEqual([],DLQ) ->
  {[],DLQ};
checkEqual([[NNr,Msg,TSclientout,TShbqin]],DLQ) ->
  DLQNNr = dlq:expectedNr(DLQ),
  if
    NNr == DLQNNr ->
      %% In dem Fall werden die auch an die DLQ weitergeleitet und aus der HBQ entfernt.
      NewDLQ = dlq:push2DLQ([NNr,Msg,TSclientout,TShbqin],DLQ,?LOGFILE),
      {[],NewDLQ};
    true ->
      {[[NNr,Msg,TSclientout,TShbqin]],DLQ}
  end;
checkEqual([[NNr,Msg,TSclientout,TShbqin]|T],DLQ) ->
  DLQNNr = dlq:expectedNr(DLQ),
  if
    NNr == DLQNNr ->
      NewDLQ = dlq:push2DLQ([NNr,Msg,TSclientout,TShbqin],DLQ,?LOGFILE),
      checkEqual(T,NewDLQ);
    true ->
      {append([[NNr,Msg,TSclientout,TShbqin]],T),DLQ}
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
  bubblesort(L, [], true).

bubblesort([], L, true) ->
  reverse(L);
bubblesort([], L, false) ->
  bubblesort(reverse(L), [], true);
bubblesort([[NNrP,MsgP,TSclientoutP,TShbqinP],[NNr,Msg,TSclientout,TShbqin]|T], Sorted, _) when NNrP > NNr ->
  bubblesort([[NNrP,MsgP,TSclientoutP,TShbqinP]|T], [[NNr,Msg,TSclientout,TShbqin]|Sorted], false);
bubblesort([H|T], Sorted, Halt) ->
  bubblesort(T, [H|Sorted], Halt).

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
