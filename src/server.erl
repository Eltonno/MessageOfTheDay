%%%-------------------------------------------------------------------
%%% @author Florian Weiß, Ditmar Lange
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 23. Sep 2018 20:33
%%%-------------------------------------------------------------------
-module(server).
-author("Elton").

%% API
-export([init/0,loop/2]).

-define(RECHNER_NAME, erlang:node()).

init() ->
  init("server.cfg")
.

init(File) ->
  {ok, Configfile} = file:consult(File),
  {ok, Latency} = vsutil:get_config_value(latency, Configfile),
  {ok, Clientlifetime} = vsutil:get_config_value(clientlifetime, Configfile),
  {ok, Servername} = vsutil:get_config_value(servername, Configfile),
  {ok, HBQname} = vsutil:get_config_value(hbqname, Configfile),
  {ok, HBQnode} = vsutil:get_config_value(hbqnode, Configfile),
  Logfile = atom_to_list(?RECHNER_NAME) ++".log",
  CMEM = cmem:initCMEM(Clientlifetime, Logfile),
  util:logging(Logfile, "Server: " ++ File ++ " geöffnet...\n"),
  net_adm:ping(HBQnode),
  timer:sleep(700),
  HBQPID = spawn(HBQnode, hbq, startHBQ, []),
  timer:sleep(700),
  {HBQname,HBQnode} ! {self(),{request, initHBQ}},    %HBQPID ! {self(),{request, initHBQ}}
  receive
    {reply, ok} ->
      util:logging(Logfile, "Server: HBQ konnte initialisiert werden\n")
  end,
  Config = [
    {latency, Latency},
    {clientLifetime, Clientlifetime},
    {servername, Servername},
    {hbqname, HBQname},
    {hbqnode, HBQnode},
    {logfile,Logfile},
    {cmem, CMEM},
    {hbqpid, HBQPID}],
  PID = spawn(?MODULE,loop,[Config,1]),
  util:logging(Logfile, "Server: Startzeit: " ++ vsutil:now2string(erlang:timestamp()) ++ " mit PID " ++ util:to_String(PID) ++ "\n"),
  register(Servername, PID),
  util:logging(Logfile, "Server: wurde registriert\n")
.

loop(Config, MsgID) ->
  {_,Logfile} = keyfind(logfile, Config),
  {_,Latency} = keyfind(latency, Config),
  {_,CMEM} = keyfind(cmem, Config),
  {_,HBQname} = keyfind(hbqname, Config),
  {_,HBQnode} = keyfind(hbqnode, Config),
  receive
    {CPID, getmessages} ->
      CNNR = cmem:getClientNNr(CMEM, CPID),
      {HBQname,HBQnode} ! {self(), {request, deliverMSG, CNNR, CPID}},
      receive
        {reply, SendNNr} ->
          util:logging(Logfile, "Server: Nachricht " ++ util:to_String(CNNR) ++ "wurde zugestellt.\n"),
          Config_Tmp_1 = keystore(cmem, Config, {cmem, cmem:updateClient(CMEM, CPID, SendNNr, Logfile)}),
          loop(Config_Tmp_1, MsgID)
      after
        500 ->
          util:logging(Logfile, "Server: Nachricht " ++ util:to_String(CNNR) ++ "konnte nicht zugestellt werden.\n"),
          loop(Config, MsgID)
      end;
    {dropmessage,[INNR,Msg,TSclientout]} ->
      {HBQname,HBQnode} ! {self(), {request,pushHBQ,[INNR,Msg,TSclientout]}},     %HBQPID ! {self(), {request, pushHBQ, [INNR, Msg, TSclientout]}}
      receive
        {reply, ok} ->
          util:logging(Logfile, "Server: Nachricht " ++ util:to_String(INNR) ++ " wurde in HBQ eingefügt.\n"),
          loop(Config, MsgID)
      after
        500 ->
          util:logging(Logfile, "Server: Nachricht " ++ util:to_String(INNR) ++ "konnte nicht in HBQ eingefügt werden.\n"),
          loop(Config, MsgID)
      end;
    {CPID,getmsgid} ->
      util:logging(Logfile, "Server: Nachrichtennummer " ++ util:to_String(MsgID) ++ " an " ++ util:to_String(CPID) ++ " gesendet\n"),
      CPID ! {nid, MsgID},
      loop(Config, MsgID + 1);
    {_,listDLQ} ->
      {HBQname,HBQnode} ! {self(),{request, listDLQ}},
      receive
        {reply, ok} ->
          util:logging(Logfile, "Server: DLQ gelistet\n")
      after
        500 ->
          util:logging(Logfile, "Server: DLQ konnte nicht gelistet werden\n")
      end,
      loop(Config, MsgID);
    {_,listHBQ} ->
      {HBQname,HBQnode} ! {self(),{request, listHBQ}},
      receive
        {reply, ok} ->
          util:logging(Logfile, "Server: HBQ gelistet\n")
      after
        500 ->
          util:logging(Logfile, "Server: HBQ konnte nicht gelistet werden\n")
      end,
      loop(Config, MsgID);
    {_,listCMEM} ->
      cmem:listCMEM(CMEM),
      loop(Config, MsgID)
  after
    Latency * 1000 ->
      {HBQname,HBQnode} ! {self(),{request, dellHBQ}},
      util:logging(Logfile, "Server: Downtime " ++ vsutil:now2string(erlang:timestamp()) ++ " vom Nachrichtenserver " ++ util:to_String(self()) ++ "\n")
      %exit("Timeout")
  end
.

%% Hilfsfunktionen für Listen
keyfind(_,[]) ->
  false;
keyfind(Key, Tuplelist) ->
  [Head|Rest] = Tuplelist,
  {K,_} = Head,
  if K == Key -> Head;
    true -> keyfind(Key,Rest)
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