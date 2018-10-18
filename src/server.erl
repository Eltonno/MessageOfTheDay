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
  util:logging(Logfile, util:to_String(HBQPID) ++ "\n"),
  {HBQname,HBQnode} ! {self(),{request, initHBQ}},    %HBQPID ! {self(),{request, initHBQ}}
  receive
    {reply, ok} ->
      util:logging(Logfile, "Server: HBQ wurde initialisiert\n")
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
  register(Servername, PID),
  util:logging(Logfile, "Server: wurde registriert\n")
.

loop(Config, MsqID) ->
  {_,Logfile} = keyfind(logfile, Config),
  {_,Latency} = keyfind(latency, Config),
  {_,CMEM} = keyfind(cmem, Config),
  {_,HBQPID} = keyfind(hbqpid, Config),
  {_,HBQname} = keyfind(hbqpid, Config),
  {_,HBQnode} = keyfind(hbqpid, Config),
  util:logging(Logfile, "Server: in loop\n"),
  receive
    {CPID, getmessages} ->
      CNNR = cmem:getClientNNr(CMEM, CPID),
      {HBQname,HBQnode} ! {self(), {request, deliverMSG, CNNR, CPID}},
      receive
        {reply, SendNNr} ->
          Config_Tmp_1 = keystore(cmem, Config, {cmem, cmem:updateClient(CMEM, CPID, SendNNr, Logfile)}),
          loop(Config_Tmp_1, MsqID)
      end;
    {dropmessage,[INNR,Msg,TSclientout]} ->
      util:logging(Logfile, util:to_String(INNR) ++ " wird an die HBQ gesendet\n"),
      {HBQname,HBQnode} ! {self(), {request,pushHBQ,[INNR, Msg, TSclientout]}},      %HBQPID ! {self(), {request, pushHBQ, [INNR, Msg, TSclientout]}}
      receive
        {reply, ok} ->
          util:logging(Logfile, "Server: Konnte in HBQ eingetragen werden\n"),
          loop(Config, MsqID)
      after
        Latency * 1000 ->
          {HBQname,HBQnode} ! {self(),{request, dellHBQ}},
          util:logging(Logfile, "Server: Downtime\n")
      end;
    {CPID,getmsgid} ->
      util:logging(Logfile, "Server: Nachrichtennummer " ++ util:to_String(MsqID) ++ " wird ausgegeben\n"),
      CPID ! {nid, MsqID},
      loop(Config, MsqID + 1);
    {_,listDLQ} ->
      {HBQname,HBQnode} ! listdlq,
      loop(Config, MsqID);
    {_,listHBQ} ->
      {HBQname,HBQnode} ! listhbq,
      loop(Config, MsqID);
    {_,listCMEM} ->
      cmem:listCMEM(CMEM),
      loop(Config, MsqID)
  after
    Latency * 1000 ->
      {HBQname,HBQnode} ! {self(),{request, dellHBQ}},
      util:logging(Logfile, "Server: Downtime\n")
  end
.
%%
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