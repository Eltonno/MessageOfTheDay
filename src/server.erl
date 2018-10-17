%%%-------------------------------------------------------------------
%%% @author Elton
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
  Logfile =  atom_to_list(Servername) ++ atom_to_list(?RECHNER_NAME) ++".log",
  CMEM = cmem:initCMEM(Clientlifetime, Logfile),
  util:logging(Logfile, "Server: " ++ File ++ " geöffnet...\n"),
  net_adm:ping(HBQnode),
  timer:sleep(500),
  HBQPID = spawn(HBQnode, hbq:startHBQ, [File]),
  timer:sleep(500),
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
  {_,Logfile} = list:keyfind(logfile, Config),
  {_,Latency} = list:keyfind(latency, Config),
  {_,CMEM} = list:keyfind(cmem, Config),
  {_,HBQPID} = list:keyfind(hbqpid, Config),
  receive
    {CPID, getmessages} ->
      CNNR = cmem:getClientNNr(CMEM, CPID),
      HBQPID ! {self(), {request, deliverMSG, CNNR, CPID}},
      receive
        {reply, SendNNr} ->
          Config_Tmp_1 = list:keystore(cmem, Config, {cmem, cmem:updateClient(CMEM, CPID, SendNNr, Logfile)}),
          loop(Config_Tmp_1, MsqID)
      end;
    {dropmessage,[INNR,Msg,TSclientout]} ->
      util:logging(Logfile, util:to_String(INNR) ++ "\n"),
      HBQPID ! {self(), {request, pushHBQ, [INNR, Msg, TSclientout]}},
      receive
        {reply, ok} ->
          ok
      end,
      loop(Config, INNR);
    {CPID,getmsgid} ->
      CPID ! {nid, MsqID},
      loop(Config, MsqID + 1);
    {CPID,listDLQ} ->
      HBQPID ! listdlq;
    {CPID,listHBQ} ->
      HBQPID ! listhbq;
    {CPID,listCMEM} ->
      cmem:listCMEM(CMEM)
  after
    Latency * 1000 ->
      ok
  end
.