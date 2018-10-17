%%%-------------------------------------------------------------------
%%% @author Elton
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 23. Sep 2018 20:33
%%%-------------------------------------------------------------------
-module(client).
-author("Elton").

%% API
-export([init/0,init/1,pre_loop/3]).

-define(GRUPPE, 1).
-define(TEAM, 03).
-define(REDAKTEUR_ATOM, redakteur).
-define(LESER_ATOM, leser).
-define(RECHNER_NAME, erlang:node()).

init() ->
  init("client.cfg")
.
init(File) ->
  {ok, Hostname} = inet:gethostname(),
  {ok, Configfile} = file:consult(File),
  {ok, Clients} = vsutil:get_config_value(clients, Configfile),
  {ok, Lifetime} = vsutil:get_config_value(lifetime, Configfile),
  {ok, Servername} = vsutil:get_config_value(servername, Configfile),
  {ok, Servernode} = vsutil:get_config_value(servernode, Configfile),
  {ok, Sendinterval} = vsutil:get_config_value(sendeintervall, Configfile),
  Config = [
    {servername, Servername},
    {clients, Clients},
    {lifetime, Lifetime},
    {node, Servernode},
    {host, Hostname},
    {intervall, Sendinterval}],
  spawner(Clients, Config)
.

spawner(0, _) ->
  ok;
spawner(Clients, Config) ->
  {_, CurrentClients} = list:keyfind(clients, Config),
  Clientname = list_to_atom("Client_" ++ util:to_String(CurrentClients-Clients+1)),
  register(Clientname, spawn(?MODULE, pre_loop, [Clientname, Config, erlang:timestamp()])),
  spawner(Clients-1, Config)
.

pre_loop(Clientname, Config, Starttime) ->
  Config_Tmp_1 = list:keystore(clientname,Config,{clientname, atom_to_list(Clientname)}),
  Config_Tmp_2 = list:keystore(starttime,Config_Tmp_1,{starttime, Starttime}),
  Config_Tmp_3 = list:keystore(logfile,Config_Tmp_2,{logfile, atom_to_list(Clientname) ++ atom_to_list(?RECHNER_NAME) ++".log"}),
  {_,Logfile} = list:keyfind(logfile, Config_Tmp_3),
  util:logging(Logfile, atom_to_list(Clientname) ++ atom_to_list(?RECHNER_NAME) ++ "-" ++ util:to_String(self()) ++ "-LAW Start: " ++ vsutil:now2string(Starttime) ++"\n"),
  main_loop(Config_Tmp_3, 0, publisher, [])
.

main_loop(Config, SessionTransactions, Role, OwnMsgs) ->
  {_,Starttime} = list:keyfind(starttime, Config),
  {_,Lifetime} = list:keyfind(lifetime, Config),
  {_,Logfile} = list:keyfind(logfile, Config),
  {_,Clientname} = list:keyfind(clientname, Config),
  {_,Servername} = list:keyfind(servername, Config),
  {_,Servernode} = list:keyfind(node, Config),
  {_,Intervall} = list:keyfind(intervall, Config),
  Diff = help:toMillis(erlang:timestamp()) - help:toMillis(Starttime),
  if
    Diff < Lifetime * 1000 ->
      case Role of
        publisher ->
          MsgID = getMsgID(Servername, Servernode),
          case SessionTransactions of
            5 ->
              NewInt = help:changeSendInterval(Intervall),
              Config_Tmp_1 = list:keystore(intervall, Config, {intervall, NewInt}),
              util:logging(Logfile, "Neues Sendeintervall: " ++ util:to_String(NewInt) ++ "(" ++ util:to_String(Intervall) ++ ")\n"),
              util:logging(Logfile, util:to_String(MsgID) ++ "te_Nachricht um " ++ util:timeMilliSecond() ++ " vergessen zu senden ******\n"),
              main_loop(Config_Tmp_1, 0, reader, OwnMsgs);
            _Otherwise ->
              Msg = util:to_String(?RECHNER_NAME) ++ util:to_String(?GRUPPE) ++ util:to_String(?TEAM) ++ ": " ++ util:to_String(MsgID) ++ "te_Nachricht. C Out: " ++ util:timeMilliSecond(),
              {Servername, Servernode} ! {dropmessage, [MsgID, Msg, erlang:timestamp()]},
              util:logging(Logfile, Clientname ++ "-" ++ atom_to_list(?RECHNER_NAME) ++ "-" ++ util:to_String(self()) ++ "-LAW: " ++  util:to_String(MsgID) ++ "te_Nachricht um " ++ util:timeMilliSecond() ++ " gesendet\n"),
              timer:sleep(trunc(Intervall * 1000)),
              main_loop(Config, SessionTransactions + 1, Role, list:append(OwnMsgs, MsgID))
          end;
        reader ->
          {Servername, Servernode} ! {self(), getmessages},
          receive
            {reply, [MsgID, Msg, TSclientout, _TShbqin, _TSdlqin, TSdlqout], false} ->
              readerLogging([MsgID, Msg, TSclientout, TSdlqout], Logfile, OwnMsgs),
              main_loop(Config,0, Role, OwnMsgs);
            {reply, [MsgID, Msg, TSclientout, _TShbqin, _TSdlqin, TSdlqout], true} ->
              readerLogging([MsgID, Msg, TSclientout, TSdlqout], Logfile, OwnMsgs),
              main_loop(Config, 0, publisher, OwnMsgs)
          end
      end;
    true ->
      util:logging(Logfile, "Downtime: " ++ util:timeMilliSecond() ++ " vom Client " ++ Clientname ++ atom_to_list(?RECHNER_NAME) ++ "-" ++ util:to_String(self()) ++ "-LAW\n"),
      exit("Lifettime is over")
  end
.

getMsgID(Servername, Servernode) ->
  {Servername, Servernode} ! {self(), getmsgid},
  receive
    {nid, Number} ->
      Number
  end.

readerLogging([MsgID, Msg, TScout, TSdlqout], Logfile, OwnMsgs) ->
  TScin = erlang:timestamp(),
  Diff = vsutil:diffTS(TScin, TSdlqout),
  Less = vsutil:lessTS(TScin, TSdlqout),
  if
    TScout == {0,0,0} ->
      if
        Less ->
          util:logging(Logfile, Msg++ " ; C In: " ++ vsutil:now2string(TScin) ++" >**Nachricht aus der Zukunft fuer Leser:" ++ vsutil:now2stringD(Diff) ++ "<\n");
        true ->
          util:logging(Logfile, Msg++ " ; C In: " ++ vsutil:now2string(TScin) ++"\n")
      end;
    true ->
      Member = list:member(MsgID, OwnMsgs),
      if
        Less ->
          if
            Member ->
              util:logging(Logfile, Msg++ "*******; C In: " ++ vsutil:now2string(TScin) ++" >**Nachricht aus der Zukunft fuer Leser:" ++ vsutil:now2stringD(Diff) ++ "<\n");
            true ->
              util:logging(Logfile, Msg++ " ; C In: " ++ vsutil:now2string(TScin) ++" >**Nachricht aus der Zukunft fuer Leser:" ++ vsutil:now2stringD(Diff) ++ "<\n")
          end;
        true ->
          if
            Member ->
              util:logging(Logfile, Msg++ "*******; C In: " ++ vsutil:now2string(TScin) ++"\n");
            true ->
              util:logging(Logfile, Msg++ " ; C In: " ++ vsutil:now2string(TScin) ++"\n")
          end
      end

  end
.