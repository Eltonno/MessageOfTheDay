%%%-------------------------------------------------------------------
%%% @author Florian Weiß, Ditmar Lange
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
  init("client.cfg").

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
  spawner(Clients, Config).

spawner(0, _) ->
  ok;
spawner(Clients, Config) ->
  {_, CurrentClients} = keyfind(clients, Config),
  Clientname = list_to_atom("Client_" ++ util:to_String(CurrentClients-Clients+1)),
  register(Clientname, spawn(?MODULE, pre_loop, [Clientname, Config, erlang:timestamp()])),
  spawner(Clients-1, Config).

pre_loop(Clientname, Config, Starttime) ->
  Config_Tmp_1 = keystore(clientname,Config,{clientname, atom_to_list(Clientname)}),
  Config_Tmp_2 = keystore(starttime,Config_Tmp_1,{starttime, Starttime}),
  Config_Tmp_3 = keystore(logfile,Config_Tmp_2,{logfile, atom_to_list(Clientname) ++ atom_to_list(?RECHNER_NAME) ++".log"}),
  {_,Logfile} = keyfind(logfile, Config_Tmp_3),
  util:logging(Logfile, atom_to_list(Clientname) ++ atom_to_list(?RECHNER_NAME) ++ "-" ++ util:to_String(self()) ++ "-LAW Start: " ++ vsutil:now2string(Starttime) ++"\n"),
  main_loop(Config_Tmp_3, 0, publisher, []).

main_loop(Config, SessionTransactions, Role, OwnMsgs) ->
  {_,Starttime} = keyfind(starttime, Config),
  {_,Lifetime} = keyfind(lifetime, Config),
  {_,Logfile} = keyfind(logfile, Config),
  {_,Clientname} = keyfind(clientname, Config),
  {_,Servername} = keyfind(servername, Config),
  {_,Servernode} = keyfind(node, Config),
  {_,Intervall} = keyfind(intervall, Config),
  Diff = toMillis(erlang:timestamp()) - toMillis(Starttime),
  if
    Diff < Lifetime * 1000 ->
      case Role of
        publisher ->
          MsgID = getMsgID(Servername, Servernode),
          case SessionTransactions of
            5 ->
              NewInt = changeSendInterval(Intervall),
              Config_Tmp_1 = keystore(intervall, Config, {intervall, NewInt}),
              util:logging(Logfile, "Neues Sendeintervall: " ++ util:to_String(NewInt) ++ "(" ++ util:to_String(Intervall) ++ ")\n"),
              util:logging(Logfile, util:to_String(MsgID) ++ "te_Nachricht um " ++ util:timeMilliSecond() ++ " vergessen zu senden ******\n"),
              main_loop(Config_Tmp_1, 0, reader, OwnMsgs);
            _Otherwise ->
              Msg = util:to_String(?RECHNER_NAME) ++ util:to_String(?GRUPPE) ++ util:to_String(?TEAM) ++ ": " ++ util:to_String(MsgID) ++ "te_Nachricht. C Out: " ++ util:timeMilliSecond(),
              {Servername, Servernode} ! {dropmessage, [MsgID, Msg, erlang:timestamp()]},
              util:logging(Logfile, Clientname ++ "-" ++ atom_to_list(?RECHNER_NAME) ++ "-" ++ util:to_String(self()) ++ "-LAW: " ++  util:to_String(MsgID) ++ "te_Nachricht um " ++ util:timeMilliSecond() ++ " gesendet\n"),
              timer:sleep(trunc(Intervall * 1000)),
              main_loop(Config, SessionTransactions + 1, Role, append(OwnMsgs, MsgID))
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
  end.

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
      Member = member(MsgID, OwnMsgs),
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
  end.

toMillis({MeSec, Sec, MiSec}) ->
  (MeSec * 1000000 + Sec) * 1000 + round(MiSec / 1000).

changeSendInterval(Sendinterval) ->
  Prob = rand:uniform(),
  if
    (Sendinterval * (Prob + 0.5)) > 2 ->
      (Sendinterval * (Prob + 0.5));
    true ->
      2
  end.

keyfind(_,[]) ->
  false;
keyfind(Key, Tuplelist) ->
  [Head|Rest] = Tuplelist,
  {K,_} = Head,
  if K == Key -> Head;
    true -> keyfind(Key,Rest)
  end.

keystore(_,[],Tupel) ->
  [Tupel];
keystore(Key,[Head|Rest],Tupel) ->
  {K,_}= Head,
  if K == Key -> append(Tupel, Rest);
    true -> keystore(Key,Rest,[Head],Tupel)
  end.

keystore(_,[],Front,Tupel) ->
  append(Front, Tupel);
keystore(Key,[Head|Rest],Front,Tupel) ->
  {K,_} = Head,
  if K == Key -> append(append(Front,Tupel),Rest);
    true -> keystore(Key,Rest,append(Front,Head),Tupel)
  end.

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

member(_,[]) ->
  false;
member(Elem, [H|T]) ->
  if
    H == Elem -> true;
    true -> member(Elem, T)
  end.