%%%-------------------------------------------------------------------
%%% @author Elton
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 25. Sep 2018 12:28
%%%-------------------------------------------------------------------
-module(help).
-author("Elton").

%% API
-export([timeout/2,toMillis/1,changeSendInterval/1]).

timeout(Time, Duration) ->
  Duration*1000 > (toMillis(erlang:timestamp()) - toMillis(Time))
.

toMillis({MeSec, Sec, MiSec}) ->
  (MeSec * 1000000 + Sec) * 1000 + round(MiSec / 1000)
.

changeSendInterval(Sendinterval) ->
  Prob = rand:uniform(),
  if
    (Sendinterval * (Prob + 0.5)) > 2 ->
      (Sendinterval * (Prob + 0.5));
    true ->
      2
  end.