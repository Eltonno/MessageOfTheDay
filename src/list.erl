%%%-------------------------------------------------------------------
%%% @author Elton
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 05. Sep 2018 15:55
%%%-------------------------------------------------------------------
-module(list).
-author("Elton").

%% API
-export([keyfind/2,keystore/3,last/1,first/1,append/2,member/2,bubblesort/1,reverse/1,length/1,delete/2]).

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

first([Head|_]) ->
  Head
.

last([]) ->
  nil;
last([Head|[]]) ->
  Head;
last([_|Tail]) ->
  last(Tail)
.

member(_,[]) ->
  false;
member(Elem, [H|T]) ->
  if
    H == Elem -> true;
    true -> member(Elem, T)
  end
.

reverse(L) ->
  reverse(L,[]).

reverse([], R) ->
  R;
reverse([H|T], R) ->
  reverse(T, append(H,R)).

%Auf die HBQ zugeschnittener Bubblesort!
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

length([]) ->
  0;
length(L) ->
  length(L,0).

length([],A) ->
  A;
length([_|T],A) ->
  length(T,A+1).

delete(_,[]) ->
  [];
delete(Key,[Head|Rest]) ->
  {K,_}= Head,
  if K == Key -> Rest;
    true -> delete(Key,Rest,[Head])
  end
.

delete(_,[],Front) ->
  Front;
delete(Key,[Head|Rest],Front) ->
  {K,_} = Head,
  if K == Key -> append(Front,Rest);
    true -> delete(Key,Rest,append(Front,Head))
  end
.