% keyword_phrase(T0,T2,Ind,C0,C2) is true if
%  T0 and T2 are list of words, such that
%        T2 is an ending of T0
%        the words in T0 before T2 (written T0-T2) form a keyword phrase
%  Ind is the individual that the keyword phrase is referring to
%  C0 and C2 are lists of relations such that
%        C0 is an ending of C2 and
%        the relations in C2-C0 give the constraints on Ind implied by the keyword phrase
% A keyword phrase is a determiner followed by adjectives followed
% by a keyword followed by an optional modifying phrase:
keyword_phrase(T0,T4,Ind,C0,C4) :-
    det(T0,T1,Ind,C0,C1),
    keyword(T1,T2,Ind,C1,C2),
    connector(T2,T3,Ind,C2,C3),
    keyword_phrase(T3,T4,Ind,C3,C4).
keyword_phrase(T,T,_,C,C).

% Determiners (articles) are ignored.
% % They do not provide any extra constaints.
det([the | T],T,_,C,C).
det([a | T],T,_,C,C).
det(T,T,_,C,C).

% Connectors are prepositions or conjunctions, which we will ignore.
% % They do not provide any extra constaints.
connector([to | T],T,_,C,C).
connector([of | T],T,_,C,C).
connector([and | T],T,_,C,C).
connector(T,T,_,C,C).

% Attributes
% for any given question topic, associate it with the appropriate attributes
copy(howtocopy).

copy(howtocopyandpaste).
paste(howtocopyandpaste).

move(howtomove).

move(howtomoveup).
up(howtomoveup).

jump(howtojump).

jump(howtojumptothebottom).
bottom(howtojumptothebottom).

% solution(Topic,A) gives you the solution string A associated with a Topic
solution(howtomove, "this is how you move").
solution(howtomoveup, "this is how you move UP").
solution(howtocopy, "this is how you copy").
solution(howtocopyandpaste, "this is how you copyandpaste").
solution(howtojump, "this is how you jump").
solution(howtojumptothebottom, "this is how you jumptothebottom").

% Map keywords in the question to the correct attribute
% keyword(T0,T1,Ind,C0,C1) is true if T0-T1 is a keyword that provides attributes C1-C0 to Ind
keyword([move | T],T,Ind,C,[move(Ind)|C]).
keyword([up | T],T,Ind,C,[up(Ind)|C]).
keyword([copy | T],T,Ind,C,[copy(Ind)|C]).
keyword([paste | T],T,Ind,C,[paste(Ind)|C]).

keyword([select | T],T,Ind,C,[select(Ind)|C]).
keyword([jump | T],T,Ind,C,[jump(Ind)|C]).
keyword([bottom | T],T,Ind,C,[bottom(Ind)|C]).

% if these words appear, they won't affect anything:
keyword([screen | T],T,_,C,C).

% question([is | T0],T2,Ind,C0,C2) :-
%     keyword_phrase(T0,T1,Ind,C0,C1),
%     mp(T1,T2,Ind,C1,C2).
% question([what,is | T0],T1,Ind,C0,C1) :-
%     mp(T0,T1,Ind,C0,C1).
% question([what,is | T0],T1,Ind,C0,C1) :-
%     keyword_phrase(T0,T1,Ind,C0,C1).
question([how, do, you | T0],T1,Ind,C0,C1) :-
    keyword_phrase(T0,T1,Ind,C0,C1).
% question([what,is | T0],T1,Ind,C0,C1) :-
%     adjectives(T0,T1,Ind,C0,C1).
% question([what | T0],T2,Ind,C0,C2) :-      % allows for a "what ... is ..."
%     keyword_phrase(T0,[is|T1],Ind,C0,C1),
%     mp(T1,T2,Ind,C1,C2).
% question([what | T0],T2,Ind,C0,C2) :-
%     keyword_phrase(T0,T1,Ind,C0,C1),
%     mp(T1,T2,Ind,C1,C2).

% ask(Q,A) gives answer A to question Q
%
% NOTE: S is a sentence, Q is that sentence as a list
ask(Q,T) :-
    atomic_list_concat(L,' ', Q),
    solution(A, T),
    question(L,[],A,[],C),
    prove_all(C).

% prove_all(L) proves all elements of L against the database
prove_all([]).
prove_all([H|T]) :-
     H,
    prove_all(T).

% example queries:
% ask("how do you move",X).
% ask("how do you move",X).
