main :- 
    current_prolog_flag(argv, Argv),
    atomic_list_concat(Argv,' ',Q),
    ask(Q, A),
    print(A),
    halt(0).

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
connector([at | T],T,_,C,C).
connector([of | T],T,_,C,C).
connector([in | T],T,_,C,C).
connector([and | T],T,_,C,C).
connector(T,T,_,C,C).

% Attributes
% for any given question topic, associate it with the appropriate attributes
copy(howtocopyandpasteselected).
copy(howtocopyandpasteline).
paste(howtocopyandpasteselected).
paste(howtocopyandpasteline).
selected(howtocopyandpasteselected).
selected(howtodeleteselected).
delete(howtodeleteselected).
delete(howtodeleteline).
move(howtomove).

file(howtojumpfile).

jump(howtojumpline).
jump(howtojumpfile).
jump(howtojumpparagraph).
jump(howtojumpword).

paragraph(howtojumpparagraph).

insert(howtoinsert).
insert(howtoinserttoendofword).
insert(howtoinserttoendofline).
insert(howtoinserttomultiplelines).
insert(howtoswitchmodetoinsert).

multiple(howtoinserttomultiplelines).
lines(howtoinserttomultiplelines).
lines(howtodeleteline).
lines(howtocopyandpasteline).

end(howtoinserttoendofword).
end(howtoinserttoendofline).
end(howtojumpline).
end(howtojumpfile).
end(howtojumpparagraph).
end(howtojumpword).

word(howtoinserttoendofword).
word(howtojumpword).
line(howtoinserttoendofline).
line(howtodeleteline).
line(howtocopyandpasteline).
line(howtojumpline).
search(howtosearch).
search(howtosearchandreplace).
replace(howtosearchandreplace).

switch(howtoswitchmodetoinsert).
switch(howtoswitchmodetonormal).
switch(howtoswitchmodetovisual).

mode(howtoswitchmodetoinsert).
mode(howtoswitchmodetonormal).
mode(howtoswitchmodetovisual).

normal(howtoswitchmodetonormal).
visual(howtoswitchmodetovisual).

quit(howtoquit).
quit(howtosaveandquit).
save(howtosaveandquit).
save(howtosave).

split(howtosplitwindowhorizontally).
split(howtosplitwindowvertically).
split(howtosplitwindow).

screen(howtosplitwindowhorizontally).
screen(howtosplitwindowvertically).
screen(howtosplitwindow).

vertically(howtosplitwindowvertically).
horizontally(howtosplitwindowhorizontally).


% solution(Topic,A) gives you the solution string A associated with a Topic
solution(howtomove, "Move left: 'h', move right: 'l', move up: 'k', move down: 'j'. You can also move using the arrow keys.").
solution(howtocopyandpasteselected, "To copy selected text, enter visual mode, select the text by moving the cursor until you cover it, and then type 'y'. Exit visual mode. Paste it by typing 'p'.").
solution(howtocopyandpasteline, "To copy the current line, make sure you are in normal mode, and then type 'yy'. You can optionally type a number first, like '3 yy', and this will copy that number of consecutive lines (in this case, 3). To paste, type 'p'.").
solution(howtodeleteselected, "To delete selected text, enter visual mode, select the text by moving the cursor until you cover it, and then type 'd'. Exit visual mode. You can paste deleted text by typing 'p'.").
solution(howtodeleteline, "To delete the current line, make sure you are in normal mode, and then type 'dd'. You can optionally type a number first, like '3 dd', and this will delete that number of consecutive lines (in this case, 3). To paste deleted text, type 'p'.").

solution(howtojumpline, "To jump to the beginning of the line, type '0'. To jump to the end of the line, type '$'").
solution(howtojumpfile, "To jump to the beginning of the file, type 'gg'. To jump to the end of the file, type 'G'").
solution(howtojumpparagraph, "To jump to the beginning of the paragraph, type '{'. To jump to the end of the paragraph, type '}'").
solution(howtojumpword, "To jump to the beginning of the word, type 'w'. To jump to the end of the word, type 'b'").

solution(howtoinsert, "this is how you insert").
solution(howtoinserttoendofword, "this is how you insert to end of word").
solution(howtoinserttoendofline, "this is how you insert to end of line").
solution(howtoinserttomultiplelines, "howtoinserttomultiplelines").
solution(howtosearchandreplace, "howtosearchandreplace").
solution(howtosearch, "howtosearch").
solution(howtoswitchmodes, "howtoswitchmode").
solution(howtoswitchmodetoinsert, "howtoswitchmodetoinsert").
solution(howtoswitchmodetovisual, "howtoswitchmodetovisual").
solution(howtoswitchmodetonormal, "howtoswitchmodetonormal").
solution(howtosave,"howtosave").
solution(howtosaveandquit, "howtosaveandquit").
solution(howtoquit, "howtoquit").
solution(howtosplitwindow, "howtosplitwindow").
solution(howtosplitwindowvertically, "howtosplitwindowvertically").
solution(howtosplitwindowhorizontally, "howtosplitwindowhorizontally").

% Map keywords in the question to the correct attribute
% keyword(T0,T1,Ind,C0,C1) is true if T0-T1 is a keyword that provides attributes C1-C0 to Ind
keyword([move | T],T,Ind,C,[move(Ind)|C]).
keyword([up | T],T,Ind,C,[up(Ind)|C]).
keyword([copy | T],T,Ind,C,[copy(Ind)|C]).
keyword([paste | T],T,Ind,C,[paste(Ind)|C]).
keyword([delete | T],T,Ind,C,[delete(Ind)|C]).

keyword([selected | T],T,Ind,C,[selected(Ind)|C]).
keyword([jump | T],T,Ind,C,[jump(Ind)|C]).
keyword([go | T],T,Ind,C,[jump(Ind)|C]).
keyword([bottom | T],T,Ind,C,[bottom(Ind)|C]).

keyword([insert | T],T,Ind,C,[insert(Ind)|C]).
keyword([end | T],T,Ind,C,[end(Ind)|C]).
keyword([word | T],T,Ind,C,[word(Ind)|C]).
keyword([line | T],T,Ind,C,[line(Ind)|C]).

keyword([search | T],T,Ind,C,[search(Ind)|C]).
keyword([replace | T],T,Ind,C,[replace(Ind)|C]).
keyword([multiple | T],T,Ind,C,[multiple(Ind)|C]).
keyword([lines | T],T,Ind,C,[lines(Ind)|C]).

% switch and change mean the same thing
keyword([change | T],T,Ind,C,[switch(Ind)|C]).
keyword([switch | T],T,Ind,C,[switch(Ind)|C]).
% mode and modes mean the same thing
keyword([mode | T],T,Ind,C,[mode(Ind)|C]).
keyword([modes | T],T,Ind,C,[mode(Ind)|C]).

keyword([visual | T],T,Ind,C,[visual(Ind)|C]).
keyword([normal | T],T,Ind,C,[normal(Ind)|C]).

keyword([save | T],T,Ind,C,[save(Ind)|C]).
keyword([quit | T],T,Ind,C,[quit(Ind)|C]).
%screen and window are the same
keyword([screen | T],T,Ind,C,[screen(Ind)|C]).
keyword([window | T],T,Ind,C,[screen(Ind)|C]).

keyword([file | T],T,Ind,C,[file(Ind)|C]).

keyword([split | T],T,Ind,C,[split(Ind)|C]).
keyword([horizontally | T],T,Ind,C,[horizontally(Ind)|C]).
keyword([vertically | T],T,Ind,C,[vertically(Ind)|C]).


% if these words appear, they won't affect anything:
keyword([number | T],T,_,C,C).
keyword([beginning | T],T,_,C,C).
keyword([start | T],T,_,C,C).
keyword([vim | T],T,_,C,C).
keyword([work | T],T,_,C,C).
keyword([text | T],T,_,C,C).

question([how, do, you | T0],T1,Ind,C0,C1) :-
    keyword_phrase(T0,T1,Ind,C0,C1).
% for questions like 'how does X work in vim'
question([how, does | T0],T1,Ind,C0,C1) :-
    keyword_phrase(T0,T1,Ind,C0,C1).
question([can, you | T0],T1,Ind,C0,C1) :-
    keyword_phrase(T0,T1,Ind,C0,C1).
question([how, can, you | T0],T1,Ind,C0,C1) :-
    keyword_phrase(T0,T1,Ind,C0,C1).
question([how, to | T0],T1,Ind,C0,C1) :-
    keyword_phrase(T0,T1,Ind,C0,C1).
question(T0,T1,Ind,C0,C1) :-
    keyword_phrase(T0,T1,Ind,C0,C1).

% ask(Q,A) gives answer A to question Q
%
% NOTE: S is a sentence, Q is that sentence as a list
%
% taken from http://stackoverflow.com/questions/19736439/delete-character-from-string-in-prolog
remove_char(S,C,X) :- atom_concat(L,R,S), atom_concat(C,W,R), atom_concat(L,W,X).
remove_char(S,_,S).

ask(Q,T) :-
    remove_char(Q, '?', Q2),
    string_lower(Q2, Q3),
    atomic_list_concat(L,' ', Q3),
    question(L,[],A,[],C),
    prove_all(C),
    solution(A, T).

% prove_all(L) proves all elements of L against the database
prove_all([]).
prove_all([H|T]) :-
     H,
    prove_all(T).

% example queries:
% ask("how do you move",X).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

vimrc_keywords_to_command([line, numbers | T],T,C,["set number"|C]).
vimrc_keywords_to_command([syntax, highlighting | T],T,C,["syntax on"|C]).
vimrc_keywords_to_command([highlight, search, results | T],T,C,["set hlsearch"|C]).

extract_features(T0,T4,C0,C4) :-
    det(T0,T1,_,C0,C1),
    vimrc_keywords_to_command(T1,T2,C1,C2),
    extract_features(T2,T4,C2,C4).
extract_features(T,T,C,C).

convert_string_list_to_word_list([],[]).
convert_string_list_to_word_list([H|T],R) :-
    atomic_list_concat(L,' ',H),
    convert_string_list_to_word_list(T,R0),
    append(L,R0,R).

% takes a list of strings L, and converts it into a list of words R0
% and then appends it together with the other list of words S or other list
make_features_list([vimrc, file, with|T], L, R) :-
    convert_string_list_to_word_list(L,R0),
    append(T,R0,R).
make_features_list(S, L, R) :-
    convert_string_list_to_word_list(L,R0),
    append(S,R0,R).

% takes a list of strings, such as:
% ['vimrc file with line numbers', 'syntax highlighting']
% and converts it into a list of words L1, such as:
% [line, numbers, syntax, highlighting]
make_vimrc_list([H|T],L1) :-
    atomic_list_concat(L,' ',H),
    make_features_list(L, T, L1).

% example query:
% get("vimrc file with line numbers, syntax highlighting, highlight search results").
% or
% get("line numbers, syntax highlighting").
get(Q) :-
    atomic_list_concat(L,', ',Q),
    make_vimrc_list(L,L1),
    extract_features(L1,[],[],C),
    create_vimrc(C).


% L is a list of features that you want in your vimrc file
% example query:
% create_vimrc(["set number", "syntax on", "set hlsearch"]).
create_vimrc(L) :-
    open('vimrc', write, Stream),
    writeFeature(L, Stream),
    close(Stream).

writeFeature([], _).
writeFeature([H|T], Stream) :-
    write(Stream, H), nl(Stream),
    writeFeature(T,Stream).
