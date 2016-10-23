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
insert(howtoinserttostartofline).
insert(howtoinserttoendofline).
insert(howtoinserttomultiplelines).
insert(howtoswitchmodetoinsert).

multiple(howtoinserttomultiplelines).
lines(howtoinserttomultiplelines).
lines(howtodeleteline).
lines(howtocopyandpasteline).

start(howtoinserttostartofline).

end(howtoinserttoendofline).
end(howtojumpline).
end(howtojumpfile).
end(howtojumpparagraph).
end(howtojumpword).

word(howtojumpword).
line(howtoinserttostartofline).
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

solution(howtojumpline, "To jump to the beginning of the line, type '0'. To jump to the end of the line, type '$'.").
solution(howtojumpfile, "To jump to the beginning of the file, type 'gg'. To jump to the end of the file, type 'G'.").
solution(howtojumpparagraph, "To jump to the beginning of the paragraph, type '{'. To jump to the end of the paragraph, type '}'.").
solution(howtojumpword, "To jump to the beginning of the word, type 'w'. To jump to the end of the word, type 'b'.").

solution(howtoinsert, "To insert before the cursor, type 'i'. To insert after the cursor, type 'a'.").
solution(howtoinserttostartofline, "To insert to beginning of line, type 'I'.").
solution(howtoinserttoendofline, "To insert to end of line, type 'A'.").
solution(howtoinserttomultiplelines, "To insert to multiple lines, press Ctrl-v to enter visual block mode, press 'j' or 'k' to select lines to insert, type 'I', write your text, then press escape.").
solution(howtosearchandreplace, "To search and replace, type ':s/<word-to-search>/<word-to-replace>'. You can also add a global and confirmation prompt by adding 'g' and 'c' flags. For instance, ':/s/foo/bar/gc' would replace every instance of foo with bar in the current file with a prompt specifying whether or not you want the replacement to occur.").
solution(howtosearch, "To search, type '/<word-to-search>'. For instance '/hello', searches for the word hello in the current document.").
solution(howtoswitchmodetoinsert, "To switch to insert mode, type 'i'.").
solution(howtoswitchmodetovisual, "To switch to visual mode, type 'v'.").
solution(howtoswitchmodetonormal, "To switch to normal mode, press 'Esc'.").
solution(howtosave,"To save, type ':w'.").
solution(howtosaveandquit, "To save and quit, type ':wq'.").
solution(howtoquit, "To quit, type ':q'. To quit without saving, type ':q!'.").
solution(howtosplitwindow, "To split window, type ':split'.").
solution(howtosplitwindowvertically, "To split window vertically, type ':vsplit'.").
solution(howtosplitwindowhorizontally, "To split window horizontally, type ':hsplit'.").

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
% start, beginning mean the same thing
keyword([start | T],T,Ind,C,[start(Ind)|C]).
keyword([beginning | T],T,Ind,C,[start(Ind)|C]).


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

% vimrc_pair(X,Y) writes property Y into .vimrc file specified by X
vimrc_pair('line numbers', 'set number').
vimrc_pair('syntax highlighting', 'syntax on').
vimrc_pair('highlight search results', 'set hlsearch').
vimrc_pair('auto indent', 'set autoindent').
vimrc_pair('enable mouse', 'set mouse=a').
vimrc_pair('search and replace word under cursor', ':nnoremap <Leader>s :%s/\<<C-r><C-w>\>//gc<Left><Left><Left>
').
vimrc_pair('vim colours', 'set t_Co=256').
vimrc_pair('80 char indicator', 'set colorcolumn=80').
vimrc_pair('highlight current line', 'set cursorline').
vimrc_pair('file type indentation', 'filetype indent on').


vimrc_keywords_to_command([Str|T],T,C,[Rule|C]) :-
    vimrc_pair(Str, Rule).

extract_features(T0,T4,C0,C4) :-
    det(T0,T1,_,C0,C1),
    vimrc_keywords_to_command(T1,T2,C1,C2),
    extract_features(T2,T4,C2,C4).
extract_features(T,T,C,C).

remove_substring(Start, All, End) :-
    concat(Start, End, All).


% We can add other versions of make_feature_list, copy the whole predicate but  replacing "vimrc file with " with other leader strings we want to allow.
make_features_list([H|T],[N|T]) :-
    remove_substring("vimrc file with ", H, N).

make_features_list(L,L).

% example query:
% get_vimrc("vimrc file with line numbers, syntax highlighting, highlight search results").
% or
% get_vimrc("line numbers, syntax highlighting").

get_vimrc(Q) :-
    atomic_list_concat(L1,', ',Q),
    make_features_list(L1,L),
    extract_features(L,[],[],C),
    create_vimrc(C).

% L is a list of features that you want in your vimrc file
% example query:
% create_vimrc("line numbers, highlight search results").
create_vimrc(L) :-
    open('vimrc', write, Stream),
    writeFeature(L, Stream),
    close(Stream).

writeFeature([], _).
writeFeature([H|T], Stream) :-
    write(Stream, H), nl(Stream),
    writeFeature(T,Stream).


% prints out what you can query to make a vimrc file
vimrc_help() :-
    print('To generate a vimrc file, call the predicate create_vimrc, passing in a list of the features you want to include. Here is a list of all the features that you can add:'),
    nl(),
    getAllFeatures(L),
    print(L),
    nl(),
    print('So for example, try calling create_vimrc("line numbers, highlight search results") to generate a vimrc which has the rule set for causing line numbers to appear on the left margin, and also the rule that will highlight any string that is matched in a search.').
    
getAllFeatures(L) :-
    findall(A, vimrc_pair(A, _), L).
    
