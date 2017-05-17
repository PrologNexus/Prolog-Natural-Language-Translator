agent:-
	write('Enter Command: '),
	perceive(Percepts),
	action(Percepts).

perceive(Percepts):-
	read(Percepts).

action(X):-
	synonym(X, X, Y),
	sentence(Y, Z),
	format(Z, Noun_Phrase, Verb, Noun_Phrase2),
	rule(_, Noun_Phrase, Verb, Noun_Phrase2, Command),
	Command.


% --- SYNONYM ---

% End Condition if no replacements
synonym([], Y, Z):-
	copy(Y, Z).

synonym([H|T], Y, Z):-
	checkDictionary(H),
	synonym(T, Y, Z).

% Reached if checkDictionary Fails
synonym([H|_], Y, Z):-
	replace(H,'directory', Y, Z).

% Checks if the given word is in our dictionary
checkDictionary(X):-
	adj(X);
	adverb(X);
        det(X);
        noun(X);
	prep(X);
	verb(X).

%Copies one list into another, taken from stackoverflow
%http://stackoverflow.com/a/1911813
copy(L,R) :- accCp(L,R).
accCp([],[]).
accCp([H|T1],[H|T2]) :- accCp(T1,T2).

% Replaces a member of a list, taken from stackoverflow
% http://stackoverflow.com/a/30016965
% code by @svick, modified to use dif/2 instead of (\=)/2
replace(_, _, [], []).
replace(O, R, [O|T], [R|T2]) :- replace(O, R, T, T2).
replace(O, R, [H|T], [H|T2]) :- dif(H,O), replace(O, R, T, T2).


% --- RULE FORMATS ---

% Sentence outputs our string into 3 different formats.
% Therefore we must standardise and find the important sections.

format(sentence(np(_, Noun_Phrase), vp(verb(Verb), np(_, Noun_Phrase2))), Noun_Phrase, Verb, Noun_Phrase2).
format(sentence(np(_, Noun_Phrase), vp(verb(Verb), pp(_, Noun_Phrase2))), Noun_Phrase, Verb, Noun_Phrase2).
format(sentence(np(_, Noun_Phrase), vp(verb(Verb), pp(_, np(_, Noun_Phrase2)))), Noun_Phrase, Verb, Noun_Phrase2).


% --- RULES ---

% rule(<rule_name>,<noun_phrase>,<verb>,<second_noun_phrase>,<consequent_outcome_as_prolog_predicate>).

rule(r1,np2(adverb(very),np2(adj(short),np2(noun(command)))),listing, np2(adj(current),np2(noun(directory))),write('ls')).
rule(r2,np2(adj(current),np2(noun(directory))),viewed,np2(adverb(more),np2(adj(fine),np2(noun(detail)))),write('ls –la')).
rule(r3,np2(noun(command)),moving,np2(adj(higher),np2(noun(directory))),write('cd ..')).
rule(r4,np2(noun(command)),moves,np2(adj(parent),np2(noun(directory))),write('cd ..')).
rule(r5,np2(noun(command)),prints,np2(adj(current),np2(noun(directory))),write('pwd')).
rule(r6,np2(noun(command)),types,np2(noun(file)),write('cat 08226.txt')).


% --- SENTENCE PARSER ---

% S -> NP VP
sentence(Sentence,sentence(Noun_Phrase,Verb_Phrase)):-
	np(Sentence,Noun_Phrase,Rem),
	vp(Rem,Verb_Phrase).

% S -> VP
sentence(Sentence, sentence(Verb_Phrase)):-
	vp(Sentence,Verb_Phrase).

% NP -> Det NP2
np([H|T],np(det(H),NP2),Rem):-
	det(H),
	np2(T,NP2,Rem).

% NP -> NP2
np(Sentence,Parse,Rem):-
	np2(Sentence,Parse,Rem).

% NP -> NP PP
np(Sentence,np(NP,PP),Rem):-
	np(Sentence,NP,Rem1),
	pp(Rem1,PP,Rem).

% NP2 -> Noun
np2([H|T],np2(noun(H)),T):-
	noun(H).

% NP2 -> Adj NP2
np2([H|T],np2(adj(H),NP2),Rem):-
	adj(H),
	np2(T,NP2,Rem).

% NP2 -> Adverb NP2
np2([H|T],np2(adverb(H),NP2),Rem):-
	adverb(H),
	np2(T,NP2,Rem).


% PP -> Prep NP
pp([H|T],pp(prep(H),NP),Rem):-
	prep(H),
	np(T,NP,Rem).

% VP -> Verb
vp([H|T],vp(verb(H)),T):-
	verb(H).

% VP -> Verb PP
vp([H|T],vp(verb(H),PP)):-
	verb(H),
	pp(T,PP,_).

% VP -> Verb NP
vp([H|T],vp(verb(H),NP)):-
	verb(H),
	np(T,NP,_).

% VP -> Verb Adverb
vp([X,Y],vp(verb(X),adverb(Y))):-
	verb(X),
	adverb(Y).

% VP -> Verb Adverb NP2
vp([X,Y|T],vp((verb(X),adverb(Y)),NP2)):-
        verb(X),
	adverb(Y),
        np2(T,NP2,_).

% --- DICTIONARY ---

%word_type('word').

% -- BASIC WORDS --

adj('current').
adj('fine').
adj('higher').
adj('parent').
adj('short').
adverb('more').
adverb('very').
det('a').
det('the').
noun('command').
noun('detail').
noun('directory').
noun('file').
noun('08226txt').
prep('and').
prep('in').
prep('to').
verb('listing').
verb('moves').
verb('moving').
verb('prints').
verb('types').
verb('viewed').











