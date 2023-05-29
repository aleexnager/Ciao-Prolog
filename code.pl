:- module(_,_,[]).
%author_data('Nager','Fernandez-Calvo','Alejandro','C200070').

%colors && rules
color(o).
color(x).

rule(o,o,o,_,o). % regla nula
rule(x,o,o,r(A,_,_,_,_,_,_),A) :- color(A).
rule(o,x,o,r(_,B,_,_,_,_,_),B) :- color(B).
rule(o,o,x,r(_,_,C,_,_,_,_),C) :- color(C).
rule(x,o,x,r(_,_,_,D,_,_,_),D) :- color(D).
rule(x,x,o,r(_,_,_,_,E,_,_),E) :- color(E).
rule(o,x,x,r(_,_,_,_,_,F,_),F) :- color(F).
rule(x,x,x,r(_,_,_,_,_,_,G),G) :- color(G).

%predicados auxiliares
my_append([], L, L).
my_append([X|Xs], L, [X|Ys]) :-
    my_append(Xs, L, Ys).

my_member(X, [_ | T]) :-
    my_member(X, T).

my_length([], 0).
my_length([_|T], N) :-
    my_length(T, N1),
    N is N1 + 1.

%predicado para verificar si un estado empieza y termina con células blancas
is_valid_state(State) :-
    my_length(State, N),
    N > 2,
    my_append([o|_], [o], State).

%cells/3
cells([], _, []). % Caso base: la lista está vacía
cells([X], _, [X]). % Caso base: la lista contiene un solo elemento
cells(State, Rule, NewState) :-
    is_valid_state(State),
    addIni(o, State, NewState2),
    addIni(o, NewState2, NewState3),
    addEnd(o, NewState3, NewState4),
    addEnd(o, NewState4, NewState5),
    use_rule(NewState5, Rule, NewState6),
    quitarBlancos(NewState6, NewState).

%quitarBlancos/2
quitarBlancos(List, Res) :-
    my_append(Res, [_,_], List).

%use_rule/3
use_rule([],_,[]).
use_rule([X],_,[X]).
use_rule([X,Y],_,[X,Y]).
use_rule([A,B,C|Rest], Rule, [D|NewState]) :-
    rule(A, B, C, Rule, D),
    use_rule([B,C|Rest], Rule, NewState).

addIni(E,[],[E]).
addIni(Elemento, [Cabeza | Cola], [Elemento, Cabeza | Cola]) :-
    addIni(Elemento, Cola, _).

addEnd(E,[],[E]).
addEnd(Elemento, [Cabeza | Cola], [Cabeza | ColaConElemento]) :-
    addEnd(Elemento, Cola, ColaConElemento).

%evol/3
evol(0, _, [o, x, o]).
evol(s(N), RuleSet, Cells) :-
    evol(N, RuleSet, NewState),
    cells(NewState, RuleSet, Cells).

%ruleset/2
steps(Cells, N) :-
    evol(N, _, Cells).

ruleset(Rule, Cells) :-
    evol(N, Rule, Cells).