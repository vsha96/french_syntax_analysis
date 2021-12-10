% Ivan Shulyugin 
% MSU CMC 425
% December 2021

% Include vocab
:- [vocabulary_french].
:- use_module(vocabulary_french).



% REQUEST EXAMPLES

% ?- check("les filles chantent haut une chanson belle").
% ?- check("je chante fort").
% ?- check("le chien chante").
% ?- check("le chien chante une chanson belle").
% ?- check("le chien grand chante une chanson belle").



% Main predicate

check(S) :- split_string(S, " ", "", L),
    string_to_atomlist(L, LAtom),
    write('INPUT: '), print(LAtom), write('\n\n'),
    
    sentence(LAtom,_,TREE, [], LAccResAgr),

    agreement_check(LAccResAgr, R, R),
    R == correct,
    write_ln('\nTREE'),
    print_tree(TREE).

string_to_atomlist([], []).
string_to_atomlist([X|XS], [A|AS]) :- 
    string_to_atom(X,A),
    string_to_atomlist(XS, AS).

print_list([]) :- write_ln('').
print_list([X | XS]) :-
    write_ln(X),print_list(XS).

print_n(_, 0) :- !.
print_n(C, N) :- write(C), N1 is (N-1), print_n(C, N1).

print_tree(L) :- in_print_tree(L, 0).
in_print_tree([], _). 
in_print_tree([X | XS], Acc) :-
    \+ is_list(X),!,
    print_n('    ',Acc),
    write_ln(X),
    in_print_tree(XS, Acc).
in_print_tree([X | XS], Acc) :-
    AccN is Acc+1,
    in_print_tree(X, AccN),
    in_print_tree(XS, Acc).

% =================
% =====GRAMMAR=====
% =================

sentence(S,S2,[sentence, NG, VG], [], LAccR) :- 
    noun_group(S,S1,NG,[],LAccR1),
    verb_group(S1,S2,VG,[],LAccR2),
    append(LAccR1, LAccR2, LAccR),
    S2 == [].

noun_group(S,S1,[noun_group, PN], LAcc, LAccR) :-
    pronoun(S,S1,Agr,PN),
    append([S*Agr], LAcc, LAccR).

noun_group(S,S2,[noun_group, A, N], LAcc, LAccR) :-
    article(S,S1,Agr,A),
    noun(S1,S2,Agr1,N),
    append([S*Agr, S1*Agr1], LAcc, LAccR).

noun_group(S,S3,[noun_group, A, N, AdjS], LAcc, LAccR) :-
    article(S,S1,Agr,A),
    noun(S1,S2,Agr1,N),
    adjSequence(S2,S3,AdjS, [], LAccR2),
    append([S*Agr, S1*Agr1 | LAccR2], LAcc, LAccR).

adjSequence(S,S1,[adj_sequence, Adj], LAcc, LAccR) :-
    adjective(S,S1,Agr,Adj),
    append([S*Agr], LAcc, LAccR).

adjSequence(S,S2,[adj_sequence, Adv, Adj], LAcc, LAccR) :-
    adverb_adj(S,S1,_,Adv),
    adjective(S1,S2,Agr1,Adj),
    append([S1*Agr1], LAcc, LAccR).

adjSequence(S,S2,[adj_sequence, Adj, AdjS], LAcc, LAccR) :-
    adjective(S,S1,Agr,Adj),
    adjSequence(S1,S2,AdjS, [], LAccR1),
    append([S*Agr | LAccR1], LAcc, LAccR).

adjSequence(S,S3,[adj_sequence, Adv, Adj, AdjS], LAcc, LAccR) :-
    adverb_adj(S,S1,_,Adv),
    adjective(S1,S2,Agr1,Adj),
    adjSequence(S2,S3,AdjS, [], LAccR2),
    append([S1*Agr1 | LAccR2], LAcc, LAccR).

verb_group(S,S1,[verb_group, V], LAcc, LAccR) :-
    verb(S,S1,Agr,V),
    append([S*Agr], LAcc, LAccR).

verb_group(S,S2,[verb_group, V, Adv], LAcc, LAccR) :-
    verb(S,S1,Agr,V),
    adverb(S1,S2,Agr1,Adv),
    append([S*Agr, S1*Agr1], LAcc, LAccR).

verb_group(S,S2,[verb_group, V, NG], LAcc, LAccR) :-
    verb(S,S1,Agr,V),
    noun_group(S1,S2,NG, [], LAccR1),
    append([S*Agr, LAccR1], LAcc, LAccR).

verb_group(S,S3,[verb_group, V, Adv, NG], LAcc, LAccR) :-
    verb(S,S1,Agr,V),
    adverb(S1,S2,Agr1,Adv),
    noun_group(S2,S3,NG, [], LAccR2),
    append([S*Agr, S1*Agr1, LAccR2], LAcc, LAccR).

pronoun([PN | S], S, Agr, [pronoun, [PN]]) :- is_pronoun(PN, Agr).

article([A | S], S, Agr, [article, [A]]) :- is_article(A,Agr).

noun([N | S], S, Agr, [noun, [N]]) :- is_noun(N,Agr).

adjective([Adj | S], S, Agr, [adjective, [Adj]]) :- is_adjective(Adj,Agr).

adverb_adj([Adv | S], S, Agr, [adverbAdj, [Adv]]) :- is_adverb_adj(Adv,Agr).

verb([V | S], S, Agr, [verb, [V]]) :- is_verb(V,Agr).

adverb([Adv | S], S, Agr, [adverb, [Adv]]) :- is_adverb(Adv, Agr).



% ===================
% =====AGREEMENT=====
% ===================

% par(person, sing/plural, male/female) + none 
% person -> 1 | 2 | 3

agreement_check([_*_], correct, correct) :- !.
agreement_check([_*_], incorrect, incorrect).
agreement_check([S1*P1, S2*P2 | L], RAcc, R) :-
    (agr(S1,P1,S2,P2,PR),!; RAcc = incorrect),
    agreement_check([S1*PR | L], RAcc, R).
agreement_check([_*_, X], RAcc, R) :-
    is_list(X),
    agreement_check(X, RAcc, R).

agr(_,P1,_,P2,R) :- agreement(P1,P2,R), !.
agr(S1,P1,S2,P2,_) :- print_mistakes(S1,P1,S2,P2), fail.

agreement(par(X1,X2,X3), par(Y1,Y2,Y3), par(Z1,Z2,Z3)) :- 
    agr_person(X1,Y1,Z1),
    agr_par(X2,Y2,Z2),
    agr_par(X3,Y3,Z3).
agr_par(X1,X1,X1) :- !.
agr_par(X1,none,X1).
agr_par(none,Y1,Y1).

agr_person(none,Y1,Y1) :- !.
agr_person(X1,none,X1) :- !.
agr_person(X1,Y1,Z1) :-
    intersection(X1,Y1,Z1),Z1 \= [].

print_mistakes(S1,P1,S2,P2) :-
    write('Err: Wrong agreement:\n'),
    write('in part:   '), write(S1), 
    append(Ssub,S2,S1),
    write('\ncontradiction between: '),
        write(Ssub),write(' and '),write(S2),write('\n'),
    find_mistakes(P1,P2).

find_mistakes(par(X1,Y1,Z1), par(X2,Y2,Z2)) :-
    X1 \= X2, X1 \= none, X2 \= none,
        write('\tperson is not agreed: '),
        write(X1), write(' vs '), write(X2), write('\n');
    Y1 \= Y2, Y1 \= none, Y2 \= none,
        write('\tnumber is not agreed: '),
        write(Y1), write(' vs '), write(Y2), write('\n');
    Z1 \= Z2, Z1 \= none, Z2 \= none,
        write('\tgender is not agreed: '),
        write(Z1), write(' vs '), write(Z2), write('\n').



