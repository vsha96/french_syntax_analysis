% ===============
% =====VOCAB=====
% ===============

% PRONOUN

is_pronoun(je, par([1], sing, none)).
is_pronoun(tu, par([2], sing, none)).
is_pronoun(il, par([3], sing, male)).
is_pronoun(elle, par([3], sing, female)).
is_pronoun(on, par([3], sing, none)).
is_pronoun(nous, par([1], plural, none)).
is_pronoun(vous, par([2], plural, none)).
is_pronoun(ils, par([3], plural, none)).
is_pronoun(elles, par([3], plural, female)).

% ARTICLE

is_article(un, par(none,sing,male)).
is_article(une, par(none,sing,female)).
is_article(le, par(none,sing,male)).
is_article(la, par(none,sing,female)).
is_article(des, par(none,plural,none)).
is_article(les, par(none,plural,none)).

% NOUN

is_noun(garçon, par(none, sing, male)).
is_noun(garçons, par(none, plural, male)).

is_noun(fille, par(none, sing, female)).
is_noun(filles, par(none, plural, female)).

is_noun(chanson, par(none, sing, female)).
is_noun(chansons, par(none, plural, female)).

is_noun(chat, par(none, sing, male)).
is_noun(chats, par(none, plural, male)).
is_noun(chatte, par(none, sing, female)).
is_noun(chattes, par(none, plural, female)).

is_noun(chien, par(none, sing, male)).
is_noun(chiens, par(none, plural, male)).
is_noun(chienne, par(none, sing, female)).
is_noun(chiennes, par(none, plural, female)).

% ADJECTIVE

is_adjective(beau, par(none, sing, male)).
is_adjective(belle, par(none, sing, female)).
is_adjective(beaux, par(none, plural, male)).
is_adjective(belles, par(none, plural, female)).

is_adjective(intéressant, par(none, sing, male)).
is_adjective(intéressante, par(none, sing, female)).
is_adjective(intéressants, par(none, plural, male)).
is_adjective(intéressantes, par(none, plural, female)).

is_adjective(petit, par(none, sing, male)).
is_adjective(petite, par(none, sing, female)).
is_adjective(petits, par(none, plural, male)).
is_adjective(petites, par(none, plural, female)).

is_adjective(grand, par(none, sing, male)).
is_adjective(grande, par(none, sing, female)).
is_adjective(grands, par(none, plural, male)).
is_adjective(grandes, par(none, plural, female)).

% ADVERB FOR ADJECTIVE

is_adverb_adj(très, par(none, none, none)).
is_adverb_adj(si, par(none, none, none)).

% VERB

is_verb(chante, par([1,3], sing, none)).
is_verb(chantes, par([2], sing, none)).
is_verb(chantons, par([1], plural, none)).
is_verb(chantez, par([2], plural, none)).
is_verb(chantent, par([3], plural, none)).

is_verb(parle, par([1,3], sing, none)).
is_verb(parles, par([2], sing, none)).
is_verb(parlons, par([1], plural, none)).
is_verb(parlez, par([2], plural, none)).
is_verb(parlent, par([3], plural, none)).

is_verb(trouve, par([1,3], sing, none)).
is_verb(trouves, par([2], sing, none)).
is_verb(trouvons, par([1], plural, none)).
is_verb(trouvez, par([2], plural, none)).
is_verb(trouvent, par([3], plural, none)).

is_verb(prends, par([1,2], sing, none)).
is_verb(prend, par([3], sing, none)).
is_verb(prenons, par([1], plural, none)).
is_verb(prenez, par([2], plural, none)).
is_verb(prennent, par([3], plural, none)).

% ADVERB FOR VERB

is_adverb(vite, par(none, none, none)).
is_adverb(fort, par(none, none, none)).
is_adverb(haut, par(none, none, none)).



