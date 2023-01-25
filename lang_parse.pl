:- use_module('./tokenize/prolog/tokenize.pl').
% :- initialization(main).
% :- initialization(once(((main ; true), halt))).
:- dynamic dict/4.

% :- use_module('./le_input.pl', [build_template_elements]).

term_dict([eng, agent], [ita, agente]).
term_dict([eng, patient], [ita, paziente]).

get_variables_inner([punct(*)|_], Var2, Var2) :- !.

get_variables_inner([A|List], Var, Var2) :-
    % write(A), nl,
    append(Var, [A], Var2),
    % \+ A == punct(*),
    get_variables_inner(List, Var2, _).

get_variables([punct(*), A|Rest], Var, Var3) :-
    append(Var, [A], Var2),
    get_variables_inner(Rest, Var2, Var3), !.

get_variables([_|Rest], _, Var) :-
    get_variables(Rest, _, Var).


lang_token(String, Lang, NewTokens) :-
    normalize_space(string(StringClean), String),
    tokenize(StringClean, Tokens),
    (   Tokens = [Lang, punct(:), space(_)|NewTokens]
    ;   Tokens = [Lang, punct(:)|NewTokens]
    ),
    get_variables(NewTokens, _, Var),
    write(Var), nl, !.


find_language(String, Lang, StringNew) :-
    dict(M),
    member(inner_dict(_, String, _), M),
    member(inner_dict(Lang, StringNew, _), M),
    !.

find_prolog(String, Lang, Prolog) :-
    dict(M),
    member(inner_dict(_, String, _), M),
    member(inner_dict(Lang, _, Prolog), M),
    !.


make_dict([],NewList) :-
    print_message(informational, "~n"-[NewList]),
    assert(dict(NewList)).

make_dict([""|GroupList], NewList) :-
    make_dict(GroupList, NewList).

make_dict([Pred|GroupList], NewList) :-
    normalize_space(atom(PredClean), Pred),
    % assert(string_to_translate(1, PredClean))
    lang_token(PredClean, word(Lang), Tokens),
    % untokenize(Tokens, NewString),
    split_string(PredClean, ":", " ", [_, NewString]),
    % member(inner_dict(Lang, NewString, Tokens), NewList),
    make_dict(GroupList, [inner_dict(Lang, NewString, Tokens)|NewList]), !.


parse_list([]) :- !.

parse_list(["\n"|List]) :-
    parse_list(List), !.

parse_list([Group|List]) :-
    split_string(Group, ",", "", GroupList),
    make_dict(GroupList, []),
    parse_list(List).

main :-
    retractall(dict(_)),
    read_file_to_string("multiple_languages.txt", F, []),
    split_string(F, ".", "", L),
    parse_list(L),
    halt. 