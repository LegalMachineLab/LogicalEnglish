:- use_module('./tokenize/prolog/tokenize.pl').
:- initialization(main).
% :- initialization(once(((main ; true), halt))).
:- dynamic local_dict/4.

:- use_module('./le_input.pl').%, [build_template, unpack_tokens]).
:- use_module('./deepl.pl').


% Set named variables
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

sublist([], []).
sublist([H|Tail], [Res|RT]) :-
    sublist(Tail, [RH|RT]),
    atom_concat(H, ' ', Temp),
    atom_concat(Temp, RH, Res).
sublist([H|Tail], [H|R]) :-
    sublist(Tail, R).

lang_token(String_, Lang, NewTokens) :-
    % From input.pl
    % ((sub_atom(String_,_,1,0,NL), memberchk(NL,['\n','\r']) ) -> String=String_ ; atom_concat(String_,'\n',String)),
    normalize_space(string(StringClean), String_),
    tokenize(StringClean, Tokens_, [cased(true), spaces(false), numbers(false)]),
    (   Tokens_ = [Lang, punct(:), space(_)|NewTokens_]
    ;   Tokens_ = [Lang, punct(:)|NewTokens_]
    ),
    % get_variables(NewTokens_, _, Var),
    le_input:unpack_tokens(NewTokens_, NewTokens),
    !.

find_language(String, Lang, StringNew) :-
    local_dict(M),
    member(inner_dict(_, String, _, _), M),
    member(inner_dict(Lang, StringNew, _, _), M),
    !.

find_prolog(String_, Lang, Prolog, Args, _) :-
    tokenize(String_, Tokens_, [cased(true), spaces(false), numbers(false)]),
    le_input:unpack_tokens(Tokens_, Tokens2),
    sublist(Tokens2, Tokens),
    local_dict(M),
    member(inner_dict(_, Tokens, _, [_|Args]), M),
    member(inner_dict(Lang, _, _, [P|_]), M),
    Prolog=..[P|Args], !.

match_template(PossibleLiteral, Map1, MapN, Literal) :- 
    %print_message(informational,'Possible Literal ~w'-[PossibleLiteral]),
    local_dict(M),
    member(inner_dict(Lang, Tokens, _, Candidate), M),
    le_input:match(Tokens, PossibleLiteral, Map1, MapN, Template), !,
    Literal =.. Tokens.

% find_prolog(String_, Lang, Prolog, Args, NewTokens) :-
%     %lang_token(String_, Lang, String),
%     tokenize(String_, Tokens_, [cased(true), spaces(false), numbers(false)]),
%     le_input:unpack_tokens(Tokens_, NewTokens),
%     le_input:build_template(NewTokens, _, Args, _, String),
%     local_dict(M),
%     member(inner_dict(_, String, _, _), M),
%     member(inner_dict(Lang, _, _, [Prolog_|Args]), M),
%     Prolog =..[Prolog_|Args], !.
%     %write_term(current_output, Prolog, [singletons(true)]).

find_prolog(String_, Lang, Prolog, Args, NewTokens) :-
    %lang_token(String_, Lang, String),
    sub_string(Lang, 0, 2, _, Lang_), string_upper(Lang_, LangT),
    deepl:translate(LangT, String_, Translation_), split_string(Translation_, "", ".", [Translation|_]),
    tokenize(Translation, Tokens_, [cased(true), spaces(false), numbers(false)]),
    le_input:unpack_tokens(Tokens_, NewTokens),
    le_input:build_template(NewTokens, A, Args, Types, String),
    local_dict(M),
    member(inner_dict(_, String, _, _), M),
    member(inner_dict(Lang, _, _, [Prolog_|Args]), M),
    Prolog = [Prolog_|Args].
    %write_term(current_output, Prolog, [singletons(true)]).

% local_dict(eng, [is_excused, A], [meeting-meeting], [A, is, excused]).

make_dict([],NewList) :-
    % print_message(informational, "~n"-[NewList]),
    write(NewList),nl,
    assert(local_dict(NewList)).

make_dict([""|GroupList], NewList) :-
    make_dict(GroupList, NewList).

make_dict([Pred|GroupList], NewList) :-
    normalize_space(atom(PredClean), Pred),
    % assert(string_to_translate(1, PredClean))
    lang_token(PredClean, word(Lang), Tokens),
    % untokenize(Tokens, NewString),
    % split_string(PredClean, ":", " ", [_, NewString]),
    % member(inner_dict(Lang, NewString, Tokens), NewList),
    le_input:build_template(Tokens, A, Args, Types, D),
    Predicate = [A|Args],
    make_dict(GroupList, [inner_dict(Lang, D, Types, Predicate)|NewList]), !.

parse_list([]) :- !.

parse_list(["\n"|List]) :-
    parse_list(List), !.

parse_list([Group|List]) :-
    split_string(Group, ",", "", GroupList),
    make_dict(GroupList, []),
    parse_list(List).

read_templates(List, Before, After) :-
    append(_, ["the templates are"|After_], List),
    append(Before, ["the knowledge base contains"|After], After_).

main :-
    retractall(local_dict(_)),
    read_file_to_string("multiple_languages.txt", F_, []),
    split_string(F_, ".", "\s\t\n", L_),
    read_templates(L_, Templates, Rules),
    parse_list(Templates). 