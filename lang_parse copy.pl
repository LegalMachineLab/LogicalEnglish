:- use_module('./tokenize/prolog/tokenize.pl').

:- dynamic dict/4.

le_lang(english).

lang_token(String, Lang, NewTokens) :-
    tokenize(String, Tokens),
    (   Tokens = [Lang, punct(:)|NewTokens]
    ;   Tokens = [Lang, punct(:), space(_)|NewTokens]
    ).

dict(Id, Lang, String, Tokens) :-
    % string_to_translate(String),
    lang_token(String, word(Lang), Tokens), !.
    % le_lang(Lang), !.

% string_to_translate(String) --> dict(1, Lang, String, Tokens).

%%%%%%

% the templates are:
% string_to_translate("english: a person is an italian citizen").
% string_to_translate("italian: una persona è cittadina italiana").

% dict(1, english, "a person is an italian citizen", [word(a), space(_), word(person), space(_), word(is), space(_), word(an), space(_), word(italian), space(_), word(citizen)]).
% dict(1, italian, "una persona è cittadina italiana", [word(una), space(_), word(persona), space(_), word(è), space(_), word(cittadina), space(_), word(italiana)]).

pl_lang(PLLang, String, PrologTokens) :-
    % string_to_translate(String) -> assert(dict(1, Lang, String, Tokens)),
    dict(Id, _, String, _),
    dict(Id, PLLang, _, PrologTokens).

build_dict :-
    retractall(dict(_, _, _, _)),
    forall(
        string_to_translate(Id, String),
        (   lang_token(String, word(Lang), Tokens),
            % untokenize(Tokens, NewString),
            split_string(String, ":", " ", [_, NewString]),
            assert(dict(Id, Lang, NewString, Tokens))
        )
    ).
    

main :-
    read_file_to_string("multiple_languages.txt", F, []),
    split_string(F, ".", "", L),
    forall(
        member(Group, L),        
        (
            split_string(Group, ",", "", GroupList),
            forall(
                member(Pred, GroupList),
                (
                    Pred \= "",
                    normalize_space(atom(PredClean), Pred),
                    assert(string_to_translate(1, PredClean))
                )
            )
        )
    ).