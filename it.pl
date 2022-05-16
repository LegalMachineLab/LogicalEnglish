:- module(casp_lang_it,
          [ scasp_message//1
          ]).
:- use_module(library(dcg/high_order)).
:- use_module('../ops', [op(_,_,_)]).

:- multifile
    scasp_messages:scasp_lang_module/2.

scasp_messages:scasp_lang_module(it, casp_lang_it).


		 /*******************************
		 *            SCASP		*
		 *******************************/

scasp_message(version(Version)) -->
    [ 'versione ~w'-[Version] ].

% Usage messages

scasp_message(source_not_found(Source)) -->
    (   \+ { access_file(Source, exist) }
    ->  [ 'File di input '-[] ], code(Source), [ ' non esiste'-[] ]
    ;   [ 'Impossibile leggere il file '-[] ], code(Source)
    ).
scasp_message(no_input_files) -->
    [ 'Nessun file input specificato!' ].
scasp_message(no_query) -->
    [ 'Il programma non coniene ?- Query.'-[] ].
scasp_message(undefined_operator(Op)) -->
    [ 'operatore clp ~p non definito'-[Op] ].
scasp_message(at_most_one_of([A,B])) -->
    ['Opzioni '], opt(A), [' e '], opt(B),
    [' non possono essere usate insieme' ].
scasp_message(at_most_one_of(List)) -->
    [ 'Al massimo una delle opzioni '-[] ],
    options(List),
    [ ' è ammessa.'-[] ].
scasp_message(opt_dcc_prev_forall) -->
    [ 'Opzione --dcc può essere usata solo con --forall=prev' ].
scasp_message(opt_incompatible(Opt1, Opt2)) -->
    [ 'Opzione ' ], opt(Opt1), [' non è compatibile con '], opt(Opt2).

% Solver messages

scasp_message(failure_calling_negation(Goal)) -->
    [ 'Failure calling negation of '-[] ], goal(Goal).
scasp_message(co_failing_in_negated_loop(Goal, NegGoal)) -->
    [ 'Co-Failing in a negated loop due to a variant call'-[], nl,
      '(extension clp-disequality required).'-[]
    ],
    curr_prev_goals(Goal, NegGoal).
scasp_message(variant_loop(Goal, PrevGoal)) -->
    [ 'Failing in a positive loop due to a variant call (tabling required).'-[]
    ],
    curr_prev_goals(Goal, PrevGoal).
scasp_message(subsumed_loop(Goal, PrevGoal)) -->
    [ 'Failing in a positive loop due to a subsumed call under clp(q).'-[]
    ],
    curr_prev_goals(Goal, PrevGoal).
scasp_message(pos_loop(fail, Goal, PrevGoal)) -->
    [ 'Positive loop failing '-[] ],
    eq_goals(Goal, PrevGoal).

scasp_message(pos_loop(continue, Goal, PrevGoal)) -->
    [ 'Positive loop continuing '-[] ],
    eq_goals(Goal, PrevGoal).
scasp_message(trace_failure(Goal, Stack)) -->
    print_check_calls_calling(Goal, Stack),
    [ ansi(warning, 'FAILURE to prove the literal: ', []) ],
    goal(Goal).

scasp_message(dcc_call(Goal, Stack)) -->
    [ 'DCC of ' ], goal(Goal),
    [ ' in ' ], print_stack(Stack).
scasp_message(dcc_discard(Goal, BodyL)) -->
    { comma_list(Body, BodyL) },
    [ 'DCC discards '], goal(Goal),
    [ ' when checking nmr ~p'-[ dcc(Goal) :- Body ] ].

% Results

scasp_message(no_models(CPU)) -->
    [ 'Non ci sono modelli (~3f secondi)'-[CPU] ].


% Justifications

scasp_message(and)       --> [ 'e' ].
scasp_message(or)        --> [ 'o' ].
scasp_message(not)       --> [ 'non è provato che' ].
scasp_message(-)         --> [ 'non si dà il caso che' ].
scasp_message(implies)   --> [ 'perché' ].
scasp_message(?)         --> [ '?' ].
scasp_message(proved)    --> ['giustificato sopra'].
scasp_message(chs)       --> ['si assume che'].
scasp_message(assume)    --> ['assumiamo che'].
scasp_message(holds)     --> [' tiene'].
scasp_message(holds_for) --> [' tiene per '].
scasp_message(not_in)    --> ['non'].
scasp_message(neq)       --> ['non è uguale a'].
scasp_message(_>_)       --> ['è maggiore di'].
scasp_message(_>=_)      --> ['è maggiore o uguale a'].
scasp_message(_<_)       --> ['è minore di'].
scasp_message(_=<_)      --> ['è minore o uguale a'].
scasp_message(_#=_)      --> ['è uguale a'].
scasp_message(_#<>_)     --> ['non è uguale a'].
scasp_message(_#>_)      --> ['è maggiore di'].
scasp_message(_#>=_)     --> ['è maggiore o uguale a'].
scasp_message(_#<_)      --> ['è minore di'].
scasp_message(_#=<_)     --> ['è minore o uguale a'].
scasp_message(global_constraints_hold) -->
    [ 'I vincoli globali tengono' ].
scasp_message(global_constraint(N)) -->
    [ 'Il vincolo globale numero ', N, ' tiene' ].


		 /*******************************
		 *       GOALS AND STACKS	*
		 *******************************/

print_check_calls_calling(Goal, Stack) -->
    [ansi(bold, '~`-t Calling: ~@ ~`-t~72|', [scasp_verbose:print_goal(Goal)]), nl],
    print_stack(Stack).

%!  print_stack(+Stack)//
%
%   This is a DCG version of print_check_stack/2 from verbose.pl

print_stack(Stack) -->
    { reverse(Stack, RevStack) },
    print_stack(RevStack, 4).

print_stack([], _) -->
    [].
print_stack([[]|As],I) -->
    !,
    { I1 is I - 4 },
    print_stack(As, I1).
print_stack([A|As],I) -->
    ['~t~*|'-[I]], goal(A), [ nl ],
    { I1 is I + 4 },
    print_stack(As,I1).

eq_goals(Goal, PrevGoal) -->
    [ '(Goal '-[] ], goal(Goal), [ ' == '-[] ], goal(PrevGoal), [')'-[]].

curr_prev_goals(Goal, NegGoal) -->
    [ nl,
      '    Current call:  '-[] ], goal(Goal), [ nl,
      '    Previous call: '-[] ], goal(NegGoal).

goal(Goal) -->
    [ ansi(code, '~@', [scasp_verbose:print_goal(Goal)]) ].


		 /*******************************
		 *             UTIL		*
		 *******************************/

options(Values) -->
    sequence(opt, [', '-[]], Values).

opt(Name) -->
    { atom_length(Name, 1) },
    !,
    [ ansi(code, '-~w', [Name]) ].
opt(Name) -->
    [ ansi(code, '--~w', [Name]) ].

list(Values) -->
    sequence(code, [', '-[]], Values).

code(Value) -->
    [ ansi(code, '~w', [Value]) ].
