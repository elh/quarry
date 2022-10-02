%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Entity-attribute-value-time quadruple knowledge base DB.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- use_module(library(persistency)).

:- persistent eav(entity:atom, attribute:atom, value:any, time:number).
:- initialization db_attach('eav.db', []).

all_entities(Es) :-
    findall(E, eav(E, _, _, _), X),
    list_to_set(X, Es).

all_attributes(As) :-
    findall(A, eav(_, A, _, _), X),
    list_to_set(X, As).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Rules.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 2020 International Society of Hypertension Global Hypertension Practice Guidelines
% ISH 2020 recommendations (evidence-based standards of care)
% https://www.ahajournals.org/doi/10.1161/HYPERTENSIONAHA.120.15026
%
% attributes: bp_office, bp_home. both are lists of [systolic, diastolic] values
% recommendations: 
%     remeasure after 3 years, remeasure after 1 year,
%     high-normal bp diagnosis, grade 1 HTN diagnosis, grade 2 HTN diagnosis

% Seed with initial bp data facts.
clear_eav_db :- retractall_eav(_, _, _, _).
initialize_eav_db :-
    get_time(Now),
    T0 is Now - 2000,
    T1 is Now - 1000,
    % dm: persistency asserta_<name> exists but is not documented.
    asserta_eav(alice, bp_office, [130, 85], T0), % alice's recommendation is remeasure_after_3_years
    asserta_eav(alice, bp_office, [125, 80], T1),
    asserta_eav(alice, bp_office, [120, 80], Now),
    asserta_eav(bob, bp_office, [180, 100], T0), % bob's recommendation is htn_grade_2
    asserta_eav(bob, bp_office, [170, 100], T1),
    asserta_eav(bob, bp_office, [160, 100], Now),
    asserta_eav(bob, bp_home, [160, 100], Now),
    asserta_eav(charlie, bp_home, [160, 100], Now). % charlie has high bp_home but no bp_office measurements

% Recommendations
recommendation(E, htn_grade_2) :- htn_grade_2(E).
recommendation(E, htn_grade_1) :- htn_grade_1(E).
recommendation(E, high_normal_bp) :- high_normal_bp(E).
recommendation(E, remeasure_after_1_year) :- remeasure_after_1_year(E).
recommendation(E, remeasure_after_3_years) :- remeasure_after_3_years(E).

% rules should be MECE. order of rules in recommendation would not matter if they are MECE.
htn_grade_2(E) :-
    high_bp_office(E),
    eav(E, bp_home, [SysHome, DiaHome], _),
    (SysHome >= 160; DiaHome >= 100).
htn_grade_1(E) :-
    high_bp_office(E),
    eav(E, bp_home, [SysHome, DiaHome], _),
    ((SysHome < 160, SysHome >= 140, DiaHome < 100); (DiaHome < 100, DiaHome >= 90, SysHome < 160)).
high_normal_bp(E) :-
    high_bp_office(E),
    eav(E, bp_home, [SysHome, DiaHome], _),
    ((SysHome < 140, SysHome >= 135, DiaHome < 90); (DiaHome < 90, DiaHome >= 85, SysHome < 140)).
remeasure_after_1_year(E) :-
    high_bp_office(E),
    eav(E, bp_home, [SysHome, DiaHome], _),
    (SysHome < 135, DiaHome < 85).
remeasure_after_3_years(E) :-
    % cannot use `\+ high_bp_office` because it will not unify for free var E.
    avg_bp_office_measurement(E, SysOffice, DiaOffice),
    (SysOffice < 130, DiaOffice < 85).

% Office bp
high_bp_office(E) :-
    avg_bp_office_measurement(E, SysOffice, DiaOffice),
    (SysOffice >= 130; DiaOffice >= 85).

% Computed using the 3 latest bp_office values.
% "3 readings - use the average of 2nd and 3rd".
avg_bp_office_measurement(E, AvgSys, AvgDia) :-
    % [E, S, D] Template is needed in order to manually pull E out and bound it.
    findnsols(3, [E, S, D], eav(E, bp_office, [S, D], _), [[BoundE, S1, D1], [_, S2, D2], _]),
    AvgSys is (S1 + S2) / 2,
    AvgDia is (D1 + D2) / 2,
    E = BoundE.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Expert System via meta logic
% Drawn from SWISH expert system example.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
prove(true) :- !.
prove((B, Bs)) :-
    !,
    prove(B),
    prove(Bs).
prove(H) :- % rule added from SO.
    predicate_property(H, built_in),
    !,
    call(H).
prove(H) :-
    clause(H, B),
    prove(B).

% prove bp_home
prove(eav(E, bp_home, [S, D], _)) :-
    format(atom(Estr), "~w", E),
    string_concat(Estr, ": bp_home systolic?", SysPrompt),
    writeln(SysPrompt),
    read(S),
    string_concat(Estr, ": bp_home diastolic?", DiaPrompt),
    writeln(DiaPrompt),
    read(D).
% prove avg_bp_office_measurement
prove(avg_bp_office_measurement(E, S, D)) :-
    format(atom(Estr), "~w", E),
    string_concat(Estr, ": bp_office systolic average?", SysPrompt),
    writeln(SysPrompt),
    read(S),
    string_concat(Estr, ": bp_office diastolic average?", DiaPrompt),
    writeln(DiaPrompt),
    read(D).
