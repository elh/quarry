# quarry

`bp.pl` contains a simple proof of concept expert system for ISH's Global Hypertension Practice Guidelines.
* Expert system using meta logic in Prolog
* Persisted, entity-attribute-value database

## Usage

To run, install SWI-Prolog or run using `nix-shell`.
```bash
swipl bp.pl
```

Set up the entity-attribute-value database.
```Prolog
?- clear_eav_db. % optional
true.

?- initialize_eav_db.
true.
```

Example `bp.pl` queries:
```Prolog
% query all values where bp_home systolic is over 140.
?- eav(E, bp_home, [SysHome, _], _), SysHome > 140.

% query for all recommendation.
?- recommendation(Person, R).

% query for the average of the 2 latest systolic and diastolic bp values for alice.
?- avg_bp_office_measurement(alice, SysOffice, DiaOffice).

% solve in the style of an expert system with user input.
?- once(prove(htn_grade_2(charlie))).
?- once(prove(htn_grade_2(dave))).
```
