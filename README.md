# quarry

To run, install SWI-Prolog or run using `nix-shell`.
```bash
swipl bp.pl
```

Set up Entity-attribute-value database.
```Prolog
?- clear_eav_db. % optional
true.

?- initialize_eav_db.
true.
```

Example queries
```Prolog
?- eav(E, bp_home, [SysHome, _], _), SysHome > 140.

?- recommendation(Person, R).

?- avg_bp_office_measurement(alice, SysOffice, DiaOffice).
```
