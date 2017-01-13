## Test old-style subfunctions with no END statements.

function r = duplicate_subfunction_old_syntax ()
  r = 0;
  bug ();

function r = bug ()
  r = 1

function r = bug ()
  r = 2
