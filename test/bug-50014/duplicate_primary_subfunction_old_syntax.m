## Test old-style subfunctions with no END statements

function r = duplicate_primary_subfunction_old_syntax ()
  r = 0;
  notbug ();

function r = notbug ()
  r = 1

function r = duplicate_primary_subfunction_old_syntax ()
  r = 2
