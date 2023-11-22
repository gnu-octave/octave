##  Test script for setting breakpoints in classdefs using line numbers.

%!function assert_dbstatus (bp0)
%!  dbs = dbstatus ();
%!  bp = [dbs(:).line];
%!  assert (length (bp) == length (bp0),
%!          'Number of breakpoints set must match');
%!  if (length (bp) != 0)
%!    assert (sort (bp) == bp0, 'Breakpoint line numbers must be equal');
%!  endif
%!endfunction

## Add breakpoints in different member functions using line numbers.
%!test <*46451>
%! dbstop classdef_breakpoints 13 7 10;
%! assert_dbstatus ([7, 10, 13]);

## Remove one breakpoint and confirm the others remain.
%!test <*46451>
%! dbclear classdef_breakpoints 10;
%! assert_dbstatus ([7, 13]);

## Add breakpoint in local function.
%!test <46451>
%! dbstop classdef_breakpoints 19;
%! assert_dbstatus ([7, 13, 20]);

## Clear all breakpoints, none should be left.
%!test <*46451>
%! dbclear classdef_breakpoints;
%! assert_dbstatus ([]);
