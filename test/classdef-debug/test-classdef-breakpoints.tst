##  Test script for setting breakpoints in classdefs
##  using line numbers or method names.

%!function assert_dbstatus (bp0)
%!  dbs = dbstatus ();
%!  bp = [dbs(:).line];
%!  assert (length (bp) == length (bp0),
%!          'Number of breakpoints set must match');
%!  if (length (bp) != 0)
%!    assert (sort (bp) == bp0, 'Breakpoint line numbers must be equal');
%!  endif
%!endfunction

## Set breakpoints by line numbers
%!test <*46451>
%! if (isguirunning ())
%!   orig_show_dbg = __event_manager_gui_preference__ ("editor/show_dbg_file",
%!                                                     "false");
%! endif
%! unwind_protect
%!   ## Add breakpoints in different member functions using line numbers.
%!   dbstop classdef_breakpoints 13 7 10;
%!   assert_dbstatus ([7, 10, 13]);
%!
%!   ## Remove one breakpoint and confirm the others remain.
%!   dbclear classdef_breakpoints 10;
%!   assert_dbstatus ([7, 13]);
%!
%!   ## Clear all breakpoints, none should be left.
%!   dbclear classdef_breakpoints;
%!   assert_dbstatus ([]);
%! unwind_protect_cleanup
%!   if (isguirunning ())
%!     __event_manager_gui_preference__ ("editor/show_dbg_file", orig_show_dbg);
%!   endif
%! end_unwind_protect

## Set breakpoints by method name
%!test
%! if (isguirunning ())
%!   orig_show_dbg = __event_manager_gui_preference__ ("editor/show_dbg_file",
%!                                                     "false");
%! endif
%! unwind_protect
%!   ## Add breakpoint in constructor
%!   dbstop @classdef_breakpoints/classdef_breakpoints;
%!   assert_dbstatus ([7]);
%!
%!   ## Add breakpoints in methods.
%!   dbstop @classdef_breakpoints/foo;
%!   dbstop @classdef_breakpoints/bar;
%!   assert_dbstatus ([7, 10, 13]);
%!
%!   ## Remove breakpoint from one method.
%!   dbclear @classdef_breakpoints/foo;
%!   assert_dbstatus ([7, 13]);
%!
%!   ## Clear all breakpoints, none should be left.
%!   dbclear classdef_breakpoints;
%!   assert_dbstatus ([]);
%! unwind_protect_cleanup
%!   if (isguirunning ())
%!     __event_manager_gui_preference__ ("editor/show_dbg_file", orig_show_dbg);
%!   endif
%! end_unwind_protect

## Try to add breakpoint in non-existent method
%!test <65610*>
%! if (isguirunning ())
%!   orig_show_dbg = __event_manager_gui_preference__ ("editor/show_dbg_file",
%!                                                     "false");
%! endif
%! unwind_protect
%!   fail ("dbstop @classdef_breakpoints/baz;", "unable to find function");
%! unwind_protect_cleanup
%!   dbclear classdef_breakpoints;
%!   if (isguirunning ())
%!     __event_manager_gui_preference__ ("editor/show_dbg_file", orig_show_dbg);
%!   endif
%! end_unwind_protect

%!test <46451>
%! if (isguirunning ())
%!   orig_show_dbg = __event_manager_gui_preference__ ("editor/show_dbg_file",
%!                                                     "false");
%! endif
%! unwind_protect
%!   ## Add breakpoint in local function.
%!   dbstop classdef_breakpoints 19;
%!   assert_dbstatus ([20]);
%! unwind_protect_cleanup
%!   if (isguirunning ())
%!     __event_manager_gui_preference__ ("editor/show_dbg_file", orig_show_dbg);
%!   endif
%! end_unwind_protect
