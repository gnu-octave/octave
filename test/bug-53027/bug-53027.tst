%!function load53027 (fname)
%!  global X;
%!  X = 2;
%!  load (fname);
%!  assert (X, 1);
%!endfunction

%!function save53027 (fname)
%!  global X;
%!  X = 1;
%!  save (fname, "X");
%!endfunction

%!test <*53027>
%! global X;
%! X = 0;
%! fname = tempname ();
%! unwind_protect
%!   save53027 (fname);
%!   assert (X, 1);
%!   load53027 (fname);
%!   assert (X, 1);
%!   load53027 (fname);
%!   assert (X, 1);
%!   clear X
%!   assert (exist ("X"), 0);
%! unwind_protect_cleanup
%!   clear -global X;
%!   unlink (fname);  # remove temp file
%! end_unwind_protect

%!test <*53027>
%! [a, b] = ntest53027a ();
%! unwind_protect
%!   assert ([a, b], [0, 0])
%! unwind_protect_cleanup
%!   clear -global x;
%! end_unwind_protect

%!test <*53027>
%! [a, b] = ntest53027b ();
%! unwind_protect
%!   assert ([a, b], [0, 0])
%! unwind_protect_cleanup
%!   clear -global x;
%! end_unwind_protect

%!test <*53027>
%! [a, b] = ntest53027c ();
%! unwind_protect
%!   assert ([a, b], [0, 0])
%! unwind_protect_cleanup
%!   clear -global x;
%! end_unwind_protect

## Previous bugs have caused segfaults when executing script twice.
%!test <*53027>
%! unwind_protect
%!   gtest53027
%!   assert (isempty (a) && isempty (c))
%!   assert (isglobal ("a") && isglobal ("c"))
%!   assert (! exist ("b"))
%!   assert (isempty (xx) && ! isglobal ("xx"))
%!   gtest53027
%!   assert (isempty (a) && isempty (c))
%!   assert (isglobal ("a") && isglobal ("c"))
%!   assert (! exist ("b"))
%!   assert (isempty (xx) && ! isglobal ("xx"))
%! unwind_protect_cleanup
%!   clear -global a b c;
%! end_unwind_protect
