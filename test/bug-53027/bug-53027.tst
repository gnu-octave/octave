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
%! save53027 (fname);
%! assert (X, 1);
%! load53027 (fname);
%! assert (X, 1);
%! load53027 (fname);
%! assert (X, 1);
%! clear X
%! assert (exist ("X"), 0);
%! clear -global X;  # cleanup after test

%!test <*53027>
%! [a, b] = ntest53027a ();
%! assert ([a, b], [0, 0])
%! clear -global x;  # cleanup after test

%!test <*53027>
%! [a, b] = ntest53027b ();
%! assert ([a, b], [0, 0])
%! clear -global x;  # cleanup after test

%!test <*53027>
%! [a, b] = ntest53027c ();
%! assert ([a, b], [0, 0])
%! clear -global x;  # cleanup after test

## Previous bugs have caused segfaults when executing script twice.
%!test <*53027>
%! gtest53027
%! assert (isempty (a) && isempty (c))
%! assert (isglobal ("a") && isglobal ("c"))
%! assert (! exist ("b"))
%! assert (isempty (xx) && ! isglobal ("xx"))
%! gtest53027
%! assert (isempty (a) && isempty (c))
%! assert (isglobal ("a") && isglobal ("c"))
%! assert (! exist ("b"))
%! assert (isempty (xx) && ! isglobal ("xx"))
%! clear -global a b c;
