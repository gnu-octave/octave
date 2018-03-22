%!function load53027 (fname)
%!  global X
%!  X = 2;
%!  load (fname);
%!  assert (X, 1);
%!endfunction

%!function save53027 (fname)
%!  global X
%!  X = 1;
%!  save (fname, "X");
%!endfunction

%!test <*53027>
%! global X
%! X = 0;
%! fname = tmpnam ();
%! save53027 (fname);
%! assert (X, 1);
%! load53027 (fname);
%! assert (X, 1);
%! load53027 (fname);
%! assert (X, 1);
%! clear X
%! assert (exist ("X"), 0);

%!test <*53027>
%! [a, b] = ntest53027a ();
%! assert ([a, b], [0, 0])

## Not fixed yet.
%!test <53027>
%! [a, b] = ntest53027b ();
%! assert ([a, b], [0, 0])
