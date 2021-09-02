%!function out = bug54490 ()
%!  global k;
%!  k = 1;
%!  out = 3;
%!endfunction

%!test <*54490>
%! global k;
%! k = 2;
%! a = [5, 6];
%! a(k) = bug54490 ();
%! assert (a, [5, 3]);
%! k = 2;
%! a = [5, 6];
%! [a(k)] = bug54490 ();
%! assert (a, [5, 3]);
%! clear -global k;  # cleanup after test
