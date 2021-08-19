%!test
%! o = bug59704_1 ();
%! o.test ();
%! assert (o.p, [])

%!test
%! o = bug59704_2 ();
%! [~, o(1)] = size (rand (2, 5));
%! assert (o(1), 5)
