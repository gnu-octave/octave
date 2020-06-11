%!test <*57941>
%! [r1, r2] = bug57941a (2);
%! assert (r1, 6);
%! assert (r2, 24);

%!test <*57941>
%! [fh1, fh2] = bug57941b (2);
%! assert (fh1 (3), 6);
%! assert (fh2 (3, 4), 24);
