%!test
%! s = rand (3, 4, "single");
%! sc = s + i * rand (3, 4, "single");
%! d = rand (3, 4, "double");
%! dc = d + i * rand (3, 4, "double");
%!
%! [sx, scx, dx, dcx] = mexnumtst (s, sc, d, dc);
%! assert (s, sx)
%! assert (sc, scx)
%! assert (d, dx)
%! assert (dc, dcx)
