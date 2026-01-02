%!test <*67403>
%! cb = class_bug67403 ();
%! # Check numArgumentsFromSubscript is not called
%! [a, b, c] = cb (1);
%! assert ([a, b, c], [3, 3, 3]);
%! assert (cb(1:3), 1);
%! assert (cb{1}.a(1:3), 1);
%! # Check numArgumentsFromSubscript is called
%! assert ([cb{1:10}], [2, 2]);
%! assert ([cb.a], [2, 2]);
