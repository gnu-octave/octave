%!test
%! warning ("off", "Octave:function-name-clash", "local");
%! f = testclass (1, 2);
%! assert (one (f), 1);
%! assert (two (f), 2);
%! rehash ();
%! assert (one (f), 1);
%! assert (two (f), 2);
%! assert (two (f), 2);
