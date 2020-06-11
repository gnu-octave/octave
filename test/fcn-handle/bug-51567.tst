%!shared a
%! a = bug51567 ();
%!
%!assert <51567> (a.doit_1 (), 13);
%!assert <51567> (a.doit_2 (), 42);
