%!test
%! a = class_bug44940 ();
%! b = a;
%! c = a ();
%! a.child = 100;
%! assert (a.child, b.child);
%! assert (a.child, c.child);
%! c.child = 500;
%! assert (a.child, b.child);
%! assert (a.child, c.child);

