%!test <44643>
%! a = bug44643 ();
%! b = bug44643 ();
%! a.parent = b;
%! b.child  = a;
%!
%! assert (isequal (a, b));
%! assert (isequal (a, a));
