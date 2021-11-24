%!test <*59617>
%! obj = bug59617 ();
%! assert (max (@() 1, obj), "@bug59617/max")
%! assert (max (obj, @() 2), "@bug59617/max")
%! assert (max (obj, obj), "@bug59617/max")
