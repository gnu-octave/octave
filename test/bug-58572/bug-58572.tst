%!test <*58572>
%! obj = bug58572 ();
%! assert (use_num (obj), false);
%! assert (obj.use_num (), false);
%! assert (isnumeric (obj), true);
%! assert (obj.isnumeric (), true);
