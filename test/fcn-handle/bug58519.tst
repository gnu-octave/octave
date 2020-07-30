%!test <*58519>
%! fieldname = "a";
%! structure = struct (fieldname, 42);
%! anonfunc = @ () structure.(fieldname);
%! assert (anonfunc (), 42)
