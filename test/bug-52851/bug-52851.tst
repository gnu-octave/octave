%!test <*52851>
%! script1
%! assert (r11, 1);
%! assert (r21, 2);
%! assert (r22, 2);

%!test <*52851>
%! script2
%! assert (r1, 1);
%! assert (r2, 2);

%!test <*52851>
%! flag = true;
%! script3
%! assert (r, 1);
%! flag = false;
%! script3
%! assert (r, 2);

%!test <*52851>
%! script4
%! assert (r1, 1);
%! assert (r2, 2);

