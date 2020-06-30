%!test
%! x = colon_op ();
%! assert (x:2:3, "colon_op:double:double");
%! assert (1:x:3, "double:colon_op:double");
%! assert (1:2:x, "double:double:colon_op");
%! assert (x:x:3, "colon_op:colon_op:double");
%! assert (1:x:x, "double:colon_op:colon_op");
%! assert (x:2:x, "colon_op:double:colon_op");
%! assert (x:x:x, "colon_op:colon_op:colon_op");
%! assert (x:2, "colon_op:double");
%! assert (1:x, "double:colon_op");
%! assert (x:x, "colon_op:colon_op");

%!test
%! x = legacy_colon_op ();
%! assert (x:2:3, "legacy_colon_op:double:double");
%! assert (1:x:3, "double:legacy_colon_op:double");
%! assert (1:2:x, "double:double:legacy_colon_op");
%! assert (x:x:3, "legacy_colon_op:legacy_colon_op:double");
%! assert (1:x:x, "double:legacy_colon_op:legacy_colon_op");
%! assert (x:2:x, "legacy_colon_op:double:legacy_colon_op");
%! assert (x:x:x, "legacy_colon_op:legacy_colon_op:legacy_colon_op");
%! assert (x:2, "legacy_colon_op:double");
%! assert (1:x, "double:legacy_colon_op");
%! assert (x:x, "legacy_colon_op:legacy_colon_op");
