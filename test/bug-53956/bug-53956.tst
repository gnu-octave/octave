%!test <*53956>
%! global dtor2_called dtor4_called;
%!
%! dtor2_called = dtor4_called = 0;
%! x = bug53956_class_3 ();
%! clear x
%! assert (dtor2_called, 1);
%!
%! dtor2_called = dtor4_called = 0;
%! x = bug53956_class_4 ();
%! clear x
%! assert (dtor2_called, 1);
%! assert (dtor4_called, 1);
%!
%! clear -global dtor2_called dtor4_called;  # cleanup after test
