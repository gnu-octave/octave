%!test <*55758>
%! global class_bug_55758_dtor_called;
%! class_bug_55758_dtor_called = false;
%!
%! assert (class_bug_55758 (5).value, 5);
%! assert (class_bug_55758_dtor_called);
%!
%! assert (class_bug_55758 (5)(1), 5);
%! assert (class_bug_55758_dtor_called);
%!
%! assert (size (class_bug_55758 (5)), [1, 1]);
%! assert (class_bug_55758_dtor_called);
%!
%! assert (numel (class_bug_55758 (5)), 1);
%! assert (class_bug_55758_dtor_called);
%! clear -global class_bug_55758_dtor_called;  # cleanup after test
