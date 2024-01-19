%!function retval = bug65153_flipud (x)
%!  global bug65153_global_isargout
%!  bug65153_global_isargout = isargout (1);
%!  retval = flipud (x);
%!endfunction

## Bug 65153 revealed two problems related to the way Octave was
## attempting to handle ignored function outputs.  One is that an
## ignored output can propagate from one function to another in some
## contexts.  For example, evaluating
##
##   [~, y] = bug65153_1 ()
##
## results in isargout(1) -> false in the call to bug65153_flipud.
## The second problem is that even though the return value is always
## set in bug65153_flipud, Octave was using the equivalent of the
## isargout information internally and not assigning the return value
## internally, so the matrix construction failed.  The second problem
## is easier to solve than the first.  We test for each problem
## separately here.

%!function [x, y] = bug65153_1 ()
%!  n = 10;
%!  x = (1:n)';
%!  y = (1:n)';
%!  [(1:n)', bug65153_flipud((1:n)')];
%!endfunction

## The bug65153_2 function tests the same problem as bug65153_1 but for
## a different context.  ANS should be assigned after the call to
## bug65153_flipud but it was not because of Octave's incorrect internal
## handling of ignored outputs.

%!function x = bug65153_2 ()
%!  global bug65153_global
%!  bug65153_flipud ((1:10)');
%!  x = ans;
%!  bug65153_global = x;
%!endfunction

%!test <65153>
%! global bug65153_global bug65153_global_isargout
%! unwind_protect
%!   [~, y] = bug65153_1 ();
%!   assert (y, (1:10)');
%! unwind_protect_cleanup
%!   clear -global bug65153_global bug65153_global_isargout
%! end_unwind_protect

%!test <65153>
%! global bug65153_global bug65153_global_isargout
%! unwind_protect
%!   [~, y] = bug65153_1 ();
%!   assert (bug65153_global_isargout, true);
%! unwind_protect_cleanup
%!   clear -global bug65153_global bug65153_global_isargout
%! end_unwind_protect

%!test <65153>
%! global bug65153_global bug65153_global_isargout
%! unwind_protect
%!   [~] = bug65153_2 ();
%!   assert (bug65153_global, flipud ((1:10)'));
%! unwind_protect_cleanup
%!   clear -global bug65153_global bug65153_global_isargout
%! end_unwind_protect

%!test <65153>
%! global bug65153_global bug65153_global_isargout
%! unwind_protect
%!   [~] = bug65153_2 ();
%!   assert (bug65153_global_isargout, true);
%! unwind_protect_cleanup
%!   clear -global bug65153_global bug65153_global_isargout
%! end_unwind_protect
