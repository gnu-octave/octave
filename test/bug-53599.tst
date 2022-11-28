%!function rval = sggval (val)
%!  global gval;
%!  if (nargin == 1)
%!    gval = val;
%!  else
%!    rval = gval;
%!  endif
%!endfunction

%!test <*53599>
%! global gval;
%! assert (isempty (gval))
%! sggval (13);
%! assert (sggval (), 13);
%! assert (gval, 13);
%! clear global gval
%! assert (sggval (), [])
%! gval = 42;
%! assert (sggval (), []);
%! clear gval
%! global gval;
%! gval = 42;
%! assert (sggval (), 42);
%! clear -global gval;  # cleanup after test
