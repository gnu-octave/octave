########################################################################
##
## Copyright (C) 2019-2023 The Octave Project Developers
##
## See the file COPYRIGHT.md in the top-level directory of this
## distribution or <https://octave.org/copyright/>.
##
## This file is part of Octave.
##
## Octave is free software: you can redistribute it and/or modify it
## under the terms of the GNU General Public License as published by
## the Free Software Foundation, either version 3 of the License, or
## (at your option) any later version.
##
## Octave is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with Octave; see the file COPYING.  If not, see
## <https://www.gnu.org/licenses/>.
##
########################################################################

%!function [fhm, fhsm] = object_method_fcns ()
%!  obj = bug51709_c ();
%!  fhm = @obj.meth;
%!  fhsm = @obj.smeth;
%!endfunction

%!test <*51709>
%! [fhm, fhsm] = object_method_fcns ();
%!
%! out = fhm (42);
%! assert (out{1}, "meth");
%! tmp = out{2};
%! assert (numel (tmp), 2);
%! assert (isobject (tmp{1}));
%! assert (tmp{2}, 42);

%!test <*51709>
%! [fhm, fhsm] = object_method_fcns ();
%!
%! out = fhsm (42);
%! assert (out{1}, "smeth");
%! tmp = out{2};
%! assert (numel (tmp), 1);
%! assert (tmp{1}, 42);

%!test <*51709>
%! fhm = @obj.meth;
%!
%! obj = bug51709_c ();
%!
%! out = fhm (42);
%! assert (out{1}, "meth");
%! tmp = out{2};
%! assert (numel (tmp), 2);
%! assert (isobject (tmp{1}));
%! assert (tmp{2}, 42);

%!test <*51709>
%! fhsm = @obj.smeth;
%!
%! obj = bug51709_c ();
%!
%! out = fhsm (42);
%! assert (out{1}, "smeth");
%! tmp = out{2};
%! assert (numel (tmp), 1);
%! assert (tmp{1}, 42);
