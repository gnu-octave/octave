########################################################################
##
## Copyright (C) 2023-2024 The Octave Project Developers
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

%%
%% Testing nargout when calling subsref for classdef classes
%%

%!test <*63841>
%! cm = containers.Map;
%! cm('first') = cls_b63841;
%! a_63841 = 0;
%! try
%!   if cm('first').a_property
%!     a_63841 = 1;
%!   endif
%! catch
%! end_try_catch
%! assert (a_63841, 1);

%!test <*63841>
%! ti = cls_b63841;
%! a = 0;
%! if ti(1:5).a_property
%!   a = 1;
%! endif
%! assert (a, 1);

%!test <*63841>
%! ti = cls_b63841;
%! a = 0;
%! for x = ti(1:5).a_property
%!   a = 1;
%! endfor
%! assert (a, 1);

%!test <*63841>
%! ti = cls_b63841;
%! [n_arg, b] = ti.call_a_method;
%! assert (n_arg, 2);

%!test <*63841>
%! ti = cls_b63841;
%! [n_arg, b] = ti{1};
%! assert (n_arg, 2);

%!test <*63841>
%! ti = cls_b63841;
%! [n_arg, b] = ti(1);
%! assert (n_arg, 2);

%!test <*63841>
%! ti = cls_b63841;
%! [n_arg, b] = ti(1).a_property;
%! assert (n_arg, 2);

%!test <*63841>
%! ti = cls_b63841;
%! [n_arg, b, c] = ti.call_a_method;
%! assert (n_arg, 3);

%!test <*63841>
%! ti = cls_b63841;
%! [n_arg, b, c] = ti{1};
%! assert (n_arg, 3);

%!test <*63841>
%! ti = cls_b63841;
%! [n_arg, b, c] = ti(1);
%! assert (n_arg, 3);

%%
%% Testing nargout when calling subsref for struct-based classes
%%

%!test <*63841>
%! ti = cls2_b63841;
%! a = 0;
%! if ti(1:5).a_property
%!   a = 1;
%! endif
%! assert (a, 1);

%!test <*63841>
%! ti = cls2_b63841;
%! a = 0;
%! for x = ti(1:5).a_property
%!   a = 1;
%! endfor
%! assert (a, 1);

%!test <*63841>
%! ti = cls2_b63841;
%! [n_arg, b] = ti.call_a_method;
%! assert (n_arg, 2);

%!test <*63841>
%! ti = cls2_b63841;
%! [n_arg, b] = ti{1};
%! assert (n_arg, 2);

%!test <*63841>
%! ti = cls2_b63841;
%! [n_arg, b] = ti(1);
%! assert (n_arg, 2);

%!test <*63841>
%! ti = cls2_b63841;
%! [n_arg, b] = ti(1).a_property;
%! assert (n_arg, 2);

%%
%% Different expressions that should produce errors (classdef and struct-based classes)
%%

%!shared ti, ti2
%! ti = cls_b63841;
%! ti2 = cls2_b63841;

%% binary operator
%!error <binary operator \'\+\' not implemented for \'scalar\' by \'cs-list\' operations> 1 + ti(1:3).a_property
%!error <binary operator \'\+\' not implemented for \'cs-list\' by \'scalar\' operations> ti(1:3).a_property + 1
%!error <binary operator \'\+\' not implemented for \'cs-list\' by \'cs-list\' operations> ti(1:3).a_property + ti(1:3).a_property
%!error <binary operator \'\+\' not implemented for \'scalar\' by \'cs-list\' operations> 1 + ti2(1:3).a_property
%!error <binary operator \'\+\' not implemented for \'cs-list\' by \'scalar\' operations> ti2(1:3).a_property + 1
%!error <binary operator \'\+\' not implemented for \'cs-list\' by \'cs-list\' operations> ti2(1:3).a_property + ti2(1:3).a_property

%% unary postfix operator
%!error <unary operator \'\'\' not implemented for \'cs-list\' operands> ti(1:3).a_property'
%!error <unary operator \'\'\' not implemented for \'cs-list\' operands> ti2(1:3).a_property'

%% unary prefix operator
%!error <unary operator \'!\' not implemented for \'cs-list\' operands> ~ti(1:3).a_property
%!error <unary operator \'!\' not implemented for \'cs-list\' operands> ~ti2(1:3).a_property

%% compound binary operation
%!error <unary operator \'\'\' not implemented for \'cs-list\' operands> 3*ti(1:3).a_property'
%!error <unary operator \'\'\' not implemented for \'cs-list\' operands> ti(1:3).a_property'*3
%!error <unary operator \'\'\' not implemented for \'cs-list\' operands> 3*ti2(1:3).a_property'
%!error <unary operator \'\'\' not implemented for \'cs-list\' operands> ti2(1:3).a_property'*3
