########################################################################
##
## Copyright (C) 2024 The Octave Project Developers
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

%!test <*65220>
%! unwind_protect
%!   addpath ('cdefdir');
%!
%!   ## Check methods first (bug was disguised if class checked first)
%!   s = help ('cdef_help1.meth1');
%!   assert (regexp (s, 'meth1: method help text ABOVE function'));
%!
%! unwind_protect_cleanup
%!   rmpath ('cdefdir');
%! end_unwind_protect

%!test <*65220>
%! unwind_protect
%!   addpath ('cdefdir');
%!
%!   s = help ('cdef_help1.meth2');
%!   assert (regexp (s, 'meth2: method help text BELOW function'));
%!
%! unwind_protect_cleanup
%!   rmpath ('cdefdir');
%! end_unwind_protect

%!test <*65220>
%! unwind_protect
%!   addpath ('cdefdir');
%!
%!   s = help ('cdef_help1.meth3');
%!   assert (regexp (s, 'meth3: method help text BELOW function'));
%!
%! unwind_protect_cleanup
%!   rmpath ('cdefdir');
%! end_unwind_protect

%!test <*65258>
%! unwind_protect
%!   addpath ('cdefdir');
%!
%!   s = help ('cdef_help1.meth4');
%!   assert (regexp (s, 'undocumented method: obj4 = meth4 \(obj, n\)'));
%!
%! unwind_protect_cleanup
%!   rmpath ('cdefdir');
%! end_unwind_protect

%!test <*65220>
%! unwind_protect
%!   addpath ('cdefdir');
%!
%!   ## Check documentation for entire class
%!   s = help ('cdef_help1');
%!   assert (regexp (s, 'class cdef_help1 : class help text ABOVE classdef'));
%!
%! unwind_protect_cleanup
%!   rmpath ('cdefdir');
%! end_unwind_protect

%!test <*65220>
%! unwind_protect
%!   addpath ('cdefdir');
%!
%!   ## Check documentation for constructor
%!   s = help ('cdef_help1.cdef_help1');
%!   assert (regexp (s, 'cdef_help1: constructor help text BELOW function'));
%!
%! unwind_protect_cleanup
%!   rmpath ('cdefdir');
%! end_unwind_protect

%!test <*65220>
%! unwind_protect
%!   addpath ('cdefdir');
%!
%!   ## Check documentation for properties
%!   s = help ('cdef_help1.prop1');
%!   assert (regexp (s, 'prop1: property help text ABOVE property'));
%!
%! unwind_protect_cleanup
%!   rmpath ('cdefdir');
%! end_unwind_protect

%!test <*65220>
%! unwind_protect
%!   addpath ('cdefdir');
%!
%!   s = help ('cdef_help1.prop2');
%!   assert (regexp (s, 'prop2: property help text in EOL-comment'));
%!
%! unwind_protect_cleanup
%!   rmpath ('cdefdir');
%! end_unwind_protect

%!test <*65220>
%! unwind_protect
%!   addpath ('cdefdir');
%!
%!   ## Check methods first (bug was disguised if class checked first)
%!   s = help ('cdef_help2.meth1');
%!   assert (regexp (s, 'meth1: method help text ABOVE function'));
%! unwind_protect_cleanup
%!   rmpath ('cdefdir');
%! end_unwind_protect

%!
%!test <*65220>
%! unwind_protect
%!   addpath ('cdefdir');
%!
%!   s = help ('cdef_help2.meth2');
%!   assert (regexp (s, 'meth2: method help text BELOW function'));
%!
%! unwind_protect_cleanup
%!   rmpath ('cdefdir');
%! end_unwind_protect

%!test <*65220>
%! unwind_protect
%!   addpath ('cdefdir');
%!
%!   s = help ('cdef_help2.meth3');
%!   assert (regexp (s, 'meth3: method help text ABOVE function'));
%!
%! unwind_protect_cleanup
%!   rmpath ('cdefdir');
%! end_unwind_protect

%!test <*65220>
%! unwind_protect
%!   addpath ('cdefdir');
%!
%!   s = help ('cdef_help2.meth4');
%!   assert (regexp (s, 'undocumented method: obj4 = meth4 \(obj, n\)'));
%!
%! unwind_protect_cleanup
%!   rmpath ('cdefdir');
%! end_unwind_protect

%!test <*65220>
%! unwind_protect
%!   addpath ('cdefdir');
%!
%!   ## Check documentation for entire class
%!   s = help ('cdef_help2');
%!   assert (regexp (s, 'class cdef_help2 : class help text BELOW classdef'));
%!
%! unwind_protect_cleanup
%!   rmpath ('cdefdir');
%! end_unwind_protect

%!test <*65220>
%! unwind_protect
%!   addpath ('cdefdir');
%!
%!   ## Check documentation for constructor
%!   s = help ('cdef_help2.cdef_help2');
%!   assert (regexp (s, 'cdef_help2: constructor help text ABOVE function'));
%!
%! unwind_protect_cleanup
%!   rmpath ('cdefdir');
%! end_unwind_protect

%!test <*65220>
%! unwind_protect
%!   addpath ('cdefdir');
%!
%!   ## Check documentation for properties
%!   s = help ('cdef_help2.prop1');
%!   assert (regexp (s, 'prop1: property help text ABOVE property'));
%!
%! unwind_protect_cleanup
%!   rmpath ('cdefdir');
%! end_unwind_protect

%!test <*65220>
%! unwind_protect
%!   addpath ('cdefdir');
%!
%!   s = help ('cdef_help2.prop2');
%!   assert (regexp (s, 'prop2: property help text in EOL-comment'));
%!
%! unwind_protect_cleanup
%!   rmpath ('cdefdir');
%! end_unwind_protect

%!test <*65258>
%! unwind_protect
%!   addpath ('cdefdir');
%!
%!   s = help ('cdef_help3');
%!   assert (regexp (s, 'undocumented constructor: obj = cdef_help3 \(p1, p2\)'));
%!
%! unwind_protect_cleanup
%!   rmpath ('cdefdir');
%! end_unwind_protect

%!test <*65258>
%! unwind_protect
%!   addpath ('cdefdir');
%!
%!   s = help ('cdef_help3.cdef_help3');
%!   assert (regexp (s, 'undocumented constructor: obj = cdef_help3 \(p1, p2\)'));
%!
%! unwind_protect_cleanup
%!   rmpath ('cdefdir');
%! end_unwind_protect

%!test <*65258>
%! unwind_protect
%!   addpath ('cdefdir');
%!
%!   s = help ('cdef_help4');
%!   assert (regexp (s, 'default constructor: obj = cdef_help4 \(\)'));
%!
%! unwind_protect_cleanup
%!   rmpath ('cdefdir');
%! end_unwind_protect

%!test <*65258>
%! unwind_protect
%!   addpath ('cdefdir');
%!
%!   s = help ('cdef_help4.cdef_help4');
%!   assert (regexp (s, 'default constructor: obj = cdef_help4 \(\)'));
%!
%! unwind_protect_cleanup
%!   rmpath ('cdefdir');
%! end_unwind_protect

%!assert <*65258> (regexp (help ('undoc_fcn'), 'undocumented function: \[x, y, z\] = undoc_fcn \(a, b, ~, c = 3\)'))
