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
%!   s = help ('cdef_help1.meth2');
%!   assert (regexp (s, 'meth2: method help text BELOW function'));
%!
%!   s = help ('cdef_help1.meth3');
%!   assert (regexp (s, 'meth3: method help text BELOW function'));
%!
%!   s = '';
%!   try
%!     s = help ('cdef_help1.meth4');
%!   catch
%!     assert (regexp (lasterr (), "'cdef_help1.meth4' is not documented"));
%!   end_try_catch
%!   if (! isempty (s))
%!     error ("Impossible state: Help text found for 'cdef_help1.meth4'");
%!   endif
%!
%!   ## Check documentation for entire class
%!   s = help ('cdef_help1');
%!   assert (regexp (s, 'class cdef_help1 : class help text ABOVE classdef'));
%!   
%!   ## Check documentation for constructor
%!   s = help ('cdef_help1.cdef_help1');
%!   assert (regexp (s, 'cdef_help1: constructor help text BELOW function'));
%!   
%!   ## Check documentation for properties
%!   s = help ('cdef_help1.prop1');
%!   assert (regexp (s, 'prop1: property help text ABOVE property'));
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
%!
%!   s = help ('cdef_help2.meth2');
%!   assert (regexp (s, 'meth2: method help text BELOW function'));
%!
%!   s = help ('cdef_help2.meth3');
%!   assert (regexp (s, 'meth3: method help text ABOVE function'));
%!
%!   s = '';
%!   try
%!     s = help ('cdef_help2.meth4');
%!   catch
%!     assert (regexp (lasterr (), "'cdef_help2.meth4' is not documented"));
%!   end_try_catch
%!   if (! isempty (s))
%!     error ("Impossible state: Help text found for 'cdef_help2.meth4'");
%!   endif
%!
%!   ## Check documentation for entire class
%!   s = help ('cdef_help2');
%!   assert (regexp (s, 'class cdef_help2 : class help text BELOW classdef'));
%!   
%!   ## Check documentation for constructor
%!   s = help ('cdef_help2.cdef_help2');
%!   assert (regexp (s, 'cdef_help2: constructor help text ABOVE function'));
%!   
%!   ## Check documentation for properties
%!   s = help ('cdef_help2.prop1');
%!   assert (regexp (s, 'prop1: property help text ABOVE property'));
%!
%!   s = help ('cdef_help2.prop2');
%!   assert (regexp (s, 'prop2: property help text in EOL-comment'));
%!
%! unwind_protect_cleanup
%!   rmpath ('cdefdir');
%! end_unwind_protect
