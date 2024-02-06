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

classdef cdef_help2
  # -*- texinfo -*-
  # class cdef_help2 : class help text BELOW classdef keyword.
  #
  # Type 'help cdef_help2'

  properties
    # prop1: property help text ABOVE property name.
    # Type "help cdef_help2.prop1"
    prop1   # prop1: EOL-comment text.  Should not be displayed
    prop2   # prop2: property help text in EOL-comment for property PROP2.  Type "help cdef_help2.prop2"
  end

  methods

    # cdef_help2: constructor help text ABOVE function keyword
    # Type 'help cdef_help2.cdef_help2'.
    # This should be shown because Octave comment character '#' is used.
    function obj = cdef_help2 (p1, p2)
      # cdef_help2: constructor help text BELOW function keyword
      # Type 'help cdef_help2.cdef_help2'.
      if (nargin ~= 2)
        obj.prop1 = 'default';
        obj.prop2 = 42;
      else
        obj.prop1 = p1;
        obj.prop2 = p2;
      end
    end

    # meth1: method help text ABOVE function
    # 
    # Type 'help cdef_help2.meth1'.
    function obj2 = meth1 (obj, n)
      obj2 = n + obj;
    end
    
    function obj2 = meth2 (obj, n)

      # meth2: method help text BELOW function
      # The blank line between function and comment is intentional.
      # Type 'help cdef_help2.meth2'.
      obj2 = n - obj;
    end

    # meth3: method help text ABOVE function
    # Type 'help cdef_help2.meth3'.
    # This should be shown because Octave comment character '#' is used.
    function obj3 = meth3 (obj, n)
      # meth3: method help text BELOW function
      # Type 'help cdef_help2.meth3'.
      # This should NOT be displayed.
      obj3 = obj + n;
    end

    function obj4 = meth4 (obj, n)

      obj4 = n - obj;
      if (n)
        # meth4: pure comment text.  This should *never* be displayed.
        # Type 'help cdef_help2.meth4'.
      end
    end

  end

end
