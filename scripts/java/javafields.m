## Copyright (C) 2007 Michael Goffioul
##
## This program is free software; you can redistribute it and/or modify
## it under the terms of the GNU General Public License as published by
## the Free Software Foundation; either version 2 of the License, or
## (at your option) any later version.
##
## This program is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with this program; If not, see <http://www.gnu.org/licenses/>.

## -*- texinfo -*-
## @deftypefn {Function file} {@var{P} =} javafields (@var{class})
##
## Returns the fields of a Java object in the form of a cell 
## array of strings. If no output variable is
## given, the result is simply printed on the standard output.
##
## @end deftypefn
## @seealso{javamethods}

function varargout = javafields(classname)
  
  if (nargin != 1)
     print_usage ();
  else
     c_methods = java_invoke ("org.octave.ClassHelper", "getFields",classname);
     method_list = strsplit (c_methods, ';');

     switch nargout
     case 0
       if (! isempty (method_list))
         disp(method_list);
       endif
     case 1
       varargout{1} = cellstr (method_list);
     endswitch
  endif

endfunction
