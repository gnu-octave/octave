## Copyright (C) 2007 Michael Goffioul
##
## This file is part of Octave.
##
## Octave is free software; you can redistribute it and/or modify it
## under the terms of the GNU General Public License as published by
## the Free Software Foundation; either version 3 of the License, or (at
## your option) any later version.
##
## Octave is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
## General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with Octave; see the file COPYING.  If not, see
## <http://www.gnu.org/licenses/>.

## -*- texinfo -*-
## @deftypefn {Function file} {@var{a} =} javaArray (@var{class}, @var{sz})
## @deftypefnx {Function file} {@var{a} =} javaArray (@var{class}, @var{m}, @var{n}, @dots{})
##
## Creates a Java array of size @var{sz} with elements of class
## @var{class}.  @var{class} may be a Java object representing a class
## or a string containing the fully qualified class name.  The size of
## the object may also be specified with individual integer arguments
## @var{m}, @var{n}, etc.
##
## The generated array is uninitialized, all elements are set to null
## if @var{class} is a reference type, or to a default value (usually 0)
## if @var{class} is a primitive type.
##
## @example
## a = javaArray ("java.lang.String", 2, 2);
## a(1,1) = "Hello";
## @end example
## @end deftypefn

function retval = javaArray (class_name, varargin)

  switch length (varargin)
    case 0
      error ("missing array size");

    case 1
      dims = varargin{1};

    otherwise
      dims = [varargin{:}];
  endswitch

  retval = java_invoke ("org.octave.ClassHelper", "createArray",
                        class_name, dims);

endfunction
