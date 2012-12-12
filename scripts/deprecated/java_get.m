## Copyright (C) 1995-2012 Kurt Hornik
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
## @deftypefn {Loadable Function} {@var{val} =} java_get (@var{obj}, @var{name})
## Get the value of the field @var{name} of the Java object @var{obj}.  For
## static fields, @var{obj} can be a string representing the fully qualified
## name of the corresponding class.
## 
## When @var{obj} is a regular Java object, structure-like indexing can be
## used as a shortcut syntax.  For instance, the two following statements are
## equivalent
## 
## @example
## @group
##   java_get (x, "field1")
##   x.field1
## @end group
## @end example
## 
## @seealso{java_set, java_invoke, javaObject}
## @end deftypefn

function retval = java_get (obj, name)

  persistent warned = false;
  if (! warned)
    warned = true;
    warning ("Octave:deprecated-function",
             "java_get is obsolete and will be removed from a future version of Octave; use structure-like indexing instead");
  endif

  if (nargin != 2)
    print_usage ();
  endif

  if (isjava (obj))
    retval = obj.(name);
  elseif (ischar (obj))
    ## FIXME: Need a solution for getting static fields of class
    ##        which does not depend on __java_get__ which will be removed.
    retval = __java_get__ (obj, name);
  else
    error ("java_get: OBJ must be a Java object");
  endif

endfunction

