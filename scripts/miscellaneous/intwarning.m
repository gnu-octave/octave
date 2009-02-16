## Copyright (C) 2008  David Bateman
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
## @deftypefn {Function File} {} intwarning (@var{action})
## @deftypefnx {Function File} {} intwarning (@var{s})
## @deftypefnx {Function File} {@var{s} =} intwarning (@dots{})
## Control the state of the warning for integer conversions and math
## operations.
##
## @table @asis
## @item "query"
## The state of the Octave integer conversion and math warnings is
## queried. If there is no output argument, then the state is printed.
## Otherwise it is returned in a structure with the fields "identifier"
## and "state".
##
## @smallexample
## @group
## intwarning ("query")
## The state of warning "Octave:int-convert-nan" is "off"
## The state of warning "Octave:int-convert-non-int-val" is "off"
## The state of warning "Octave:int-convert-overflow" is "off"
## The state of warning "Octave:int-math-overflow" is "off"
## @end group
## @end smallexample 
##
## @item "on"
## Turn integer conversion and math warnings "on". If there is no output
## argument, then nothing is printed. Otherwise the original state of
## the state of the integer conversion and math warnings is returned in
## a structure array.
##
## @item "off"
## Turn integer conversion and math warnings "on". If there is no output
## argument, then nothing is printed. Otherwise the original state of
## the state of the integer conversion and math warnings is returned in
## a structure array.
## @end table
##
## The original state of the integer warnings can be restored by passing
## the structure array returned by @code{intwarning} to a later call to
## @code{intwarning}. For example
##
## @example
## s = intwarning ("off");
## @dots{}
## intwarning (s);
## @end example
## @seealso{warning}
## @end deftypefn

function y = intwarning (x)

  if (nargin != 1)
    print_usage ();
  else
    if (nargout > 0)
      y = warning("query", "Octave:int-convert-nan");
      y = [y; warning("query", "Octave:int-convert-non-int-val")];
      y = [y; warning("query", "Octave:int-convert-overflow")];
      y = [y; warning("query", "Octave:int-math-overflow")];
    endif
    if (ischar (x))
      if (strcmpi (x, "query"))
	if (nargout == 0)
	  __print_int_warn_state__ ("Octave:int-convert-nan");
	  __print_int_warn_state__ ("Octave:int-convert-non-int-val");
	  __print_int_warn_state__ ("Octave:int-convert-overflow");
	  __print_int_warn_state__ ("Octave:int-math-overflow");
	  printf("\n");
	endif
      elseif (strcmpi (x, "on"))
	warning ("on", "Octave:int-convert-nan");
	warning ("on", "Octave:int-convert-non-int-val");
	warning ("on", "Octave:int-convert-overflow");
	warning ("on", "Octave:int-math-overflow");
      elseif (strcmpi (x, "off"))
	warning ("off", "Octave:int-convert-nan");
	warning ("off", "Octave:int-convert-non-int-val");
	warning ("off", "Octave:int-convert-overflow");    
	warning ("off", "Octave:int-math-overflow");    
      else
	error ("intwarning: unrecognized argument");
      endif
    elseif (isstruct(x))
      for fld = fieldnames (x)
	if (strcmp ("Octave:int-convert-nan") || 
	    strcmp ("Octave:int-convert-non-int-val") || 
	    strcmp ("Octave:int-convert-overflow") ||
	    strcmp ("Octave:int-cmath-overflow"))
	  s = getfield (x, fld);
	  if (! ischar (s) || !(strcmpi("s","on") || strcmpi("s","off")))
	    error ("intwarning: unexpected warning state");
	  endif
	  warning (s, fld);
	else
	  error ("intwarning: unrecognized integer warning %s", fld);
	endif
      endfor
    else
      error ("intwarning: unexpected input");
    endif
  endif
endfunction

function __print_int_warn_state__ (s)
  fprintf ("The state of warning \"%s\" is \"%s\"\n", 
	   s, warning ("query", s).state);
endfunction
