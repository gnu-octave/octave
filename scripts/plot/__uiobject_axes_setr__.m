## Copyright (C) 2005 John W. Eaton
##
## This file is part of Octave.
##
## Octave is free software; you can redistribute it and/or modify it
## under the terms of the GNU General Public License as published by
## the Free Software Foundation; either version 2, or (at your option)
## any later version.
##
## Octave is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
## General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with Octave; see the file COPYING.  If not, write to the Free
## Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
## 02110-1301, USA.

## -*- texinfo -*-
## @deftypefn {Function File} {@var{s}} __uiobject_axes_setr__ (@var{p}, @var{v}, @dots{}))
## Set properties for axes objects.
## @end deftypefn

## Author: jwe

function obj = __uiobject_axes_setr__ (h, varargin)

  obj = get (h);

  if (rem (nargin-1, 2) == 0)
    if (isstruct (obj))
      for i = 1:2:nargin-1
	property = varargin{i};
	if (ischar (property))
	  key = tolower (property);
	  if (isfield (obj, key))
	    val = varargin{i+1};
	    if (isfield (obj, "__validators__"))
	      validators = obj.__validators__;
	      if (isfield (validators, key))
		feval (validators.(key), val);
	      endif
	    endif
	    switch (key)
	      case {"title", "xlabel", "ylabel", "zlabel"}
		val = __uiobject_text_ctor__ (h, "string", val);

	      case {"xlim", "ylim", "zlim"}
		obj.(strcat (key, "mode")) = "manual";

	      case "dataaspectratio"
		obj.dataaspectratiomode = "manual";

	      case {"xtick", "ytick", "ztick"}
		obj.(strcat (key, "mode")) = "manual";

	      case {"xticklabel", "yticklabel", "zticklabel"}
		obj.(strcat (key, "mode")) = "manual";

	    endswitch
	    obj.(key) = val;
	  else
	    warning ("set: unrecognized property `%s' for uiobject `%s'",
		     property, obj.type);
	  endif
	else
	  error ("set: expecting property name to be a character string");
	endif
      endfor
    else
      error ("__uiobject_axes_setr__: expecting axes object as first arg");
    endif
  else
    print_usage ();
  endif

endfunction
