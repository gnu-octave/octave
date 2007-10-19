## Copyright (C) 2007  David Bateman
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
## @deftypefn {Function File} {} fill (@var{x}, @var{y}, @var{c})
## @deftypefnx {Function File} {} fill (@var{x1}, @var{y1}, @var{c1}, @var{x2}, @var{y2}, @var{c2})
## @deftypefnx {Function File} {} fill (@dots{}, @var{prop}, @var{val})
## @deftypefnx {Function File} {} fill (@var{h}, @dots{})
## @deftypefnx {Function File} {@var{h} = } fill (@dots{})
## Create one or more filled patch objects, returning a patch object for each.
## @end deftypefn

function h = fill (varargin)

  htmp = [];

  if (isscalar (varargin{1}) && ishandle (varargin{1}))
    h = varargin {1};
    if (! strcmp (get (h, "type"), "axes"))
      error ("fill: expecting first argument to be an axes object");
    endif

    iargs = __find_patches__ (varargin{:}) + 1;
    oldh = gca ();
    unwind_protect
      axes (h);

      for i = 1 : length (iargs)
	if (i == length (iargs))
	  args = varargin (iargs(i):end);
        else
	  args = varargin (iargs(i):iargs(i+1)-1);
	endif
	newplot ();
	[tmp, fail] = __patch__ (h, args{:});
	if (fail)
	  print_usage();
	endif
	htmp (end + 1) = tmp;
      endfor
    unwind_protect_cleanup
      axes (oldh);
    end_unwind_protect
  else
    iargs = __find_patches__ (varargin{:});
    for i = 1 : length (iargs)
      if (i == length (iargs))
	args = varargin (iargs(i):end);
      else
        args = varargin (iargs(i):iargs(i+1)-1);
      endif
      newplot ();
      [tmp, fail] = __patch__ (gca (), args{:});
      if (fail)
	print_usage();
      endif
      htmp (end + 1) = tmp;
    endfor
  endif
  if (nargout > 0)
    h = htmp;
  endif
endfunction

function iargs = __find_patches__ (varargin)
  iargs = [];
  i = 1;
  while (i < nargin)
    iargs (end + 1) = i;
    if (ischar (varargin {i}) && (strcmp (tolower (varargin{i}), "faces") || 
				  strcmp (tolower (varargin{i}), "vertices")))
      i += 4;
    elseif (isnumeric (varargin {i}))
      i += 2;
    endif

    if (i <= nargin)
      while (true);
	if (ischar (varargin {i}) && 
	    (strcmp (tolower (varargin{i}), "faces") || 
	     strcmp (tolower (varargin{i}), "vertices")))
	  break;
	elseif (isnumeric (varargin {i}))
	  ## Assume its the colorspec
	  i++;
	  break;
	elseif (ischar (varargin {i}))
	  colspec = tolower (varargin {i});
	  collen = length (colspec);

	  if (strncmp (colspec, "blue", collen) ||
	      strncmp (colspec, "black", collen) ||
	      strncmp (colspec, "k", collen) ||
	      strncmp (colspec, "black", collen) ||
	      strncmp (colspec, "red", collen) ||
	      strncmp (colspec, "green", collen) ||
	      strncmp (colspec, "yellow", collen) ||
	      strncmp (colspec, "magenta", collen) ||
	      strncmp (colspec, "cyan", collen) ||
	      strncmp (colspec, "white", collen))
	    i++;
	    break;
	  endif
	else
	  i += 2;
	endif
      endwhile
    endif
  endwhile
endfunction

%!demo
%! close all;
%! t1 = (1/16:1/8:1)'*2*pi;
%! t2 = ((1/16:1/8:1)' + 1/32)*2*pi;
%! x1 = sin(t1) - 0.8;
%! y1 = cos(t1);
%! x2 = sin(t2) + 0.8;
%! y2 = cos(t2);
%! h = fill(x1,y1,'r',x2,y2,'g')
