## Copyright (C) 2007-2012 David Bateman
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
## @deftypefn  {Function File} {} fill (@var{x}, @var{y}, @var{c})
## @deftypefnx {Function File} {} fill (@var{x1}, @var{y1}, @var{c1}, @var{x2}, @var{y2}, @var{c2})
## @deftypefnx {Function File} {} fill (@dots{}, @var{prop}, @var{val})
## @deftypefnx {Function File} {} fill (@var{h}, @dots{})
## @deftypefnx {Function File} {@var{h} =} fill (@dots{})
## Create one or more filled patch objects.
##
## The optional return value @var{h} is an array of graphics handles to
## the created patch objects.
## @seealso{patch}
## @end deftypefn

function retval = fill (varargin)

  [h, varargin] = __plt_get_axis_arg__ ("fill", varargin{:});

  htmp = [];
  iargs = __find_patches__ (varargin{:});

  oldh = gca ();
  unwind_protect
    axes (h);

    nextplot = get (h, "nextplot");
    for i = 1 : length (iargs)
      if (i > 1 && strncmp (nextplot, "replace", 7))
        set (h, "nextplot", "add");
      endif
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
    if (strncmp (nextplot, "replace", 7))
      set (h, "nextplot", nextplot);
    endif
  unwind_protect_cleanup
    axes (oldh);
  end_unwind_protect

  if (nargout > 0)
    retval = htmp;
  endif

endfunction

function iargs = __find_patches__ (varargin)
  iargs = [];
  i = 1;
  while (i < nargin)
    iargs (end + 1) = i;
    if (ischar (varargin{i})
        && (strcmpi (varargin{i}, "faces")
            || strcmpi (varargin{i}, "vertices")))
      i += 4;
    elseif (isnumeric (varargin{i}))
      i += 2;
    endif

    if (i <= nargin)
      while (true);
        if (ischar (varargin{i})
            && (strcmpi (varargin{i}, "faces")
                || strcmpi (varargin{i}, "vertices")))
          break;
        elseif (isnumeric (varargin{i}))
          ## Assume its the colorspec
          i++;
          break;
        elseif (ischar (varargin{i}))
          colspec = tolower (varargin{i});
          collen = length (colspec);

          if (strncmp (colspec, "blue", collen)
              || strncmp (colspec, "black", collen)
              || strncmp (colspec, "k", collen)
              || strncmp (colspec, "black", collen)
              || strncmp (colspec, "red", collen)
              || strncmp (colspec, "green", collen)
              || strncmp (colspec, "yellow", collen)
              || strncmp (colspec, "magenta", collen)
              || strncmp (colspec, "cyan", collen)
              || strncmp (colspec, "white", collen))
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
%! clf
%! t1 = (1/16:1/8:1)*2*pi;
%! t2 = ((1/16:1/8:1) + 1/32)*2*pi;
%! x1 = sin (t1) - 0.8;
%! y1 = cos (t1);
%! x2 = sin (t2) + 0.8;
%! y2 = cos (t2);
%! h = fill (x1,y1,'r', x2,y2,'g');

