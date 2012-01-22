## Copyright (C) 2005-2012 John W. Eaton
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
## @deftypefn  {Function File} {} clf ()
## @deftypefnx {Function File} {} clf ("reset")
## @deftypefnx {Function File} {} clf (@var{hfig})
## @deftypefnx {Function File} {} clf (@var{hfig}, "reset")
## @deftypefnx {Function File} {@var{h} =} clf (@dots{})
## Clear the current figure window.  @code{clf} operates by deleting child
## graphics objects with visible handles (@code{handlevisibility} = on).
## If @var{hfig} is specified operate on it instead of the current figure.
## If the optional argument @code{"reset"} is specified, all objects including
## those with hidden handles are deleted.
## 
## The optional return value @var{h} is the graphics handle of the figure
## window that was cleared.
## @seealso{cla, close, delete}
## @end deftypefn

## Author: jwe

function retval = clf (varargin)

  if (nargin > 2)
    print_usage ();
  elseif (nargin > 1)
    if (isfigure (varargin{1}) && ischar (varargin{2})
        && strcmpi (varargin{2}, "reset"))
      oldfig = gcf;
      hfig = varargin{1};
      do_reset = true;
    else
      print_usage ();
    endif
  elseif (nargin == 1)
    if (isfigure (varargin{1}))
      oldfig = gcf;
      hfig = varargin{1};
      do_reset = false;
    elseif (ischar (varargin{1}) && strcmpi (varargin{1}, "reset"))
      hfig = gcf;
      oldfig = hfig;
      do_reset = true;
    else
      print_usage ();
    endif
  else
    hfig = gcf;
    oldfig = hfig;
    do_reset = false;
  endif

  if (do_reset)
    ## Select all the children, including the one with hidden handles.
    hc = allchild (hfig);
    reset (hfig);
  else
    ## Select only the chilren with visible handles.
    hc = get (hfig, "children");
  endif

  ## Delete the children.
  delete (hc);

  if (nargout > 0)
    retval = hfig;
  endif

endfunction


%!test
%! hf = figure ("visible", "off");
%! unwind_protect
%!   l = line;
%!   assert (!isempty (get (gcf, "children")));
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

%!test
%! hf = figure ("visible", "off");
%! unwind_protect
%!   clf;
%!   assert (isempty (get (gcf, "children")));
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

