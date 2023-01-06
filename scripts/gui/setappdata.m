########################################################################
##
## Copyright (C) 2010-2023 The Octave Project Developers
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

## -*- texinfo -*-
## @deftypefn  {} {} setappdata (@var{h}, @var{name}, @var{value})
## @deftypefnx {} {} setappdata (@var{h}, @var{name1}, @var{value1}, @var{name2}, @var{value3}, @dots{})
## @deftypefnx {} {} setappdata (@var{h}, @{@var{name1}, @var{name2}, @dots{}@}, @{@var{value1}, @var{value2}, @dots{}@})
## Set the application data @var{name} to @var{value} for the graphics object
## with handle @var{h}.
##
## @var{h} may also be a vector of graphics handles.  If the application data
## with the specified @var{name} does not exist, it is created.
##
## Multiple @var{name}/@var{value} pairs can be specified.  Alternatively, a
## cell array of @var{names} and a corresponding cell array of @var{values} can
## be specified.  For details on obtaining a list of valid application data
## properties, @pxref{XREFgetappdata,,@code{getappdata}}.
##
## @seealso{getappdata, isappdata, rmappdata, guidata, get, set, getpref,
## setpref}
## @end deftypefn

function setappdata (h, varargin)

  if (nargin < 3)
    print_usage ();
  endif

  h = h(:).';
  if (! all (ishghandle (h)))
    error ("setappdata: H must be a scalar or vector of graphic handles");
  elseif (mod (numel (varargin), 2) != 0)
    error ("setappdata: NAME/VALUE arguments must occur in pairs");
  endif

  if (iscellstr (varargin{1}))
    if (nargin != 3)
      error ("setappdata: only 3 arguments possible when NAME is a cellstr");
    elseif (! iscell (varargin{2}))
      varargin{2} = varargin(2);  # convert to cell
    endif
    names = varargin{1};
    values = varargin{2};
    n_names = numel (names);
    n_value = numel (values);
    if (n_value == 1 && n_names > 1)
      values = repmat (values, [1, n_names]);
    elseif (n_names != n_value)
      error ("setappdata: number of NAME and VALUE arguments must match");
    endif
    varargin = cell (1, 2*numel (names));
    varargin(1:2:end) = names;
    varargin(2:2:end) = values;

  elseif (! all (cellfun ("isclass", varargin(1:2:end), "char")))
    error ("setappdata: NAME must be a string or cellstr");
  endif

  for hg = h
    appdata = get (hg, "__appdata__");

    ## Slow, but not likely to be that many elements in loop
    for narg = 1:2:numel (varargin)
      appdata.(varargin{narg}) = varargin{narg+1};
    endfor

    set (hg, "__appdata__", appdata);
  endfor

endfunction


%!test
%! unwind_protect
%!   setappdata (0, "%hello%", "world");
%!   assert (isappdata (0, "%hello%"), true);
%!   assert (getappdata (0, "%hello%"), "world");
%! unwind_protect_cleanup
%!   rmappdata (0, "%hello%");
%! end_unwind_protect

%!test
%! unwind_protect
%!   setappdata (0, "%data1%", ones (3), "%data2%", "hello world");
%!   assert (getappdata (0, "%data1%"), ones (3));
%!   assert (getappdata (0, "%data2%"), "hello world");
%!   setappdata (0, "%data1%", zeros (3));
%!   assert (getappdata (0, "%data1%"), zeros (3));
%! unwind_protect_cleanup
%!   rmappdata (0, "%data1%", "%data2%");
%! end_unwind_protect

%!test
%! unwind_protect
%!   setappdata (0, {"%data1%", "%data2%"}, {ones(3), "hello world"});
%!   assert (getappdata (0, "%data1%"), ones (3));
%!   assert (getappdata (0, "%data2%"), "hello world");
%!   setappdata (0, "%data1%", zeros (3));
%!   assert (getappdata (0, "%data1%"), zeros (3));
%!   rmappdata (0, "%data1%", "%data2%");
%!   setappdata (0, {"%data1%", "%data2%"}, pi);
%!   assert (getappdata (0, "%data1%"), pi);
%!   assert (getappdata (0, "%data2%"), pi);
%! unwind_protect_cleanup
%!   rmappdata (0, "%data1%", "%data2%");
%! end_unwind_protect

## Test input validation
%!error <Invalid call> setappdata ()
%!error <Invalid call> setappdata (0)
%!error <Invalid call> setappdata (0, "name")
%!error <H must be a scalar .* graphic handle> setappdata (-1, "foo", "bar")
%!error <NAME/VALUE arguments must occur in pairs> setappdata (0, "1", 2, "3")
%!error <only 3 arguments possible> setappdata (0, {"1"}, 2, "3", 4)
%!error <number of NAME and VALUE arguments must match>
%! setappdata (0, {"1", "2"}, {2, 3, 4})
%!error <NAME must be a string> setappdata (0, 1, 2)
