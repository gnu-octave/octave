## Copyright (C) 2010-2011 Ben Abbott <bpabbott@mac.com>
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
## @deftypefn  {Function File} {@var{C} =} textscan (@var{fid}, @var{format})
## @deftypefnx {Function File} {@var{C} =} textscan (@var{fid}, @var{format}, @
## @var{n})
## @deftypefnx {Function File} {@var{C} =} textscan (@var{fid}, @var{format}, @
## @var{param}, @var{value}, @dots{})
## @deftypefnx {Function File} {@var{C} =} textscan (@var{fid}, @var{format}, @
## @var{n}, @var{param}, @var{value}, @dots{})
## @deftypefnx {Function File} {@var{C} =} textscan (@var{str}, @dots{})
## @deftypefnx {Function File} {[@var{C}, @var{position}] =} textscan (@dots{})
## Read data from a text file.
##
## The file associated with @var{fid} is read and parsed according to
## @var{format}.  The function behaves like @code{strread} except it works by
## parsing a file instead of a string.  See the documentation of
## @code{strread} for details.  In addition to the options supported by
## @code{strread}, this function supports one more:
## @itemize
## @item "headerlines":
## @end itemize
## The first @var{value} number of lines of @var{str} are skipped.
##
## The optional input, @var{n}, specifes the number of lines to be read from
## the file, associated with @var{fid}.
##
## The output, @var{C}, is a cell array whose length is given by the number
## of format specifiers.
##
## The second output, @var{position}, provides the position, in characters,
## from the beginning of the file.
##
## @seealso{dlmread, fscanf, load, strread, textread}
## @end deftypefn

function [C, p] = textscan (fid, format, varargin)

  ## Check input
  if (nargin < 1)
    print_usage ();
  elseif (nargin == 1 || isempty (format))
    format = "%f";
  endif

  if (nargin > 2 && isnumeric (varargin{1}))
    nlines = varargin{1};
    args = varargin(2:end);
  else
    nlines = Inf;
    args = varargin;
  endif

  if (! any (strcmpi (args, "emptyvalue")))
    ## Matlab returns NaNs for missing values
    args{end+1} = "emptyvalue";
    args{end+1} = NaN;
  endif

  if (isa (fid, "double") && fid > 0 || ischar (fid))
    if (ischar (format))
      if (ischar (fid))
        if (nargout == 2)
          error ("textscan: cannot provide position information for character input");
        endif
        str = fid;
      else
        ## Maybe skip header lines
        headerlines = find (strcmpi (args, "headerlines"), 1);
        if (! isempty (headerlines))
          fskipl (fid, headerlines);
          args(headerlines:headerlines+1) = [];
        endif
        if (isfinite (nlines))
          str = "";
          for n = 1:nlines
            str = strcat (str, fgets (fid));
          endfor
            else
          str = fread (fid, "char=>char").';
        endif
      endif

      ## Determine the number of data fields
      num_fields = numel (strfind (format, "%")) - ...
                   numel (idx_star = strfind (format, "%*"));

      ## Call strread to make it do the real work
      C = cell (1, num_fields);
      [C{:}] = strread (str, format, args{:});

      if (ischar (fid) && isfinite (nlines))
        C = cellfun (@(x) x(1:nlines), C, "uniformoutput", false);
      endif

      if (nargout == 2)
        p = ftell (fid);
      endif

    else
      error ("textscan: FORMAT must be a valid specification");
    endif
  else
    error ("textscan: first argument must be a file id or character string");
  endif

endfunction

%!test
%! str = "1,  2,  3,  4\n 5,  ,  ,  8\n 9, 10, 11, 12";
%! fmtstr = "%f %d %f %s";
%! c = textscan (str, fmtstr, 2, "delimiter", ",", "emptyvalue", -Inf);
%! assert (isequal (c{1}, [1;5]))
%! assert (length (c{1}), 2);
%! assert (iscellstr (c{4}))
%! assert (isequal (c{3}, [3; -Inf]))

%!test
%! b = [10:10:100];
%! b = [b; 8*b/5];
%! str = sprintf ("%g miles/hr = %g kilometers/hr\n", b);
%! fmt = "%f miles/hr = %f kilometers/hr";
%! c = textscan (str, fmt);
%! assert (b(1,:)', c{1})
%! assert (b(2,:)', c{2})


