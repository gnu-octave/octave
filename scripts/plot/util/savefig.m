## Copyright (C) 2018 Guillaume Flandin
##
## This file is part of Octave.
##
## Octave is free software: you can redistribute it and/or modify it
## under the terms of the GNU General Public License as published by
## the Free Software Foundation, either version 3 of the License, or
## (at your option) any later version.
## your option) any later version.
##
## Octave is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with Octave; see the filename COPYING.  If not, see
## <https://www.gnu.org/licenses/>.

## -*- texinfo -*-
## @deftypefn  {} {} savefig ()
## @deftypefnx {} {} savefig (@var{filename})
## @deftypefnx {} {} savefig (@var{h}, @var{filename})
## @deftypefnx {} {} savefig (@var{h})
## @deftypefnx {} {} savefig (@var{h}, @var{filename}, @qcode{"compact"})
## Save graphics handle @var{h} to file @var{filename}.
##
## If unspecified, @var{h} is set to the current figure.
##
## If unspecified, @var{filename} is set to @file{"Untitled.fig"}.  If
## @var{filename} does not have an extension, a default extension @file{".fig"}
## will be added.
##
## A third input argument will be accepted but ignored, for Matlab
## compatibility.
##
## @seealso{hgsave, hdl2struct, openfig}
## @end deftypefn

function savefig (varargin)

  ## Default values for input arguments
  h = [];
  filename = "Untitled.fig";

  ## Check input arguments
  if (nargin == 1)
    if (ishghandle (varargin{1}))
      h = varargin{1};
    elseif (ischar (varargin{1}))
      filename = varargin{1};
    else
      error ("savefig: invalid input");
    endif
  elseif (nargin == 2 || nargin == 3)
    if (! ishghandle (varargin{1}))
      error ("savefig: invalid figure handle");
    endif
    h = varargin{1};
    if (! ischar (varargin{2}))
      error ("savefig: invalid filename");
    endif
    filename = varargin{2};
    # Input "compact" ignored (Matlab compatibility)
  elseif (nargin > 3)
    print_usage ();
  endif

  ## Check figure handle input
  if (isempty (h))
    h = gcf ();
  endif

  ## Check filename extension
  [~, ~, ext] = fileparts (filename);
  if (isempty (ext))
    filename = [filename ".fig"];
  endif

  ## Save file
  hgsave (h, filename);

endfunction

%!test
%! unwind_protect
%!   h = figure ("Visible", "off");
%!   ftmp = [tempname() ".fig"];
%!   savefig (h, ftmp);
%!   savefig (ftmp);
%!   savefig (h, ftmp, "compact");
%! unwind_protect_cleanup
%!   close (h);
%!   unlink (ftmp);
%! end_unwind_protect
