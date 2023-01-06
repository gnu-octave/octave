########################################################################
##
## Copyright (C) 2018-2023 The Octave Project Developers
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
##
########################################################################

## -*- texinfo -*-
## @deftypefn  {} {} savefig ()
## @deftypefnx {} {} savefig (@var{h})
## @deftypefnx {} {} savefig (@var{filename})
## @deftypefnx {} {} savefig (@var{h}, @var{filename})
## @deftypefnx {} {} savefig (@var{h}, @var{filename}, @qcode{"compact"})
## Save figure windows specified by graphics handle(s) @var{h} to file
## @var{filename}.
##
## If unspecified, @var{h} is the current figure returned by @code{gcf}.
##
## If unspecified, @var{filename} is set to @file{"Untitled.fig"}.  If
## @var{filename} does not have an extension then the default extension
## @file{".fig"} will be added.
##
## If the optional third input @qcode{"compact"} is present then the data
## will be compressed to save more space.
##
## @seealso{hgsave, hdl2struct, openfig}
## @end deftypefn

function savefig (varargin)

  if (nargin > 3)
    print_usage ();
  endif

  ## Default values for input arguments
  h = [];
  filename = "Untitled.fig";
  fmt = "-binary";

  ## Check input arguments
  if (nargin == 1)
    if (all (isfigure (varargin{1})))
      h = varargin{1};
    elseif (ischar (varargin{1}))
      filename = varargin{1};
    else
      error ("savefig: first argument must be a figure handle or filename");
    endif
  else
    if (! all (isfigure (varargin{1})))
      error ("savefig: H must be a valid figure handle");
    endif
    h = varargin{1};
    if (! ischar (varargin{2}))
      error ("savefig: FILENAME must be a string");
    endif
    filename = varargin{2};
    if (nargin == 3)
      if (strcmpi (varargin{3}, "compact"))
        fmt = "-zip";
      else
        warning ("savefig: unrecognized option '%s'", varargin{3});
      endif
    endif
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

  ## Save handles to file
  hgsave (h, filename, fmt);

endfunction


%!test
%! unwind_protect
%!   h = figure ("visible", "off");
%!   ftmp = [tempname() ".fig"];
%!   savefig (h, ftmp);
%!   savefig (ftmp);
%! unwind_protect_cleanup
%!   close (h);
%!   unlink (ftmp);
%! end_unwind_protect

%!testif HAVE_ZLIB
%! unwind_protect
%!   h = figure ("visible", "off");
%!   ftmp = [tempname() ".fig"];
%!   savefig (h, ftmp, "compact");
%! unwind_protect_cleanup
%!   close (h);
%!   unlink (ftmp);
%! end_unwind_protect

## Test input validation
%!error savefig (1,2,3,4)
%!error <must be a figure handle or filename> savefig (struct ())
%!error <H must be a valid figure handle> savefig ([0, -1], "foobar")
%!error <FILENAME must be a string>
%! unwind_protect
%!   h = figure ("visible", "off");
%!   savefig (h, -1);
%! unwind_protect_cleanup
%!   close (h);
%! end_unwind_protect
%!warning <unrecognized option 'foobar'>
%! unwind_protect
%!   h = figure ("visible", "off");
%!   ftmp = [tempname() ".fig"];
%!   savefig (h, ftmp, "foobar");
%! unwind_protect_cleanup
%!   close (h);
%!   unlink (ftmp);
%! end_unwind_protect
