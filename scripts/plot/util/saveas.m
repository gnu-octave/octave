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
## @deftypefn  {} {} saveas (@var{h}, @var{filename})
## @deftypefnx {} {} saveas (@var{h}, @var{filename}, @var{fmt})
## Save graphics object @var{h} to the file @var{filename} in graphics format
## @var{fmt}.
##
## If @var{h} is the handle to a figure object, that figure object is saved.
## If @var{h} is the handle to a different graphics object, the figure
## containing that graphics object is saved.
##
## All device formats accepted by @code{print} may be used.  Common formats
## are:
##
## @table @code
##
##   @item ofig
##     Octave figure file format (default)
##
##   @item mfig
##     Two files: Octave m-file @file{filename.m} containing code
##     to open Octave figure file @file{filename.ofig}
##
##   @item ps
##     PostScript
##
##   @item eps
##     Encapsulated PostScript
##
##   @item pdf
##     Portable Document Format
##
##   @item jpg
##     JPEG Image
##
##   @item png
##     Portable Network Graphics image
##
##   @item emf
##     Enhanced MetaFile
##
##   @item tif
##     TIFF Image, compressed
##
## @end table
##
## If @var{fmt} is omitted it is extracted from the extension of
## @var{filename}.  The default format when there is no extension is
## @qcode{"ofig"}.
##
## @example
## @group
## clf ();
## surf (peaks);
## saveas (1, "figure1.png");
## @end group
## @end example
##
## @seealso{print, savefig, hgsave, orient}
## @end deftypefn

function saveas (h, filename, fmt)

  if (nargin < 2)
    print_usage ();
  endif

  if (! ishghandle (h))
    error ("saveas: H must be a graphics handle");
  endif
  if (! ischar (filename))
    error ("saveas: FILENAME must be a string");
  endif

  if (isfigure (h))
    fig = h;
  else
    fig = ancestor (h, "figure");
  endif

  default_fmt = "ofig";

  if (nargin == 2)
    ## Attempt to infer format from filename
    [~, ~, ext] = fileparts (filename);
    if (isempty (ext))
      ext = ["." default_fmt];
      filename = [filename ext];
    endif
    fmt = ext(2:end);
  endif

  if (nargin == 3)
    if (! ischar (fmt))
      error ("saveas: FMT must be a string");
    elseif (isempty (fmt))
      fmt = default_fmt;
    endif
    [~, ~, ext] = fileparts (filename);
    if (isempty (ext))
      ext = ["." fmt];
      filename = [filename ext];
    endif
  endif

  fmt = tolower (fmt);

  if (any (strcmp (fmt, {"ofig", "fig"})))
    savefig (fig, filename);
  elseif (any (strcmp (fmt, {"m", "mfig"})))
    [d, n] = fileparts (filename);
    mfilename = fullfile (d, [n ".m"]);
    figfilename = fullfile (d, [n ".ofig"]);

    savefig (fig, figfilename);

    fid = fopen (mfilename, "wt");
    if (fid < 0)
      error ("saveas: could not open '%s' for writing", mfilename);
    endif
    fprintf (fid, ['h = openfig ("' figfilename '");' "\n"]);
    fclose (fid);
  else
    prt_opt = ["-d" fmt];

    print (fig, filename, prt_opt);
  endif

endfunction


## Test input validation
%!error <Invalid call> saveas ()
%!error <Invalid call> saveas (1)
%!error <H must be a graphics handle> saveas (Inf, "tst.pdf")
%!error <FILENAME must be a string> saveas (0, 1)
%!error <FMT must be a string> saveas (0, "tst.pdf", 1)
