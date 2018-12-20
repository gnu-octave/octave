## Copyright (C) 2010-2018 Kai Habel
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

## -*- texinfo -*-
## @deftypefn  {} {} saveas (@var{h}, @var{filename})
## @deftypefnx {} {} saveas (@var{h}, @var{filename}, @var{fmt})
## Save graphic object @var{h} to the file @var{filename} in graphic format
## @var{fmt}.
##
## All device formats accepted by @code{print} may be used.  Common formats
## are:
##
## @table @code
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
## @qcode{"pdf"}.
##
## @example
## @group
## clf ();
## surf (peaks);
## saveas (1, "figure1.png");
## @end group
## @end example
##
## @seealso{print, hgsave, orient}
## @end deftypefn

## Author: Kai Habel

function saveas (h, filename, fmt)

  if (nargin != 2 && nargin != 3)
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

  if (nargin == 2)
    ## Attempt to infer format from filename
    [~, ~, ext] = fileparts (filename);
    if (! isempty (ext))
      fmt = ext(2:end);
    else
      fmt = "pdf";
    endif
  endif

  if (nargin == 3)
    if (! ischar (fmt))
      error ("saveas: FMT must be a string");
    endif
    [~, ~, ext] = fileparts (filename);
    if (isempty (ext))
      filename = [filename "." fmt];
    endif
  endif

  prt_opt = ["-d" tolower(fmt)];

  print (fig, filename, prt_opt);

endfunction

## Test input validation
%!error saveas ()
%!error saveas (1)
%!error saveas (1,2,3,4)
%!error <H must be a graphics handle> saveas (Inf, "tst.pdf")
%!error <FILENAME must be a string> saveas (0, 1)
%!error <FMT must be a string> saveas (0, "tst.pdf", 1)
