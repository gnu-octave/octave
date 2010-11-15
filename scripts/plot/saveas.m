## Copyright (C) 2010 Kai Habel
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
## @deftypefn  {Function File} {} saveas (@var{h}, @var{file_name})
## @deftypefnx  {Function File} {} saveas (@var{h}, @var{file_name}, @var{file_ext})
## Save the graphic object @var{h} to file @var{file_name} in graphic
## format @var{file_ext}. 
## 
## @var{file_ext} should be one of the following formats:
##
## @table @code
##   @item ps
##     Postscript
##   @item eps
##     Encapsulated Postscript
##   @item jpg
##     JPEG Image
##   @item png
##     PNG Image
##   @item emf
##     Enhanced Meta File
##   @item pdf
##     Portable Document Format
## @end table
##
## All device formats specified in @code{print} can also be used. If @var{file_ext}
## is omitted it is extracted from @var{file_name}. The default value is
## pdf.
##
## @example
## figure (1);
## clf ();
## surf (peaks);
## saveas(1, "figure1.png");
## @end example
##
## @seealso{print}
## @end deftypefn

## Author: Kai Habel

function  saveas (h, fname, fext = "pdf")

  if ((nargin != 2) && (nargin != 3))
    print_usage ();
  endif

  if (ishandle (h))
    if (isfigure (h))
      fig = h;
    else
      fig = ancestor (h, "figure")
    endif
  else
    error ("first argument must be a graphics handle");
  endif
  
  if (!ischar(fname))
    error ("file_name must be a string.");
  endif

  if (nargin == 2)
    [~, ~, ext] = fileparts (fname);
    if (!isempty (ext))
      fext = ext(2:end);
    endif
  endif

  if (nargin == 3)
    if (!ischar (fname))
      error ("fext must be a string.");
    endif

    [~, ~, ext] = fileparts (fname);

    if (isempty (ext))
      fname = strcat (fname, ".", fext);
    endif
  endif

  prt_opt = strcat ("-d", tolower (fext));
  
  print (fname, prt_opt)
endfunction
