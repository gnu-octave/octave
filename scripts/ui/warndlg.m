## Copyright (C) 2010 Martin Hepperle
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
## @deftypefn  {Function File} {@var{h} =} warndlg (@var{msg})
## @deftypefnx {Function File} {@var{h} =} warndlg (@var{msg}, @var{title})
## Display @var{msg} using a warning dialog box. 
##
## The message may have multiple lines separated by newline characters
## ("\n"), or it may be a cellstr array with one element for each
## line.  The optional input @var{title} (character string) can be used to
## set the dialog caption.  The default title is "Warning Dialog".
##
## @seealso{helpdlg, inputdlg, listdlg, questdlg}
## @end deftypefn

function retval = warndlg (msg, title = "Warning Dialog")

  if (nargin < 1 || nargin > 2)
    print_usage ();
  endif

  retval = message_dialog ("warndlg", msg, title, "warn");

endfunction


%!demo
%! disp ('- test warndlg with prompt only.');
%! warndlg ('Oh, a warning occured');

%!demo
%! disp ('- test warndlg with prompt and caption.');
%! warndlg ('Oh, No...','This is the last Warning');

