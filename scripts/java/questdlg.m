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
## @deftypefn  {Function File} {@var{btn} =} questdlg (@var{msg})
## @deftypefnx {Function File} {@var{btn} =} questdlg (@var{msg}, @var{title})
## @deftypefnx {Function File} {@var{btn} =} questdlg (@var{msg}, @var{title}, @var{default})
## @deftypefnx {Function File} {@var{btn} =} questdlg (@var{msg}, @var{title}, @var{btn1}, @var{btn2}, @var{default})
## @deftypefnx {Function File} {@var{btn} =} questdlg (@var{msg}, @var{title}, @var{btn1}, @var{btn2}, @var{btn3}, @var{default})
## Display @var{msg} using a question dialog box and return the caption
## of the activated button.
##
## The dialog may contain two or three buttons which will all close the dialog.
##
## The message may have multiple lines separated by newline characters
## (@code{"\n"}), or it may be a cellstr array with one element for each
## line.  The optional @var{title} (character string) can be used to
## decorate the dialog caption.
##
## The string @var{default} identifies the default button, 
## which is activated by pressing the @kbd{ENTER} key.
## It must match one of the strings given in @var{btn1}, @var{btn2} or
## @var{btn3}.
##
## If only @var{msg} and @var{title} are specified, three buttons with
## the default captions "Yes", "No", and "Cancel" are used.
##
## If only two button captions, @var{btn1} and @var{btn2}, are specified 
## the dialog will have only these two buttons.
##
## @seealso{errordlg, helpdlg, inputdlg, listdlg, warndlg}
## @end deftypefn

function btn = questdlg (msg, title = "Question Dialog", varargin)

  if (nargin < 1 || nargin > 6)
    print_usage ();
  endif
  
  if (! ischar (msg))
    if (iscell (msg))
      msg = sprintf ("%s\n", msg{:});
      msg(end) = "";
    else
      error ("questdlg: MSG must be a character string or cellstr array");
    endif
  endif

  if (! ischar (title))
    error ("questdlg: TITLES must be a character string");
  endif

  options{1} = "Yes";      # button1
  options{2} = "No";       # button2
  options{3} = "Cancel";   # button3
  options{4} = "Yes";      # default

  switch (numel (varargin))
    case 1
      ## default button string
      options{4} = varargin{1};  # default

    case 3
      ## two buttons and default button string
      options{1} = varargin{1};  # button1
      options{2} = "";           # not used, no middle button
      options{3} = varargin{2};  # button3
      options{4} = varargin{3};  # default

    case 4
      ## three buttons and default button string
      options{1} = varargin{1};  # button1
      options{2} = varargin{2};  # button2
      options{3} = varargin{3};  # button3
      options{4} = varargin{4};  # default

    otherwise
      print_usage ();

  endswitch

  btn = javaMethod ("questdlg", "org.octave.JDialogBox", msg,
                     title, options);

endfunction

