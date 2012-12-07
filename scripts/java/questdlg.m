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
## @deftypefn  {Function file} {@var{p} =} questdlg (@var{msg}, @var{title})
## @deftypefnx {Function file} @var{p} = questdlg (@var{msg}, @var{title}, @var{default})
## @deftypefnx {Function file} @var{p} = questdlg (@var{msg}, @var{title}, @var{btn1}, @var{btn2}, @var{default})
## @deftypefnx {Function file} @var{p} = questdlg (@var{msg}, @var{title}, @var{btn1}, @var{btn2}, @var{btn3}, @var{default})
## Display @var{msg} using a question dialog box and return the caption
## of the activated button.
##
## The dialog may contain two or three buttons which all close the dialog. 
##
## The message may have multiple lines separated by newline characters
## (@code{"\n"}), or it may be a cellstr array with one element for each
## line.  The optional @var{title} (character string) can be used to
## decorate the dialog caption.
##
## The string @var{default} identifies the default button, 
## which is activated by pressing the ENTER key.
## It must match one of the strings given in @var{btn1}, @var{btn2} or
## @var{btn3}.
##
## If only @var{msg} and @var{title} are specified, three buttons with
## the default captions @code{"Yes"}, @code{"No"}, and @code{"Cancel"}
## are used.
##
## If only two button captions @var{btn1} and @var{btn2} are specified, 
## the dialog will have only these two buttons.
##
## @seealso{errordlg, helpdlg, inputdlg, listdlg, warndlg}
## @end deftypefn

function ret = questdlg (question, varargin)

  if (numel (varargin) < 1)
    print_usage ();
  endif
  
  options{1} = "Yes";      ## button1
  options{2} = "No";       ## button2
  options{3} = "Cancel";   ## button3
  options{4} = "Yes";      ## default

  if (! ischar (question))
    if (iscell (question))
      question = cell2mlstr (question);
    else
      error ("questdlg: character string or cellstr array expected for message");
    endif
  endif

  switch (numel (varargin))
    case 1
      ## title was given
      title = varargin{1};

    case 2
      ## title and default button string
      title = varargin{1};
      options{4} = varargin{2}; ## default

    case 4
      ## title, two buttons and default button string
      title = varargin{1};
      options{1} = varargin{2}; ## button1
      options{2} = "";          ## not used, no middle button
      options{3} = varargin{3}; ## button3
      options{4} = varargin{4}; ## default

    case 5
      ## title, three buttons and default button string
      title      = varargin{1};
      options{1} = varargin{2}; ## button1
      options{2} = varargin{3}; ## button2
      options{3} = varargin{4}; ## button3
      options{4} = varargin{5}; ## default

    otherwise
      print_usage ();
  endswitch

  if (! ischar (title))
    error ("questdlg: character string expected for title");
  endif

  ret = java_invoke ("org.octave.JDialogBox", "questdlg", question,
                     title, options);

endfunction

