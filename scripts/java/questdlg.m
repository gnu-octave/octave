## Copyright (C) 2010 Martin Hepperle
##
## This program is free software; you can redistribute it and/or modify
## it under the terms of the GNU General Public License as published by
## the Free Software Foundation; either version 2 of the License, or
## (at your option) any later version.
##
## This program is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with this program; If not, see <http://www.gnu.org/licenses/>.

## -*- texinfo -*-
## @deftypefn  {Function file} {@var{P} =} questdlg (@var{MESSAGE}, @var{TITLE})
## @deftypefnx {Function file} @var{P} = questdlg (@var{MESSAGE}, @var{TITLE}, @var{DEFAULT})
## @deftypefnx {Function file} @var{P} = questdlg (@var{MESSAGE}, @var{TITLE}, @var{BTN1}, @var{BTN2}, @var{DEFAULT})
## @deftypefnx {Function file} @var{P} = questdlg (@var{MESSAGE}, @var{TITLE}, @var{BTN1}, @var{BTN2}, @var{BTN3}, @var{DEFAULT})
##
## Displays the @var{MESSAGE} using a question dialog box. 
## The dialog contains two or three buttons which all close the dialog. 
## It returns the caption of the activated button.
##
## @var{message} can have multiple lines separated by newline characters
## ("\n"), or it can be a cellstr array (one element for each line).
## The optional @var{TITLE} (character string) can be used to decorate the
## dialog caption.
## The string @var{DEFAULT} identifies the default button, 
## which is activated by pressing the ENTER key.
## It must match one of the strings given in @var{BTN1}, @var{BTN2} or @var{BTN3}.
##
## If only @var{MESSAGE} and @var{TITLE} are specified, three buttons with
## the default captions "Yes", "No", "Cancel" are used.
##
## If only two button captions @var{BTN1} and @var{BTN2} are specified, 
## the dialog will have only these two buttons.
##
## @end deftypefn
## @seealso{errordlg, helpdlg, inputdlg, listdlg, warndlg}

function ret = questdlg (question, varargin)

  if length (varargin) < 1
    print_usage();
  end
  
  options{1} = 'Yes';      % button1
  options{2} = 'No';       % button2
  options{3} = 'Cancel';   % button3
  options{4} = 'Yes';      % default

  if (! ischar (question))
    if (iscell (question))
      question = cell2mlstr (question);
    else
      error ("questdlg: character string or cellstr array expected for message");
    endif
  endif

  switch length (varargin)
  case 1
     % title was given
     title = varargin{1};
  case 2
     % title and default button string
     title      = varargin{1};
     options{4} = varargin{2}; % default
  case 4
     % title, two buttons and default button string
     title      = varargin{1};
     options{1} = varargin{2}; % button1
     options{2} = '';          % not used, no middle button
     options{3} = varargin{3}; % button3
     options{4} = varargin{4}; % default
  case 5
     % title, three buttons and default button string
     title      = varargin{1};
     options{1} = varargin{2}; % button1
     options{2} = varargin{3}; % button2
     options{3} = varargin{4}; % button3
     options{4} = varargin{5}; % default
  otherwise
     print_usage();
  end

  if (! ischar (title))
    error ("questdlg: character string expected for title");
  endif

  ret = java_invoke ('org.octave.JDialogBox', 'questdlg', question, title, options);

end
