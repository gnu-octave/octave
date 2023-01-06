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
## @deftypefn  {} {@var{btn} =} questdlg (@var{msg})
## @deftypefnx {} {@var{btn} =} questdlg (@var{msg}, @var{title})
## @deftypefnx {} {@var{btn} =} questdlg (@var{msg}, @var{title}, @var{default})
## @deftypefnx {} {@var{btn} =} questdlg (@var{msg}, @var{title}, @var{btn1}, @var{btn2}, @var{default})
## @deftypefnx {} {@var{btn} =} questdlg (@var{msg}, @var{title}, @var{btn1}, @var{btn2}, @var{btn3}, @var{default})
## Display @var{msg} using a question dialog box and return the caption of
## the activated button.
##
## The message may have multiple lines separated by newline characters ("\n"),
## or it may be a cellstr array with one element for each line.
##
## The optional @var{title} (character string) can be used to specify the
## dialog caption.  It defaults to @qcode{"Question Dialog"}.
##
## The dialog may contain two or three buttons which will all close the dialog.
##
## The string @var{default} identifies the default button, which is activated
## by pressing the @key{ENTER} key.  It must match one of the strings given
## in @var{btn1}, @var{btn2}, or @var{btn3}.
##
## If only @var{msg} and @var{title} are specified, three buttons with the
## default captions @qcode{"Yes"}, @qcode{"No"}, and @qcode{"Cancel"} are used.
##
## If only two button captions, @var{btn1} and @var{btn2}, are specified the
## dialog will have only these two buttons.
##
## Examples:
##
## @example
## @group
## btn = questdlg ("Close Octave?", "Some fancy title", ...
##                 "Yes", "No", "No");
## if (strcmp (btn, "Yes"))
##   exit ();
## endif
## @end group
## @end example
##
## @seealso{errordlg, helpdlg, inputdlg, listdlg, msgbox, warndlg}
## @end deftypefn

function btn = questdlg (msg, title = "Question Dialog", varargin)

  if (nargin < 1 || nargin > 6)
    print_usage ();
  endif

  if (! ischar (msg))
    if (! iscell (msg))
      error ("questdlg: MSG must be a character string or cellstr array");
    endif

    msg = sprintf ("%s\n", msg{:});
    msg(end) = "";
  endif

  if (! ischar (title))
    error ("questdlg: TITLE must be a character string");
  endif

  options{1} = "Yes";      # button1
  options{2} = "No";       # button2
  options{3} = "Cancel";   # button3
  options{4} = "Yes";      # default

  defbtn_error_msg = "questdlg: DEFAULT must match one of the button options";

  switch (numel (varargin))
    case 0
      ## use default default

    case 1
      ## default button string
      options{4} = varargin{1};  # default
      if (! any (strcmp (options{4}, options(1:3))))
        error (defbtn_error_msg);
      endif

    case 3
      ## two buttons and default button string
      options{1} = varargin{1};  # button1
      options{2} = "";           # not used, no middle button
      options{3} = varargin{2};  # button3
      options{4} = varargin{3};  # default
      if (! any (strcmp (options{4}, options([1 3]))))
        error (defbtn_error_msg);
      endif

    case 4
      ## three buttons and default button string
      options{1} = varargin{1};  # button1
      options{2} = varargin{2};  # button2
      options{3} = varargin{3};  # button3
      options{4} = varargin{4};  # default
      if (! any (strcmp (options{4}, options(1:3))))
        error (defbtn_error_msg);
      endif

  endswitch

  if (__event_manager_have_dialogs__ ())
    btn = __event_manager_question_dialog__ (msg, title, options{1}, options{2},
                                             options{3}, options{4});
  else
    error ("questdlg is not available in this version of Octave");
  endif

endfunction


%!demo
%! disp ('- test questdlg with two buttons');
%! a = questdlg ('Would you like some free money?',...
%!               '$ $ $ $ $ $ $ $ $ $ $ $ $ $ $ $ $ $ $',...
%!               'No', 'Cancel', 'Cancel');
%! if (strcmp (a, 'No'))
%!   msgbox ('Suit yourself.', 'Message Box');
%! endif

%!demo
%! disp ('- test questdlg with message and title only.');
%! a = 'Yes';
%! c = 0;
%! while (strcmp (a, 'Yes') && ! c)
%!   a = questdlg ('Close this Question Dialog?', 'Reductio Ad Absurdum');
%!   if (strcmp (a, 'Yes'))
%!     q = 'Are you sure?';
%!     while (strcmp (a, 'Yes'))
%!       a = questdlg (q, 'Reductio Ad Absurdum');
%!       word = ' really';
%!       i = strfind (q, word);
%!       if (isempty (i))
%!         i = strfind (q, ' sure');
%!         q = [q '!'];
%!       else
%!         word = [word ','];
%!       endif
%!       q = [q(1:i-1) word q(i:end)];
%!     endwhile
%!   endif
%!   if (strcmp (a, 'Cancel'))
%!     uiwait (warndlg ('Answer "Yes" or "No".', 'Warning Dialog'));
%!     c = 1;
%!   endif
%! endwhile
%! msgbox ('Whew!');

%!demo
%! disp ('- test questdlg with five inputs');
%! ans = questdlg ('Are you ready Steve?', 'Brian', 'No', 'Uh huh', 'Uh huh');
%! if (! strcmp (ans, 'No'))
%!   ans = questdlg ('Andy?', 'Brian', 'No', 'Yeah', 'Yeah');
%!   if (! strcmp (ans, 'No'))
%!     ans = questdlg ('Mick?', 'Brian', 'No', 'Okay', 'Okay');
%!     if (! strcmp (ans, 'No'))
%!       ans = msgbox ("Well all right, fellas.    \n\n     Let''s GO!!!!!",...
%!                     'Ballroom Blitz', 'none');
%!     endif
%!   endif
%! endif

## Test input validation
%!error <Invalid call> questdlg ()
%!error <Invalid call> questdlg (1,2,3,4,5,6,7)
%!error <MSG must be a character string or cellstr array> questdlg (1)
%!error <TITLE must be a character string> questdlg ("msg", 1)
%!error <DEFAULT must match one of the button> questdlg ("msg", "title", "ABC")
%!error <DEFAULT must match one of the button>
%! questdlg ("msg", "title", "btn1", "btn2", "ABC");
%!error <DEFAULT must match one of the button>
%! questdlg ("msg", "title", "btn1", "btn2", "btn3", "ABC");
