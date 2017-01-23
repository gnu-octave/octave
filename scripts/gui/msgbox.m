## Copyright (C) 2010-2016 Martin Hepperle
##
## This file is part of Octave.
##
## Octave is free software; you can redistribute it and/or modify it
## under the terms of the GNU General Public License as published by
## the Free Software Foundation; either version 3 of the License, or
## (at your option) any later version.
##
## Octave is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with Octave; see the file COPYING.  If not, see
## <http://www.gnu.org/licenses/>.

## -*- texinfo -*-
## @deftypefn  {} {@var{h} =} msgbox (@var{msg})
## @deftypefnx {} {@var{h} =} msgbox (@var{msg}, @var{title})
## @deftypefnx {} {@var{h} =} msgbox (@var{msg}, @var{title}, @var{icon})
## @deftypefnx {} {@var{h} =} msgbox (@dots{}, @var{createmode})
## Display @var{msg} using a message dialog box.
##
## The message may have multiple lines separated by newline characters ("\n"),
## or it may be a cellstr array with one element for each line.
##
## The optional input @var{title} (character string) can be used to decorate
## the dialog caption.
##
## The optional argument @var{icon} selects a dialog icon.
## It can be one of @qcode{"none"} (default), @qcode{"error"}, @qcode{"help"},
## or @qcode{"warn"}.
##
## The return value is always 1.
##
## Compatibility Note: The optional argument @var{createmode} is accepted for
## @sc{matlab} compatibility, but is not implemented.  A valid @var{createmode}
## is either one of the character strings @qcode{"nonmodal"}, @qcode{"modal"},
## or @qcode{"replace"}, or a structure containing a field
## @qcode{"WindowStyle"} with one of the three character strings.
##
## Examples:
##
## @example
## @group
## msgbox ("Some message for the user.");
## msgbox ("Some message\nwith two lines.");
## msgbox (@{"Some message", "with two lines."@});
## msgbox ("Some message for the user.", "Fancy caption");
##
## % A message dialog box with error icon
## msgbox ("Some message for the user.", "Fancy caption", "error");
## @end group
## @end example
##
## @seealso{errordlg, helpdlg, inputdlg, listdlg, questdlg, warndlg}
## @end deftypefn

function retval = msgbox (msg, varargin)

  narginchk (1, 4);

  if (! ischar (msg))
    if (iscell (msg))
      msg = sprintf ("%s\n", msg{:});
      msg(end) = "";
    else
      error ("MSG must be a character string or cellstr array");
    endif
  endif

  box_title = "";
  box_icon = "none";

  if (nargin > 1)
    ## check last element to be a structure CREATEMODE
    if (isstruct (varargin{end}) && isfield (varargin{end}, "WindowStyle"))
      varargin{end} = varargin{end}.WindowStyle;
    endif
    ## print warning for unsupported CREATEMODE
    if ((ischar (varargin{end}))
        && (ismember (varargin{end}, {"nonmodal", "modal", "replace"})))
      warning ("CREATEMODE %s is not yet supported", varargin{end});
      nargin = nargin - 1;
    elseif (nargin == 4)
      error ("CREATEMODE is not a valid type");
    endif

    if ((nargin > 1) && (! ischar (varargin{1})))
      error ("TITLE must be a character string");
    else
      box_title = varargin{1};
    endif

    if (nargin > 2)
      box_icon = varargin{2};
      switch (box_icon)
        case {"error", "help", "warn", "none"}
          ## do nothing, all valid
        case "custom"
          warning ("custom icons are not yet supported");
        otherwise
          error ("ICON is not a valid type")
      endswitch
    endif
  endif

  ## make a GUI element or print to console
  if (__octave_link_enabled__ ())
    retval = __octave_link_message_dialog__ (box_icon, msg, box_title);
  else
    disp (sprintf ("\n%s:\t%s\n\t%s\n", upper (box_icon), box_title,
      strrep (msg, "\n", "\n\t")));
    retval = 1;
  endif

endfunction


## Test input validation
%!error <narginchk> msgbox (1, 2, 3, 4, 5)
%!error <MSG must be a character string> msgbox (1)
%!error <TITLE must be a character string> msgbox ("msg", 1)
%!error <ICON is not a valid type> msgbox ("msg", "title", 1)
%!error <CREATEMODE is not a valid> msgbox ("msg", "title", "help", "wrong")
