## Copyright (C) 2009 Søren Hauberg
##
## This program is free software; you can redistribute it and/or modify it
## under the terms of the GNU General Public License as published by
## the Free Software Foundation; either version 3 of the License, or (at
## your option) any later version.
##
## This program is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
## General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with this program; see the file COPYING.  If not, see
## <http://www.gnu.org/licenses/>.

## -*- texinfo -*-
## @deftypefn {Command} help @var{name}
## Display the help text for @var{name}.
## If invoked without any arguments, @code{help} prints a list
## of all the available operators and functions.
## 
## For example, the command @kbd{help help} prints a short message
## describing the @code{help} command.
## 
## The help command can give you information about operators, but not the
## comma and semicolons that are used as command separators.  To get help
## for those, you must type @kbd{help comma} or @kbd{help semicolon}.
## @seealso{doc, which, lookfor}
## @end deftypefn

## PKG_ADD: mark_as_command help

function help (name)
  if (nargin == 0)
    disp ("Help is available for the topics listed below.");
    disp ("");
    
    disp ("*** operators:");
    operators = __operators__ ();
    disp (list_in_columns (operators (:, 1)));

    disp ("*** reserved words:");
    keywords = __keywords__ ();
    disp (list_in_columns (keywords (:, 1)));

    disp ("*** available functions:");
    functions = __list_functions__ ();
    disp (list_in_columns (functions));
    
  elseif (nargin == 1 && ischar (name))
    ## Get help text
    [text, format] = get_help_text (name);
    
    ## Take action depending on help text format
    switch (lower (format))
      case "plain text"
        status = 0;
      case "texinfo"
        [text, status] = makeinfo (text, "plain text");
      case "html"
        [text, status] = strip_html_tags (text);
      case "not found"
        error ("help: `%s' not found\n", name);
      otherwise
        error ("help: internal error: unsupported help text format: '%s'\n", format);
    endswitch
    
    ## Print text
    if (status != 0)
      warning ("makeinfo: Texinfo formatting filter exited abnormally");
      warning ("makeinfo: raw Texinfo source of help text follows...\n");
    endif

    which (name);
    printf ("\n%s\n%s", text, __additional_help_message__ ());
    
  else
    error ("help: invalid input\n");
  endif
endfunction

