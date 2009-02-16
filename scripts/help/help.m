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

function help (name)

  if (nargin == 0)

    puts ("\n\
  For help with individual commands and functions type\n\
\n\
    help NAME\n\
\n\
  (replace NAME with the name of the command or function you would\n\
  like to learn more about).\n\
\n\
  For a more detailed introduction to GNU Octave, please consult the\n\
  manual.  To read the manual from the prompt type\n\
\n\
    doc\n\
\n\
  GNU Octave is supported and developed by its user community.\n\
  For more information visit http://www.octave.org.\n\n");

  elseif (nargin == 1 && ischar (name))

    ## Get help text
    [text, format] = get_help_text (name);
    
    ## Take action depending on help text format
    switch (lower (format))
      case "plain text"
        status = 0;
      case "texinfo"
        [text, status] = __makeinfo__ (text, "plain text");
      case "html"
        [text, status] = strip_html_tags (text);
      case "not documented"
        error ("help: `%s' is not documented\n", name);
      case "not found"
        [text, status] = do_contents (name);
        if (status != 0)
          error ("help: `%s' not found\n", name);
        endif
      otherwise
        error ("help: internal error: unsupported help text format: '%s'\n", format);
    endswitch
    
    ## Print text
    if (status != 0)
      warning ("help: Texinfo formatting filter exited abnormally; raw Texinfo source of help text follows...\n");
    endif

    which (name);
    printf ("\n%s\n%s", text, __additional_help_message__ ());

  else
    error ("help: invalid input\n");
  endif

endfunction

function [text, status] = do_contents (name)
  text = "";
  status = 1;

  d = find_dir_in_path (name);
  if (!isempty (d))
    p = path ();
    unwind_protect
      ## Only include 'd' in the path, and then get the help text of 'Contents'
      path (d);
      [text, format] = get_help_text ("Contents");

      ## Take action depending on help text format
      switch (lower (format))
        case "plain text"
          status = 0;
        case "texinfo"
          [text, status] = __makeinfo__ (text, "plain text");
        case "html"
          [text, status] = strip_html_tags (text);
      endswitch
    unwind_protect_cleanup
      ## Restore path
      path (p);
    end_unwind_protect
  endif
endfunction
