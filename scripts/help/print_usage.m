########################################################################
##
## Copyright (C) 2009-2024 The Octave Project Developers
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
## @deftypefn  {} {} print_usage ()
## @deftypefnx {} {} print_usage (@var{name})
## Print the usage message for the function @var{name}.
##
## When called with no input arguments the @code{print_usage} function displays
## the usage message of the currently executing function.
## @seealso{help}
## @end deftypefn

function print_usage (name)

  x = dbstack ();
  ## Handle input
  if (nargin == 0)
    ## Determine the name of the calling function
    if (numel (x) > 1)
      name = x(2).name;
    else
      error ("Octave:invalid-context", "print_usage: invalid function\n");
    endif
    fullname = evalin ("caller", 'mfilename ("fullpath")');
    if (strcmp (fullname(end-length (name)+1:end), name))
      fullname = [fullname ".m"];
    endif
  elseif (! ischar (name))
    error ("Octave:invalid-input-arg",
           "print_usage: input argument must be a string");
  else
    fullname = name;
  endif

  ## Determine if we were called from top level.
  at_toplev = length (x) < 2 || (length (x) == 2 && strcmp (x(2).name, name));

  ## Do the actual work
  [text, format] = get_help_text (fullname);
  max_len = 80;
  switch (lower (format))
    case "plain text"
      [usage_string, status] = get_usage_plain_text (text, max_len);
    case "texinfo"
      [usage_string, status] = get_usage_texinfo (text, max_len);
    case "html"
      [usage_string, status] = get_usage_html (text, max_len);
    case "not documented"
      error ("print_usage: '%s' is not documented\n", name);
    case "not found"
      error ("print_usage: '%s' not found\n", name);
    otherwise
      error ("print_usage: internal error: unsupported help text format: '%s'\n", format);
  endswitch

  ## Raise the final error
  if (status != 0)
    warning ("print_usage: Texinfo formatting filter exited abnormally");
    warning ("print_usage: raw Texinfo source of help text follows...\n");
  endif

  ## We don't want to start the debugger here if debug_on_error is true
  ## so we set it to false and make the change local.  Then
  ## debug_on_error will be reset to true after this function returns
  ## and the debugger will start at the location of the call to
  ## print_usage.
  debug_on_error (false, "local");

  if (at_toplev)
    error ("Octave:invalid-fun-call",
           "Invalid call to %s.  Correct usage is:\n\n%s\n%s",
           name, usage_string, __additional_help_message__ ());
  else
    msg = sprintf ("Invalid call to %s.  Correct usage is:\n\n%s",
                   name, usage_string);
    ## Ensure that the error doesn't end up with a newline, as that disables
    ## backtraces.
    if (msg(end) == "\n")
      msg(end) = " ";
    endif

    error ("Octave:invalid-fun-call", msg);
  endif

endfunction

function [retval, status] = get_usage_plain_text (help_text, max_len)
  ## Extract first line by searching for a double line-end.
  line_end_idx = strfind (help_text, "\n\n");
  retval = help_text (1:min ([line_end_idx , max_len, length(help_text)]));
  status = 0;
endfunction

function [retval, status] = get_usage_texinfo (help_text, max_len)

  ## Lines ending with "@\n" are continuation lines, so they should be
  ## concatenated with the following line.
  help_text = strrep (help_text, "@\n", " ");

  ## Find, and keep, lines that start with @def or @end def.  This should
  ## include things such as @deftypefn, @deftypefnx, @defvar, etc. and their
  ## corresponding @end's.
  def_idx = strfind (help_text, "@def");
  if (isempty (def_idx))
    [retval, status] = get_usage_plain_text (help_text, max_len);
    return;
  endif

  endf_idx = strfind (help_text, "@end def");
  def_idx = sort ([def_idx, endf_idx]);
  endl_idx = find (help_text == "\n");
  buffer = "";
  for k = 1:length (def_idx)
    endl = endl_idx(find (endl_idx > def_idx(k), 1));
    if (isempty (endl))
      buffer = [buffer, help_text(def_idx(k):end), "\n"];
    else
      buffer = [buffer, help_text(def_idx(k):endl)];
    endif
  endfor

  ## Generate plain text from the markup.
  [retval, status] = __makeinfo_restricted__ (buffer);
  if (status != 0)  # something went wrong in the restricted conversion.
    ## Use the full conversion.
    [retval, status] = __makeinfo__ (buffer, "plain text");
  endif

endfunction

function [retval, status] = __makeinfo_restricted__ (buffer)
  ## This function converts the very narrow subset of Texinfo commands
  ## that are actually used by `print_usage` into plain text,
  ## without requiring a slow call to `system ("makeinfo")`.
  ##
  ## As of April 2024, only the following Texinfo commands are being used
  ## by print_usage strings in Octave core:
  ##
  ## @deftypefn, @deftypefnx, @end deftypefn, @var, @qcode, @dots, @, @@
  ##
  ## Note: this function is meant only for print_usage (), not for help (),
  ## which continues to use the full makeinfo.

  retval = "";
  status = -1;  # assume failure to convert

  ## The simple replacements first.
  buffer = strrep (buffer, "@deftypefnx", "");
  buffer = strrep (buffer, "@end deftypefn\n", "");
  buffer = strrep (buffer, "@deftypefn ", "");
  buffer = strrep (buffer, "@qcode", "");
  buffer = strrep (buffer, "@dots", "...");

  ## Capitalize variable names, so "@var{foo}" becomes "@var{FOO}".
  ## FIXME: Replace this loop with a call to regexprep once transformation
  ## features are available.
  for j = strfind (buffer, "@var")  # each instance of @var
    ## Upper-case the part between the relevant braces.
    k = find (buffer(j:end) == '}', 1) + j - 1;
    buffer((j+5):(k-1)) = upper (buffer((j+5):(k-1)));
  endfor

  ## Don't need @var any more, nor any braces.
  buffer = strrep (buffer, "@var", "");
  buffer(buffer == '{' | buffer == '}') = [];

  ## There should not be any Texinfo commands remaining at this point.
  ## Search for this with the regex "@[a-z]" because other instances of @
  ## occur next to upper case letters or non-letters by this point.
  if (any (regexp (buffer, "@[a-z]")))
    ## There is some Texinfo command in the deftypefn line
    ## outside the scope of this restricted function,
    ## so return with status set to failure.
    return
  endif

  ## Process any remaining @ signs.
  ## Context: A line like this:
  ##    setappdata (H, @NAME1, NAME2, ...@, @VALUE1, VALUE2, ...@)
  ## should become
  ##    setappdata (H, {NAME1, NAME2, ...}, {VALUE1, VALUE2, ...})
  ## But a line like this:
  ##   A = cellfun (@@FCN, C)
  ## should become:
  ##   A = cellfun (@FCN, C)
  ## and not this:
  ##   A = cellfun ({}FCN, C)
  ## Currently the code uses a guard string to protect the @@,
  ## then replaces @ with { or }, then unprotects the @@ and changes it to @.
  ## FIXME: Simplify this conversion sequence if possible.

  if (any (buffer == '@'))
    guardstring = "ZAQWSXCDERFV";  # some improbable sequence
    buffer = strrep (buffer, "@@", guardstring);
    ff = find (buffer == '@');  # these show up in pairs, verified with assert
    buffer(ff(1:2:end)) = '{';  # odd-numbered locations become {
    buffer(ff(2:2:end)) = '}';  # even-numbered ones become }
    buffer = strrep (buffer, guardstring, "@");
  endif

  retval = buffer;
  status = 0;  # == success
endfunction

function [retval, status] = get_usage_html (help_text, max_len)

  ## Strip tags
  [help_text, status] = strip_html_tags (help_text);

  ## Extract first line with plain text method.
  retval = get_usage_plain_text (help_text, max_len);

endfunction


## Stop reporting function as missing tests.  No good tests possible.
%!assert (1)
