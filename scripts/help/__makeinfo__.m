## Copyright (C) 2009-2011 S�ren Hauberg
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
## @deftypefn  {Function File} {[@var{retval}, @var{status}] =} __makeinfo__ (@var{text}, @var{output_type})
## @deftypefnx {Function File} {[@var{retval}, @var{status}] =} __makeinfo__ (@var{text}, @var{output_type}, @var{see_also})
## Undocumented internal function.
## @end deftypefn

## Run @code{makeinfo} on a given text.
##
## The string @var{text} is run through the @code{__makeinfo__} program
## to generate output in various formats. This string must contain valid
## Texinfo formatted text.
##
## The @var{output_type} selects the format of the output. This can be either
## @t{"html"}, @t{"texinfo"}, or @t{"plain text"}. By default this is
## @t{"plain text"}. If @var{output_type} is @t{"texinfo"}, the @t{@@seealso}
## macro is expanded, but otherwise the text is unaltered.
##
## If the optional argument @var{see_also} is present, it is used to expand the
## Octave specific @t{@@seealso} macro. This argument must be a function handle,
## that accepts a cell array of strings as input argument (each elements of the
## array corresponds to the arguments to the @t{@@seealso} macro), and return
## the expanded string. If this argument is not given, the @t{@@seealso} macro
## will be expanded to the text
##
## @example
## See also: arg1, arg2@, ...
## @end example
##
## @noindent
## for @t{"plain text"} output, and
##
## @example
## See also: @@ref@{arg1@}, @@ref@{arg2@}, ...
## @end example
##
## @noindent
## otherwise.
##
## The optional output argument @var{status} contains the exit status of the
## @code{makeinfo} program as returned by @code{system}.

function [retval, status] = __makeinfo__ (text, output_type = "plain text", see_also = [])

  ## Check input
  if (nargin == 0)
    print_usage ();
  endif

  if (!ischar (text))
    error ("__makeinfo__: first input argument must be a string");
  endif

  if (!ischar (output_type))
    error ("__makeinfo__: second input argument must be a string");
  endif

  ## Define the function which expands @seealso macro
  if (isempty (see_also))
    if (strcmpi (output_type, "plain text"))
      see_also = @simple_see_also;
    else
      see_also = @simple_see_also_with_refs;
    endif
  endif

  if (!isa (see_also, "function_handle"))
    error ("__makeinfo__: third input argument must be the empty matrix, or a function handle");
  endif

  ## It seems like makeinfo sometimes gets angry if the first character
  ## on a line is a space, so we remove these.
  text = strrep (text, "\n ", "\n");

  ## Handle @seealso macro
  SEE_ALSO = "@seealso";
  starts = strfind (text, SEE_ALSO);
  for start = fliplr (starts)
    if (start == 1 || (text (start-1) != "@"))
      bracket_start = find (text (start:end) == "{", 1);
      stop = find (text (start:end) == "}", 1);
      if (!isempty (stop) && !isempty (bracket_start))
        stop += start - 1;
        bracket_start += start - 1;
      else
        bracket_start = start + length (SEE_ALSO);
        stop = find (text (start:end) == "\n", 1);
        if (isempty (stop))
          stop = length (text);
        else
          stop += start - 1;
        endif
      endif
      see_also_args = text (bracket_start+1:(stop-1));
      see_also_args = strtrim (strsplit (see_also_args, ","));
      expanded = see_also (see_also_args);
      text = strcat (text (1:start-1), expanded, text (stop+1:end));
    endif
  endfor

  ## Handle @nospell macro
  NOSPELL = "@nospell";
  starts = strfind (text, NOSPELL);
  for start = fliplr (starts)
    if (start == 1 || (text (start-1) != "@"))
      bracket_start = find (text (start:end) == "{", 1);
      stop = find (text (start:end) == "}", 1);
      if (!isempty (stop) && !isempty (bracket_start))
        stop += start - 1;
        bracket_start += start - 1;
      else
        bracket_start = start + length (NOSPELL);
        stop = find (text (start:end) == "\n", 1);
        if (isempty (stop))
          stop = length (text);
        else
          stop += start - 1;
        endif
      endif
      text(stop) = [];
      text(start:bracket_start) = [];
    endif
  endfor

  if (strcmpi (output_type, "texinfo"))
    status = 0;
    retval = text;
    return;
  endif

  ## Create the final TeXinfo input string
  text = sprintf ("\\input texinfo\n\n%s\n\n@bye\n", text);

  unwind_protect
    ## Write Texinfo to tmp file
    template = "octave-help-XXXXXX";
    [fid, name, msg] = mkstemp (fullfile (P_tmpdir, template), true);
    if (fid < 0)
      error ("__makeinfo__: could not create temporary file");
    endif
    fwrite (fid, text);
    fclose (fid);

    ## Take action depending on output type
    switch (lower (output_type))
      case "plain text"
         cmd = sprintf ("%s --no-headers --no-warn --force --no-validate %s",
                        makeinfo_program (), name);
      case "html"
         cmd = sprintf ("%s --no-headers --html --no-warn --no-validate --force %s",
                        makeinfo_program (), name);
      otherwise
        error ("__makeinfo__: unsupported output type: '%s'", output_type);
    endswitch

    ## Call makeinfo
    [status, retval] = system (cmd);

  unwind_protect_cleanup
    if (exist (name, "file"))
      delete (name);
    endif
  end_unwind_protect
endfunction

function expanded = simple_see_also (args)
  expanded = strcat ("\nSee also:", sprintf (" %s,", args {:}));
  expanded = strcat (expanded (1:end-1), "\n\n");
endfunction

function expanded = simple_see_also_with_refs (args)
  expanded = strcat ("\nSee also:", sprintf (" @ref{%s},", args {:}));
  expanded = strcat (expanded (1:end-1), "\n\n");
endfunction
