########################################################################
##
## Copyright (C) 2009-2023 The Octave Project Developers
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
## @deftypefn  {} {[@var{retval}, @var{status}] =} __makeinfo__ (@var{text}, @var{output_type})
## @deftypefnx {} {[@var{retval}, @var{status}] =} __makeinfo__ (@var{text}, @var{output_type}, @var{fsee_also})
## Undocumented internal function.
## @end deftypefn

## Run @code{makeinfo} on a given text.
##
## The string @var{text} is run through the @code{__makeinfo__} program
## to generate output in various formats.  This string must contain valid
## Texinfo formatted text.
##
## The @var{output_type} selects the format of the output.  This can be either
## @t{"html"}, @t{"texinfo"}, or @t{"plain text"}.  By default this is
## @t{"plain text"}.  If @var{output_type} is @t{"texinfo"}, the @t{@@seealso}
## macro is expanded, but otherwise the text is unaltered.
##
## If the optional argument @var{see_also} is present, it is used to expand the
## Octave specific @t{@@seealso} macro.  This argument must be a function
## handle, that accepts a cell array of strings as input argument (each
## elements of the array corresponds to the arguments to the @t{@@seealso}
## macro), and return the expanded string.  If this argument is not given, the
## @t{@@seealso} macro will be expanded to the text
##
## @example
## See also: arg1, arg2, ...
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

function [retval, status] = __makeinfo__ (text, output_type = "plain text", fsee_also)

  ## Check input
  if (nargin < 1)
    print_usage ();
  endif

  if (! ischar (text))
    error ("__makeinfo__: first input argument must be a string");
  endif

  if (! ischar (output_type))
    error ("__makeinfo__: second input argument must be a string");
  endif

  ## NOTE: The 3rd argument is used by Octave Forge function
  ##       generate_package_html, not by core Octave.  This functionality
  ##       can only be removed when that function has been updated.
  if (nargin < 3)
    if (strcmpi (output_type, "plain text"))
      fsee_also = @(T) ["\nSee also:", sprintf(" %s,", T{:})(1:end-1), "\n"];
    else
      fsee_also = @(T) ["\nSee also:", sprintf(" @ref{%s},", T{:})(1:end-1), "\n"];
    endif
  endif

  if (! is_function_handle (fsee_also))
    error ("__makeinfo__: third input argument must be a function handle");
  endif

  ## Formatting in m-files has an extra space at the beginning of every line.
  ## Remove these unwanted spaces if present.  First text char is "\n" delim.
  if (text(2) == " ")
    text = strrep (text, "\n ", "\n");
  endif
  ## Texinfo crashes if @end tex does not appear first on the line.
  text = regexprep (text, '^ +@end tex', '@end tex', 'lineanchors');
  ## Replace @seealso with Octave specific @xseealso macro, and escape '@'
  [s, e] = regexp (text, '@seealso{[^}]*}');
  cum_rep = 0;
  for (i_match = 1:numel (s))
    esc_text = strrep (text(((s(i_match)+8:e(i_match))+i_match-1)+cum_rep), '@', '@@');
    text = [text(1:s(i_match)+i_match+cum_rep-1), 'xseealso', esc_text, ...
            text(e(i_match)+1+i_match+cum_rep-1:end)];
    cum_rep += numel (esc_text) - (e(i_match)-(s(i_match)+8)+1);
  endfor

  ## We don't want *ref macros to clutter plain text output with "Note ..."
  if (strcmp (output_type, "plain text"))
    text  = regexprep (text, '@ref{(?:[^}]*?),?(?:XREF)?([^,}]+)}', '$1');
    text  = regexprep (text, '@xref{(?:[^}]*?),?(?:XREF)?([^,}]+)}', 'See $1');
    text  = regexprep (text, '@pxref{(?:[^}]*?),?(?:XREF)?([^,}]+)}', 'see $1');
  endif

  file = texi_macros_file ();
  fid = fopen (file, "r");
  if (fid < 0)
    error ("unable to open %s for reading", file);
  else
    macros_text = fread (fid, Inf, "*char")';
    text = [macros_text text];
  endif
  fclose (fid);

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
    [fid, name] = mkstemp (fullfile (tempdir, template), true);
    if (fid < 0)
      error ("__makeinfo__: could not create temporary file");
    endif
    fprintf (fid, "%s", text);
    fclose (fid);

    ## Take action depending on output type
    switch (lower (output_type))
      case "plain text"
        cmd = sprintf ('%s --no-headers --no-warn --no-validate --plaintext --output=- "%s"',
                       makeinfo_program (), name);
      case "html"
        cmd = sprintf ('%s --no-headers --html --no-warn --no-validate --output=- "%s"',
                       makeinfo_program (), name);
      otherwise
        error ("__makeinfo__: unsupported output type: '%s'", output_type);
    endswitch

    ## Call makeinfo
    [status, retval] = system (cmd);

    ## On error, retry with force to ensure user gets *something*
    if (status)
      cmd = regexprep (cmd, '--output=', '--force --output=', 'once');
      [status_force, retval] = system (cmd);
      ## original return value usually more useful
      if (status_force)
        status = status_force;
      endif
    endif

    ## Clean up extra newlines generated by makeinfo
    if (strcmpi (output_type, "plain text"))
      if (numel (retval) > 2 && retval(end-1:end) == "\n\n")
        retval = retval(1:end-2);
      endif
    endif

    ## Clean up start of @deftypefn expansion which includes extra ':'
    retval = regexprep (retval, '^ -- : +', ' -- ', "lineanchors");

  unwind_protect_cleanup
    if (exist (name, "file"))
      delete (name);
    endif
  end_unwind_protect

endfunction


## No test needed for internal helper function.
%!assert (1)
