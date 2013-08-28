## Copyright (C) 2009-2012 Søren Hauberg
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
## @deftypefn  {Function File} {[@var{retval}, @var{status}] =} __makeinfo__ (@var{text})
## @deftypefnx {Function File} {[@var{retval}, @var{status}] =} __makeinfo__ (@var{text}, @var{output_type})
## Undocumented internal function.
## @end deftypefn

## Run @code{makeinfo} on a given text.
##
## The string @var{text} is run through the @code{__makeinfo__} program
## to generate output in various formats.  This string must contain valid
## Texinfo formatted text.
##
## The @var{output_type} selects the format of the output.  This can be either
## @qcode{"html"}, @qcode{"texinfo"}, or @qcode{"plain text"}.  By default this
## is @qcode{"plain text"}. 
##
## The optional output argument @var{status} contains the exit status of the
## @code{makeinfo} program as returned by @code{system}.

function [retval, status] = __makeinfo__ (text, output_type = "plain text")

  ## Check input
  if (nargin < 1 || nargin > 2)
    print_usage ();
  endif

  if (! ischar (text))
    error ("__makeinfo__: first input argument must be a string");
  endif

  if (! ischar (output_type))
    error ("__makeinfo__: OUTPUT_TYPE must be a string");
  endif

  ## Formatting in m-files has an extra space at the beginning of every line.
  ## Remove these unwanted spaces if present.  First text char is "\n" delim.
  if (text(2) == " ")
    text = strrep (text, "\n ", "\n");
  endif
  ## Texinfo crashes if @end tex does not appear first on the line.
  text = regexprep (text, '^ +@end tex', '@end tex', 'lineanchors');

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
    fwrite (fid, text);
    fclose (fid);

    ## Take action depending on output type
    switch (lower (output_type))
      case "plain text"
        cmd = sprintf ("%s --no-headers --no-warn --no-validate --force %s",
                       makeinfo_program (), name);
      case "html"
        cmd = sprintf ("%s --html --no-headers --no-warn --no-validate --force %s",
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


## No test needed for internal helper function.
%!assert (1)

