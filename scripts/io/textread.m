## Copyright (C) 2009-2012 Eric Chassande-Mottin, CNRS (France)
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
## @deftypefn  {Function File} {[@var{a}, @dots{}] =} textread (@var{filename})
## @deftypefnx {Function File} {[@var{a}, @dots{}] =} textread (@var{filename}, @var{format})
## @deftypefnx {Function File} {[@var{a}, @dots{}] =} textread (@var{filename}, @var{format}, @var{n})
## @deftypefnx {Function File} {[@var{a}, @dots{}] =} textread (@var{filename}, @var{format}, @var{prop1}, @var{value1}, @dots{})
## @deftypefnx {Function File} {[@var{a}, @dots{}] =} textread (@var{filename}, @var{format}, @var{n}, @var{prop1}, @var{value1}, @dots{})
## Read data from a text file.
##
## The file @var{filename} is read and parsed according to @var{format}.  The
## function behaves like @code{strread} except it works by parsing a file
## instead of a string.  See the documentation of @code{strread} for details.
##
## In addition to the options supported by @code{strread}, this function
## supports two more:
##
## @itemize
## @item "headerlines":
## The first @var{value} number of lines of @var{filename} are skipped.
##
## @item "endofline":
## Specify a single character or "\r\n".  If no value is given, it will be
## inferred from the file.  If set to "" (empty string) EOLs are ignored as
## delimiters.
## @end itemize
##
## The optional input @var{n} specifes the number of times to use
## @var{format} when parsing, i.e., the format repeat count.
##
## @seealso{strread, load, dlmread, fscanf, textscan}
## @end deftypefn

function varargout = textread (filename, format = "%f", varargin)

  ## Check input
  if (nargin < 1)
    print_usage ();
  endif

  if (! ischar (filename) || ! ischar (format))
    error ("textread: FILENAME and FORMAT arguments must be strings");
  endif

  ## Read file
  fid = fopen (filename, "r");
  if (fid == -1)
    error ("textread: could not open '%s' for reading", filename);
  endif

  ## Skip header lines if requested
  headerlines = find (strcmpi (varargin, "headerlines"), 1);
  ## Beware of zero valued headerline, fskipl would skip to EOF
  if (! isempty (headerlines) && (varargin{headerlines + 1} > 0))
    fskipl (fid, varargin{headerlines + 1});
    varargin(headerlines:headerlines+1) = [];
  endif

  if (nargin > 2 && isnumeric (varargin{1}))
    nlines = varargin{1};
  else
    nlines = Inf;
  endif

  if (isfinite (nlines) && (nlines >= 0))
    str = tmp_str = "";
    n = 0;
    ## FIXME: Can this be done without slow loop?
    while (ischar (tmp_str) && n++ <= nlines)
      str = strcat (str, tmp_str);
      tmp_str = fgets (fid);
    endwhile
  else
    str = fread (fid, "char=>char").';
  endif
  fclose (fid);

  if (isempty (str))
    warning ("textread: empty file");
    return;
  endif

  endofline = find (strcmpi (varargin, "endofline"), 1);
  if (! isempty (endofline))
    ## 'endofline' option set by user.
    if (! ischar (varargin{endofline + 1}));
      error ("textread: character value required for EndOfLine");
    endif
  else
    ## Determine EOL from file.  Search for EOL candidates in first 3000 chars
    eol_srch_len = min (length (str), 3000);
    ## First try DOS (CRLF)
    if (! isempty (findstr ("\r\n", str(1 : eol_srch_len))))
      eol_char = "\r\n";
    ## Perhaps old Macintosh? (CR)
    elseif (! isempty (findstr ("\r", str(1 : eol_srch_len))))
      eol_char = "\r";
    ## Otherwise, use plain UNIX (LF)
    else
      eol_char = "\n";
    endif
    ## Set up default endofline param value
    varargin(end+1:end+2) = {'endofline', eol_char};
  endif

  ## Set up default whitespace param value if needed
  if (isempty (find (strcmpi ('whitespace', varargin))))
    varargin(end+1:end+2) = {'whitespace', " \b\t"};
  endif

  ## Call strread to make it do the real work
  [varargout{1:max (nargout, 1)}] = strread (str, format, varargin {:});

endfunction


%!test
%! f = tmpnam();
%! d = rand (5, 3);
%! dlmwrite (f, d, 'precision', '%5.2f');
%! [a, b, c] = textread (f, "%f %f %f", "delimiter", ",", "headerlines", 3);
%! unlink(f);
%! assert (a, d(4:5, 1), 1e-2);
%! assert (b, d(4:5, 2), 1e-2);
%! assert (c, d(4:5, 3), 1e-2);

%% Test input validation
%!error textread ()
%!error textread (1)
%!error <arguments must be strings> textread (1, '%f')
%!error <arguments must be strings> textread ("fname", 1)

