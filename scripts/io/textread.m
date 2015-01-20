## Copyright (C) 2009-2013 Eric Chassande-Mottin, CNRS (France)
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
## @item @qcode{"headerlines"}:
## The first @var{value} number of lines of @var{filename} are skipped.
##
## @item @qcode{"endofline"}:
## Specify a single character or @qcode{"\r\n"}.  If no value is given, it
## will be inferred from the file.  If set to "" (empty string) EOLs are
## ignored as delimiters.
## @end itemize
##
## The optional input @var{n} specifies the number of data lines to read; in
## this sense it differs slightly from the format repeat count in strread.
##
## If the format string is empty (not: omitted) and the file contains only
## numeric data (excluding headerlines), textread will return a rectangular
## matrix with the number of columns matching the number of numeric fields on
## the first data line of the file.  Empty fields are returned as zero values.
##
## @seealso{strread, load, dlmread, fscanf, textscan}
## @end deftypefn

function varargout = textread (filename, format = "%f", varargin)

  BUFLENGTH = 4096;       # Read buffer to speed up processing @var{n}

  ## Check input
  if (nargin < 1)
    print_usage ();
  endif

  if (! ischar (filename) || ! ischar (format))
    error ("textread: FILENAME and FORMAT arguments must be strings");
  endif

  if (! isempty (varargin) && isnumeric (varargin{1}))
    nlines = varargin{1};
  else
    nlines = Inf;
  endif
  if (nlines < 1)
    printf ("textread: N = 0, no data read\n");
    varargout = cell (1, nargout);
    return;
  endif

  ## Read file
  fid = fopen (filename, "r");
  if (fid == -1)
    error ("textread: could not open '%s' for reading", filename);
  endif

  ## Skip header lines if requested
  headerlines = find (strcmpi (varargin, "headerlines"), 1);
  if (! isempty (headerlines))
    ## Beware of missing or wrong headerline value
    if (headerlines  == numel (varargin)
       || ! isnumeric (varargin{headerlines + 1}))
      error ("missing or illegal value for 'headerlines'" );
    endif
    ## Avoid conveying floats to fskipl
    varargin{headerlines + 1} = round (varargin{headerlines + 1});
    ## Beware of zero valued headerline, fskipl would skip to EOF
    if (varargin{headerlines + 1} > 0)
      fskipl (fid, varargin{headerlines + 1});
      varargin(headerlines:headerlines+1) = [];
      nargin = nargin - 2;
    elseif (varargin{headerlines + 1} < 0)
      warning ("textread: negative headerline value ignored");
    endif
  endif
  st_pos = ftell (fid);

  ## Read a first file chunk. Rest follows after endofline processing
  [str, count] = fscanf (fid, "%c", BUFLENGTH);
  if (isempty (str) || count < 1)
    warning ("textread: empty file");
    varargout = cell (1, nargout);
    return;
  endif

  endofline = find (strcmpi (varargin, "endofline"), 1);
  if (! isempty (endofline))
    ## 'endofline' option set by user.
    if (! ischar (varargin{endofline + 1}));
      error ("character value required for EndOfLine");
    endif
  else
    ## Determine EOL from file.  Search for EOL candidates in first BUFLENGTH chars
    eol_srch_len = min (length (str), BUFLENGTH);
    ## First try DOS (CRLF)
    if (! isempty (strfind (str(1 : eol_srch_len), "\r\n")))
      eol_char = "\r\n";
    ## Perhaps old Macintosh? (CR)
    elseif (! isempty (strfind (str(1 : eol_srch_len), "\r")))
      eol_char = "\r";
    ## Otherwise, use plain *nix (LF)
    else
      eol_char = "\n";
    endif
    ## Set up default endofline param value
    varargin(end+1:end+2) = {"endofline", eol_char};
  endif

  ## Now that we know what EOL looks like, we can process format_repeat_count.
  ## FIXME The below isn't ML-compatible: counts lines, not format string uses
  if (isfinite (nlines) && (nlines > 0))
    l_eol_char = length (eol_char);
    eoi = findstr (str, eol_char);
    n_eoi = length (eoi);
    nblks = 0;
    ## Avoid slow repeated str concatenation, first seek requested end of data
    while (n_eoi < nlines && count == BUFLENGTH)
      [nstr, count] = fscanf (fid, "%c", BUFLENGTH);
      if (count > 0)
        ## Watch out for multichar EOL being missed across buffer boundaries
        if (l_eol_char > 1)
          str = [str(end - length (eol_char) + 2 : end) nstr];
        else
          str = nstr;
        endif
        eoi = findstr (str, eol_char);
        n_eoi += numel (eoi);
        ++nblks;
      endif
    endwhile
    ## Found EOL delimiting last requested line. Compute ptr (incl. EOL)
    if (isempty (eoi))
      printf ("textread: format repeat count specified but no endofline found\n");
      eoi_pos = nblks * BUFLENGTH + count;
    else
      eoi_pos = (nblks * BUFLENGTH) + eoi(end + min (nlines, n_eoi) - n_eoi);
    endif
    fseek (fid, st_pos, "bof");
    str = fscanf (fid, "%c", eoi_pos);
  else
    fseek (fid, st_pos, "bof");
    str = fread (fid, "char=>char").';
  endif
  fclose (fid);

  ## Set up default whitespace param value if needed
  if (isempty (find (strcmpi ("whitespace", varargin))))
    varargin(end+1:end+2) = {"whitespace", " \b\t"};
  endif

  ## Call strread to make it do the real work
  [varargout{1:max (nargout, 1)}] = strread (str, format, varargin {:});

  ## Hack to concatenate/reshape numeric output into 2D array (undocumented ML)
  ## In ML this only works in case of an empty format string
  if (isempty (format))
    ## Get number of fields per line.
    ## 1. Get eol_char position
    iwhsp = find (strcmpi ("whitespace", varargin));
    whsp = varargin{iwhsp + 1};
    idx = regexp (str, eol_char, "once");
    ## 2. Get first data line til EOL. Avoid corner case of just one line
    if (! isempty (idx))
      str = str(1:idx-1);
    endif
    idelimiter = find (strcmpi (varargin, "delimiter"), 1);
    if (isempty (idelimiter))
      ## Assume delimiter = whitespace
      ## 3A. whitespace incl. consecutive whitespace => single space
      str = regexprep (str, sprintf ("[%s]+", whsp), ' ');
      ## 4A. Remove possible leading & trailing spaces
      str = strtrim (str);
      ## 5A. Count spaces, add one to get nr of data fields per line
      ncols = numel (strfind (str, " ")) + 1;
    else
      ## 3B. Just count delimiters. FIXME: delimiters could occur in literals
      delimiter = varargin {idelimiter+1};
      ncols = numel (regexp (str, sprintf ("[%s]", delimiter))) + 1;
    endif
    ## 6. Reshape; watch out, we need a transpose
    nrows = ceil (numel (varargout{1}) / ncols);
    pad = mod (numel (varargout{1}), ncols);
    if (pad > 0)
      pad = ncols - pad;
      varargout{1}(end+1 : end+pad) = NaN;
    endif
    varargout{1} = reshape (varargout{1}, ncols, nrows)';
    ## ML replaces empty values with NaNs
    varargout{1}(find (isnan (varargout{1}))) = 0;
  endif

endfunction


%!test
%! f = tmpnam ();
%! d = rand (5, 3);
%! dlmwrite (f, d, "precision", "%5.2f");
%! [a, b, c] = textread (f, "%f %f %f", "delimiter", ",", "headerlines", 3);
%! unlink (f);
%! assert (a, d(4:5, 1), 1e-2);
%! assert (b, d(4:5, 2), 1e-2);
%! assert (c, d(4:5, 3), 1e-2);

%!test
%! f = tmpnam ();
%! d = rand (7, 2);
%! dlmwrite (f, d, "precision", "%5.2f");
%! [a, b] = textread (f, "%f, %f", "headerlines", 1);
%! unlink (f);
%! assert (a, d(2:7, 1), 1e-2);

%% Test reading 2D matrix with empty format
%!test
%! f = tmpnam ();
%! d = rand (5, 2);
%! dlmwrite (f, d, "precision", "%5.2f");
%! A = textread (f, "", "headerlines", 3);
%! unlink (f);
%! assert (A, d(4:5, :), 1e-2);

%% Read multiple lines using empty format string
%!test
%! f = tmpnam ();
%! unlink (f);
%! fid = fopen (f, "w");
%! d = rand (1, 4);
%! fprintf (fid, "  %f %f   %f  %f ", d);
%! fclose (fid);
%! A = textread (f, "");
%! unlink (f);
%! assert (A, d, 1e-6);

%% Empty format, corner case = one line w/o EOL
%!test
%! f = tmpnam ();
%! unlink (f);
%! fid = fopen (f, "w");
%! d = rand (1, 4);
%! fprintf (fid, "  %f %f   %f  %f ", d);
%! fclose (fid);
%! A = textread (f, "");
%! unlink (f);
%! assert (A, d, 1e-6);

%% Read multiple lines using empty format string, missing data (should be 0)
%!test
%! f = tmpnam ();
%! unlink (f);
%! fid = fopen (f, "w");
%! d = rand (1, 4);
%! fprintf (fid, "%f, %f, ,  %f,  %f ", d);
%! fclose (fid);
%! A = textread (f, "");
%! unlink (f);
%! assert (A, [ d(1:2) 0 d(3:4)], 1e-6);

%% Test with empty positions - ML returns 0 for empty fields
%!test
%! f = tmpnam ();
%! unlink (f);
%! fid = fopen (f, "w");
%! d = rand (1, 4);
%! fprintf (fid, ",2,,4\n5,,7,\n");
%! fclose (fid);
%! A = textread (f, "", "delimiter", ",");
%! unlink (f);
%! assert (A, [0 2 0 4; 5 0 7 0], 1e-6);

%% Another test with empty format + positions, now with more incomplete lower
%% row (must be appended with zeros to get rectangular matrix)
%!test
%! f = tmpnam ();
%! unlink (f);
%! fid = fopen (f, "w");
%! d = rand (1, 4);
%! fprintf (fid, ",2,,4\n5,\n");
%! fclose (fid);
%! A = textread (f, "", "delimiter", ",");
%! unlink (f);
%! assert (A, [0 2 0 4; 5 0 0 0], 1e-6);

%% Test input validation
%!error textread ()
%!error textread (1)
%!error <arguments must be strings> textread (1, "%f")
%!error <arguments must be strings> textread ("fname", 1)
%!error <missing or illegal value for> textread (file_in_loadpath ("textread.m"), "", "headerlines")
%!error <missing or illegal value for> textread (file_in_loadpath ("textread.m"), "", "headerlines", 'hh')
%!error <character value required for> textread (file_in_loadpath ("textread.m"), "%s", "endofline", true)

