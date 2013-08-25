## Copyright (C) 2012-2013 Erik Kjellson
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
## @deftypefn  {Function File} {@var{A} =} importdata (@var{fname})
## @deftypefnx {Function File} {@var{A} =} importdata (@var{fname}, @var{delimiter})
## @deftypefnx {Function File} {@var{A} =} importdata (@var{fname}, @var{delimiter}, @var{header_rows})
## @deftypefnx {Function File} {[@var{A}, @var{delimiter}] =} importdata (@dots{})
## @deftypefnx {Function File} {[@var{A}, @var{delimiter}, @var{header_rows}] =} importdata (@dots{})
## Importing data from file.
##
## Importing the contents of file @var{fname} into workspace.
##
## Input parameters:
##
## @itemize
## @item @var{fname}
## The file name for the file to import.
## 
## @item @var{delimiter}
## The character separating columns of data.  Use @code{\t} for tab.
## (Only valid for ascii files)
##
## @item @var{header_rows}
## Number of header rows before the data begins.  (Only valid for ascii files)
## @end itemize
##
## Different file types are supported:
##
## @itemize
## @item ASCII table
##
## Importing ASCII table using the specified number of header rows and
## the specified delimiter.
##
## @item Image file
##
## @item @sc{matlab} file
##
## @item Spreadsheet files (depending on external software)
##
## @item WAV file
##
## @end itemize
##
## @seealso{textscan, dlmread, csvread, load}
## @end deftypefn

## Author: Erik Kjellson <erikiiofph7@users.sourceforge.net>

function [output, delimiter, header_rows] = importdata (varargin)

  ## Default values
  fname   = "";
  delimiter  = "";
  header_rows = -1;

  ##########

  ## Check input arguments

  if (nargin < 1)
    print_usage ();
  endif

  fname = varargin{1};
  ## Check that the file name really is a string
  if (! ischar (fname))
    error ("importdata: file name needs to be a string");
  endif
  if ( strcmpi (fname, "-pastespecial"))
    error ("importdata: option -pastespecial not implemented");
  endif

  if (nargin > 1)
    delimiter = varargin{2};
    ## Check that the delimiter really is a string
    if (!ischar (delimiter))
      error("importdata: delimiter needs to be a character");
    endif
    if (length (delimiter) > 1 && !strcmpi (delimiter, "\\t"))
      error("importdata: delimiter cannot be longer than 1 character");
    endif
    if (strcmpi (delimiter, "\\"))
      delimiter = "\\\\";
    endif
  endif

  if (nargin > 2)
    header_rows = varargin{3};
    if (!isnumeric (header_rows) || header_rows < 0)
      error ("importdata: number of header rows needs to be an integer number >= 0");
    endif
  endif

  if (nargin > 3)
    error ("importdata: too many input arguments");
  endif

  ##########

  ## Check file format
  ## Get the extension from the file name.
  [d n fileExt v] = fileparts (fname);
  ## Make sure file extension is in lower case.
  fileExt = lower (fileExt);

  switch (fileExt)
    case {".au", ".snd"}
      error ("importdata: not implemented for file format %s", fileExt);
    case ".avi"
      error ("importdata: not implemented for file format %s", fileExt);
    case {".bmp", ".cur", ".gif", ".hdf", ".ico", ".jpe", ".jpeg", ".jpg", ...
          ".pbm", ".pcx", ".pgm", ".png", ".pnm", ".ppm", ".ras", ...
          ".tif", ".tiff", ".xwd"}
      delimiter  = NaN;
      header_rows = 0;
      [output.cdata, output.colormap, output.alpha] = imread (fname);
    case ".mat"
      delimiter  = NaN;
      header_rows = 0;
      output = load (fname);
    case {".wk1", ".xls", ".xlsx", ".dbf", ".pxl"}
      ## If there's no Excel file support simply fall back to unimplemented.m
      output = xlsread (fname);
    case {".ods", ".sxc", ".fods", ".uos", ".xml"}
      ## unimplemented.m only knows ML functions; odsread isn't one but is in OF
      try
        output = odsread (fname);
      catch
        ## Fall back to unimplemented.m.
        output = xlsread (fname);
      end_try_catch
    case {".wav", ".wave"}
      delimiter  = NaN;
      header_rows = 0;
      [output.data, output.fs] = wavread (fname);
    otherwise
      ## Assume the file is in ascii format.
      [output, delimiter, header_rows]  = ...
          importdata_ascii (fname, delimiter, header_rows);
  endswitch

  ## If there are any empty fields in the output structure, then remove them
  if (isstruct (output) && length (output) == 1)
    fields = fieldnames (output);
    for i=1:length (fields)
      if (isempty (getfield (output, fields{i})))
        output = rmfield (output, fields{i});
      endif
    endfor

    ## If only one field is left, replace the structure with the field,
    ## i.e. output = output.onlyFieldLeft

    ## Update the list of fields
    fields = fieldnames (output);
    if (length (fields) == 1)
      output = getfield (output, fields{1});
    endif
  endif
endfunction


########################################

function [output, delimiter, header_rows] = ...
      importdata_ascii (fname, delimiter, header_rows)

  ## Define the fields in the output structure so that the order will be
  ## correct.

  output.data       = [];
  output.textdata   = {};
  output.rowheaders = {};
  output.colheaders = {};

  [fid, msg] = fopen (fname, "r");
  if (fid == -1)
    error (msg);
  endif

  header_rows_estimate = 0;
  header_cols_estimate = 0;
  while (1)
    ## For the first few rows, get one row at a time as opposed to reading
    ## the whole file.
    row = fgetl (fid);

    ## If no delimiter determined yet, make a guess.
    if (isempty (delimiter))
      ## The tab will take precendence.
      if ~isempty (regexp (row, "[\\t]", "once"))
        delimiter = "\t";
      endif

      ## Then a comma.
      if ~isempty (regexp (row, ",", "once"))
        delimiter = ",";
      endif

      ## Next, a space will be used, but perhaps should check
      ## for character string indicators like ' or " in a more
      ## robust version.
      if ~isempty (regexp (row, " ", "once"))
        delimiter = " ";
      endif
    elseif (strcmp (delimiter, '\t'))
      ## When delimiter = "\\t" convert it to a tab, done for Matlab compatibility.
      delimiter = "\t";
    endif

    if (delimiter == " ")
      row_entries = regexp (strtrim (row), " {1,}", "split");
    else
      row_entries = regexp (row, delimiter, "split");
    endif
    row_data = str2double (row_entries);
    if (header_rows < 0 && all (isnan (row_data)) || ...
        header_rows >= 0 && header_rows_estimate < header_rows)
      header_rows_estimate++;
      output.textdata{end + 1, 1} = row;
      output.colheaders = row_entries;
    else
      c = find (! isnan (row_data));
      header_cols_estimate = c(1) - 1;

      ## The number of header rows and header columns is now known.
      break;
    endif

  endwhile

  if (header_rows < 0)
    header_rows = header_rows_estimate;
  endif
  header_cols = header_cols_estimate;

  fclose (fid);

  ## If it is important to remove white space at the front of rows, it is
  ## probably more efficient to read in the character data stream, modify
  ## it using regexp index manipulations then send that to a temporary file
  ## and call dlmread() on the temporary file.
  if (delimiter == " ")
    file_content = fileread (fname);

    ## Convert all carriage returns to line feeds for simplicity.
    file_content (regexp (file_content, "\r")) = "\n";

    ## Remove all consecutive space characters
    lidx = logical (ones (size (file_content)));
    widx = regexp (file_content, "[ ]");
    lidx (widx ([false (diff (widx) == 1)])) = false;
    file_content = file_content(lidx);

    ## Remove all spaces before and after a newline
    lidx = logical (ones (size (file_content)));
    lidx (regexp (file_content, " \n")) = false;
    lidx (regexp (file_content, "\n ") + 1) = false;
    file_content = file_content(lidx);

    ## Save to temporary file and continue by using the new name
    fname  = tmpnam ();
    fid = fopen (fname, "w");
    fputs (fid, file_content);
    fclose (fid);
  endif

  ## Now let the efficient built-in routine do the bulk of the work.
  output.data = dlmread (fname, delimiter, header_rows, header_cols, "emptyvalue", NaN);

  nanidx = isnan (output.data);
  if (header_cols > 0)
    nanidx = [(true (size (nanidx, 1), header_cols)) nanidx];
  endif
  if (any (nanidx (:)))

    file_content = fileread (fname);

    ## Convert all carriage returns to line feeds for simplicity.
    file_content (regexp (file_content, "\r")) = "\n";

    ## Remove all consecutive space characters
    lidx = logical (ones (size (file_content)));
    widx = regexp (file_content, "[ ]");
    lidx (widx ([false (diff (widx) == 1)])) = false;
    file_content = file_content(lidx);

    ## Remove all lines consisting of a single white space or nothing.
    lidx = logical (ones (size (file_content)));
    nidx = regexp (file_content, "\n");
    lidx (nidx ([false (diff (nidx) == 1)])) = false;
    n_nidx = nidx([false (diff (nidx) == 2)]);
    n_nidx = n_nidx (isspace (file_content (n_nidx - 1)));
    lidx (n_nidx) = false;
    lidx (n_nidx - 1) = false;
    file_content = file_content(lidx);

    rowend = regexp (file_content, "\n");
    rowstart = [0 rowend] + 1;
    rowstart = rowstart (header_rows + 1:end);
    rowend = [rowend length(file_content)];
    rowend = rowend (header_rows + 1:end);
    rows_to_process = find (any (nanidx, 2));
    for i = 1:length (rows_to_process)
      r = rows_to_process (i);
      row_cells = regexp (file_content (rowstart (r):rowend (r)), delimiter, "split");
      output.textdata (end + 1:end + sum (nanidx (r,:)), 1) = row_cells (nanidx (r,:));
      if (header_cols)
        output.rowheaders (end + 1, :) = row_cells (1:header_cols);
      endif
    endfor
  endif

  ## Final cleanup to satisfy output configuration
  if (all (cellfun ("isempty", output.textdata)))
    output = output.data;
  elseif (! isempty (output.rowheaders) && ! isempty (output.colheaders))
    output = struct ('data', {output.data}, 'textdata', {output.textdata});
  endif

endfunction


########################################

%!test
%! ## Comma separated values
%! A = [3.1 -7.2 0; 0.012 6.5 128];
%! fn  = tmpnam ();
%! fid = fopen (fn, "w");
%! fputs (fid, "3.1,-7.2,0\n0.012,6.5,128");
%! fclose (fid);
%! [a1,d1,h1] = importdata (fn, ",");
%! [a2,d2,h2] = importdata (fn);
%! unlink (fn);
%! assert (a1, A);
%! assert (d1, ",");
%! assert (h1, 0);
%! assert (a2, A);
%! assert (d2, ",");
%! assert (h2, 0);

%!test
%! ## Tab separated values
%! A = [3.1 -7.2 0; 0.012 6.5 128];
%! fn  = tmpnam ();
%! fid = fopen (fn, "w");
%! fputs (fid, "3.1\t-7.2\t0\n0.012\t6.5\t128");
%! fclose (fid);
%! [a1,d1,h1] = importdata (fn, "\t");
%! [a2,d2,h2] = importdata (fn);
%! unlink (fn);
%! assert (a1, A);
%! assert (d1, "\t");
%! assert (h1, 0);
%! assert (a2, A);
%! assert (d2, "\t");
%! assert (h2, 0);

%!test
%! ## Space separated values, using multiple spaces to align in columns.
%! A = [3.1 -7.2 0; 0.012 6.5 128];
%! fn  = tmpnam ();
%! fid = fopen (fn, "w");
%! fprintf (fid, "%10.3f %10.3f %10.3f\n", A');
%! fclose (fid);
%! [a1,d1,h1] = importdata (fn, " ");
%! [a2,d2,h2] = importdata (fn);
%! unlink (fn);
%! assert (a1, A);
%! assert (d1, " ");
%! assert (h1, 0);
%! assert (a2, A);
%! assert (d2, " ");
%! assert (h2, 0);

%!test
%! ## Header text
%! A.data = [3.1 -7.2 0; 0.012 6.5 128];
%! A.textdata = {"This is a header row."; ...
%!               "this row does not contain any data, but the next one does."};
%! A.colheaders = A.textdata (2);
%! fn  = tmpnam ();
%! fid = fopen (fn, "w");
%! fprintf (fid, "%s\n", A.textdata{:});
%! fputs (fid, "3.1\t-7.2\t0\n0.012\t6.5\t128");
%! fclose (fid);
%! [a,d,h] = importdata (fn, '\t');
%! unlink (fn);
%! assert (a, A);
%! assert (d, "\t");
%! assert (h, 2);

%!test
%! ## Column headers, only last row is returned in colheaders
%! A.data = [3.1 -7.2 0; 0.012 6.5 128];
%! A.textdata = {"Label1\tLabel2\tLabel3";
%!               "col1\tcol2\tcol3"};
%! A.colheaders = {"col1", "col2", "col3"};
%! fn  = tmpnam ();
%! fid = fopen (fn, "w");
%! fprintf (fid, "%s\n", A.textdata{:});
%! fputs (fid, "3.1\t-7.2\t0\n0.012\t6.5\t128");
%! fclose (fid);
%! [a,d,h] = importdata (fn, '\t');
%! unlink (fn);
%! assert (a, A);
%! assert (d, "\t");
%! assert (h, 2);

%!test
%! ## Row headers
%! A.data = [3.1 -7.2 0; 0.012 6.5 128];
%! A.textdata = {"row1"; "row2"};
%! A.rowheaders = A.textdata;
%! fn  = tmpnam ();
%! fid = fopen (fn, "w");
%! fputs (fid, "row1\t3.1\t-7.2\t0\nrow2\t0.012\t6.5\t128");
%! fclose (fid);
%! [a,d,h] = importdata (fn, '\t');
%! unlink (fn);
%! assert (a, A);
%! assert (d, "\t");
%! assert (h, 0);

%!test
%! ## Row/Column headers and Header Text
%! A.data = [3.1 -7.2 0; 0.012 6.5 128];
%! A.textdata = {"This is introductory header text"
%!               "      col1 col2 col3"
%!               "row1"
%!               "row2"};
%! fn  = tmpnam ();
%! fid = fopen (fn, "w");
%! fprintf (fid, "%s\n", A.textdata{1:2});
%! fputs (fid, "row1\t3.1\t-7.2\t0\nrow2\t0.012\t6.5\t128");
%! fclose (fid);
%! [a,d,h] = importdata (fn, '\t');
%! unlink (fn);
%! assert (a, A);
%! assert (d, "\t");
%! assert (h, 2);

%!test
%! ## Ignore empty rows containing only spaces
%! A = [3.1 -7.2 0; 0.012 6.5 128];
%! fn  = tmpnam ();
%! fid = fopen (fn, "w");
%! fprintf (fid, "%10.3f %10.3f %10.3f\n", A(1,:));
%! fputs (fid, "      ");
%! fprintf (fid, "%10.3f %10.3f %10.3f\n", A(2,:));
%! fclose (fid);
%! [a,d,h] = importdata (fn, " ");
%! unlink (fn);
%! assert (a, A);
%! assert (d, " ");
%! assert (h, 0);

%!test
%! ## Exponentials
%! A = [3.1 -7.2 0; 0.012 6.5 128];
%! fn  = tmpnam ();
%! fid = fopen (fn, "w");
%! fputs (fid, "+3.1e0\t-72E-1\t0\n12e-3\t6.5\t128");
%! fclose (fid);
%! [a,d,h] = importdata (fn, '\t');
%! unlink (fn);
%! assert (a, A);
%! assert (d, "\t");
%! assert (h, 0);

%!test
%! ## Complex numbers
%! A = [3.1 -7.2 0-3.4i; 0.012 -6.5+7.2i 128];
%! fn  = tmpnam ();
%! fid = fopen (fn, "w");
%! fputs (fid, "3.1\t-7.2\t0-3.4i\n0.012\t-6.5+7.2i\t128");
%! fclose (fid);
%! [a,d,h] = importdata (fn, '\t');
%! unlink (fn);
%! assert (a, A);
%! assert (d, "\t");
%! assert (h, 0);

## FIXME: Currently commented out (8/23/13) because I can't determine whether
## Matlab processes exceptional values.
%!#test
%! ## Exceptional values (Inf, NaN, NA)
%! A = [3.1 Inf NA; -Inf NaN 128];
%! fn  = tmpnam ();
%! fid = fopen (fn, "w");
%! fputs (fid, "3.1\tInf\tNA\n-Inf\tNaN\t128");
%! fclose (fid);
%! [a,d,h] = importdata (fn, '\t');
%! unlink (fn);
%! assert (a, A);
%! assert (d, "\t");
%! assert (h, 0);

%!test
%! ## Missing values
%! A = [3.1 NaN 0; 0.012 6.5 128];
%! fn  = tmpnam ();
%! fid = fopen (fn, "w");
%! fputs (fid, "3.1\t\t0\n0.012\t6.5\t128");
%! fclose (fid);
%! [a,d,h] = importdata (fn, '\t');
%! unlink (fn);
%! assert (a, A);
%! assert (d, "\t");
%! assert (h, 0);

%!test
%! ## CRLF for line breaks
%! A = [3.1 -7.2 0; 0.012 6.5 128];
%! fn  = tmpnam ();
%! fid = fopen (fn, "w");
%! fputs (fid, "3.1\t-7.2\t0\r\n0.012\t6.5\t128");
%! fclose (fid);
%! [a,d,h] = importdata (fn, '\t');
%! unlink (fn);
%! assert (a, A);
%! assert (d, "\t");
%! assert (h, 0);

%!test
%! ## CR for line breaks
%! A = [3.1 -7.2 0; 0.012 6.5 128];
%! fn  = tmpnam ();
%! fid = fopen (fn, "w");
%! fputs (fid, "3.1\t-7.2\t0\r0.012\t6.5\t128");
%! fclose (fid);
%! [a,d,h] = importdata (fn, '\t');
%! unlink (fn);
%! assert (a, A);
%! assert (d, "\t");
%! assert (h, 0);

