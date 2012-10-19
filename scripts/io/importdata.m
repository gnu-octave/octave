## Copyright (C) 2012 Erik Kjellson <erikiiofph7@users.sourceforge.net>
##
## This program is free software; you can redistribute it and/or modify
## it under the terms of the GNU General Public License as published by
## the Free Software Foundation; either version 3 of the License, or
## (at your option) any later version.
##
## This program is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with this program; if not, see <http://www.gnu.org/licenses/>.

## -*- texinfo -*-
## @deftypefn  {Function File} {@var{A} =} importdata (@var{fname})
## @deftypefnx {Function File} {@var{A} =} importdata (@var{fname}, @var{delimiter})
## @deftypefnx {Function File} {@var{A} =} importdata (@var{fname}, @var{delimiter},  @var{header_rows})
## @deftypefnx {Function File} {[@var{A}, @var{delimiter}] =} importdata (...)
## @deftypefnx {Function File} {[@var{A}, @var{delimiter}, @var{header_rows}] =} importdata (...)
## Importing data from file.
##
## Importing the contents of file @var{fname} into workspace.
##
## Input parameters:
## @itemize
## @item @var{fname}
## The file name for the file to import.
## 
## @item @var{delimiter}
## The character separating columns of data. Use @code{\t} for tab.
## (Only valid for ascii files)
##
## @item @var{header_rows}
## Number of header rows before the data begins. (Only valid for ascii files)
## @end itemize
##
## Different file types are supported:
## @itemize
## @item Ascii table
##
## Importing ascii table using the specified number of header rows and
## the specified delimiter.
##
## @item Image file
##
## @item @sc{Matlab} file
##
## @item Wav file
##
## @end itemize
##
## @seealso{textscan, dlmread, csvread, load}
## @end deftypefn

## Author: Erik Kjellson <erikiiofph7@users.sourceforge.net>
## 2012-10-16 First version


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
    if (length (delimiter) > 1 && !strcmpi (delimiter, "\t"))
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

  switch fileExt
    case {".au", ".snd"}
      error (sprintf ("importdata: not implemented for file format %s", 
                      fileExt));
    case ".avi"
      error (sprintf ("importdata: not implemented for file format %s", 
                      fileExt));
    case {".bmp", ".cur", ".gif", ".hdf", ".ico", ".jpe", ".jpeg", ".jpg", \
          ".pbm", ".pcx", ".pgm", ".png", ".pnm", ".ppm", ".ras", \
          ".tif", ".tiff", ".xwd"}
      delimiter  = NaN;
      header_rows = 0;
      [output.cdata, output.colormap, output.alpha] = imread (fname);
    case ".mat"
      delimiter  = NaN;
      header_rows = 0;
      output = load (fname);
    case ".wk1"
      error (sprintf ("importdata: not implemented for file format %s", 
                      fileExt));
    case {".xls", ".xlsx"}
      ## FIXME: implement Excel import.
      error (sprintf ("importdata: not implemented for file format %s", 
                      fileExt));
    case {".wav", ".wave"}
      delimiter  = NaN;
      header_rows = 0;
      [output.data, output.fs] = wavread (fname);
    otherwise
      ## Assume the file is in ascii format.
      [output, delimiter, header_rows]  = \
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

function [output, delimiter, header_rows] = \
      importdata_ascii (fname, delimiter, header_rows)

  ## Define the fields in the output structure so that the order will be
  ## correct.

  output.data       = [];
  output.textdata   = [];
  output.rowheaders = [];
  output.colheaders = [];

  ## Read file into string and count the number of header rows
  file_content = fileread (fname);

  ## The characters need to be in a row vector instead of a column
  ## vector to be recognized as a proper string.
  file_content = file_content(:)';

  ## Split the file into rows (using \r\n or \n as delimiters between rows).
  file_content_rows = regexp (file_content, "\r?\n", "split");

  ## FIXME: guess delimiter, if it isn't defined
  if (isempty (delimiter))
    error ("importdata: Guessing delimiter is not implemented yet, you have to specify it.");
  endif

  ## FIXME: A more intelligent way to count number of header rows. This
  ## is needed e.g. when delimiter=' ' and the header contains spaces...

  ## If number of header rows is undefined, then count the number of
  ## header rows by step through row by row and look for the delimiter.
  ## Assume that the header can't contain any delimiter.
  if (header_rows < 0)
    header_rows = 0;
    for i=1:length (file_content_rows)
      if (isempty (regexp(file_content_rows{i}, delimiter, "once")))
        header_rows++;
      else
        ## Data part has begun and therefore no more header rows can be
        ## found
        break;
      endif
    endfor
  endif

  ## Put the header rows in output.textdata.
  if (header_rows > 0)
    output.textdata   = file_content_rows (1:header_rows)';
  endif

  ## If space is the delimiter, then remove spaces in the beginning of
  ## each data row.
  if (strcmpi (delimiter, " "))
    for i=(header_rows+1):length (file_content_rows)
      ## strtrim does not only remove the leading spaces but also the
      ## tailing spaces, but that doesn't really matter.
      file_content_rows{i} = strtrim (file_content_rows{i});
    endfor
  endif

  ## Remove empty data rows. Go through them backwards so that you wont
  ## get out of bounds.
  for i=length (file_content_rows):-1:(header_rows + 1)
    if (length (file_content_rows{i}) < 1)
      file_content_rows = [file_content_rows(1:i-1), \
                           file_content_rows(i+1:length(file_content_rows))];
    endif
  endfor

  ## Count the number of data columns. If there are different number of
  ## columns, use the greatest value.
  data_columns = 0;
  delimiter_pattern = delimiter;
  ## If space is the delimiter, then multiple spaces should count as ONE
  ## delimiter. Also ignore leading spaces.
  if (strcmpi (delimiter, " "))
    delimiter_pattern = ' +';
  endif
  for i=(header_rows+1):length(file_content_rows)
    data_columns = max(data_columns, 
                       length (regexp (file_content_rows{i},
                                       delimiter_pattern, "split")));
  endfor


  ## Go through the data and put it in either output.data or
  ## output.textdata depending on if it is numeric or not.
  output.data = NaN (length (file_content_rows) - header_rows, data_columns);
  for i=(header_rows+1):length(file_content_rows)
    ## Only use the row if it contains anything other than white-space
    ## characters.
    if (length (regexp (file_content_rows{i}, "\S","match")) > 0)
      row_data = regexp (file_content_rows{i}, delimiter_pattern, "split");

      for j=1:length(row_data)
        ## Try to convert the column to a number, if it works put it in
        ## output.data, otherwise in output.textdata
        if (length(str2num(row_data{j})) > 0)
          output.data ((i-header_rows), j) = str2num (row_data{j});
        else
          output.textdata{i,j} = row_data{j};
        endif
      endfor

    endif
  endfor

  ## Check wether rowheaders or colheaders should be used
  if ((header_rows == data_columns) && (size (output.textdata, 2) == 1))
    output.rowheaders = output.textdata;
  elseif (size (output.textdata, 2) == data_columns)
    output.colheaders = output.textdata(end,:);
  endif

  ## When delimiter = "\\t" convert it to a tab, as is done in the Matlab
  ## version
  if (strcmpi (delimiter, "\\t"))
    delimiter = "\t";
  endif
endfunction
