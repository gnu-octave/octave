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
## @deftypefn  {Function File} {@var{A} =} importdata (@var{fileName})
## @deftypefnx {Function File} {@var{A} =} importdata (@var{fileName}, @var{delimiter})
## @deftypefnx {Function File} {@var{A} =} importdata (@var{fileName}, @var{delimiter},  @var{headerRows})
## @deftypefnx {Function File} {[@var{A}, @var{delimiter}] =} importdata (...)
## @deftypefnx {Function File} {[@var{A}, @var{delimiter}, @var{headerRows}] =} importdata (...)
## Importing data from file.
##
## Importing the contents of file @var{fileName} into workspace.
##
## Input parameters:
## @table @input
## @item @var{fileName}
## The file name for the file to import.
## 
## @item @var{delimiter}
## The character separating columns of data. Use @code{\t} for tab. (Only valid for ascii files)
##
## @item @var{headerRows}
## Number of header rows before the data begins. (Only valid for ascii files)
## @end table
##
## Different file types are supported:
## @itemize
## @item Ascii table
##
## Importing ascii table using the specified number of header rows and the specified delimiter.
##
## @item Image file
##
## @item Matlab file
##
## @item Wav file
##
## @end table
##
## @seealso{textscan, dlmread, csvread, load}
## @end deftypefn

## Author: Erik Kjellson <erikiiofph7@users.sourceforge.net>
## 2012-10-16 First version


function [output, delimiter, headerRows] = importdata(varargin)

  # Default values
  fileName   = '';
  delimiter  = '';
  headerRows = -1;

  ##########

  # Check input arguments

  if (nargin < 1)
    print_usage ();
  endif

  fileName = varargin{1};
  # Check that the file name really is a string
  if not(ischar(fileName))
    error('importdata: File name needs to be a string.')
  endif
  if strcmpi(fileName, '-pastespecial')
    error('importdata: Option ''-pastespecial'' not implemented.')
  endif

  if (nargin > 1)
    delimiter = varargin{2};
    # Check that the delimiter really is a string
    if not(ischar(delimiter))
      error('importdata: Delimiter needs to be a character.')
    endif
    if ((length(delimiter) > 1) && not(strcmpi(delimiter, '\t')))
      error('importdata: Delimiter cannot be longer than 1 character.')
    endif
    if strcmpi(delimiter, '\')
      delimiter = '\\';
    endif
  endif

  if (nargin > 2)
    headerRows = varargin{3};
    if ((~isnumeric(headerRows)) || (headerRows < 0))
      error('importdata: Number of header rows needs to be an integer number >= 0.')
    endif
  endif

  if (nargin > 3)
    error('importdata: Too many input arguments.')
  endif

  ##########

  # Check file format
  # Get the extension from the file name.
  [d n fileExt v] = fileparts(fileName);
  # Make sure file extension is in lower case.
  fileExt = lower(fileExt);

  switch fileExt
    case {'.au','.snd'}
      error(['importdata: Not implemented for file format ''' fileExt '''.'])
    case '.avi'
      error(['importdata: Not implemented for file format ''' fileExt '''.'])
    case {'.bmp', '.cur', '.gif', '.hdf', '.ico', '.jpe', '.jpeg', '.jpg', '.pbm', '.pcx', '.pgm', '.png', '.pnm', '.ppm', '.ras', '.tif', '.tiff', '.xwd'}
      delimiter  = NaN;
      headerRows = 0;
      [output.cdata, output.colormap, output.alpha] = imread(fileName);
    case '.mat'
      delimiter  = NaN;
      headerRows = 0;
      output = load(fileName);
    case '.wk1'
      error(['importdata: Not implemented for file format ''' fileExt '''.'])
    case {'.xls', '.xlsx'}
  #FIXME: implement Excel import.
      error(['importdata: Not implemented for file format ''' fileExt '''.'])
    case {'.wav', '.wave'}
      delimiter  = NaN;
      headerRows = 0;
      [output.data, output.fs] = wavread(fileName);
    otherwise
      # Assume the file is in ascii format.
      [output, delimiter, headerRows] = importdata_ascii(fileName, delimiter, headerRows);
  endswitch

  # If there are any empty fields in the output structure, then remove them
  if (isstruct(output) && (length(output) == 1))
    fields = fieldnames(output);
    for i=1:length(fields)
      if isempty(getfield(output, fields{i}))
        output = rmfield(output, fields{i});
      endif
    endfor

    # If only one field is left, replace the structure with the field, i.e. output = output.onlyFieldLeft
    # Update the list of fields
    fields = fieldnames(output);
    if (length(fields) == 1)
      output = getfield(output, fields{1});
    endif
  endif
endfunction


########################################

function [output, delimiter, headerRows] = importdata_ascii(fileName, delimiter, headerRows)

  # Define the fields in the output structure so that the order will be correct.
  output.data       = [];
  output.textdata   = [];
  output.rowheaders = [];
  output.colheaders = [];

  # Read file into string and count the number of header rows

  fileContent = fileread(fileName);
  # The characters need to be in a row vector instead of a column vector to be recognized as a proper string.
  if (size(fileContent,2) == 1)
    fileContent = fileContent';
  endif

  # Split the file into rows (using \r\n or \n as delimiters between rows).
  fileContentRows = regexp(fileContent, '\r?\n', 'split');

  #FIXME: guess delimiter, if it isn't defined
  if (isempty(delimiter))
    error('importdata: Guessing delimiter is not implemented yet, you have to specify it.')
  endif

  #FIXME: A more intelligent way to count number of header rows. This is needed e.g. when delimiter=' ' and the header contains spaces...

  # If number of header rows is undefined, then count the number of header rows by step through row by row and look for the delimiter.
  # Assume that the header can't contain any delimiter.
  if (headerRows < 0)
    headerRows = 0;
    for i=1:length(fileContentRows)
      if (isempty(regexp(fileContentRows{i}, delimiter, 'once')))
        headerRows++;
      else
        # Data part has begun and therefore no more header rows can be found
        break;
      endif
    endfor
  endif

  # Put the header rows in output.textdata.
  if (headerRows > 0)
    output.textdata   = fileContentRows(1:headerRows)';
  endif

  # If space is the delimiter, then remove spaces in the beginning of each data row.
  if strcmpi(delimiter, ' ')
    for i=(headerRows+1):length(fileContentRows)
      # strtrim does not only remove the leading spaces but also the tailing spaces, but that doesn't really matter.
      fileContentRows{i} = strtrim(fileContentRows{i});
    endfor
  endif

  # Remove empty data rows. Go through them backwards so that you wont get out of bounds.
  for i=length(fileContentRows):-1:(headerRows+1)
    if (length(fileContentRows{i}) < 1)
      fileContentRows = [fileContentRows(1:i-1), fileContentRows(i+1:length(fileContentRows))];
    endif
  endfor

  # Count the number of data columns.
  # If there are different number of columns, use the greatest value.
  dataColumns = 0;
  delimiterPattern = delimiter;
  # If space is the delimiter, then multiple spaces should count as ONE delimiter. Also ignore leading spaces.
  if (strcmpi(delimiter, ' '))
    delimiterPattern = ' +';
  endif
  for i=(headerRows+1):length(fileContentRows)
    dataColumns = max(dataColumns, length(regexp(fileContentRows{i}, delimiterPattern, 'split')));
  endfor


  # Go through the data and put it in either output.data or output.textdata depending on if it is numeric or not.
  output.data = NaN(length(fileContentRows)-headerRows, dataColumns);
  for i=(headerRows+1):length(fileContentRows)
    # Only use the row if it contains anything other than white-space characters.
    if (length(regexp(fileContentRows{i}, '\S','match')) > 0)
      rowData = regexp(fileContentRows{i}, delimiterPattern, 'split');
      for j=1:length(rowData)
        # Try to convert the column to a number, if it works put it in output.data, otherwise in output.textdata
        if (length(str2num(rowData{j})) > 0)
          output.data((i-headerRows),j) = str2num(rowData{j});
        else
          output.textdata{i,j} = rowData{j};
        endif
      endfor
    endif
  endfor

  # Check wether rowheaders or colheaders should be used
  if ((headerRows == dataColumns) && (size(output.textdata,2) == 1))
    output.rowheaders = output.textdata;
  elseif (size(output.textdata,2) == dataColumns)
    output.colheaders = output.textdata(end,:);
  endif

  # When delimiter = '\t' convert it to a tab, as is done in the Matlab version
  if (strcmpi(delimiter, '\t'))
    delimiter = sprintf('\t');
  endif
endfunction
