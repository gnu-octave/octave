## Copyright (C) 2005 Søren Hauberg
## 
## This program is free software; you can redistribute it and/or modify
## it under the terms of the GNU General Public License as published by
## the Free Software Foundation; either version 2 of the License, or
## (at your option) any later version.
## 
## This program is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
## 
## You should have received a copy of the GNU General Public License
## along with this program; if not, write to the Free Software
## Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA

## -*- texinfo -*-
## @deftypefn {Function File} unzip (@var{filename})
## @deftypefnx {Function File} unzip (@var{filename}, @var{outputdir})
## Unpacks the archive @var{filename} using the unzip program.
## The resulting files are placed in the directory @var{outputdir},
## which defaults to the current directory.
## @end deftypefn

## Author: Søren Hauberg <hauberg at gmail dot com>

function files = unzip(filename, outputdir)
    if (nargin == 0)
        print_usage("unzip");
    elseif (nargin == 1)
        outputdir = ".";
    endif
    
    ## Make sure filename and outputdir are strings
    if (!ischar(filename) || !ischar(outputdir))
        error("All arguments must be strings.\n");
    endif
    
    ## Should we append ".zip" to filename?
    if (length(filename) <= 4 || !strcmp(filename(end-3:end), ".zip"))
        filename = sprintf("%s.zip", filename);
    endif

    ## Call unzip
    [output, status] = system(["unzip -o " filename " -d " outputdir]);
    if (status != 0)
        error("unzip returned the following error: %s\n", output);
    endif
    
    ## Create list of extracted files. This might depend on which version
    ## unzip that it used, although I do not know this for sure.
    if (nargout)
        files = strrep(output, "  inflating: ", "");
        files = split(files, "\n");
        # remove first and last line from the output
        files = files(2:end-1, :);
    endif
endfunction

