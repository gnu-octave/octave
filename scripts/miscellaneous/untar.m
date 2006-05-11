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
## @deftypefn {Function File} untar (@var{filename})
## @deftypefnx {Function File} untar (@var{filename}, @var{outputdir})
## Unpacks the archive @var{filename} using the tar program.
## The resulting files are placed in the directory @var{outputdir},
## which defaults to the current directory.
## The used uncompressing algorithm depends on the extension of
## @var{filename}. If @var{filename} doesn't have an extension a list
## of supported extensions is tried.
## @end deftypefn

## Author: Søren Hauberg <hauberg at gmail dot com>

# XXX: how do we support returning the extracted filenames?
# If strip was sane, we could simply return strip(output),
# which is what is currently done. However this is not
# Matlab compatible, since Matlab returns a cellstr and
# this implementation returns a matrix.
function out = untar(filename, outputdir)
    if (nargin == 0)
        print_usage("untar");
    elseif (nargin == 1)
        outputdir = ".";
    endif
    
    ## If filename is a sq_string, convert it to a string
    filename = sprintf("%s", filename);
    
    ## XXX: what about "" (i.e. no extension) ?
    supported_extensions = {"tar", "tar.gz", "tgz", "tar.bz", "tar.bz2", "tbz", "tbz2"};

    ## Make sure filename and outputdir are strings
    if (!ischar(filename) || !ischar(outputdir))
        error("All arguments must be strings.\n");
    endif
    
    ## Get extension of filename
    dots = find(filename == ".");
    for dot = dots
        curext = filename(dot+1:end);
        if (any(strcmp(curext, supported_extensions)))
            ext = curext;
            break;
        endif
    endfor

    ## If no extension was found, iterate over possible extensions
    ## and try to append them to filename
    if (!exist("ext", "var"))
        for i = 1:length(supported_extensions)
            curext = supported_extensions{i};
            if (exist([filename "." curext], "file"))
                filename = [filename "." curext];
                ext = curext;
                break;
            endif
        endfor
    endif

    ## If no usable extension was found give an error    
    if (!exist("ext", "var"))
        error("No supported extension was found for %s\n", filename);
    endif
    
    ## Determine which flags to use with tar
    switch (ext)
        case {"tar"} flag = "";
        case {"tar.gz", "tgz"} flag = "z";
        case {"tar.bz", "tar.bz2", "tbz", "tbz2"} flag = "j";
    endswitch

    ## Call tar
    [output, status] = system(["tar -" flag "xvf " filename " -C " outputdir]);
    if (status != 0)
        error("tar returned the following error: %s\n", output);
    endif
    
    if (nargout > 0)
        out = split(output, "\n");
    endif
endfunction
