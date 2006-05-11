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
## @deftypefn {Function File} tar (@var{filename}, @var{files})
## @deftypefnx{Function File} tar (@var{filename}, @var{files}, @var{root})
## @deftypefnx{Function File} @var{entries} = tar (...)
## Packs the files listed in @var{files} into @var{filename} using
## the @code{tar} program. @var{files} must either be a string or a cell
## array of strings containing the files to be packed. The extension of
## @var{filename} determines if the tar-file is also to be compressed
## (if no extension is present @code{.tar} will be appended to @var{filename})
## @table @asis
## @item @code{.tar}
##   No compression.
## @item @code{.tar.gz} or @code{.tgz}
##   The tar-file will be compressed using @code{gzip}.
## @item @code{tar.bz}, @code{tar.bz2}, @code{tbz}, or @code{tbz2}
##   The tar-file will be compressed using @code{bzip2}.
## @end table
## The optional argument @var{root} changes the relative path of @var{files}
## from the current directory.
##
## If an output argument is requested the filename entries in the archive
## is returned.
## 
## @end deftypefn
## @seealso{untar, gzip, gunzip, zip, unzip}

## Author: Søren Hauberg <hauberg at gmail dot com>

function entries = tar(filename, files, root)
    if (nargin < 2 || nargin > 3)
        print_usage("tar");
    elseif (nargin == 2)
        root = ".";
    endif
    
    supported_extensions = {"tar", "tar.gz", "tgz", "tar.bz", "tar.bz2", "tbz", "tbz2"};

    ## Test type of input
    if (ischar(files))
        files = {files};
    endif
    if (!ischar(filename) || !iscellstr(files) || !ischar(root))
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

    ## If no extension was found default to "tar"
    if (!exist("ext", "var"))
        filename = sprintf("%s.tar", filename);
        ext = "tar";
    endif

    ## Determine which flags to use with tar
    switch (ext)
        case {"tar"} flag = "";
        case {"tar.gz", "tgz"} flag = "z";
        case {"tar.bz", "tar.bz2", "tbz", "tbz2"} flag = "j";
    endswitch

    ## Call tar
    [output, status] = system(["tar -" flag "cvf " filename " -C " root sprintf(" %s", files{:})]);
    if (status != 0)
        error("tar returned the following error: %s\n", output);
    endif
    
    if (nargout)
        entries = split(output(1:end-1), "\n");
    endif
endfunction
