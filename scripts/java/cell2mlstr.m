## Copyright (C) 2012 Philip Nienhuis <prnienhuis@users.sf.net>
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

## cell2mlstr - convert text cells in cellstr arrray to multiline text
## separated by EOL

## Author: Philip <prnienhuis@users.sf.net>
## Based on a suggestion by D. Bateman,
## https://savannah.gnu.org/bugs/?func=detailitem&item_id=31468#comment4
## Created: 2012-06-29

function retval = cell2mlstr (cstr)

  if (! iscellstr (cstr))
    ## Only use char elements
    cstr = cstr (find (cellfun ("ischar", cstr)));
  endif

  ## Treat cell string array as multi-line text
  cstr(1:2:2*numel (cstr)) = cstr;
  cstr(2:2:numel (cstr)) = "\n";

  retval = [cstr{:}];

endfunction
