## Copyright (C) 1996, 2000, 2005, 2007
##               Auburn University.  All rights reserved.
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

## Undocumented internal function.

## -*- texinfo -*-
## @deftypefn {Function File} {} __syschnamesl__ (@var{olist}, @var{old_names}, @var{inames}, @var{listname})
## used internally in syschnames
## item olist: index list
## old_names: original list names
## inames: new names
## listname: name of index list
##
## combines the two string lists old_names and inames
## @end deftypefn

function old_names = __syschnamesl__ (olist, old_names, inames, listname)

  probstr = [];

  if (max (olist) > rows (old_names))
    probstr = sprintf ("index list value(s) exceed(s) number of signals (%d)",
		       rows (old_names));

  elseif (length (olist) > rows (inames))
    probstr = sprintf ("index list dimension exceeds number of replacement names (%d)",
		       rows (inames));

  elseif (isempty (olist))
    probstr = [];    # do nothing, no changes

  elseif (min (size (olist)) != 1)
    probstr = "index list must be either a vector or an empty matrix";

  elseif (max (olist) > rows (old_names))
    probstr = sprintf ("max(%s)=%d > %d, too big", listname,
		       max (olist), rows (old_names));

  elseif (min (olist) < 1)
    probstr = sprintf ("min(%s)=%d < 1, too small", listname, min (olist));

  else
    if (length(olist) == 1)
        len_in = columns (inames);
        len_out = columns (old_names);

      if (len_in < len_out)
        inames(1,(len_in+1):(len_out)) = zeros (1, len_out-len_in);
      endif

      old_names(olist,1:length(inames)) = inames;
    elseif (length(olist) > 1)
      for ii = 1:length(olist)
        mystr = inames(ii,:);
        len_my = columns (mystr);
        len_out = columns (old_names);

        if (len_my < len_out)
          mystr(1,(len_my+1):len_out) = repmat (" ", 1, len_out-len_my);
          len_my = len_out;
        endif

        old_names(olist(ii),1:len_my) = mystr;
      endfor
    endif
  endif
  if (! isempty (probstr))
    ## the following lines are NOT debugging code!
    disp ("Problem in syschnames: old names are")
    __outlist__ (old_names," ")
    disp ("new names are")
    __outlist__(inames,"    ")
    disp ("list indices are")
    disp (olist)
    error (sprintf ("syschnames: \"%s\" dim=(%d x %d)--\n\t%s\n", ...
		    listname, rows (olist), columns (olist), probstr));
  endif

  ## change zeros  to blanks
  if (find (old_names == 0))
    ## disp("__syschnamesl__: old_names contains zeros ")
    ## old_names
    ## disp("/__syschnamesl__");

    [ii, jj] = find (old_names == 0);
    for idx = 1:length(ii)
      old_names(ii(idx),jj(idx)) = " ";
    endfor

    ## disp("__syschnamesl__: old_names fixed zeros ")
    ## old_names
    ## disp("/__syschnamesl__");
  endif

  ## just in case it's not a string anymore
  if (! ischar (old_names))
    old_names = char (old_names);
  endif

  ## disp("__syschnamesl__: exit, old_names=")
  ## old_names
  ## disp("/__syschnamesl__: exiting")

endfunction
