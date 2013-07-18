## Copyright (C) 2013 John Donoghue
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
## @deftypefn {Function File} prefdir 
## @deftypefnx {Function File} {folder =} prefdir 
## Return the folder that contains the preferences for octave.
##
## Examples:
##
## Display the preferences folder
## @example
## prefdir
## @end example
##
## Change to the preferences folder
## @example
## cd(prefdir)
## @end example
## @end deftypefn

## Author: John Donoghue
## Version: 0.01

function folder = prefdir ()

    folder = getenv("HOME");
      
endfunction