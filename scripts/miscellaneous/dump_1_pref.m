# Copyright (C) 1994, 1995 John W. Eaton
# 
# This file is part of Octave.
# 
# Octave is free software; you can redistribute it and/or modify it
# under the terms of the GNU General Public License as published by the
# Free Software Foundation; either version 2, or (at your option) any
# later version.
# 
# Octave is distributed in the hope that it will be useful, but WITHOUT
# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
# FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
# for more details.
# 
# You should have received a copy of the GNU General Public License
# along with Octave; see the file COPYING.  If not, write to the Free
# Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

function dump_1_pref (file, var)

  if (nargin != 2)
    usage ("dump_1_pref (file, var)");
  endif

  eval (sprintf ("tmp = %s;", var));

  if (isstr (tmp))
    fprintf (file, "  %s = \"%s\"\n", var, undo_string_escapes (tmp));
  elseif (is_scalar (tmp))
    fprintf (file, "  %s = %g\n", var, tmp);
  else
    fprintf (file, "  %s = \"wrong type arg\"\n", var);
  endif

endfunction
