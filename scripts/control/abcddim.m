# Copyright (C) 1993, 1994, 1995 John W. Eaton
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

function [n, m, p] = abcddim (a, b, c, d)

# Usage: [n, m, p] = abcddim (a, b, c, d)
#
# Check for compatibility of the dimensions of the matrices defining
# the linear system (a, b, c, d).
#
# Returns n = number of system states,
#         m = number of system inputs,
#         p = number of system outputs.
#
# Note: n = 0 (pure gain block) is returned without warning.
#
# Returns n = m = p = -1 if the system is not compatible.
#
# See also: is_abcd

# Written by A. S. Hodel (scotte@eng.auburn.edu) August 1993.
# a s hodel: modified to accept pure-gain systems aug 1996
# $Revision: 1.17 $
# $Log: abcddim.m,v $
# Revision 1.17  1998-12-10 03:06:31  jwe
# *** empty log message ***
#
# Revision 2.0.0.2  1998/12/08  23:29:21  hodel
# Octave-Marsyas Interface updated for signals-as-lists
#
# Revision 2.0.0.1  1998/12/08  21:40:44  hodel
# Dummy version to match ftp.eng.auburn.edu version number
#
# Revision 2.0.0.0  1998/12/08  21:36:51  hodel
# Branch for beta release patches
#
# Revision 2.0  1998/12/08  21:34:56  hodel
# Initial beta release of signals-as-lists rewrite;
# sysdimensions now takes opt as an argument
#
# Revision 2.0.0.1  1998/12/08  20:54:18  hodel
# sysdimensions takes opt parameter now
#
# Revision 2.0.0.0  1998/12/08  20:30:08  hodel
# beta release revision
#
# Revision 2.0  1998/12/08  20:27:55  hodel
# Initial list rewrite of OCST
#
# Revision 1.2  1998/10/05 17:12:56  hodelas
# various bug changes
#
# Revision 1.1.1.1  1998/05/19 20:24:05  jwe
#
# Revision 1.4  1997/12/01 16:44:22  scotte
# *** empty log message ***
#
# Revision 1.3  1997/02/12 15:38:14  hodel
# *** empty log message ***
#
#
# fixed typo
#
# Revision 1.1  1997/02/12 11:34:53  hodel
# Initial revision
#
# Revision 1.7  1997/02/07 15:29:56  scotte
# fixed is_square check to allow for empty a matrix
# (this allows for pure gain blocks)
#

  if (nargin != 4)
    error ("abcddim: four arguments required");
  endif

  n = m = p = -1;

  [a,an,am] = abcddims(a);
  [b,bn,bm] = abcddims(b);
  [c,cn,cm] = abcddims(c);
  [d,dn,dm] = abcddims(d);

  if ( (!is_square(a)) & (!isempty(a)) )
    warning (["abcddim: a is not square (",num2str(an),"x",num2str(am),")"]);
    return
  endif

  if( (bm == 0) & (dm == 0) )
    warning("abcddim: no inputs");
  elseif (bn != am)
    warning (["abcddim: a(",num2str(an),"x",num2str(am), ...
      " and b(",num2str(bn),"x",num2str(bm),") are not compatible"]);
    return
  endif

  if( (cn == 0) & (dn == 0 ) )
    warning("abcddim: no outputs");
  elseif (cm != an)
    warning (["abcddim: a(",num2str(an),"x",num2str(am), ...
	" and c(",num2str(cn),"x",num2str(cm),") are not compatible"]);
    return
  endif

  have_connections = (bn*cn != 0);

  if( (dn == 0) & have_connections)
    warning("abcddim: empty d matrix passed; setting compatibly with b, c");
    [d,dn,dm] = abcddims(zeros(cn,bm));
  endif

  if(an > 0)
    [dn, dm] = size(d);
    if ( (cn != dn) & have_connections )
      warning (["abcddim: c(",num2str(cn),"x",num2str(cm), ...
	" and d(",num2str(dn),"x",num2str(dm),") are not compatible"]);
      return
    endif

    if ( (bm != dm) & have_connections )
      warning (["abcddim: b(",num2str(bn),"x",num2str(bm), ...
	  " and d(",num2str(dn),"x",num2str(dm),") are not compatible"]);
      return
    endif

    m = bm;
    p = cn;
  else
    [p,m] = size(d);
  endif
  n = an;
endfunction
