# Copyright (C) 1996 A. Scottedward Hodel 
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
 
function sys = sysscale(sys,outscale,inscale,outname,inname)
#
# function sys = sysscale(sys,outscale,inscale[,outname,inname])
# scale inputs/outputs of a system.
#
# inputs:
#   sys: structured system
#   outscale, inscale: constant matrices of appropriate dimension
# output: sys: resulting open loop system:
#
#           -----------    -------    -----------
#     u --->| inscale |--->| sys |--->| outscale |---> y
#           -----------    -------    -----------
# 
# If the input names and output names are not given and the scaling matrices
# are not square, then default names will be given to the inputs and/or
# outputs.
#
# A warning message is printed if outscale attempts to add continuous
# system outputs to discrete system outputs; otherwise yd is set appropriately

# A. S. Hodel August 1995
# modified by John Ingram 7-15-96
# $Revision: 1.1.1.1 $

  if( (nargin < 3) || (nargin > 5)  )
    usage("retsys = sysscale(Asys,output_list,input_list{,inname,outname})");
  elseif (!is_struct(sys))
    error("sys must be a structured system");
  endif
 
  # check for omitted scales
  if(isempty(outscale))
    outscale = eye(rows(sys.outname)); 
  endif 
  if(isempty(inscale))
    inscale = eye(rows(sys.inname));
  endif 

  # check dimensions of scaling matrices
  if((columns(sys.b)!=rows(inscale)) & (columns(sys.d)!=rows(inscale)))
    error('inscale is not compatible with the system inputs');
  elseif( (columns(outscale)!=rows(sys.c)) & ...
	(columns(outscale)!=rows(sys.d)))
    error("outscale is not compatible with the system outputs");
  endif
  
  outc = find(sys.yd==0);
  outd = find(sys.yd==1);

  #disp("sysscale: outc,outd=")
  #disp(outc)
  #disp(outd)
  #disp("sysscale")

  if(length(outc) & length(outd))
    for ii = 1:rows(outscale)
      nci = norm(outscale(ii,outc));
      ndi = norm(outscale(ii,outd));

      #disp(["sysscale: ii=",num2str(ii),", nci, ndi="])
      #disp(nci)
      #disp(ndi)
      #disp("syscale")

      if( nci & ndi)
        warning(["sysscale: outscale(",num2str(ii), ...
	  ",:) sums continuous and discrete outputs; setting output to cont"])
        yd(ii) = 0;
      else
        yd(ii) = (ndi != 0);
      endif
  
      #disp(["sysscale: yd(,",num2str(ii),"=",num2str(yd(ii)),": press a key"]);
      #kbhit
    endfor
  else
    yd = ones(1,rows(outscale))*( length(outd) > 0);
  endif
  sys.yd = yd;

  sys.b = (sys.b)*inscale;
  sys.d = (sys.d)*inscale;
  sys.c = outscale*(sys.c);
  sys.d = outscale*(sys.d);

  if( !is_square(outscale) )
    # strip extra output names (if any)
    sys.outname = sys.outname(1:min(rows(outscale),columns(outscale)),:);
    if( nargin < 4)
      warning("sysscale: outscale not square, outname not specified");
      warning("sysscale:  using default output names");
      outname = sysdefioname(rows(sys.c),"y");
    endif
  else
    outname = sys.outname;
  endif
  if( !is_square(inscale) )
    # strip extra output names (if any)
    sys.inname = sys.inname(1:min(rows(inscale),columns(inscale)),:);
    if(nargin < 5)
      warning("sysscale: inscale not square, inname not specified");
      warning("sysscale:  using default input names");
      inname = sysdefioname(columns(sys.b),"u");
    endif
  else
    inname = sys.inname;
  endif

  sys = syschnames(sys,"out",1:rows(outname),outname);
  sys = syschnames(sys,"in",1:rows(inname),inname);

endfunction
