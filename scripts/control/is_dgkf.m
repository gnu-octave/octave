# Copyright (C) 1996,1998 A. Scottedward Hodel 
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
 
function [retval,dgkf_struct] = is_dgkf(Asys,nu,ny,tol)
  # function [retval,dgkf_struct] = is_dgkf(Asys,nu,ny{,tol})
  # Determine whether a continuous time state space system meets
  # assumptions of DGKF algorithm.  
  # Partitions system into: [z] = [A  | Bw  Bu  ][w]
  #                         [y]   [Cz | Dzw Dzu ][u]
  #                               [Cy | Dyw Dyu ]
  #If necessary, orthogonal transformations Qw, Qz and nonsingular
  # transformations Ru, Ry are applied to respective vectors w, z, u, y 
  # in order to satisfy DGKF assumptions.  Loop shifting is used if Dyu block
  # is nonzero.
  #
  # inputs: 
  #        Asys: structured system
  #	   nu: number of controlled inputs
  #        ny: number of measured outputs
  #        tol: threshhold for 0.  Default: 200*eps
  # outputs: 
  #    retval: true(1) if system passes check, false(0) otherwise
  #    dgkf_struct: data structure of is_dgkf results.  Entries:
  #      nw, nz: dimensions of w, z
  #      A: system A matrix
  #      Bw: (n x nw) Qw-transformed disturbance input matrix
  #      Bu: (n x nu) Ru-transformed controlled input matrix;
  #          Note: B = [Bw Bu] 
  #      Cz: (nz x n) Qz-transformed error output matrix
  #      Cy: (ny x n) Ry-transformed measured output matrix 
  #          Note: C = [Cz; Cy] 
  #      Dzu, Dyw: off-diagonal blocks of transformed D matrix that enter 
  #          z, y from u, w respectively
  #      Ru: controlled input transformation matrix 
  #      Ry: observed output transformation matrix
  #      Dyu_nz: nonzero if the Dyu block is nonzero.
  #      Dyu: untransformed Dyu block
  #      dflg: nonzero if the system is discrete-time
  #   
  #    is_dgkf exits with an error if the system is mixed discrete/continuous
  #
  # [1] Doyle, Glover, Khargonekar, Francis, "State Space Solutions
  #     to Standard H2 and Hinf Control Problems," IEEE TAC August 1989
  #
  # [2] Maciejowksi, J.M.: "Multivariable feedback design,"
  #     Addison-Wesley, 1989, ISBN 0-201-18243-2
  #
  # [3] Keith Glover and John C. Doyle: "State-space formulae for all
  #     stabilizing controllers that satisfy and h-infinity-norm bound
  #     and relations to risk sensitivity,"
  #     Systems & Control Letters 11, Oct. 1988, pp 167-172.
  # [4] P. A. Iglesias and K. Glover, "State-space approach to discrete-time
  #     H-infinity control."  Int. J. Control, 1991, V. 54, #5, 1031-1073.
  #
  
  #  Written by A. S. Hodel
  #  Updated by John Ingram July 1996 to accept structured systems
  #
  # Revised by Kai P Mueller April 1998 to solve the general H_infinity
  # problem using unitary transformations Q (on w and z)
  # and non-singular transformations R (on u and y) such
  # that the Dzu and Dyw matrices of the transformed plant
  #
  #    ~
  #    P  (the variable Asys here)
  #
  # become
  #
  #    ~            -1         T
  #    D  = Q   D   R   = [ 0 I ]  or [ I ],
  #     12   12  12  12
  #
  #    ~            T
  #    D  = R   D   Q   = [ 0 I ] or [ I ].
  #     21   21  21  21
  #
  # This transformation together with the algorithm in [1] solves
  # the general problem (see [2] for example). 

  if (nargin < 3) | (nargin > 4)
    usage("[retval,dgkf_struct] = is_dgkf(Asys,nu,ny{,tol})");
  elseif (! is_scalar(nu) | ! is_scalar(ny) )
    error("is_dgkf: arguments 2 and 3 must be scalars")
  elseif (! is_struct(Asys) )
    error("Argument 1 must be a system data structure");
  endif
  if(nargin < 4)
    tol = 200*eps;
  elseif( !is_sample(tol) )
    error("is_dgkf: tol must be a positive scalar")
  endif

  retval = 1;		# assume passes test

  dflg = is_digital(Asys);
  [Anc, Anz, nin, nout ] = sysdimensions(Asys);

  if( Anz == 0 & Anc == 0 )
    error("is_dgkf: no system states");
  elseif( nu >= nin )
    error("is_dgkf: insufficient number of disturbance inputs");
  elseif( ny >= nout )
    error("is_dgkf: insufficient number of regulated outputs");
  endif

  nw = nin - nu;           nw1 = nw + 1;
  nz = nout - ny;          nz1 = nz + 1;

  [A,B,C,D] = sys2ss(Asys);
  # scale input/output for numerical reasons
  if(norm(C,'fro')*norm(B,'fro') == 0)
    error("||C||*||B|| = 0; no dynamic connnection from inputs to outputs");
  endif
  xx = sqrt(norm(B, Inf) / norm(C, Inf));
  B = B / xx;  C = C * xx;

  # partition matrices
  			Bw = B(:,1:nw);		Bu = B(:,nw1:nin);
  Cz = C(1:nz,:);	Dzw = D(1:nz,1:nw);	Dzu = D(1:nz,nw1:nin);
  Cy = C(nz1:nout,:);	Dyw = D(nz1:nout,1:nw);	Dyu = D(nz1:nout,nw1:nin);

  # Check for loopo shifting
  Dyu_nz = (norm(Dyu,Inf) != 0);
  if (Dyu_nz)
    warning("is_dgkf: D22 nonzero; performing loop shifting");
  endif

  # 12 - rank condition at w = 0
  xx =[A Bu; Cz Dzu];
  [nr, nc] = size(xx);
  irank = rank(xx);
  if (irank != nc)
    retval = 0;
    warning(sprintf("rank([A Bu; Cz Dzu]) = %d, need %d; n=%d, nz=%d, nu=%d", ...
	irank,nc,(Anc+Anz),nz,nu));
    warning(" *** 12-rank condition violated at w = 0.");
  endif

  # 21 - rank condition at w = 0
  xx =[A Bw; Cy Dyw];
  [nr, nc] = size(xx);
  irank = rank(xx);
  if (irank != nr)
    retval = 0;
    warning(sprintf("rank([A Bw; Cy Dyw]) = %d, need %d; n=%d, ny=%d, nw=%d", ...
	irank,nr,(Anc+Anz),ny,nw));
    warning(" *** 21-rank condition violated at w = 0.");
  endif

  # can Dzu be transformed to become [0 I]' or [I]?
  # This ensures a normalized weight
  [Qz, Ru] = qr(Dzu);
  irank = rank(Ru);
  if (irank != nu)
    retval = 0;
    warning(sprintf("*** rank(Dzu(%d x %d) = %d", nz, nu, irank));
    warning(" *** Dzu does not have full column rank.");
  endif
  if (nu >= nz)
    Qz = Qz(:,1:nu)';
  else
    Qz = [Qz(:,(nu+1):nz) Qz(:,1:nu)]';
  endif
  Ru = Ru(1:nu,:);

  # can Dyw be transformed to become [0 I] or [I]?
  # This ensures a normalized weight
  [Qw, Ry] = qr(Dyw');
  irank = rank(Ry);
  if (irank != ny)
    retval = 0;
    warning(sprintf("*** rank(Dyw(%d x %d) = %d", ny, nw, irank));
    warning(" *** Dyw does not have full row rank.");
  endif

  if (ny >= nw)
    Qw = Qw(:,1:ny);
  else
    Qw = [Qw(:,(ny+1):nw) Qw(:,1:ny)];
  endif
  Ry = Ry(1:ny,:)';

  # transform P by Qz/Ru and Qw/Ry
  Bw  = Bw*Qw;
  Bu  = Bu/Ru;
  B   = [Bw Bu];
  Cz  = Qz*Cz;
  Cy  = Ry\Cy;
  C   = [Cz; Cy];
  Dzw = Qz*Dzw*Qw;
  Dzu = Qz*Dzu/Ru;
  Dyw = Ry\Dyw*Qw;

  # pack the return structure
  dgkf_struct.nw	= nw;
  dgkf_struct.nu	= nu;
  dgkf_struct.nz	= nz;
  dgkf_struct.ny	= ny;
  dgkf_struct.A		= A;
  dgkf_struct.Bw	= Bw;
  dgkf_struct.Bu	= Bu;
  dgkf_struct.Cz	= Cz;
  dgkf_struct.Cy	= Cy;
  dgkf_struct.Dzw	= Dzw;
  dgkf_struct.Dzu	= Dzu;
  dgkf_struct.Dyw	= Dyw;
  dgkf_struct.Dyu	= Dyu;
  dgkf_struct.Ru	= Ru;
  dgkf_struct.Ry	= Ry;
  dgkf_struct.Dyu_nz	= Dyu_nz;
  dgkf_struct.dflg	= dflg;

endfunction
