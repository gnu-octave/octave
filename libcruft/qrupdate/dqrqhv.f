c Copyright (C) 2008  VZLU Prague, a.s., Czech Republic
c 
c Author: Jaroslav Hajek <highegg@gmail.com>
c 
c This source is free software; you can redistribute it and/or modify
c it under the terms of the GNU General Public License as published by
c the Free Software Foundation; either version 2 of the License, or
c (at your option) any later version.
c 
c This program is distributed in the hope that it will be useful,
c but WITHOUT ANY WARRANTY; without even the implied warranty of
c MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
c GNU General Public License for more details.
c 
c You should have received a copy of the GNU General Public License
c along with this software; see the file COPYING.  If not, see
c <http://www.gnu.org/licenses/>.
c 
      subroutine dqrqhv(m,n,k,Q,ldq,R,ldr,u,rr)
c purpose:      given an m-by-k matrix Q, an upper trapezoidal 
c               k-by-n matrix R, and an m-vector u, this subroutine 
c               updates the matrices Q -> Q1 and R -> R1 so that 
c               Q1 = Q*G', R1 = G*R, w1(2:m) = 0 with G orthogonal, 
c               R1 upper Hessenberg, and w1 = Q1'*u.
c               (real version)
c arguments:
c m (in)        number of rows of the matrix Q.
c n (in)        number of columns of the matrix R.
c k (in)        number of columns of Q and rows of R. k <= m.
c Q (io)        on entry, the orthogonal matrix Q.
c               on exit, the updated matrix Q1.
c ldq (in)      leading dimension of Q.
c R (io)        on entry, the upper triangular matrix R.
c               on exit, the updated upper Hessenberg matrix R1.
c ldr (in)      leading dimension of R.
c u (in)        the m-vector u.
c rr (out)      the first element of Q1'*u on exit.
c
c               if Q is orthogonal, so is Q1. It is not strictly
c               necessary, however.
      integer m,n,k,ldq,ldr
      double precision Q(ldq,*),R(ldr,*),u(*),rr
      double precision c
      double precision s,w,w1,ddot
      external xerbla,ddot,dlartg,drot
      integer i,info
c quick return if possible.
      if (k <= 0) return
c check arguments.      
      info = 0
      if (k > m) then
        info = 3
      else if (ldq < 1) then
        info = 5
      else if (ldr < 1) then
        info = 7
      end if
      if (info /= 0) then
        call xerbla('DQRQHV',info)
      end if
c form each element of w = Q'*u when necessary.
      rr = ddot(m,Q(1,k),1,u,1)
      do i = k-1,1,-1
        w1 = rr
        w = ddot(m,Q(1,i),1,u,1)
        call dlartg(w,w1,c,s,rr)
c apply rotation to rows of R if necessary        
        if (i <= n) then
          call drot(n+1-i,R(i,i),ldr,R(i+1,i),ldr,c,s)
        end if
c apply rotation to columns of Q
        call drot(m,Q(1,i),1,Q(1,i+1),1,c,s)
      end do
      end 

