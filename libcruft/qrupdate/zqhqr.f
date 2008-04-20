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
      subroutine zqhqr(m,n,k,Q,ldq,R,ldr)
c purpose:      given an k-by-n upper Hessenberg matrix R and
c               an m-by-k matrix Q, this subroutine updates
c               R -> R1 and Q -> Q1 so that R1 is upper 
c               trapezoidal, R1 = G*R and Q1 = Q*G', where
c               G is an unitary matrix, giving Q1*R1 = Q*R.
c               (complex version)
c arguments:
c m (in)        number of rows of the matrix Q
c n (in)        number of columns of the matrix R
c k (in)        number of columns of Q and rows of R.
c Q (io)        on entry, the unitary matrix Q
c               on exit, the updated matrix Q1
c ldq (in)      leading dimension of Q
c R (io)        on entry, the upper triangular matrix R
c               on exit, the updated upper Hessenberg matrix R1
c ldr (in)      leading dimension of R
c
      integer m,n,k,ldq,ldr
      double complex Q(ldq,*),R(ldr,*)
      double precision c
      double complex s,rr
      external xerbla,zlartg,zrot
      integer info,i
c quick return if possible.
      if (n <= 0 .or. k <= 1) return
c check arguments.
      info = 0
      if (ldq < 1) then
        info = 5
      else if (ldr < 1) then
        info = 7
      end if
      if (info /= 0) then
        call xerbla('ZQHQR',info)
      end if
c triangularize      
      do i = 1,min(k-1,n)
        call zlartg(R(i,i),R(i+1,i),c,s,rr)
        R(i,i) = rr
        R(i+1,i) = 0d0
        if (i < n) then
          call zrot(n-i,R(i,i+1),ldr,R(i+1,i+1),ldr,c,s)
        end if
c apply rotation to Q        
        if (m > 0) then
          call zrot(m,Q(1,i),1,Q(1,i+1),1,c,conjg(s))
        end if
      end do
      end 

