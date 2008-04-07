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

      subroutine dchinx(n,R,R1,j,u,info)
c purpose:      given an upper triangular matrix R that is a Cholesky
c               factor of a symmetric positive definite matrix A, i.e.
c               A = R'*R, this subroutine updates R -> R1 so that
c               R1'*R1 = A1, A1(jj,jj) = A, A(j,:) = u', A(:,j) = u,
c               jj = [1:j-1,j+1:n+1].
c               (real version)
c arguments:
c n (in)        the order of matrix R
c R (in)        the original upper trapezoidal matrix R
c R1 (out)      the updated matrix R1
c j (in)        the position of the inserted row/column
c u (in)        the vector (n+1) determining the rank-1 update
c info (out)    on exit, if info = 1, the 
c               definiteness.

      integer n,j,info
      double precision R(n,n),R1(n+1,n+1),u(n+1)
      double precision rho,Qdum,w,dnrm2
      external dcopy,dlacpy,dtrsv,dnrm2
      integer jj

c quick return if possible
      if (n == 0) then
        if (u(1) <= 0) then
          info = 1
          return
        else
          R(1,1) = sqrt(u(1))
        end if
      end if

c check arguments      
      info = 0
      if (n < 0) then
        info = 1
      else if (j < 1 .or. j > n+1) then
        info = 4
      end if
      if (info /= 0) then
        call xerbla('DQRINX',info)
      end if

c copy shifted vector
      if (j > 1) then
        call dcopy(j-1,u,1,R1(1,j),1)
      end if
      w = u(j)
      if (j < n+1) then
        call dcopy(n-j+1,u(j+1),1,R1(j,j),1)
      end if
      
c check for singularity of R
      do i = 1,n
        if (R(i,i) == 0d0) then
          info = 2
          return
        end if
      end do
c form R' \ u
      call dtrsv('U','T','N',n,R,n,R1(1,j),1)
      rho = dnrm2(n,R1(1,j),1)
c check positive definiteness      
      rho = u(j) - rho**2
      if (rho <= 0d0) then
        info = 1
        return
      end if
      R1(n+1,n+1) = sqrt(rho)

c setup the new matrix R1
      do i = 1,n+1
        R1(n+1,i) = 0d0
      end do
      if (j > 1) then
        call dlacpy('0',n,j-1,R(1,1),n,R1(1,1),n+1)
      end if
      if (j <= n) then
        call dlacpy('0',n,n-j+1,R(1,j),n,R1(1,j+1),n+1)
c retriangularize
        jj = min(j+1,n)
        call dqrqhu(0,n+1-j,n-j,Qdum,1,R1(j,jj),n+1,R1(j,j),w)
        R1(j,j) = w
        do jj = j+1,n
          R1(jj,j) = 0d0
        end do
      end if

      end
