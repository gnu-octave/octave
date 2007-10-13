c Copyright (C) 2006, 2007 John W. Eaton
c 
c This file is part of Octave.
c 
c Octave is free software; you can redistribute it and/or modify it
c under the terms of the GNU General Public License as published by the
c Free Software Foundation; either version 3 of the License, or (at your
c option) any later version.
c 
c Octave is distributed in the hope that it will be useful, but WITHOUT
c ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
c FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
c for more details.
c 
c You should have received a copy of the GNU General Public License
c along with Octave; see the file COPYING.  If not, see
c <http://www.gnu.org/licenses/>.

      subroutine mexFunction (nlhs, plhs, nrhs, prhs)

      implicit none

      integer*4 nlhs, nrhs

* The following will need to be integer*8 on 64-bit systems, otherwise
* these variables won't be large enough to hold pointers...
      integer*4 plhs(*), prhs(*)

      integer*4 mxIsString, mxGetString, mxGetN, mexCallMATLAB
      integer*4 status, len
      character*100 str

      call mexPrintf ('Hello, World!')

      if (nrhs .lt. 1 .or. mxIsString (prhs(1)) .ne. 1) then
        call mexErrMsgTxt ('function name expected')
      endif

      len = mxGetN (prhs(1))

      status = mxGetString (prhs(1), str, 100)

      call mexPrintf ('FORTRAN will call the interpreter now')

      status = mexCallMATLAB (nlhs, plhs, nrhs-1, prhs(2), str(1:len))

      return
      end
