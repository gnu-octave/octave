/*

This file is part of Octave.

Octave is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 2, or (at your option) any
later version.

Octave is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with Octave; see the file COPYING.  If not, write to the Free
Software Foundation, 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.

*/

#if !defined (octave_oct_fftw_h)
#define octave_oct_fftw_h 1

#include <cstddef>

#if defined (HAVE_DFFTW_H)
#include <dfftw.h>
#else
#include <fftw.h>
#endif

#include "oct-cmplx.h"

class
octave_fftw
{
public:
  static int fft (const Complex*, Complex *, size_t);
  static int ifft (const Complex*, Complex *, size_t);

  static int fft2d (Complex*, size_t, size_t);
  static int ifft2d (Complex*, size_t, size_t);

private:
  octave_fftw ();
  octave_fftw (const octave_fftw&);
  octave_fftw& operator = (const octave_fftw&);
};

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/

