// Template array classes with like-type math ops
/*

Copyright (C) 1996-2012 John W. Eaton

This file is part of Octave.

Octave is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 3 of the License, or (at your
option) any later version.

Octave is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with Octave; see the file COPYING.  If not, see
<http://www.gnu.org/licenses/>.

*/

#if !defined (octave_MArrayN_h)
#define octave_MArrayN_h 1

#include "MArray.h"
#define MArrayN MArray

// If we're with GNU C++, issue a warning.
#ifdef __GNUC__
#warning Using MArrayN<T> is deprecated. Use MArray<T>.
#endif

#endif
