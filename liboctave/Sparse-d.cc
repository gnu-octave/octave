/*

Copyright (C) 2004 David Bateman
Copyright (C) 1998-2004 Andy Adler

Octave is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 2, or (at your option) any
later version.

Octave is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with this program; see the file COPYING.  If not, write to the
Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
Boston, MA 02110-1301, USA.

*/

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

// Instantiate Sparse matrix of double values.

#include "Sparse.h"
#include "Sparse.cc"

INSTANTIATE_SPARSE_AND_ASSIGN (double, OCTAVE_API);

#if 0
template std::ostream& operator << (std::ostream&, const Sparse<double>&);
#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
