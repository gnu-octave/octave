/*

Copyright (C) 1996, 1997 John W. Eaton

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

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

// Instantiate Arrays of bool values.

#include "Array.h"
#include "Array.cc"

template class Array<bool>;

template int assign (Array<bool>&, const Array<bool>&);

template int assign (Array<bool>&, const Array<bool>&, const bool&);

#include "Array2.h"
#include "Array2.cc"

template class Array2<bool>;

template int assign (Array2<bool>&, const Array2<bool>&);

template int assign (Array2<bool>&, const Array2<bool>&, const bool&);

#include "DiagArray2.h"
#include "DiagArray2.cc"

template class DiagArray2<bool>;

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
