// NLP.h                                                -*- C++ -*-
/*

Copyright (C) 1992, 1993 John W. Eaton

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
Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

*/

#if !defined (_NLP_h)
#define _NLP_h 1

#include "Objective.h"
#include "Bounds.h"
#include "LinConst.h"
#include "NLConst.h"
#include "Matrix.h"

#ifndef Vector
#define Vector ColumnVector
#endif

class NLP
{
 public:

  NLP (void);

  NLP (const Vector& x, const Objective& phi);

  NLP (const Vector& x, const Objective& phi, const Bounds& b);

  NLP (const Vector& x, const Objective& phi, const Bounds& b, const
       LinConst& lc);

  NLP (const Vector& x, const Objective& phi, const Bounds& b, const
       LinConst& lc, const NLConst& nlc);

  NLP (const Vector& x, const Objective& phi, const LinConst& lc); 

  NLP (const Vector& x, const Objective& phi, const LinConst& lc,
       const NLConst& nlc);

  NLP (const Vector& x, const Objective& phi, const NLConst& nlc); 

  NLP (const Vector& x, const Objective& phi, const Bounds& b, const
       NLConst& nlc);

  int size (void) const;

 protected:

  Vector x;
  Objective phi;  
  Bounds bnds;
  LinConst lc;
  NLConst nlc;
};

inline NLP::NLP (void) {}

inline NLP::NLP (const Vector& xx, const Objective& obj)
  : x (xx), phi (obj) {}

inline NLP::NLP (const Vector& xx, const Objective& obj, const Bounds& b)
  : x (xx), phi (obj), bnds (b) {}

inline NLP::NLP (const Vector& xx, const Objective& obj, const Bounds& b,
		 const LinConst& l) 
  : x (xx), phi (obj), bnds (b), lc (l) {}

inline NLP::NLP (const Vector& xx, const Objective& obj, const Bounds& b,
		 const LinConst& l, const NLConst& nl) 
  : x (xx), phi (obj), bnds (b), lc (l), nlc (nl) {}

inline NLP::NLP (const Vector& xx, const Objective& obj, const LinConst& l)
  : x (xx), phi (obj), lc (l) {}

inline NLP::NLP (const Vector& xx, const Objective& obj, const LinConst& l,
		 const NLConst& nl) 
  : x (xx), phi (obj), lc (l), nlc (nl) {}

inline NLP::NLP (const Vector& xx, const Objective& obj, const NLConst& nl)
  : x (xx), phi (obj), nlc (nl) {}

inline NLP::NLP (const Vector& xx, const Objective& obj, const Bounds& b,
		 const NLConst& nl) 
  : x (xx), phi (obj), bnds (b), nlc (nl) {}

inline int
NLP::size (void) const
{
  return x.capacity ();
}

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; page-delimiter: "^/\\*" ***
;;; End: ***
*/
