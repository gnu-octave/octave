// NLP.h                                                -*- C++ -*-
/*

Copyright (C) 1992, 1993, 1994, 1995 John W. Eaton

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

#if !defined (octave_NLP_h)
#define octave_NLP_h 1

#include "dColVector.h"
#include "Objective.h"
#include "Bounds.h"
#include "LinConst.h"
#include "NLConst.h"
#include "base-min.h"

class NLP : public base_minimizer
{
 public:

  NLP (void) : base_minimizer () { }

  NLP (const ColumnVector& x, const Objective& obj)
    : base_minimizer (x), phi (obj) { }

  NLP (const ColumnVector& x, const Objective& obj, const Bounds& b)
    : base_minimizer (x), phi (obj), bnds (b) { }

  NLP (const ColumnVector& x, const Objective& obj, const Bounds& b,
       const LinConst& l)
    : base_minimizer (x), phi (obj), bnds (b), lc (l) { }

  NLP (const ColumnVector& x, const Objective& obj, const Bounds& b,
       const LinConst& l, const NLConst& nl)
    : base_minimizer (x), phi (obj), bnds (b), lc (l), nlc (nl) { }

  NLP (const ColumnVector& x, const Objective& obj, const LinConst& l)
    : base_minimizer (x), phi (obj), lc (l) { }

  NLP (const ColumnVector& x, const Objective& obj, const LinConst& l,
       const NLConst& nl)
    : base_minimizer (x), phi (obj), lc (l), nlc (nl) { }

  NLP (const ColumnVector& x, const Objective& obj, const NLConst& nl)
    : base_minimizer (x), phi (obj), nlc (nl) { }

  NLP (const ColumnVector& x, const Objective& obj, const Bounds& b,
       const NLConst& nl)
    : base_minimizer (x), phi (obj), bnds (b), nlc (nl) { }

  NLP& operator = (const NLP& a)
    {
      if (this != &a)
	{
	  x = a.x;
	  phi = a.phi;  
	  bnds = a.bnds;
	  lc = a.lc;
	  nlc = a.nlc;
	}

      return *this;
    }

  virtual ~NLP (void) { }

  int size (void) const { return x.capacity (); }

 protected:

  Objective phi;  
  Bounds bnds;
  LinConst lc;
  NLConst nlc;
};

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; page-delimiter: "^/\\*" ***
;;; End: ***
*/
