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

#if !defined (octave_NLEqn_h)
#define octave_NLEqn_h 1

#include <cfloat>
#include <cmath>

#include "NLEqn-opts.h"

class
NLEqn : public NLFunc, public NLEqn_options
{
public:

  NLEqn (void)
    : NLFunc (), NLEqn_options (), x (), solution_status (0) { }

  NLEqn (const ColumnVector& xx, const NLFunc f) 
    : NLFunc (f), NLEqn_options (), x (xx), solution_status (0) { }

  NLEqn (const NLEqn& a)
    : NLFunc (a.fun, a.jac), NLEqn_options (), x (a.x),
      solution_status (a.solution_status) { }

  NLEqn& operator = (const NLEqn& a)
    {
      if (this != &a)
	{
	  NLFunc::operator = (a);
	  NLEqn_options::operator = (a);

	  x = a.x;
	  solution_status = a.solution_status;
	}
      return *this;
    }

  ~NLEqn (void) { }

  void set_states (const ColumnVector& xx) { x = xx; }

  ColumnVector states (void) const { return x; }

  int size (void) const { return x.capacity (); }

  ColumnVector solve (void)
    {
      int info;
      return solve (info);
    }

  ColumnVector solve (const ColumnVector& xvec)
    {
      set_states (xvec);
      int info;
      return solve (info);
    }

  ColumnVector solve (const ColumnVector& xvec, int& info)
    {
      set_states (xvec);
      return solve (info);
    }

  ColumnVector solve (int& info);

  int solution_state (void) const { return solution_status; }

  bool solution_ok (void) const { return solution_status == 1; }

  std::string error_message (void) const;

private:

  ColumnVector x;
  int solution_status;

  void error (const char* msg);
};

#endif

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
