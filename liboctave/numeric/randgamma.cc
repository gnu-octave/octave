////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2006-2023 The Octave Project Developers
//
// See the file COPYRIGHT.md in the top-level directory of this
// distribution or <https://octave.org/copyright/>.
//
// This file is part of Octave.
//
// Octave is free software: you can redistribute it and/or modify it
// under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// Octave is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with Octave; see the file COPYING.  If not, see
// <https://www.gnu.org/licenses/>.
//
////////////////////////////////////////////////////////////////////////

/* Original version written by Paul Kienzle distributed as free
   software in the in the public domain.  */

/*

double randg (a)
void fill_randg (a,n,x)

Generate a series of standard gamma distributions.

See: Marsaglia G and Tsang W (2000), "A simple method for generating
gamma variables", ACM Transactions on Mathematical Software 26(3) 363-372

Needs the following defines:
* NAN: value to return for Not-A-Number
* RUNI: uniform generator on (0,1)
* RNOR: normal generator
* REXP: exponential generator, or -log(RUNI) if one isn't available
* INFINITE: function to test whether a value is infinite

Test using:
  mean = a
  variance = a
  skewness = 2/sqrt(a)
  kurtosis = 3 + 6/sqrt(a)

Note that randg can be used to generate many distributions:

gamma(a,b) for a>0, b>0 (from R)
  r = b*randg(a)
beta(a,b) for a>0, b>0
  r1 = randg(a,1)
  r = r1 / (r1 + randg(b,1))
Erlang(a,n)
  r = a*randg(n)
chisq(df) for df>0
  r = 2*randg(df/2)
t(df) for 0<df<inf (use randn if df is infinite)
  r = randn () / sqrt(2*randg(df/2)/df)
F(n1,n2) for 0<n1, 0<n2
  r1 = 2*randg(n1/2)/n1 or 1 if n1 is infinite
  r2 = 2*randg(n2/2)/n2 or 1 if n2 is infinite
  r = r1 / r2
negative binonial (n, p) for n>0, 0<p<=1
  r = randp((1-p)/p * randg(n))
  (from R, citing Devroye(1986), Non-Uniform Random Variate Generation)
non-central chisq(df,L), for df>=0 and L>0 (use chisq if L=0)
  r = randp(L/2)
  r(r>0) = 2*randg(r(r>0))
  r(df>0) += 2*randg(df(df>0)/2)
  (from R, citing formula 29.5b-c in Johnson, Kotz, Balkrishnan(1995))
Dirichlet(a1,...,ak) for ai > 0
  r = (randg(a1),...,randg(ak))
  r = r / sum(r)
  (from GSL, citing Law & Kelton(1991), Simulation Modeling and Analysis)
*/

#if defined (HAVE_CONFIG_H)
#  include "config.h"
#endif

#include <cmath>

#include "lo-ieee.h"
#include "randgamma.h"
#include "randmtzig.h"

OCTAVE_BEGIN_NAMESPACE(octave)

template <typename T> void rand_gamma (T a, octave_idx_type n, T *r)
{
  octave_idx_type i;
  /* If a < 1, start by generating gamma (1+a) */
  const T d = (a < 1. ? 1.+a : a) - 1./3.;
  const T c = 1./std::sqrt (9.*d);

  /* Handle invalid cases */
  if (a <= 0 || lo_ieee_isinf (a))
    {
      for (i=0; i < n; i++)
        r[i] = numeric_limits<T>::NaN ();
      return;
    }

  for (i=0; i < n; i++)
    {
      T x, xsq, v, u;
    restart:
      x = rand_normal<T> ();
      v = (1+c*x);
      v *= (v*v);
      if (v <= 0)
        goto restart; /* rare, so don't bother moving up */
      u = rand_uniform<T> ();
      xsq = x*x;
      if (u >= 1.-0.0331*xsq*xsq && std::log (u) >= 0.5*xsq + d*(1-v+std::log (v)))
        goto restart;
      r[i] = d*v;
    }
  if (a < 1)
    {
      /* Use gamma(a) = gamma(1+a)*U^(1/a) */
      /* Given REXP = -log(U) then U^(1/a) = exp(-REXP/a) */
      for (i = 0; i < n; i++)
        r[i] *= exp (-rand_exponential<T> () / a);
    }
}

template OCTAVE_API void rand_gamma (double, octave_idx_type, double *);
template OCTAVE_API void rand_gamma (float, octave_idx_type, float *);

OCTAVE_END_NAMESPACE(octave)
