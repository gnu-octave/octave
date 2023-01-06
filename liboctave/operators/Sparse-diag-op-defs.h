////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2009-2023 The Octave Project Developers
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

#if ! defined (octave_Sparse_diag_op_defs_h)
#define octave_Sparse_diag_op_defs_h 1

#include "octave-config.h"

#include "lo-array-errwarn.h"

// Matrix multiplication

template <typename RT, typename DM, typename SM>
RT do_mul_dm_sm (const DM& d, const SM& a)
{
  const octave_idx_type nr = d.rows ();
  const octave_idx_type nc = d.cols ();

  const octave_idx_type a_nr = a.rows ();
  const octave_idx_type a_nc = a.cols ();

  if (nc != a_nr)
    octave::err_nonconformant ("operator *", nr, nc, a_nr, a_nc);

  RT r (nr, a_nc, a.nnz ());

  octave_idx_type l = 0;

  for (octave_idx_type j = 0; j < a_nc; j++)
    {
      r.xcidx (j) = l;
      const octave_idx_type colend = a.cidx (j+1);
      for (octave_idx_type k = a.cidx (j); k < colend; k++)
        {
          const octave_idx_type i = a.ridx (k);
          if (i >= nr) break;
          r.xdata (l) = d.dgelem (i) * a.data (k);
          r.xridx (l) = i;
          l++;
        }
    }

  r.xcidx (a_nc) = l;

  r.maybe_compress (true);
  return r;
}

template <typename RT, typename SM, typename DM>
RT do_mul_sm_dm (const SM& a, const DM& d)
{
  const octave_idx_type nr = d.rows ();
  const octave_idx_type nc = d.cols ();

  const octave_idx_type a_nr = a.rows ();
  const octave_idx_type a_nc = a.cols ();

  if (nr != a_nc)
    octave::err_nonconformant ("operator *", a_nr, a_nc, nr, nc);

  const octave_idx_type mnc = (nc < a_nc ? nc: a_nc);
  RT r (a_nr, nc, a.cidx (mnc));

  for (octave_idx_type j = 0; j < mnc; ++j)
    {
      const typename DM::element_type s = d.dgelem (j);
      const octave_idx_type colend = a.cidx (j+1);
      r.xcidx (j) = a.cidx (j);
      for (octave_idx_type k = a.cidx (j); k < colend; ++k)
        {
          r.xdata (k) = s * a.data (k);
          r.xridx (k) = a.ridx (k);
        }
    }
  for (octave_idx_type j = mnc; j <= nc; ++j)
    r.xcidx (j) = a.cidx (mnc);

  r.maybe_compress (true);
  return r;
}

// FIXME: functors such as this should be gathered somewhere
template <typename T>
struct identity_val
{
public:
  typedef T argument_type;
  typedef T result_type;
  T operator () (const T x) { return x; }
};

// Matrix addition

template <typename RT, typename SM, typename DM, typename OpA, typename OpD>
RT inner_do_add_sm_dm (const SM& a, const DM& d, OpA opa, OpD opd)
{
  using std::min;
  const octave_idx_type nr = d.rows ();
  const octave_idx_type nc = d.cols ();
  const octave_idx_type n = min (nr, nc);

  const octave_idx_type a_nr = a.rows ();
  const octave_idx_type a_nc = a.cols ();

  const octave_idx_type nz = a.nnz ();
  RT r (a_nr, a_nc, nz + n);
  octave_idx_type k = 0;

  for (octave_idx_type j = 0; j < nc; ++j)
    {
      octave_quit ();
      const octave_idx_type colend = a.cidx (j+1);
      r.xcidx (j) = k;
      octave_idx_type k_src = a.cidx (j), k_split;

      for (k_split = k_src; k_split < colend; k_split++)
        if (a.ridx (k_split) >= j)
          break;

      for (; k_src < k_split; k_src++, k++)
        {
          r.xridx (k) = a.ridx (k_src);
          r.xdata (k) = opa (a.data (k_src));
        }

      if (k_src < colend && a.ridx (k_src) == j)
        {
          r.xridx (k) = j;
          r.xdata (k) = opa (a.data (k_src)) + opd (d.dgelem (j));
          k++; k_src++;
        }
      else
        {
          r.xridx (k) = j;
          r.xdata (k) = opd (d.dgelem (j));
          k++;
        }

      for (; k_src < colend; k_src++, k++)
        {
          r.xridx (k) = a.ridx (k_src);
          r.xdata (k) = opa (a.data (k_src));
        }

    }
  r.xcidx (nc) = k;

  r.maybe_compress (true);
  return r;
}

template <typename RT, typename DM, typename SM>
RT do_commutative_add_dm_sm (const DM& d, const SM& a)
{
  // Extra function to ensure this is only emitted once.
  return inner_do_add_sm_dm<RT> (a, d,
                                 identity_val<typename SM::element_type> (),
                                 identity_val<typename DM::element_type> ());
}

template <typename RT, typename DM, typename SM>
RT do_add_dm_sm (const DM& d, const SM& a)
{
  if (a.rows () != d.rows () || a.cols () != d.cols ())
    octave::err_nonconformant ("operator +",
                               d.rows (), d.cols (), a.rows (), a.cols ());
  else
    return do_commutative_add_dm_sm<RT> (d, a);
}

template <typename RT, typename DM, typename SM>
RT do_sub_dm_sm (const DM& d, const SM& a)
{
  if (a.rows () != d.rows () || a.cols () != d.cols ())
    octave::err_nonconformant ("operator -",
                               d.rows (), d.cols (), a.rows (), a.cols ());

  return inner_do_add_sm_dm<RT> (a, d,
                                 std::negate<typename SM::element_type> (),
                                 identity_val<typename DM::element_type> ());
}

template <typename RT, typename SM, typename DM>
RT do_add_sm_dm (const SM& a, const DM& d)
{
  if (a.rows () != d.rows () || a.cols () != d.cols ())
    octave::err_nonconformant ("operator +",
                               a.rows (), a.cols (), d.rows (), d.cols ());

  return do_commutative_add_dm_sm<RT> (d, a);
}

template <typename RT, typename SM, typename DM>
RT do_sub_sm_dm (const SM& a, const DM& d)
{
  if (a.rows () != d.rows () || a.cols () != d.cols ())
    octave::err_nonconformant ("operator -",
                               a.rows (), a.cols (), d.rows (), d.cols ());

  return inner_do_add_sm_dm<RT> (a, d,
                                 identity_val<typename SM::element_type> (),
                                 std::negate<typename DM::element_type> ());
}

#endif
