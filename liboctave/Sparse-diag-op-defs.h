/* -*- C++ -*-

Copyright (C) 2009 Jason Riedy, Jaroslav Hajek

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

#if !defined (octave_sparse_diag_op_defs_h)
#define octave_sparse_diag_op_defs_h 1

template <typename RT, typename DM, typename SM>
RT do_mul_dm_sm (const DM& d, const SM& a)
{
  const octave_idx_type nr = d.rows ();
  const octave_idx_type nc = d.cols ();

  const octave_idx_type a_nr = a.rows ();
  const octave_idx_type a_nc = a.cols ();

  if (nc != a_nr)
    {
      gripe_nonconformant ("operator *", nr, nc, a_nr, a_nc);
      return RT ();
    }
  else
   {
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
}

template <typename RT, typename SM, typename DM>
RT do_mul_sm_dm (const SM& a, const DM& d)
{
  const octave_idx_type nr = d.rows ();
  const octave_idx_type nc = d.cols ();

  const octave_idx_type a_nr = a.rows ();
  const octave_idx_type a_nc = a.cols ();

  if (nr != a_nc)
    {
      gripe_nonconformant ("operator *", a_nr, a_nc, nr, nc);
      return RT ();
    }
  else
   {

     const octave_idx_type mnc = nc < a_nc ? nc: a_nc;
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
}

#endif // octave_sparse_diag_op_defs_h
