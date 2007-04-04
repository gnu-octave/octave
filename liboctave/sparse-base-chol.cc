/*

Copyright (C) 2005 David Bateman
Copyright (C) 1998-2005 Andy Adler

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

#include "sparse-base-chol.h"
#include "sparse-util.h"
#include "lo-error.h"
#include "oct-sparse.h"
#include "oct-spparms.h"
#include "quit.h"
#include "MatrixType.h"

#ifdef HAVE_CHOLMOD
// Can't use CHOLMOD_NAME(drop)(0.0, S, cm). It doesn't treat complex matrices
template <class chol_type, class chol_elt, class p_type>
void 
sparse_base_chol<chol_type, chol_elt, p_type>::sparse_base_chol_rep::drop_zeros 
  (const cholmod_sparse* S)
{
  chol_elt sik;
  octave_idx_type *Sp, *Si;
  chol_elt *Sx;
  octave_idx_type pdest, k, ncol, p, pend;

  if (! S)
    return;

  Sp = static_cast<octave_idx_type *>(S->p);
  Si = static_cast<octave_idx_type *>(S->i);
  Sx = static_cast<chol_elt *>(S->x);
  pdest = 0;
  ncol = S->ncol;

  for (k = 0; k < ncol; k++)
    {
      p = Sp [k];
      pend = Sp [k+1];
      Sp [k] = pdest;
      for (; p < pend; p++)
	{
	  sik = Sx [p];
	  if (CHOLMOD_IS_NONZERO (sik))
	    {
	      if (p != pdest)
		{
		  Si [pdest] = Si [p];
		  Sx [pdest] = sik;
		}
	      pdest++;
	    }
	}
    }
  Sp [ncol] = pdest;
}
#endif

template <class chol_type, class chol_elt, class p_type>
octave_idx_type
sparse_base_chol<chol_type, chol_elt, p_type>::sparse_base_chol_rep::init 
  (const chol_type& a, bool natural)
{
  volatile octave_idx_type info = 0;
#ifdef HAVE_CHOLMOD
  octave_idx_type a_nr = a.rows ();
  octave_idx_type a_nc = a.cols ();

  if (a_nr != a_nc)
    {
      (*current_liboctave_error_handler) 
	("SparseCHOL requires square matrix");
      return -1;
    }

  cholmod_common *cm = &Common;

  // Setup initial parameters
  CHOLMOD_NAME(start) (cm);
  cm->prefer_zomplex = false;

  double spu = octave_sparse_params::get_key ("spumoni");
  if (spu == 0.)
    {
      cm->print = -1;
      cm->print_function = 0;
    }
  else
    {
      cm->print = static_cast<int> (spu) + 2;
      cm->print_function =&SparseCholPrint;
    }

  cm->error_handler = &SparseCholError;
  cm->complex_divide = CHOLMOD_NAME(divcomplex);
  cm->hypotenuse = CHOLMOD_NAME(hypot);

#ifdef HAVE_METIS
  // METIS 4.0.1 uses malloc and free, and will terminate if it runs
  // out of memory.  Use CHOLMOD's memory guard for METIS, which
  // allocates a huge block of memory (and then immediately frees it)
  // before calling METIS
  cm->metis_memory = 2.0;

#if defined(METIS_VERSION)
#if (METIS_VERSION >= METIS_VER(4,0,2))
  // METIS 4.0.2 uses function pointers for malloc and free.
  METIS_malloc = cm->malloc_memory;
  METIS_free   = cm->free_memory;
  // Turn off METIS memory guard.
  cm->metis_memory   = 0.0;
#endif
#endif
#endif

  cm->final_asis = false;
  cm->final_super = false;
  cm->final_ll = true;
  cm->final_pack = true;
  cm->final_monotonic = true;
  cm->final_resymbol = false;

  cholmod_sparse A;
  cholmod_sparse *ac = &A;
  double dummy;
  ac->nrow = a_nr;
  ac->ncol = a_nc;

  ac->p = a.cidx();
  ac->i = a.ridx();
  ac->nzmax = a.nnz();
  ac->packed = true;
  ac->sorted = true;
  ac->nz = 0;
#ifdef IDX_TYPE_LONG
  ac->itype = CHOLMOD_LONG;
#else
  ac->itype = CHOLMOD_INT;
#endif
  ac->dtype = CHOLMOD_DOUBLE;
  ac->stype = 1;
#ifdef OCTAVE_CHOLMOD_TYPE
  ac->xtype = OCTAVE_CHOLMOD_TYPE;
#else
  ac->xtype = CHOLMOD_REAL;
#endif

  if (a_nr < 1)
    ac->x = &dummy;
  else
    ac->x = a.data();

  // use natural ordering if no q output parameter
  if (natural)
    {
      cm->nmethods = 1 ;
      cm->method [0].ordering = CHOLMOD_NATURAL ;
      cm->postorder = false ;
    }

  cholmod_factor *Lfactor;
  BEGIN_INTERRUPT_IMMEDIATELY_IN_FOREIGN_CODE;
  Lfactor = CHOLMOD_NAME(analyze) (ac, cm);
  CHOLMOD_NAME(factorize) (ac, Lfactor, cm);
  cond = CHOLMOD_NAME(rcond) (Lfactor, cm);
  END_INTERRUPT_IMMEDIATELY_IN_FOREIGN_CODE;

  minor_p = Lfactor->minor;
  is_pd = cm->status == CHOLMOD_OK;
  info = (is_pd ? 0 : cm->status);

  BEGIN_INTERRUPT_IMMEDIATELY_IN_FOREIGN_CODE;
  Lsparse = CHOLMOD_NAME(factor_to_sparse) (Lfactor, cm);
  END_INTERRUPT_IMMEDIATELY_IN_FOREIGN_CODE;

  if (minor_p > 0 && minor_p < a_nr)
    {
      size_t n1 = a_nr + 1;
      Lsparse->p = CHOLMOD_NAME(realloc) (minor_p+1, sizeof(octave_idx_type),
				    Lsparse->p, &n1, cm);
      BEGIN_INTERRUPT_IMMEDIATELY_IN_FOREIGN_CODE;
      CHOLMOD_NAME(reallocate_sparse) 
	(static_cast<octave_idx_type *>(Lsparse->p)[minor_p], Lsparse, cm);
      END_INTERRUPT_IMMEDIATELY_IN_FOREIGN_CODE;
      Lsparse->ncol = minor_p;
    }

  drop_zeros (Lsparse);

  if (! natural)
    {
      perms.resize (a_nr);
      for (octave_idx_type i = 0; i < a_nr; i++)
	perms(i) = static_cast<octave_idx_type *>(Lfactor->Perm)[i];
    }

  static char tmp[] = " ";

  BEGIN_INTERRUPT_IMMEDIATELY_IN_FOREIGN_CODE;
  CHOLMOD_NAME(free_factor) (&Lfactor, cm);
  CHOLMOD_NAME(finish) (cm);
  CHOLMOD_NAME(print_common) (tmp, cm);
  END_INTERRUPT_IMMEDIATELY_IN_FOREIGN_CODE;
#else
  (*current_liboctave_error_handler) 
    ("Missing CHOLMOD. Sparse cholesky factorization disabled");
#endif
  return info;
}

template <class chol_type, class chol_elt, class p_type>
chol_type 
sparse_base_chol<chol_type, chol_elt, p_type>::L (void) const
{
#ifdef HAVE_CHOLMOD
  cholmod_sparse *m = rep->L();
  octave_idx_type nc = m->ncol;
  octave_idx_type nnz = m->nzmax;
  chol_type ret (m->nrow, nc, nnz);
  for (octave_idx_type j = 0; j < nc+1; j++)
    ret.xcidx(j) = static_cast<octave_idx_type *>(m->p)[j];
  for (octave_idx_type i = 0; i < nnz; i++)
    {
      ret.xridx(i) = static_cast<octave_idx_type *>(m->i)[i];
      ret.xdata(i) = static_cast<chol_elt *>(m->x)[i];
    }
  return ret;
#else
  return chol_type();
#endif
}

template <class chol_type, class chol_elt, class p_type>
p_type 
sparse_base_chol<chol_type, chol_elt, p_type>::
sparse_base_chol_rep::Q (void) const
{
#ifdef HAVE_CHOLMOD
  octave_idx_type n = Lsparse->nrow;
  p_type p (n, n, n);

  for (octave_idx_type i = 0; i < n; i++)
    {
      p.xcidx(i) = i;
      p.xridx(i) = static_cast<octave_idx_type>(perms(i));
      p.xdata(i) = 1;
    }
  p.xcidx(n) = n;

  return p;
#else
  return p_type();
#endif
}

template <class chol_type, class chol_elt, class p_type>
chol_type 
sparse_base_chol<chol_type, chol_elt, p_type>::inverse (void) const
{
  chol_type retval;
#ifdef HAVE_CHOLMOD
  cholmod_sparse *m = rep->L();
  octave_idx_type n = m->ncol;
  ColumnVector perms = rep->perm();
  chol_type ret;
  double rcond2;
  octave_idx_type info;
  MatrixType mattype (MatrixType::Upper);
  chol_type linv = L().transpose().inverse(mattype, info, rcond2, 1, 0);

  if (perms.length() == n)
    {
      p_type Qc = Q();
      retval = Qc * linv.transpose() * linv * Qc.transpose();
    }
  else
    retval = linv.transpose() * linv;
#endif
  return retval;
}

/*
;;; Local Variables: ***
;;; mode: C++ ***
;;; End: ***
*/
