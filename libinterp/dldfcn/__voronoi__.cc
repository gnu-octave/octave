////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2000-2023 The Octave Project Developers
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

/*
20. Augiust 2000 - Kai Habel: first release
*/

/*
2003-12-14 Rafael Laboissiere <rafael@laboissiere.net>
Added optional second argument to pass options to the underlying
qhull command
*/

#if defined (HAVE_CONFIG_H)
#  include "config.h"
#endif

#include <cstdio>

#include <limits>
#include <string>

#include "Array.h"
#include "boolMatrix.h"
#include "dMatrix.h"
#include "dRowVector.h"
#include "oct-locbuf.h"
#include "unwind-prot.h"

#include "Cell.h"
#include "defun-dld.h"
#include "error.h"
#include "errwarn.h"
#include "ov.h"
#include "ovl.h"

#if defined (HAVE_QHULL)
#  include "oct-qhull.h"
#  if defined (NEED_QHULL_R_VERSION)
char qh_version[] = "__voronoi__.oct 2007-07-24";
#  endif
#endif

OCTAVE_BEGIN_NAMESPACE(octave)

#if defined (HAVE_QHULL)

static void
free_qhull_memory (qhT *qh)
{
  qh_freeqhull (qh, ! qh_ALL);

  int curlong, totlong;
  qh_memfreeshort (qh, &curlong, &totlong);

  if (curlong || totlong)
    warning ("__voronoi__: did not free %d bytes of long memory (%d pieces)",
             totlong, curlong);
}

static bool
octave_qhull_dims_ok (octave_idx_type dim, octave_idx_type n, const char *who)
{
  if (sizeof (octave_idx_type) > sizeof (int))
    {
      int maxval = std::numeric_limits<int>::max ();

      if (dim > maxval || n > maxval)
        error ("%s: dimension too large for Qhull", who);
    }

  return true;
}

#endif

DEFUN_DLD (__voronoi__, args, ,
           doc: /* -*- texinfo -*-
@deftypefn  {} {@var{C}, @var{F} =} __voronoi__ (@var{caller}, @var{pts})
@deftypefnx {} {@var{C}, @var{F} =} __voronoi__ (@var{caller}, @var{pts}, @var{options})
@deftypefnx {} {@var{C}, @var{F}, @var{Inf_Pts} =} __voronoi__ (@dots{})
Undocumented internal function.
@end deftypefn */)
{
#if defined (HAVE_QHULL)

  int nargin = args.length ();

  if (nargin < 2 || nargin > 3)
    print_usage ();

  std::string caller = args(0).xstring_value ("__voronoi__: CALLER must be a string");

  octave_value_list retval;

  Matrix points = args(1).matrix_value ();
  const octave_idx_type dim = points.columns ();
  const octave_idx_type num_points = points.rows ();

  if (! octave_qhull_dims_ok (dim, num_points, "__voronoi__"))
    return ovl (0.0);

  points = points.transpose ();

  std::string options;

  if (dim <= 3)
    options = " Qbb";
  else
    options = " Qbb Qx";

  if (nargin == 3)
    {
      octave_value opt_arg = args(2);

      if (opt_arg.is_string ())
        options = ' ' + opt_arg.string_value ();
      else if (opt_arg.isempty ())
        ; // Use default options.
      else if (opt_arg.iscellstr ())
        {
          options = "";

          Array<std::string> tmp = opt_arg.cellstr_value ();

          for (octave_idx_type i = 0; i < tmp.numel (); i++)
            options += ' ' + tmp(i);
        }
      else
        error ("%s: OPTIONS must be a string, cell array of strings, or empty",
               caller.c_str ());
    }

  boolT ismalloc = false;

  // Set the outfile pointer to stdout for status information.
  FILE *outfile = nullptr;
  FILE *errfile = stderr;

  qhT context = { };
  qhT *qh = &context;

  std::string cmd = "qhull v" + options;

  int exitcode = qh_new_qhull (qh, dim, num_points, points.fortran_vec (),
                               ismalloc, &cmd[0], outfile, errfile);

  unwind_action free_memory ([qh] () { free_qhull_memory (qh); });

  if (exitcode)
    error ("%s: qhull failed", caller.c_str ());

  // Calling findgood_all provides the number of Voronoi vertices
  // (sets qh->num_good).

  qh_findgood_all (qh, qh->facet_list);

  octave_idx_type num_voronoi_regions
    = qh->num_vertices - qh_setsize (qh, qh->del_vertices);

  octave_idx_type num_voronoi_vertices = qh->num_good;

  // Find the voronoi centers for all facets.

  qh_setvoronoi_all (qh);

  facetT *facet;
  vertexT *vertex;
  octave_idx_type k;

  // Find the number of Voronoi vertices for each Voronoi cell and
  // store them in NI so we can use them later to set the dimensions
  // of the RowVector objects used to collect them.

  FORALLfacets
  {
    facet->seen = false;
  }

  OCTAVE_LOCAL_BUFFER (octave_idx_type, ni, num_voronoi_regions);
  for (octave_idx_type i = 0; i < num_voronoi_regions; i++)
    ni[i] = 0;

  k = 0;

  FORALLvertices
  {
    if (qh->hull_dim == 3)
      qh_order_vertexneighbors (qh, vertex);

    bool infinity_seen = false;

    facetT *neighbor, * *neighborp;

    FOREACHneighbor_ (vertex)
    {
      if (neighbor->upperdelaunay)
        {
          if (! infinity_seen)
            {
              infinity_seen = true;
              ni[k]++;
            }
        }
      else
        {
          neighbor->seen = true;
          ni[k]++;
        }
    }

    k++;
  }

  // If Qhull finds fewer regions than points, we will pad the end
  // of the at_inf and C arrays so that they always contain at least
  // as many elements as the given points array.

  // FIXME: is it possible (or does it make sense) for
  // num_voronoi_regions to ever be larger than num_points?

  octave_idx_type nr = (num_points > num_voronoi_regions
                        ? num_points : num_voronoi_regions);

  boolMatrix at_inf (nr, 1, false);

  // The list of Voronoi vertices.  The first element is always
  // Inf.
  Matrix F (num_voronoi_vertices+1, dim);

  for (octave_idx_type d = 0; d < dim; d++)
    F(0, d) = numeric_limits<double>::Inf ();

  // The cell array of vectors of indices into F that represent the
  // vertices of the Voronoi regions (cells).

  Cell C (nr, 1);

  // Now loop through the list of vertices again and store the
  // coordinates of the Voronoi vertices and the lists of indices
  // for the cells.

  FORALLfacets
  {
    facet->seen = false;
  }

  octave_idx_type i = 0;
  k = 0;

  FORALLvertices
  {
    if (qh->hull_dim == 3)
      qh_order_vertexneighbors (qh, vertex);

    bool infinity_seen = false;

    octave_idx_type idx = qh_pointid (qh, vertex->point);

    octave_idx_type num_vertices = ni[k++];

    // Qhull seems to sometimes produces regions with a single
    // vertex.  Is that a bug?  How can a region have just one
    // vertex?  Let's skip it.

    if (num_vertices == 1)
      continue;

    RowVector facet_list (num_vertices);

    octave_idx_type m = 0;

    facetT *neighbor, * *neighborp;

    FOREACHneighbor_(vertex)
    {
      if (neighbor->upperdelaunay)
        {
          if (! infinity_seen)
            {
              infinity_seen = true;
              facet_list(m++) = 1;
              at_inf(idx) = true;
            }
        }
      else
        {
          if (! neighbor->seen)
            {
              i++;
              for (octave_idx_type d = 0; d < dim; d++)
                F(i, d) = neighbor->center[d];

              neighbor->seen = true;
              neighbor->visitid = i;
            }

          facet_list(m++) = neighbor->visitid + 1;
        }
    }

    C(idx) = facet_list;
  }

  retval = ovl (F, C, at_inf);

  return retval;

#else

  octave_unused_parameter (args);

  std::string caller
    = (args.length () > 0
       ? args(0).xstring_value ("__voronoi__: CALLER must be a string")
       : "__voronoi__");

  err_disabled_feature (caller, "Qhull");

#endif
}

/*
## No test needed for internal helper function.
%!assert (1)
*/

OCTAVE_END_NAMESPACE(octave)
