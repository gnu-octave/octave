////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2019-2023 The Octave Project Developers
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

 References:

 @article{
    title = {Particle Tracing Algorithms for 3D Curvilinear Grids},
    year = {2000},
    author = {Nielson, Gregory and Uller, H. and Sadarjoen, I. and Walsum, Theo and Hin, Andrea and Post, Frits}
 }

 @article{
    title = {Sources of error in the graphical analysis of CFD results},
    publisher = {Journal of Scientific Computing},
    year = {1988},
    volume = {3},
    number = {2},
    pages = {149--164},
    author = {Buning, Pieter G.},
 }

*/

#if defined (HAVE_CONFIG_H)
#  include "config.h"
#endif

#include "defun.h"
#include "error.h"
#include "ovl.h"

OCTAVE_BEGIN_NAMESPACE(octave)

// Coordinates of a point in C-Space (unit square mesh)

struct Vector2
{
  double x, y;
};

// The integer value and the fractional value from a point in C-Space.
// Equivalent to the cell index the point is located in and the local
// coordinates of the point in the cell.

struct Cell2
{
  double fcx, fcy;
  signed long idx, idy;
};

struct Vector3
{
  double x, y, z;
};

struct Cell3
{
  double fcx, fcy, fcz;
  signed long idx, idy, idz;
};

static inline void
number_to_fractional (signed long *id, double *fc, const double u)
{
  *id = floor (u);
  *fc = u - *id;
}

static inline octave_idx_type
handle_border_index (const octave_idx_type id, const octave_idx_type N)
{
  return (id < N - 1 ? id : N - 2);
}

static inline void
handle_border (octave_idx_type *id2, double *fc2, const octave_idx_type id1,
               const double fc1, const octave_idx_type N)
{
  if (id1 < N - 1)
    {
      *id2 = id1;
      *fc2 = fc1;
    }
  else
    {
      *id2 = N - 2;
      *fc2 = 1.0;
    }
}

static inline double
bilinear (const double u11, const double u21, const double u12,
          const double u22, const double x, const double y)
{
  return (u11 * (1-x) * (1-y) +
          u21 * x * (1-y) +
          u12 * (1-x) * y +
          u22 * x * y);
}

static inline Cell2
vector_to_cell2d (const Vector2 X)
{
  Cell2 Z;

  number_to_fractional (&Z.idx, &Z.fcx, X.x);
  number_to_fractional (&Z.idy, &Z.fcy, X.y);

  return (Z);
}

static inline bool
is_in_definition_set2d (const Cell2 X, const octave_idx_type cols,
                        const octave_idx_type rows)
{
  return ( (((X.idx >= 0) && (X.idx < cols-1)) ||
            ((X.idx == cols-1) && (X.fcx == 0.0))) &&
           (((X.idy >= 0) && (X.idy < rows-1)) ||
            ((X.idy == rows-1) && (X.fcy == 0.0))) );
}

static inline Vector2
add2d (const Cell2 X, const Vector2 Y)
{
  Vector2 Z = {X.idx + X.fcx + Y.x,
               X.idy + X.fcy + Y.y
              };

  return (Z);
}

static inline Vector2
vector_interpolation2d (const Cell2 X, const Matrix& u, const Matrix& v,
                        const octave_idx_type cols, const octave_idx_type rows)
{
  Vector2 V;
  double fcx, fcy;
  octave_idx_type idx, idy;

  handle_border (&idx, &fcx, X.idx, X.fcx, cols);
  handle_border (&idy, &fcy, X.idy, X.fcy, rows);

  V.x = bilinear (u(idy, idx), u(idy, idx+1), u(idy+1, idx),
                  u(idy+1, idx+1), fcx, fcy);
  V.y = bilinear (v(idy, idx), v(idy, idx+1), v(idy+1, idx),
                  v(idy+1, idx+1), fcx, fcy);

  return (V);
}

// Apply the Jacobian matrix on the vector V.
// The step vector length is set to h.

static inline Vector2
calculate_step_vector2d (const Cell2 X, const Vector2 V,
                         const RowVector& tx, const RowVector& ty,
                         const octave_idx_type cols, const octave_idx_type rows,
                         const double h)
{
  Vector2 S;

  const octave_idx_type idx = handle_border_index (X.idx, cols);
  const octave_idx_type idy = handle_border_index (X.idy, rows);

  const double x = V.x * tx(idx);
  const double y = V.y * ty(idy);
  const double n = 1.0 / sqrt (x*x + y*y);
  S.x = h * n * x;
  S.y = h * n * y;

  return (S);
}

static inline bool
is_singular2d (const Vector2 V)
{
  return ((math::isnan (V.x) || math::isnan (V.y)) ||
          ((V.x == 0) && (V.y == 0)));
}

static void
euler2d (const octave_idx_type cols, const octave_idx_type rows,
         const Matrix& u, const Matrix& v,
         const RowVector& tx, const RowVector& ty,
         const double zeta, const double xi,
         const double h, const octave_idx_type maxnverts,
         Matrix& buffer, octave_idx_type *nverts)
{
  Vector2 V0, V1, S0, X1, Xnxt, S1;
  const Vector2 X0 = {zeta, xi};
  Cell2 X0f, X1f;

  octave_idx_type i = 0;

  buffer(i, 0) = X0.x;
  buffer(i, 1) = X0.y;

  X0f = vector_to_cell2d (X0);
  while (true)
    {
      if (! is_in_definition_set2d (X0f, cols, rows))
        break;

      V0 = vector_interpolation2d (X0f, u, v, cols, rows);
      if (is_singular2d (V0))
        break;

      S0 = calculate_step_vector2d (X0f, V0, tx, ty, cols, rows, h);

      X1 = add2d (X0f, S0);
      X1f = vector_to_cell2d (X1);
      if (! is_in_definition_set2d (X1f, cols, rows))
        break;

      V1 = vector_interpolation2d (X1f, u, v, cols, rows);
      if (is_singular2d (V1))
        break;

      S1 = calculate_step_vector2d (X1f, V1, tx, ty, cols, rows, h);

      // Runge Kutta - Heun's Scheme
      const Vector2 S = {0.5 * (S0.x + S1.x),
                         0.5 * (S0.y + S1.y)
                        };
      Xnxt = add2d (X0f, S);

      X0f = vector_to_cell2d (Xnxt);
      if (! is_in_definition_set2d (X0f, cols, rows))
        break;

      i++;
      buffer(i, 0) = Xnxt.x;
      buffer(i, 1) = Xnxt.y;

      if (i + 1 >= maxnverts)
        break;
    }

  *nverts = i + 1;
}

static inline double
trilinear (const double u111, const double u211, const double u121,
           const double u221, const double u112, const double u212,
           const double u122, const double u222,
           const double x, const double y, const double z)
{
  return (u111 * (1-x) * (1-y) * (1-z) +
          u211 * x * (1-y) * (1-z) +
          u121 * (1-x) * y * (1-z) +
          u221 * x * y * (1-z) +
          u112 * (1-x) * (1-y) * z +
          u212 * x * (1-y) * z +
          u122 * (1-x) * y * z +
          u222 * x * y * z);
}

static inline Cell3
vector_to_cell3d (const Vector3 X)
{
  Cell3 Z;

  number_to_fractional (&Z.idx, &Z.fcx, X.x);
  number_to_fractional (&Z.idy, &Z.fcy, X.y);
  number_to_fractional (&Z.idz, &Z.fcz, X.z);

  return (Z);
}

static inline bool
is_in_definition_set3d (const Cell3 X, const octave_idx_type nx,
                        const octave_idx_type ny, const octave_idx_type nz)
{
  return ( (((X.idx >= 0) && (X.idx < nx-1)) ||
            ((X.idx == nx-1) && (X.fcx == 0.0))) &&
           (((X.idy >= 0) && (X.idy < ny-1)) ||
            ((X.idy == ny-1) && (X.fcy == 0.0))) &&
           (((X.idz >= 0) && (X.idz < nz-1)) ||
            ((X.idz == nz-1) && (X.fcz == 0.0))) );
}

static inline Vector3
add3d (const Cell3 X, const Vector3 Y)
{
  Vector3 Z = {X.idx + X.fcx + Y.x,
               X.idy + X.fcy + Y.y,
               X.idz + X.fcz + Y.z
              };

  return (Z);
}

static inline Vector3
vector_interpolation3d (const Cell3 X, const NDArray& u, const NDArray& v,
                        const NDArray& w, const octave_idx_type nx,
                        const octave_idx_type ny, const octave_idx_type nz)
{
  Vector3 V;
  double fcx, fcy, fcz;
  octave_idx_type idx, idy, idz;

  handle_border (&idx, &fcx, X.idx, X.fcx, nx);
  handle_border (&idy, &fcy, X.idy, X.fcy, ny);
  handle_border (&idz, &fcz, X.idz, X.fcz, nz);

  V.x = trilinear (u(idy, idx, idz), u(idy, idx+1, idz),
                   u(idy+1, idx, idz), u(idy+1, idx+1, idz),
                   u(idy, idx, idz+1), u(idy, idx+1, idz+1),
                   u(idy+1, idx, idz+1), u(idy+1, idx+1, idz+1),
                   fcx, fcy, fcz);
  V.y = trilinear (v(idy, idx, idz), v(idy, idx+1, idz),
                   v(idy+1, idx, idz), v(idy+1, idx+1, idz),
                   v(idy, idx, idz+1), v(idy, idx+1, idz+1),
                   v(idy+1, idx, idz+1), v(idy+1, idx+1, idz+1),
                   fcx, fcy, fcz);
  V.z = trilinear (w(idy, idx, idz), w(idy, idx+1, idz),
                   w(idy+1, idx, idz), w(idy+1, idx+1, idz),
                   w(idy, idx, idz+1), w(idy, idx+1, idz+1),
                   w(idy+1, idx, idz+1), w(idy+1, idx+1, idz+1),
                   fcx, fcy, fcz);

  return (V);
}

static inline Vector3
calculate_step_vector3d (const Cell3 X, const Vector3 V,
                         const RowVector& tx, const RowVector& ty, const RowVector& tz,
                         const octave_idx_type nx, const octave_idx_type ny,
                         const octave_idx_type nz, const double h)
{
  Vector3 S;

  const octave_idx_type idx = handle_border_index (X.idx, nx);
  const octave_idx_type idy = handle_border_index (X.idy, ny);
  const octave_idx_type idz = handle_border_index (X.idz, nz);

  const double x = V.x * tx(idx);
  const double y = V.y * ty(idy);
  const double z = V.z * tz(idz);
  const double n = 1.0 / sqrt (x*x + y*y + z*z);
  S.x = h * n * x;
  S.y = h * n * y;
  S.z = h * n * z;

  return (S);
}

static inline bool
is_singular3d (const Vector3 V)
{
  return ((math::isnan (V.x) || math::isnan (V.y) || math::isnan (V.z)) ||
          ((V.x == 0) && (V.y == 0) && (V.z == 0)));
}

static void
euler3d (const octave_idx_type nx, const octave_idx_type ny, const octave_idx_type nz,
         const NDArray& u, const NDArray& v, const NDArray& w,
         const RowVector& tx, const RowVector& ty, const RowVector& tz,
         const double zeta, const double xi, const double rho,
         const double h, const octave_idx_type maxnverts,
         Matrix& buffer, octave_idx_type *nverts)
{
  Vector3 V0, V1, S0, X1, Xnxt, S1;
  const Vector3 X0 = {zeta, xi, rho};
  Cell3 X0f, X1f;

  octave_idx_type i = 0;
  buffer(i, 0) = X0.x;
  buffer(i, 1) = X0.y;
  buffer(i, 2) = X0.z;

  X0f = vector_to_cell3d (X0);
  while (true)
    {
      if (! is_in_definition_set3d (X0f, nx, ny, nz))
        break;

      V0 = vector_interpolation3d (X0f, u, v, w, nx, ny, nz);
      if (is_singular3d (V0))
        break;

      S0 = calculate_step_vector3d (X0f, V0, tx, ty, tz, nx, ny, nz, h);

      X1 = add3d (X0f, S0);

      X1f = vector_to_cell3d (X1);
      if (! is_in_definition_set3d (X1f, nx, ny, nz))
        break;

      V1 = vector_interpolation3d (X1f, u, v, w, nx, ny, nz);
      if (is_singular3d (V1))
        break;

      S1 = calculate_step_vector3d (X1f, V1, tx, ty, tz, nx, ny, nz, h);

      // Runge Kutta - Heun's Scheme
      const Vector3 S = {0.5 * (S0.x + S1.x),
                         0.5 * (S0.y + S1.y),
                         0.5 * (S0.z + S1.z)
                        };
      Xnxt = add3d (X0f, S);

      X0f = vector_to_cell3d (Xnxt);
      if (! is_in_definition_set3d (X0f, nx, ny, nz))
        break;

      i++;
      buffer(i, 0) = Xnxt.x;
      buffer(i, 1) = Xnxt.y;
      buffer(i, 2) = Xnxt.z;

      if (i + 1 >= maxnverts)
        break;

    }

  *nverts = i + 1;
}

static octave_value
streameuler2d_internal (const octave_value_list& args)
{

  const int nargin = args.length ();
  if (nargin != 8)
    print_usage ();

  const Matrix U = args(0).matrix_value ();
  const Matrix V = args(1).matrix_value ();
  const RowVector TX = args(2).row_vector_value ();
  const RowVector TY = args(3).row_vector_value ();
  const double zeta = args(4).double_value ();
  const double xi = args(5).double_value ();
  const double h = args(6).double_value ();
  const octave_idx_type maxnverts = args(7).idx_type_value ();

  const octave_idx_type rows = U.rows ();
  const octave_idx_type cols = U.columns ();

  octave_idx_type nverts;
  Matrix buffer (maxnverts, 2);

  euler2d (cols, rows, U, V, TX, TY, zeta, xi, h, maxnverts,
           buffer, &nverts);

  Matrix xy = buffer.extract (0, 0, nverts-1, 1);

  return octave_value (xy);
}

static octave_value
streameuler3d_internal (const octave_value_list& args, const char *fcn)
{

  const int nargin = args.length ();
  if (nargin != 11)
    print_usage ();

  const NDArray U = args(0).array_value ();
  const NDArray V = args(1).array_value ();
  const NDArray W = args(2).array_value ();
  const RowVector TX = args(3).row_vector_value ();
  const RowVector TY = args(4).row_vector_value ();
  const RowVector TZ = args(5).row_vector_value ();
  const double zeta = args(6).double_value ();
  const double xi = args(7).double_value ();
  const double rho = args(8).double_value ();
  const double h = args(9).double_value ();
  const octave_idx_type maxnverts = args(10).idx_type_value ();

  const dim_vector dims = args(0).dims ();
  const int ndims = dims.ndims ();
  if (ndims != 3)
    error ("%s: dimension must be 3", fcn);

  octave_idx_type nverts;
  Matrix buffer (maxnverts, 3);

  euler3d (dims(1), dims(0), dims(2), U, V, W, TX, TY, TZ, zeta, xi, rho,
           h, maxnverts, buffer, &nverts);

  Matrix xyz = buffer.extract (0, 0, nverts-1, 2);

  return octave_value (xyz);
}

DEFUN (__streameuler2d__, args, ,
       doc: /* -*- texinfo -*-
@deftypefn {} {@var{output} =} __streameuler2d__ (@var{U}, @var{V}, @var{TX}, @var{TY}, @var{ZETA}, @var{XI}, @var{H}, @var{MAXNVERTS})
Calculate the streamline in a vector field @code{[@var{U}, @var{V}]} starting
from a seed point at position @code{[@var{ZETA}, @var{XI}]}.  The integrator
used is Heun's Scheme.  The step size can be controlled by @var{H}.  The
Jacobian matrix can be defined for each grid cell by
@code{[@var{TX}, @var{TY}]}.

@seealso{streamline, stream2, stream3, __streameuler3d__}
@end deftypefn */)
{
  return streameuler2d_internal (args);
}

DEFUN (__streameuler3d__, args, ,
       doc: /* -*- texinfo -*-
@deftypefn {} {@var{output} =} __streameuler3d__ (@var{U}, @var{V}, @var{W}, @var{TX}, @var{TY}, @var{TZ}, @var{ZETA}, @var{XI}, @var{RHO}, @var{H}, @var{MAXNVERTS})
Calculate the streamline in a vector field @code{[@var{U}, @var{V}, @var{W}]}
starting from a seed point at position
@code{[@var{ZETA}, @var{XI}, @var{RHO}]}.  The integrator used is Heun's
Scheme.  The step size can be controlled by @var{H}.  The Jacobian matrix can
be defined for each grid cell by @code{[@var{TX}, @var{TY}, @var{TZ}]}.

@seealso{streamline, stream2, stream3, __streameuler2d__}
@end deftypefn */)
{
  return streameuler3d_internal (args, "__streameuler3d__");
}

OCTAVE_END_NAMESPACE(octave)
