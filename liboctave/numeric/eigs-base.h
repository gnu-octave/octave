/*

Copyright (C) 2005-2015 David Bateman

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

#if ! defined (octave_eigs_base_h)
#define octave_eigs_base_h 1

#include <iosfwd>

class ColumnVector;
class ComplexColumnVector;
class Matrix;
class ComplexMatrix;

typedef ColumnVector (*EigsFunc) (const ColumnVector& x, int& eigs_error);

typedef ComplexColumnVector (*EigsComplexFunc) (const ComplexColumnVector& x, int& eigs_error);

template <typename M>
octave_idx_type
EigsRealSymmetricMatrix (const M& m, const std::string typ,
                         octave_idx_type k, octave_idx_type p,
                         octave_idx_type& info, Matrix& eig_vec,
                         ColumnVector& eig_val, const M& _b,
                         ColumnVector& permB, ColumnVector& resid,
                         std::ostream& os, double tol, bool rvec,
                         bool cholB, int disp, int maxit);

template <typename M>
octave_idx_type
EigsRealSymmetricMatrixShift (const M& m, double sigma,
                              octave_idx_type k, octave_idx_type p,
                              octave_idx_type& info, Matrix& eig_vec,
                              ColumnVector& eig_val, const M& _b,
                              ColumnVector& permB, ColumnVector& resid,
                              std::ostream& os, double tol, bool rvec,
                              bool cholB, int disp, int maxit);

extern OCTAVE_API octave_idx_type
EigsRealSymmetricFunc (EigsFunc fun, octave_idx_type n,
                       const std::string& _typ, double sigma,
                       octave_idx_type k, octave_idx_type p,
                       octave_idx_type& info, Matrix& eig_vec,
                       ColumnVector& eig_val, ColumnVector& resid,
                       std::ostream& os, double tol, bool rvec,
                       bool /* cholB */, int disp, int maxit);

template <typename M>
octave_idx_type
EigsRealNonSymmetricMatrix (const M& m, const std::string typ,
                            octave_idx_type k, octave_idx_type p,
                            octave_idx_type& info, ComplexMatrix& eig_vec,
                            ComplexColumnVector& eig_val, const M& _b,
                            ColumnVector& permB, ColumnVector& resid,
                            std::ostream& os, double tol, bool rvec,
                            bool cholB, int disp, int maxit);

template <typename M>
octave_idx_type
EigsRealNonSymmetricMatrixShift (const M& m, double sigmar,
                                 octave_idx_type k, octave_idx_type p,
                                 octave_idx_type& info,
                                 ComplexMatrix& eig_vec,
                                 ComplexColumnVector& eig_val, const M& _b,
                                 ColumnVector& permB, ColumnVector& resid,
                                 std::ostream& os, double tol, bool rvec,
                                 bool cholB, int disp, int maxit);

extern OCTAVE_API octave_idx_type
EigsRealNonSymmetricFunc (EigsFunc fun, octave_idx_type n,
                          const std::string& _typ, double sigmar,
                          octave_idx_type k, octave_idx_type p,
                          octave_idx_type& info, ComplexMatrix& eig_vec,
                          ComplexColumnVector& eig_val, ColumnVector& resid,
                          std::ostream& os, double tol, bool rvec,
                          bool /* cholB */, int disp, int maxit);

template <typename M>
octave_idx_type
EigsComplexNonSymmetricMatrix (const M& m, const std::string typ,
                               octave_idx_type k, octave_idx_type p,
                               octave_idx_type& info, ComplexMatrix& eig_vec,
                               ComplexColumnVector& eig_val, const M& _b,
                               ColumnVector& permB,
                               ComplexColumnVector& cresid,
                               std::ostream& os, double tol, bool rvec,
                               bool cholB, int disp, int maxit);

template <typename M>
octave_idx_type
EigsComplexNonSymmetricMatrixShift (const M& m, Complex sigma,
                                    octave_idx_type k, octave_idx_type p,
                                    octave_idx_type& info,
                                    ComplexMatrix& eig_vec,
                                    ComplexColumnVector& eig_val, const M& _b,
                                    ColumnVector& permB,
                                    ComplexColumnVector& cresid,
                                    std::ostream& os, double tol, bool rvec,
                                    bool cholB, int disp, int maxit);

extern OCTAVE_API octave_idx_type
EigsComplexNonSymmetricFunc (EigsComplexFunc fun, octave_idx_type n,
                             const std::string& _typ, Complex sigma,
                             octave_idx_type k, octave_idx_type p,
                             octave_idx_type& info, ComplexMatrix& eig_vec,
                             ComplexColumnVector& eig_val,
                             ComplexColumnVector& cresid, std::ostream& os,
                             double tol, bool rvec, bool /* cholB */,
                             int disp, int maxit);

#endif
