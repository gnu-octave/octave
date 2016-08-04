// Copyright (C) 1996, 1997 John W. Eaton
// Copyright (C) 2006 Pascal Dupuis <Pascal.Dupuis@uclouvain.be>
//
// This program is free software; you can redistribute it and/or modify it under
// the terms of the GNU General Public License as published by the Free Software
// Foundation; either version 3 of the License, or (at your option) any later
// version.
//
// This program is distributed in the hope that it will be useful, but WITHOUT
// ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
// FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
// details.
//
// You should have received a copy of the GNU General Public License along with
// this program; if not, see <http://www.gnu.org/licenses/>.

#if !defined (octave_ComplexGSVD_h)
#define octave_ComplexGSVD_h 1

#include "octave-config.h"

#include <iosfwd>

#include "dDiagMatrix.h"
#include "CMatrix.h"
#include "dbleGSVD.h"

class
ComplexGSVD
{
public:

  ComplexGSVD (void) { }

  ComplexGSVD (const ComplexMatrix& a, const ComplexMatrix& b,  
           GSVD::type gsvd_type = GSVD::economy) 
    { 
      init (a, b, gsvd_type); 
    }

  ComplexGSVD (const ComplexMatrix& a, const ComplexMatrix& b, 
           octave_idx_type& info, GSVD::type gsvd_type = GSVD::economy)
    {
      info = init (a, b, gsvd_type);
    }

  ComplexGSVD (const ComplexGSVD& a)
    : type_computed (a.type_computed),
      sigmaA (a.sigmaA), sigmaB (a.sigmaB), 
      left_smA (a.left_smA), left_smB (a.left_smB), right_sm (a.right_sm),
      R(a.R) { }

  ComplexGSVD& operator = (const ComplexGSVD& a)
    {
      if (this != &a)
    {
      type_computed = a.type_computed;
      sigmaA = a.sigmaA;
      sigmaB = a.sigmaB;
      left_smA = a.left_smA;
      left_smB = a.left_smB;
      right_sm = a.right_sm;
      R = a.R;
    }

      return *this;
    }

  ~ComplexGSVD (void) { }

  DiagMatrix singular_values_A (void) const { return sigmaA; }
  DiagMatrix singular_values_B (void) const { return sigmaB; }

  ComplexMatrix left_singular_matrix_A (void) const;
  ComplexMatrix left_singular_matrix_B (void) const;

  ComplexMatrix right_singular_matrix (void) const;
  ComplexMatrix R_matrix (void) const;

  friend std::ostream&  operator << (std::ostream& os, const ComplexGSVD& a);

private:

  GSVD::type type_computed;

  DiagMatrix sigmaA, sigmaB;
  ComplexMatrix left_smA, left_smB;
  ComplexMatrix right_sm, R;

  octave_idx_type init (const ComplexMatrix& a, const ComplexMatrix& b, 
            GSVD::type gsvd_type = GSVD::economy);
};

#endif
