/*

Copyright (C) 2017-2018 John W. Eaton

This file is part of Octave.

Octave is free software: you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

Octave is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with Octave; see the file COPYING.  If not, see
<https://www.gnu.org/licenses/>.

*/

#if defined (HAVE_CONFIG_H)
#  include "config.h"
#endif

#include "pt-binop.h"
#include "pt-cbinop.h"
#include "pt-walk.h"

namespace octave
{
  void
  tree_walker::visit_boolean_expression (tree_boolean_expression& expr)
  {
    visit_binary_expression (expr);
  }

  void
  tree_walker::visit_compound_binary_expression (tree_compound_binary_expression& expr)
  {
    visit_binary_expression (expr);
  }
}
