////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2018-2023 The Octave Project Developers
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

#if defined (HAVE_CONFIG_H)
#  include "config.h"
#endif

#include "defun.h"
#include "interpreter.h"
#include "ov.h"
#include "ovl.h"
#include "settings.h"
#include "variables.h"

OCTAVE_BEGIN_NAMESPACE(octave)

settings::settings (void)
  : m_display_tokens (false), m_token_count (0),
    m_lexer_debug_flag (false)
{ }

octave_value settings::display_tokens (const octave_value_list& args,
                                       int nargout)
{
  return set_internal_variable (m_display_tokens, args, nargout,
                                "__display_tokens__");
}

octave_value settings::lexer_debug_flag (const octave_value_list& args,
    int nargout)
{
  return set_internal_variable (m_lexer_debug_flag, args, nargout,
                                "__lexer_debug_flag__");
}

DEFMETHOD (__display_tokens__, interp, args, nargout,
           doc: /* -*- texinfo -*-
@deftypefn  {} {@var{val} =} __display_tokens__ ()
@deftypefnx {} {@var{old_val} =} __display_tokens__ (@var{new_val})
@deftypefnx {} {@var{old_val} =} __display_tokens__ (@var{new_val}, "local")
Query or set the internal variable that determines whether Octave's
lexer displays tokens as they are read.
@seealso{__lexer_debug_flag__, __token_count__}
@end deftypefn */)
{
  settings& stgs = interp.get_settings ();

  return stgs.display_tokens (args, nargout);
}

DEFMETHOD (__token_count__, interp, , ,
           doc: /* -*- texinfo -*-
@deftypefn {} {@var{n} =} __token_count__ ()
Return the number of language tokens processed since Octave startup.
@seealso{__lexer_debug_flag__, __display_tokens__}
@end deftypefn */)
{
  settings& stgs = interp.get_settings ();

  return octave_value (stgs.token_count ());
}

DEFMETHOD (__lexer_debug_flag__, interp, args, nargout,
           doc: /* -*- texinfo -*-
@deftypefn  {} {@var{val} =} __lexer_debug_flag__ ()
@deftypefnx {} {@var{old_val} =} __lexer_debug_flag__ (@var{new_val})
Query or set the internal flag that determines whether Octave's lexer prints
debug information as it processes an expression.
@seealso{__display_tokens__, __token_count__, __parse_debug_flag__}
@end deftypefn */)
{
  settings& stgs = interp.get_settings ();

  return stgs.lexer_debug_flag (args, nargout);
}

OCTAVE_END_NAMESPACE(octave)
