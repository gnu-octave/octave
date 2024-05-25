////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2014-2024 The Octave Project Developers
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

#if defined (HAVE_QSCINTILLA)

#include <Qsci/qscilexer.h>

#include "console-lexer.h"
#include "gui-settings.h"
#include "gui-preferences-cs.h"
#include "gui-utils.h"


OCTAVE_BEGIN_NAMESPACE(octave)

QString
console_lexer::description (int style) const
{
  switch (style)
    {
      case Default:
        return tr ("Default");
      case Error:
        return tr ("Error");
      case Prompt:
        return tr ("Prompt");
      default:
        return QString ();
    }
};

const char *
console_lexer::language () const
{
  return "Console Output";
}

const char *
console_lexer::lexer () const
{
  return "console-output";
}

// Returns the foreground colour of the text for a style.
QColor
console_lexer::defaultColor(int style) const
{
  gui_settings settings;

  int mode = settings.int_value (cs_color_mode);
  QColor fgc = settings.color_value (cs_colors[0], mode);
  QColor bgc = settings.color_value (cs_colors[1], mode);

  switch (style)
    {
      case Default:
        return fgc;

      case Error:
        return interpolate_color (cs_error_color, fgc,
                                  cs_error_interp[0], cs_error_interp[1]);

      case Prompt:
        return interpolate_color (fgc, bgc,
                                  cs_prompt_interp[0], cs_prompt_interp[1]);

      default:
        return fgc;
  }
}


// Returns the font of the text for a style.
QFont
console_lexer::defaultFont(int style) const
{
  gui_settings settings;
  QFont f (settings.string_value (cs_font));

  switch (style)
    {
      case Default:
      case Error:
      case Prompt:

      default:
        return f;
    }
}

OCTAVE_END_NAMESPACE(octave)

#endif
