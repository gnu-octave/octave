////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2011-2023 The Octave Project Developers
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

#include <QLabel>

#include "Container.h"
#include "TextControl.h"
#include "QtHandlesUtils.h"

#include "octave-qobject.h"

OCTAVE_BEGIN_NAMESPACE(octave)

TextControl *
TextControl::create (octave::base_qobject& oct_qobj,
                     octave::interpreter& interp, const graphics_object& go)
{
  Object *parent = parentObject (interp, go);

  if (parent)
    {
      Container *container = parent->innerContainer ();

      if (container)
        return new TextControl (oct_qobj, interp, go, new QLabel (container));
    }

  return nullptr;
}

TextControl::TextControl (octave::base_qobject& oct_qobj,
                          octave::interpreter& interp,
                          const graphics_object& go, QLabel *label)
  : BaseControl (oct_qobj, interp, go, label)
{
  uicontrol::properties& up = properties<uicontrol> ();

  label->setAutoFillBackground (true);
  label->setTextFormat (Qt::PlainText);
  label->setWordWrap (false);
  label->setAlignment (Utils::fromHVAlign (up.get_horizontalalignment (),
                                           up.get_verticalalignment ()));
  label->setText (Utils::fromStringVector (up.get_string_vector ()).join ("\n"));
}

TextControl::~TextControl (void)
{ }

void
TextControl::update (int pId)
{
  uicontrol::properties& up = properties<uicontrol> ();
  QLabel *label = qWidget<QLabel> ();

  switch (pId)
    {
    case uicontrol::properties::ID_STRING:
      label->setText (Utils::fromStringVector (up.get_string_vector ()).join ("\n"));
      break;

    case uicontrol::properties::ID_HORIZONTALALIGNMENT:
    case uicontrol::properties::ID_VERTICALALIGNMENT:
      label->setAlignment (Utils::fromHVAlign (up.get_horizontalalignment (),
                                               up.get_verticalalignment ()));
      break;

    default:
      BaseControl::update (pId);
      break;
    }
}

OCTAVE_END_NAMESPACE(octave)
