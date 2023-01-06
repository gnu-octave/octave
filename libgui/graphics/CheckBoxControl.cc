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

#include <QCheckBox>

#include "CheckBoxControl.h"
#include "Container.h"

#include "octave-qobject.h"

OCTAVE_BEGIN_NAMESPACE(octave)

CheckBoxControl *
CheckBoxControl::create (octave::base_qobject& oct_qobj,
                         octave::interpreter& interp,
                         const graphics_object& go)
{
  Object *parent = parentObject (interp, go);

  if (parent)
    {
      Container *container = parent->innerContainer ();

      if (container)
        return new CheckBoxControl (oct_qobj, interp, go,
                                    new QCheckBox (container));
    }

  return nullptr;
}

CheckBoxControl::CheckBoxControl (octave::base_qobject& oct_obj,
                                  octave::interpreter& interp,
                                  const graphics_object& go, QCheckBox *box)
  : ButtonControl (oct_obj, interp, go, box)
{
  uicontrol::properties& up = properties<uicontrol> ();

  box->setAutoFillBackground (true);
  if (up.enable_is ("inactive"))
    box->setCheckable (false);
}

CheckBoxControl::~CheckBoxControl (void)
{ }

void
CheckBoxControl::update (int pId)
{
  uicontrol::properties& up = properties<uicontrol> ();
  QCheckBox *box = qWidget<QCheckBox> ();

  switch (pId)
    {
    case uicontrol::properties::ID_ENABLE:
      {
        if (up.enable_is ("inactive"))
          box->setCheckable (false);
        else
          box->setCheckable (true);
        ButtonControl::update (pId);
      }
      break;

    default:
      ButtonControl::update (pId);
      break;
    }
}

OCTAVE_END_NAMESPACE(octave);
