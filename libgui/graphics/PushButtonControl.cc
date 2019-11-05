/*

Copyright (C) 2011-2019 Michael Goffioul

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

#include <QPushButton>
#include <QtDebug>

#include "PushButtonControl.h"
#include "Container.h"
#include "QtHandlesUtils.h"

#include "octave-qobject.h"

namespace QtHandles
{

  PushButtonControl*
  PushButtonControl::create (octave::base_qobject& oct_qobj,
                             const graphics_object& go)
  {
    Object *parent = Object::parentObject (go);

    if (parent)
      {
        Container *container = parent->innerContainer ();

        if (container)
          return new PushButtonControl (oct_qobj, go,
                                        new QPushButton (container));
      }

    return nullptr;
  }

  PushButtonControl::PushButtonControl (octave::base_qobject& oct_qobj,
                                        const graphics_object& go,
                                        QPushButton *btn)
    : ButtonControl (oct_qobj, go, btn)
  {
    uicontrol::properties& up = properties<uicontrol> ();

    btn->setAutoFillBackground (true);
    octave_value cdat = up.get_cdata ();
    QImage img = Utils::makeImageFromCData (cdat,
                                            cdat.rows (), cdat.columns ());
    btn->setIcon (QIcon (QPixmap::fromImage (img)));
  }

  PushButtonControl::~PushButtonControl (void)
  { }

  void
  PushButtonControl::update (int pId)
  {
    uicontrol::properties& up = properties<uicontrol> ();
    QPushButton *btn = qWidget<QPushButton> ();

    switch (pId)
      {
      case uicontrol::properties::ID_CDATA:
        {
          octave_value cdat = up.get_cdata ();
          QImage img = Utils::makeImageFromCData (cdat,
                                                  cdat.rows (),
                                                  cdat.columns ());
          btn->setIcon (QIcon (QPixmap::fromImage (img)));
        }
        break;

      default:
        ButtonControl::update (pId);
        break;
      }
  }

}
