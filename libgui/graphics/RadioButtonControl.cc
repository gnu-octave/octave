/*

Copyright (C) 2011 Michael Goffioul.

This file is part of QtHandles.

Foobar is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

QtHandles is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with Foobar.  If not, see <http://www.gnu.org/licenses/>.

*/

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <QRadioButton>

#include "RadioButtonControl.h"
#include "Container.h"
#include "Utils.h"

//////////////////////////////////////////////////////////////////////////////

namespace QtHandles
{

//////////////////////////////////////////////////////////////////////////////

RadioButtonControl* RadioButtonControl::create (const graphics_object& go)
{
  Object* parent = Object::parentObject (go);

  if (parent)
    {
      Container* container = parent->innerContainer ();

      if (container)
	return new RadioButtonControl (go, new QRadioButton (container));
    }

  return 0;
}

//////////////////////////////////////////////////////////////////////////////

RadioButtonControl::RadioButtonControl (const graphics_object& go,
					QRadioButton* radio)
    : ButtonControl (go, radio)
{
  radio->setAutoFillBackground (true);
  radio->setAutoExclusive (false);
}

//////////////////////////////////////////////////////////////////////////////

RadioButtonControl::~RadioButtonControl (void)
{
}

//////////////////////////////////////////////////////////////////////////////

};
