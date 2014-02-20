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

#include <QAction>
#include <QIcon>

#include "Figure.h"
#include "MouseModeActionGroup.h"

//////////////////////////////////////////////////////////////////////////////

namespace QtHandles
{

//////////////////////////////////////////////////////////////////////////////

MouseModeActionGroup::MouseModeActionGroup (QObject* parent)
  : QObject (parent), m_current (0)
{
  m_actions.append (new QAction (QIcon (":/images/rotate.png"),
				 tr ("Rotate"), this));
  m_actions.append (new QAction (QIcon (":/images/zoom.png"),
				 tr ("Zoom"), this));
  m_actions.append (new QAction (QIcon (":/images/pan.png"),
				 tr ("Pan"), this));
  m_actions.append (new QAction (QIcon (":/images/select.png"),
				 tr ("Select"), this));
  m_actions[2]->setEnabled (false);
  m_actions[3]->setEnabled (false);

  foreach (QAction* a, m_actions)
    {
      a->setCheckable (true);
      connect (a, SIGNAL (toggled (bool)), this, SLOT (actionToggled (bool)));
    }
}

//////////////////////////////////////////////////////////////////////////////

MouseModeActionGroup::~MouseModeActionGroup (void)
{
}

//////////////////////////////////////////////////////////////////////////////

void MouseModeActionGroup::actionToggled (bool checked)
{
  if (! checked)
    {
      if (sender () == m_current)
	{
	  m_current = 0;
	  emit modeChanged (NoMode);
	}
    }
  else
    {
      int i = m_actions.indexOf (qobject_cast<QAction*> (sender ()));

      if (i >= 0)
	{
	  m_current = m_actions[i];
	  for (int j = 0; j < m_actions.size (); j++)
	    if (j != i)
	      m_actions[j]->setChecked (false);
	  emit modeChanged (static_cast<MouseMode> (i+1));
	}
    }
}

//////////////////////////////////////////////////////////////////////////////

MouseMode MouseModeActionGroup::mouseMode (void) const
{
  int i = (m_current ? -1 : m_actions.indexOf (m_current));

  return static_cast<MouseMode> (i+1);
}

//////////////////////////////////////////////////////////////////////////////

};
