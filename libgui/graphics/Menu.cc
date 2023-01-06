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

#include <QAction>
#include <QMainWindow>
#include <QMenu>
#include <QMenuBar>

#include "Figure.h"
#include "Menu.h"
#include "QtHandlesUtils.h"

#include "octave-qobject.h"

OCTAVE_BEGIN_NAMESPACE(octave)

static QKeySequence
accelSequence (const uimenu::properties& up)
{
  std::string s (up.get_accelerator ());

  if (! s.empty ())
    {
      char c = s[0];
      int keyMod = Qt::CTRL;

      if (c >= 'A' && c <= 'Z')
        keyMod |= Qt::SHIFT;
      if (c >= 'a' && c <= 'z')
        c -= ('a' - 'A');
      if (c >= 'A' && c <= 'Z')
        return QKeySequence (keyMod | static_cast<int> (c));
    }

  return QKeySequence ();
}

Menu *
Menu::create (octave::base_qobject& oct_qobj, octave::interpreter& interp,
              const graphics_object& go)
{
  Object *parent_obj = parentObject (interp, go);

  if (parent_obj)
    {
      QObject *qObj = parent_obj->qObject ();

      if (qObj)
        return new Menu (oct_qobj, interp, go, new QAction (qObj),
                         parent_obj);
    }

  return nullptr;
}

Menu::Menu (octave::base_qobject& oct_qobj, octave::interpreter& interp,
            const graphics_object& go, QAction *action, Object *xparent)
  : Object (oct_qobj, interp, go, action), m_parent (nullptr),
    m_separator (nullptr)
{
  uimenu::properties& up = properties<uimenu> ();

  action->setText (Utils::fromStdString (up.get_text ()));

  if (up.is_checked ())
    {
      action->setCheckable (true);
      action->setChecked (up.is_checked ());
    }

  action->setEnabled (up.is_enable ());
  action->setShortcut (accelSequence (up));
  action->setVisible (up.is_visible ());

  if (up.is_separator ())
    {
      m_separator = new QAction (action);
      m_separator->setSeparator (true);
      m_separator->setVisible (up.is_visible ());
    }

  MenuContainer *menuContainer = dynamic_cast<MenuContainer *> (xparent);

  if (menuContainer)
    m_parent = menuContainer->menu ();

  if (m_parent)
    {
      int pos = static_cast<int> (up.get_position ());

      if (pos <= 0)
        {
          if (m_separator)
            m_parent->insertAction (nullptr, m_separator);
          m_parent->insertAction (nullptr, action);

          int count = 0;

          for (auto *a : m_parent->actions ())
            if (! a->isSeparator ())
              count++;

          up.get_property ("position").set
            (octave_value (static_cast<double> (count)), true, false);
        }
      else
        {

          int count = 0;
          QAction *before = nullptr;

          for (auto *a : m_parent->actions ())
            {
              if (! a->isSeparator ())
                {
                  count++;
                  if (pos <= count)
                    {
                      before = a;
                      break;
                    }
                }
            }

          if (m_separator)
            m_parent->insertAction (before, m_separator);
          m_parent->insertAction (before, action);

          if (before)
            updateSiblingPositions ();
          else
            up.get_property ("position").set
              (octave_value (static_cast<double> (count+1)), true, false);
        }
    }

  connect (action, &QAction::triggered, this, &Menu::actionTriggered);
}

Menu::~Menu (void)
{ }

void
Menu::update (int pId)
{
  uimenu::properties& up = properties<uimenu> ();
  QAction *action = qWidget<QAction> ();

  switch (pId)
    {
    case uimenu::properties::ID_TEXT:
      action->setText (Utils::fromStdString (up.get_text ()));
      break;

    case uimenu::properties::ID_CHECKED:
      if (up.is_checked ())
        {
          action->setCheckable (true);
          action->setChecked (up.is_checked ());
        }
      else
        {
          action->setChecked (false);
          action->setCheckable (false);
        }
      break;

    case uimenu::properties::ID_ENABLE:
      action->setEnabled (up.is_enable ());
      break;

    case uimenu::properties::ID_ACCELERATOR:
      if (! action->menu ())
        action->setShortcut (accelSequence (up));
      break;

    case uimenu::properties::ID_SEPARATOR:
      if (up.is_separator ())
        {
          if (! m_separator)
            {
              m_separator = new QAction (action);
              m_separator->setSeparator (true);
              m_separator->setVisible (up.is_visible ());
              if (m_parent)
                m_parent->insertAction (action, m_separator);
            }
        }
      else
        {
          if (m_separator)
            delete m_separator;
          m_separator = nullptr;
        }
      break;

    case uimenu::properties::ID_VISIBLE:
      action->setVisible (up.is_visible ());
      if (m_separator)
        m_separator->setVisible (up.is_visible ());
      break;

    case uimenu::properties::ID_POSITION:
      {
        if (m_separator)
          m_parent->removeAction (m_separator);

        m_parent->removeAction (action);

        int pos = static_cast<int> (up.get_position ());
        QAction *before = nullptr;

        if (pos > 0)
          {
            int count = 0;

            for (auto *a : m_parent->actions ())
              {
                if (! a->isSeparator ())
                  {
                    count++;
                    if (pos <= count)
                      {
                        before = a;
                        break;
                      }
                  }
              }
          }

        if (m_separator)
          m_parent->insertAction (before, m_separator);

        m_parent->insertAction (before, action);

        updateSiblingPositions ();
      }
      break;

    default:
      Object::update (pId);
      break;
    }
}

QWidget *
Menu::menu (void)
{
  QAction *action = qWidget<QAction> ();
  QMenu *action_menu = action->menu ();

  if (! action_menu)
    {
      action_menu = new QMenu (action->parentWidget ());
      action->setMenu (action_menu);
      action->setShortcut (QKeySequence ());
      connect (action_menu, &QMenu::aboutToShow, this, &Menu::actionHovered);
    }

  return action_menu;
}

void
Menu::actionTriggered (void)
{
  QAction *action = qWidget<QAction> ();

  if (action->isCheckable ())
    action->setChecked (! action->isChecked ());
  emit gh_callback_event (m_handle, "menuselectedfcn");
}

void
Menu::actionHovered (void)
{
  emit gh_callback_event (m_handle, "menuselectedfcn");
}

void
Menu::updateSiblingPositions (void)
{
  if (m_parent)
    {
      double count = 1.0;

      for (auto *a : m_parent->actions ())
        {
          if (! a->isSeparator ())
            {
              Object *aObj = Object::fromQObject (a);

              if (aObj)
                {
                  graphics_object go = aObj->object ();

                  // Probably overkill as a uimenu child can only be another
                  // uimenu object.
                  if (go.isa ("uimenu"))
                    {
                      uimenu::properties& up = Utils::properties<uimenu> (go);

                      up.get_property ("position").set
                        (octave_value (count), true, false);
                    }
                }

              count++;
            }
        }
    }
}

OCTAVE_END_NAMESPACE(octave)
