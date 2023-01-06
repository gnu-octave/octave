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
#include <QActionEvent>
#include <QApplication>
#include <QEvent>
#include <QIcon>
#include <QMainWindow>
#include <QPixmap>
#include <QTimer>
#include <QToolBar>

#include "Figure.h"
#include "ToolBar.h"
#include "QtHandlesUtils.h"

#include "gui-preferences-global.h"
#include "octave-qobject.h"

OCTAVE_BEGIN_NAMESPACE(octave)

static QIcon makeEmptyIcon (void)
{
  QPixmap pix (16, 16);

  pix.fill (Qt::transparent);

  return QIcon (pix);
}

static QAction *
addEmptyAction (QToolBar *bar)
{
  static const QIcon empty_icon = makeEmptyIcon ();

  QAction *a = bar->addAction (empty_icon, "Empty Toolbar");

  a->setEnabled (false);
  a->setToolTip ("");

  return a;
}

ToolBar *
ToolBar::create (octave::base_qobject& oct_qobj, octave::interpreter& interp,
                 const graphics_object& go)
{
  Object *parent = parentObject (interp, go);

  if (parent)
    {
      QWidget *parentWidget = parent->qWidget<QWidget> ();

      if (parentWidget)
        return new ToolBar (oct_qobj, interp, go,
                            new QToolBar (parentWidget));
    }

  return nullptr;
}

ToolBar::ToolBar (octave::base_qobject& oct_qobj, octave::interpreter& interp,
                  const graphics_object& go, QToolBar *bar)
  : Object (oct_qobj, interp, go, bar), m_empty (nullptr), m_figure (nullptr)
{
  uitoolbar::properties& tp = properties<uitoolbar> ();

  bar->setFloatable (false);
  bar->setMovable (false);
  bar->setVisible (tp.is_visible ());
  bar->setStyleSheet (bar->styleSheet () + global_toolbar_style);

  m_empty = addEmptyAction (bar);

  m_figure =
    dynamic_cast<Figure *> (Object::fromQObject (bar->parentWidget ()));

  if (m_figure)
    m_figure->addCustomToolBar (bar, tp.is_visible (),
                                tp.get_tag () == "__default_toolbar__");

  bar->installEventFilter (this);
}

ToolBar::~ToolBar (void)
{ }

void
ToolBar::update (int pId)
{
  uitoolbar::properties& tp = properties<uitoolbar> ();
  QToolBar *bar = qWidget<QToolBar> ();

  switch (pId)
    {
    case base_properties::ID_VISIBLE:
      if (m_figure)
        m_figure->showCustomToolBar (bar, tp.is_visible ());
      break;

    default:
      Object::update (pId);
      break;
    }
}

bool
ToolBar::eventFilter (QObject *watched, QEvent *xevent)
{
  if (watched == qObject ())
    {
      switch (xevent->type ())
        {
        case QEvent::ActionAdded:
        case QEvent::ActionRemoved:
          {
            QActionEvent *ae = dynamic_cast<QActionEvent *> (xevent);
            QToolBar *bar = qWidget<QToolBar> ();

            if (ae->action () != m_empty)
              {
                if (xevent->type () == QEvent::ActionAdded)
                  {
                    if (bar->actions ().size () == 2)
                      QTimer::singleShot (0, this, &ToolBar::hideEmpty);
                  }
                else
                  {
                    if (bar->actions ().size () == 1)
                      m_empty->setVisible (true);
                  }
              }
          }
          break;

        default:
          break;
        }
    }

  return false;
}

void
ToolBar::hideEmpty (void)
{
  m_empty->setVisible (false);
}

void
ToolBar::beingDeleted (void)
{
  if (m_figure)
    {
      QToolBar *bar = qWidget<QToolBar> ();

      if (bar)
        m_figure->showCustomToolBar (bar, false);
    }
}

OCTAVE_END_NAMESPACE(octave)
