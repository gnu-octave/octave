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

#include <QEvent>
#include <QKeyEvent>
#include <QMouseEvent>
#include <QWidget>

#include "BaseControl.h"
#include "ContextMenu.h"
#include "QtHandlesUtils.h"

#include "graphics.h"
#include "interpreter.h"

OCTAVE_BEGIN_NAMESPACE(octave)

static void
updatePalette (const uicontrol::properties& props, QWidget *w)
{
  QPalette p = w->palette ();

  if (props.style_is ("edit")
      || props.style_is ("listbox"))
    {
      Matrix bg_color = props.get_backgroundcolor_rgb ();
      // Matlab compatibility: Default color is ignored, and rendered as
      // white ([1.0, 1.0, 1.0]).  See bug #58261.
      if (bg_color(0) == bg_color(1) && bg_color(0) == bg_color(2)
          && (std::abs (bg_color(1) - 0.94) < .005))
        bg_color.fill (1.0);

      p.setColor (QPalette::Active, QPalette::Base,
                  Utils::fromRgb (bg_color));
      p.setColor (QPalette::Inactive, QPalette::Base,
                  Utils::fromRgb (bg_color));
      p.setColor (QPalette::Active, QPalette::Text,
                  Utils::fromRgb (props.get_foregroundcolor_rgb ()));
      p.setColor (QPalette::Inactive, QPalette::Text,
                  Utils::fromRgb (props.get_foregroundcolor_rgb ()));
    }
  else if (props.style_is ("popupmenu"))
    {
      // popupmenu (QComboBox) is a listbox with a button.
      // This requires setting colors for both.
      QColor bcol = Utils::fromRgb (props.get_backgroundcolor_rgb ());
      QColor fcol = Utils::fromRgb (props.get_foregroundcolor_rgb ());
      QString qss = QString (":enabled { background: %1 none;\n"
                             "color: %2; }")
        .arg(bcol.name ()).arg (fcol.name ());
      w->setStyleSheet(qss);
      return;
    }
  else if (props.style_is ("radiobutton")
           || props.style_is ("checkbox"))
    {
      p.setColor (QPalette::Active, QPalette::Button,
                  Utils::fromRgb (props.get_backgroundcolor_rgb ()));
      p.setColor (QPalette::Inactive, QPalette::Button,
                  Utils::fromRgb (props.get_backgroundcolor_rgb ()));
      p.setColor (QPalette::Active, QPalette::WindowText,
                  Utils::fromRgb (props.get_foregroundcolor_rgb ()));
      p.setColor (QPalette::Inactive, QPalette::WindowText,
                  Utils::fromRgb (props.get_foregroundcolor_rgb ()));
    }
  else if (props.style_is ("pushbutton")
           || props.style_is ("togglebutton"))
    {
      QColor bcol = Utils::fromRgb (props.get_backgroundcolor_rgb ());
      QColor fcol = Utils::fromRgb (props.get_foregroundcolor_rgb ());
      QString qss = QString (":enabled { background: %1 none;\n"
                             "color: %2; }")
        .arg(bcol.name ()).arg (fcol.name ());
      w->setStyleSheet(qss);
      return;
    }
  else
    {
      p.setColor (QPalette::Active, QPalette::Window,
                  Utils::fromRgb (props.get_backgroundcolor_rgb ()));
      p.setColor (QPalette::Inactive, QPalette::Window,
                  Utils::fromRgb (props.get_backgroundcolor_rgb ()));
      p.setColor (QPalette::Active, QPalette::WindowText,
                  Utils::fromRgb (props.get_foregroundcolor_rgb ()));
      p.setColor (QPalette::Inactive, QPalette::WindowText,
                  Utils::fromRgb (props.get_foregroundcolor_rgb ()));
    }

  w->setPalette (p);
}

BaseControl::BaseControl (octave::base_qobject& oct_qobj,
                          octave::interpreter& interp,
                          const graphics_object& go, QWidget *w)
  : Object (oct_qobj, interp, go, w), m_normalizedFont (false),
    m_keyPressHandlerDefined (false)
{
  qObject ()->setObjectName ("UIControl");
  init (w);
}

void
BaseControl::init (QWidget *w, bool callBase)
{
  if (callBase)
    Object::init (w, callBase);

  uicontrol::properties& up = properties<uicontrol> ();

  Matrix bb = up.get_boundingbox (false);
  w->setGeometry (octave::math::round (bb(0)), octave::math::round (bb(1)),
                  octave::math::round (bb(2)), octave::math::round (bb(3)));
  w->setFont (Utils::computeFont<uicontrol> (up, bb(3)));
  updatePalette (up, w);
  if (up.enable_is ("inactive"))
    w->blockSignals (true);
  else
    w->setEnabled (up.enable_is ("on"));
  w->setToolTip (Utils::fromStdString (up.get_tooltipstring ()));
  w->setVisible (up.is_visible ());
  m_keyPressHandlerDefined = ! up.get_keypressfcn ().isempty ();

  w->installEventFilter (this);

  m_normalizedFont = up.fontunits_is ("normalized");
}

BaseControl::~BaseControl (void)
{ }

void
BaseControl::redraw (void)
{
  update (uicontrol::properties::ID_POSITION);
}

void
BaseControl::update (int pId)
{
  uicontrol::properties& up = properties<uicontrol> ();
  QWidget *w = qWidget<QWidget> ();

  switch (pId)
    {
    case uicontrol::properties::ID_POSITION:
      {
        Matrix bb = up.get_boundingbox (false);
        w->setGeometry (octave::math::round (bb(0)), octave::math::round (bb(1)),
                        octave::math::round (bb(2)), octave::math::round (bb(3)));
      }
      break;

    case uicontrol::properties::ID_FONTNAME:
    case uicontrol::properties::ID_FONTSIZE:
    case uicontrol::properties::ID_FONTWEIGHT:
    case uicontrol::properties::ID_FONTANGLE:
      w->setFont (Utils::computeFont<uicontrol> (up));
      break;

    case uicontrol::properties::ID_FONTUNITS:
      // FIXME: We shouldn't have to do anything, octave should update
      //        the "fontsize" property automatically to the new units.
      //        Hence the actual font used shouldn't change.
      m_normalizedFont = up.fontunits_is ("normalized");
      break;

    case uicontrol::properties::ID_BACKGROUNDCOLOR:
    case uicontrol::properties::ID_FOREGROUNDCOLOR:
      updatePalette (up, w);
      break;

    case uicontrol::properties::ID_ENABLE:
      if (up.enable_is ("inactive"))
        {
          w->blockSignals (true);
          w->setEnabled (true);
        }
      else
        {
          w->blockSignals (false);
          w->setEnabled (up.enable_is ("on"));
        }
      break;

    case uicontrol::properties::ID_TOOLTIPSTRING:
      w->setToolTip (Utils::fromStdString (up.get_tooltipstring ()));
      break;

    case base_properties::ID_VISIBLE:
      w->setVisible (up.is_visible ());
      break;

    case uicontrol::properties::ID_KEYPRESSFCN:
      m_keyPressHandlerDefined = ! up.get_keypressfcn ().isempty ();
      break;

    case uicontrol::properties::ID___FOCUS__:
      if (up.is___focus__ ())
        w->setFocus ();
      else
        w->clearFocus ();
      break;

    default:
      break;
    }
}

bool
BaseControl::eventFilter (QObject *watched, QEvent *xevent)
{
  gh_manager& gh_mgr = m_interpreter.get_gh_manager ();

  switch (xevent->type ())
    {
    case QEvent::Resize:
      if (m_normalizedFont)
        {
          octave::autolock guard (gh_mgr.graphics_lock ());

          qWidget<QWidget> ()->setFont (Utils::computeFont<uicontrol>
                                        (properties<uicontrol> ()));
        }
      break;

    case QEvent::MouseButtonPress:
      {
        octave::autolock guard (gh_mgr.graphics_lock ());

        QMouseEvent *m = dynamic_cast<QMouseEvent *> (xevent);
        graphics_object go = object ();
        uicontrol::properties& up = Utils::properties<uicontrol> (go);
        graphics_object fig = go.get_ancestor ("figure");
        if (fig)
          {
            emit gh_set_event (fig.get_handle (), "currentobject",
                               m_handle.value (), false);

            if (m->button () != Qt::LeftButton || ! up.enable_is ("on"))
              {
                emit gh_set_event (fig.get_handle (), "selectiontype",
                                   Utils::figureSelectionType (m), false);
                emit gh_set_event (fig.get_handle (), "currentpoint",
                                   Utils::figureCurrentPoint (fig, m),
                                   false);
                emit gh_callback_event (fig.get_handle (),
                                        "windowbuttondownfcn");
                emit gh_callback_event (m_handle, "buttondownfcn");

                if (m->button () == Qt::RightButton)
                  ContextMenu::executeAt (m_interpreter, up, m->globalPos ());
              }
            else
              {
                if (up.style_is ("listbox"))
                  emit gh_set_event (fig.get_handle (), "selectiontype",
                                     Utils::figureSelectionType (m),
                                     false);
                else
                  emit gh_set_event (fig.get_handle (), "selectiontype",
                                     octave_value ("normal"), false);
              }
          }
      }
      break;

    case QEvent::MouseMove:
      if (qWidget<QWidget> ()->hasMouseTracking ())
        {
          octave::autolock guard (gh_mgr.graphics_lock ());

          QMouseEvent *m = dynamic_cast<QMouseEvent *> (xevent);
          graphics_object go = object ();
          graphics_object fig = go.get_ancestor ("figure");

          if (fig)
            {
              emit gh_set_event (fig.get_handle (), "currentpoint",
                                 Utils::figureCurrentPoint (fig, m), false);
              emit gh_callback_event (fig.get_handle (),
                                      "windowbuttonmotionfcn");
            }
        }
      break;

    case QEvent::KeyPress:
      if (m_keyPressHandlerDefined)
        {
          octave::autolock guard (gh_mgr.graphics_lock ());

          octave_scalar_map keyData =
            Utils::makeKeyEventStruct (dynamic_cast<QKeyEvent *> (xevent));
          graphics_object fig = object ().get_ancestor ("figure");

          emit gh_set_event (fig.get_handle (), "currentcharacter",
                             keyData.getfield ("Character"), false);
          emit gh_callback_event (m_handle, "keypressfcn", keyData);
        }
      break;

    case QEvent::FocusIn:
      emit gh_set_event (m_handle, "__focus__", "on", false);
      break;

    case QEvent::FocusOut:
      emit gh_set_event (m_handle, "__focus__", "off", false);
      break;

    default:
      break;
    }

  return Object::eventFilter (watched, xevent);
}

OCTAVE_END_NAMESPACE(octave)
