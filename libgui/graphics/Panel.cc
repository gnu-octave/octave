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
#include <QFrame>
#include <QLabel>
#include <QMouseEvent>
#include <QTimer>

#include "Canvas.h"
#include "Container.h"
#include "ContextMenu.h"
#include "Panel.h"
#include "QtHandlesUtils.h"

#include "octave-qobject.h"

#include "graphics.h"
#include "interpreter.h"

OCTAVE_BEGIN_NAMESPACE(octave)

static int
frameStyleFromProperties (const uipanel::properties& pp)
{
  if (pp.bordertype_is ("none"))
    return QFrame::NoFrame;
  else if (pp.bordertype_is ("etchedin"))
    return (QFrame::Box | QFrame::Sunken);
  else if (pp.bordertype_is ("etchedout"))
    return (QFrame::Box | QFrame::Raised);
  else if (pp.bordertype_is ("beveledin"))
    return (QFrame::Panel | QFrame::Sunken);
  else if (pp.bordertype_is ("beveledout"))
    return (QFrame::Panel | QFrame::Raised);
  else
    return (QFrame::Panel | QFrame::Plain);
}

static void
setupPalette (const uipanel::properties& pp, QPalette& p)
{
  p.setColor (QPalette::Window,
              Utils::fromRgb (pp.get_backgroundcolor_rgb ()));
  p.setColor (QPalette::WindowText,
              Utils::fromRgb (pp.get_foregroundcolor_rgb ()));
  p.setColor (QPalette::Light,
              Utils::fromRgb (pp.get_highlightcolor_rgb ()));
  p.setColor (QPalette::Dark,
              Utils::fromRgb (pp.get_shadowcolor_rgb ()));
}

static int
borderWidthFromProperties (const uipanel::properties& pp)
{
  int bw = 0;

  if (! pp.bordertype_is ("none"))
    {
      bw = octave::math::round (pp.get_borderwidth ());
      if (pp.bordertype_is ("etchedin") || pp.bordertype_is ("etchedout"))
        bw *= 2;
    }

  return bw;
}

Panel *
Panel::create (octave::base_qobject& oct_qobj, octave::interpreter& interp,
               const graphics_object& go)
{
  Object *parent = parentObject (interp, go);

  if (parent)
    {
      Container *container = parent->innerContainer ();

      if (container)
        return new Panel (oct_qobj, interp, go, new QFrame (container));
    }

  return nullptr;
}

Panel::Panel (octave::base_qobject& oct_qobj, octave::interpreter& interp,
              const graphics_object& go, QFrame *frame)
  : Object (oct_qobj, interp, go, frame), m_container (nullptr),
    m_title (nullptr), m_blockUpdates (false),
    m_previous_bbox (Matrix (1, 4, 0))
{
  uipanel::properties& pp = properties<uipanel> ();

  frame->setObjectName ("UIPanel");
  frame->setAutoFillBackground (true);
  Matrix bb = pp.get_boundingbox (false);
  frame->setGeometry (octave::math::round (bb(0)), octave::math::round (bb(1)),
                      octave::math::round (bb(2)), octave::math::round (bb(3)));
  frame->setFrameStyle (frameStyleFromProperties (pp));
  frame->setLineWidth (octave::math::round (pp.get_borderwidth ()));
  QPalette pal = frame->palette ();
  setupPalette (pp, pal);
  frame->setPalette (pal);

  m_container = new Container (frame, oct_qobj, interp);
  m_container->canvas (m_handle);

  connect (m_container, SIGNAL (interpeter_event (const fcn_callback&)),
           this, SIGNAL (interpeter_event (const fcn_callback&)));

  connect (m_container, SIGNAL (interpeter_event (const meth_callback&)),
           this, SIGNAL (interpeter_event (const meth_callback&)));

  if (frame->hasMouseTracking ())
    {
      for (auto *w : frame->findChildren<QWidget *> ())
        w->setMouseTracking (true);
    }

  QString title = Utils::fromStdString (pp.get_title ());
  if (! title.isEmpty ())
    {
      m_title = new QLabel (title, frame);
      m_title->setAutoFillBackground (true);
      m_title->setContentsMargins (4, 0, 4, 0);
      m_title->setPalette (pal);
      m_title->setFont (Utils::computeFont<uipanel> (pp, bb(3)));
    }

  frame->installEventFilter (this);
  m_container->installEventFilter (this);

  graphics_object fig (go.get_ancestor ("figure"));
  if (! fig.get ("keypressfcn").isempty ())
    m_container->canvas (m_handle)->addEventMask (Canvas::KeyPress);

  if (! fig.get ("keyreleasefcn").isempty ())
    m_container->canvas (m_handle)->addEventMask (Canvas::KeyRelease);

  if (pp.is_visible ())
    QTimer::singleShot (0, frame, &QFrame::show);
  else
    frame->hide ();
}

Panel::~Panel (void)
{ }

bool
Panel::eventFilter (QObject *watched, QEvent *xevent)
{
  if (! m_blockUpdates)
    {
      gh_manager& gh_mgr = m_interpreter.get_gh_manager ();

      if (watched == qObject ())
        {
          switch (xevent->type ())
            {
            case QEvent::Resize:
              {
                octave::autolock guard (gh_mgr.graphics_lock ());

                graphics_object go = object ();

                if (go.valid_object ())
                  {
                    if (m_title)
                      {
                        const uipanel::properties& pp =
                          Utils::properties<uipanel> (go);

                        if (pp.fontunits_is ("normalized"))
                          {
                            QFrame *frame = qWidget<QFrame> ();

                            m_title->setFont (Utils::computeFont<uipanel>
                                              (pp, frame->height ()));
                            m_title->resize (m_title->sizeHint ());
                          }
                      }
                    updateLayout ();
                  }
              }
              break;

            case QEvent::MouseButtonPress:
              {
                QMouseEvent *m = dynamic_cast<QMouseEvent *> (xevent);

                if (m->button () == Qt::RightButton)
                  {
                    octave::autolock guard (gh_mgr.graphics_lock ());

                    graphics_object go = object ();

                    if (go.valid_object ())
                      ContextMenu::executeAt (m_interpreter,
                                              go.get_properties (),
                                              m->globalPos ());
                  }
              }
              break;

            default:
              break;
            }
        }
      else if (watched == m_container)
        {
          switch (xevent->type ())
            {
            case QEvent::Resize:
              if (qWidget<QWidget> ()->isVisible ())
                {
                  octave::autolock guard (gh_mgr.graphics_lock ());

                  graphics_object go = object ();

                  if (go.valid_object ())
                    go.get_properties ().update_boundingbox ();
                }
              break;

            default:
              break;
            }
        }
    }

  return false;
}

void
Panel::update (int pId)
{
  uipanel::properties& pp = properties<uipanel> ();
  QFrame *frame = qWidget<QFrame> ();

  m_blockUpdates = true;

  switch (pId)
    {
    case uipanel::properties::ID_POSITION:
      {
        Matrix bb = pp.get_boundingbox (false);
        if (m_previous_bbox(0) != bb(0) || m_previous_bbox(1) != bb(1)
            || m_previous_bbox(2) != bb(2) || m_previous_bbox(3) != bb(3))
          {
            frame->setGeometry (octave::math::round (bb(0)),
                                octave::math::round (bb(1)),
                                octave::math::round (bb(2)),
                                octave::math::round (bb(3)));
            updateLayout ();
          }
        m_previous_bbox = bb;
      }
      break;

    case uipanel::properties::ID_BORDERWIDTH:
      frame->setLineWidth (octave::math::round (pp.get_borderwidth ()));
      updateLayout ();
      break;

    case uipanel::properties::ID_BACKGROUNDCOLOR:
    case uipanel::properties::ID_FOREGROUNDCOLOR:
    case uipanel::properties::ID_HIGHLIGHTCOLOR:
    case uipanel::properties::ID_SHADOWCOLOR:
      {
        QPalette pal = frame->palette ();

        setupPalette (pp, pal);
        frame->setPalette (pal);
        if (m_title)
          m_title->setPalette (pal);
      }
      break;

    case uipanel::properties::ID_TITLE:
      {
        QString title = Utils::fromStdString (pp.get_title ());

        if (title.isEmpty ())
          {
            if (m_title)
              delete m_title;
            m_title = nullptr;
          }
        else
          {
            if (! m_title)
              {
                QPalette pal = frame->palette ();

                m_title = new QLabel (title, frame);
                m_title->setAutoFillBackground (true);
                m_title->setContentsMargins (4, 0, 4, 0);
                m_title->setPalette (pal);
                m_title->setFont (Utils::computeFont<uipanel> (pp));
                m_title->show ();
              }
            else
              {
                m_title->setText (title);
                m_title->resize (m_title->sizeHint ());
              }
          }
        updateLayout ();
      }
      break;

    case uipanel::properties::ID_TITLEPOSITION:
      updateLayout ();
      break;

    case uipanel::properties::ID_BORDERTYPE:
      frame->setFrameStyle (frameStyleFromProperties (pp));
      updateLayout ();
      break;

    case uipanel::properties::ID_FONTNAME:
    case uipanel::properties::ID_FONTSIZE:
    case uipanel::properties::ID_FONTWEIGHT:
    case uipanel::properties::ID_FONTANGLE:
      if (m_title)
        {
          m_title->setFont (Utils::computeFont<uipanel> (pp));
          m_title->resize (m_title->sizeHint ());
          updateLayout ();
        }
      break;

    case uipanel::properties::ID_VISIBLE:
      frame->setVisible (pp.is_visible ());
      updateLayout ();
      break;

    default:
      break;
    }

  m_blockUpdates = false;
}

void
Panel::redraw (void)
{
  update (uipanel::properties::ID_POSITION);

  Canvas *canvas = m_container->canvas (m_handle);

  if (canvas)
    canvas->redraw ();
}

void
Panel::updateLayout (void)
{
  uipanel::properties& pp = properties<uipanel> ();
  QFrame *frame = qWidget<QFrame> ();

  Matrix bb = pp.get_boundingbox (true);
  int bw = borderWidthFromProperties (pp);

  frame->setFrameRect (QRect (octave::math::round (bb(0)) - bw,
                              octave::math::round (bb(1)) - bw,
                              octave::math::round (bb(2)) + 2*bw, octave::math::round (bb(3)) + 2*bw));
  m_container->setGeometry (octave::math::round (bb(0)),
                            octave::math::round (bb(1)),
                            octave::math::round (bb(2)), octave::math::round (bb(3)));

  if (m_blockUpdates)
    pp.update_boundingbox ();

  if (m_title)
    {
      QSize sz = m_title->sizeHint ();
      int offset = 5;

      if (pp.titleposition_is ("lefttop"))
        m_title->move (bw+offset, 0);
      else if (pp.titleposition_is ("righttop"))
        m_title->move (frame->width () - bw - offset - sz.width (), 0);
      else if (pp.titleposition_is ("leftbottom"))
        m_title->move (bw+offset, frame->height () - sz.height ());
      else if (pp.titleposition_is ("rightbottom"))
        m_title->move (frame->width () - bw - offset - sz.width (),
                       frame->height () - sz.height ());
      else if (pp.titleposition_is ("centertop"))
        m_title->move (frame->width () / 2 - sz.width () / 2, 0);
      else if (pp.titleposition_is ("centerbottom"))
        m_title->move (frame->width () / 2 - sz.width () / 2,
                       frame->height () - sz.height ());
    }
}

void
Panel::do_connections (const QObject *receiver, const QObject * /* emitter */)
{
  Object::do_connections (receiver);
  Object::do_connections (receiver, m_container->canvas (m_handle));
}

OCTAVE_END_NAMESPACE(octave);
