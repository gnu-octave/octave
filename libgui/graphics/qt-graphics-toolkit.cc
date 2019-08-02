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

#include <cstdint>

#include <QApplication>
#include <QFontMetrics>
#include <QThread>

#include "Logger.h"
#include "Object.h"
#include "ObjectFactory.h"
#include "ObjectProxy.h"
#include "QtHandlesUtils.h"
#include "qt-graphics-toolkit.h"

#include "event-manager.h"
#include "interpreter.h"

//#if INTPTR_MAX == INT32_MAX
//# define OCTAVE_PTR_TYPE octave_uint32
//# define OCTAVE_INTPTR_TYPE uint32_t
//# define OCTAVE_PTR_SCALAR uint32_scalar_value
//#else
# define OCTAVE_PTR_TYPE octave_uint64
# define OCTAVE_INTPTR_TYPE uint64_t
# define OCTAVE_PTR_SCALAR uint64_scalar_value
//#endif

namespace QtHandles
{

  static std::string
  toolkitObjectProperty (const graphics_object& go)
  {
    if (go.isa ("figure"))
      return "__plot_stream__";
    else if (go.isa ("uicontrol")
             || go.isa ("uipanel")
             || go.isa ("uibuttongroup")
             || go.isa ("uimenu")
             || go.isa ("uicontextmenu")
             || go.isa ("uitable")
             || go.isa ("uitoolbar")
             || go.isa ("uipushtool")
             || go.isa ("uitoggletool"))
      return "__object__";
    else
      qCritical ("QtHandles::qt_graphics_toolkit: no __object__ property known for object "
                 "of type %s", go.type ().c_str ());

    return "";
  }

  qt_graphics_toolkit::qt_graphics_toolkit (octave::interpreter& interp)
    : QObject (), base_graphics_toolkit ("qt"), m_interpreter (interp),
      m_factory (new ObjectFactory ())
  {
    if (QThread::currentThread () != QApplication::instance ()->thread ())
      m_factory->moveToThread (QApplication::instance ()->thread ());

    connect (this, SIGNAL (createObject (qt_graphics_toolkit *, double)),
             m_factory, SLOT (createObject (qt_graphics_toolkit *, double)),
             Qt::BlockingQueuedConnection);
  }

  qt_graphics_toolkit::~qt_graphics_toolkit (void)
  {
    delete m_factory;
  }

  bool
  qt_graphics_toolkit::initialize (const graphics_object& go)
  {
    if (go.isa ("figure")
        || go.isa ("uicontrol")
        || go.isa ("uipanel")
        || go.isa ("uibuttongroup")
        || go.isa ("uimenu")
        || go.isa ("uicontextmenu")
        || go.isa ("uitable")
        || go.isa ("uitoolbar")
        || go.isa ("uipushtool")
        || go.isa ("uitoggletool"))
      {
        // FIXME: We need to unlock the mutex here but we have no way to know if
        // if it was previously locked by this thread, and thus if we should
        // re-lock it.
        gh_manager::unlock ();

        Logger::debug ("qt_graphics_toolkit::initialize %s from thread %08x",
                       go.type ().c_str (), QThread::currentThreadId ());

        ObjectProxy *proxy = new ObjectProxy ();
        graphics_object gObj (go);

        OCTAVE_PTR_TYPE tmp (reinterpret_cast<OCTAVE_INTPTR_TYPE> (proxy));
        gObj.get_properties ().set (toolkitObjectProperty (go), tmp);

        emit createObject (this, go.get_handle ().value ());

        return true;
      }

    return false;
  }

  void
  qt_graphics_toolkit::update (const graphics_object& go, int pId)
  {
    // Rule out obvious properties we want to ignore.
    if (pId == figure::properties::ID___PLOT_STREAM__
        || pId == uicontrol::properties::ID___OBJECT__
        || pId == uipanel::properties::ID___OBJECT__
        || pId == uibuttongroup::properties::ID___OBJECT__
        || pId == uimenu::properties::ID___OBJECT__
        || pId == uicontextmenu::properties::ID___OBJECT__
        || pId == uitable::properties::ID___OBJECT__
        || pId == uitoolbar::properties::ID___OBJECT__
        || pId == uipushtool::properties::ID___OBJECT__
        || pId == uitoggletool::properties::ID___OBJECT__
        || pId == base_properties::ID___MODIFIED__)
      return;

    Logger::debug ("qt_graphics_toolkit::update %s(%d) from thread %08x",
                   go.type ().c_str (), pId, QThread::currentThreadId ());

    ObjectProxy *proxy = toolkitObjectProxy (go);

    if (proxy)
      {
        if (go.isa ("uicontrol")
            && pId == uicontrol::properties::ID_STYLE)
          {
            // Special case: we need to recreate the control widget
            // associated with the octave graphics_object

            finalize (go);
            initialize (go);
          }
        else
          proxy->update (pId);
      }
  }

  void
  qt_graphics_toolkit::finalize (const graphics_object& go)
  {
    // FIXME: We need to unlock the mutex here but we have no way to know if
    // if it was previously locked by this thread, and thus if we should
    // re-lock it.
    gh_manager::unlock ();

    Logger::debug ("qt_graphics_toolkit::finalize %s from thread %08x",
                   go.type ().c_str (), QThread::currentThreadId ());

    ObjectProxy *proxy = toolkitObjectProxy (go);

    if (proxy)
      {
        proxy->finalize ();
        delete proxy;

        graphics_object gObj (go);

        gObj.get_properties ().set (toolkitObjectProperty (go), Matrix ());
      }
  }

  void
  qt_graphics_toolkit::redraw_figure (const graphics_object& go) const
  {
    if (go.get_properties ().is_visible ())
      {
        ObjectProxy *proxy = toolkitObjectProxy (go);

        if (proxy)
          proxy->redraw ();
      }
  }

  void
  qt_graphics_toolkit::show_figure (const graphics_object& go) const
  {
    if (go.get_properties ().is_visible ())
      {
        ObjectProxy *proxy = toolkitObjectProxy (go);

        if (proxy)
          proxy->show ();
      }
  }

  void
  qt_graphics_toolkit::print_figure (const graphics_object& go,
                         const std::string& term,
                         const std::string& file_cmd,
                         const std::string& /*debug_file*/) const
  {
    ObjectProxy *proxy = toolkitObjectProxy (go);

    if (proxy)
      proxy->print (QString::fromStdString (file_cmd),
                    QString::fromStdString (term));
  }

  uint8NDArray
  qt_graphics_toolkit::get_pixels (const graphics_object& go) const
  {
    uint8NDArray retval;

    if (go.isa ("figure"))
      {
        ObjectProxy *proxy = toolkitObjectProxy (go);

        if (proxy)
          retval = proxy->get_pixels ();
      }

    return retval;
  }

  Matrix
  qt_graphics_toolkit::get_text_extent (const graphics_object& go) const
  {
    Matrix ext (1, 4, 0.0);

    if (go.isa ("uicontrol"))
      {
        octave_value str = go.get ("string");
        if (! str.isempty ())
          {
            const uicontrol::properties& up =
              dynamic_cast<const uicontrol::properties&> (go.get_properties ());
            Matrix bb = up.get_boundingbox (false);
            QFont font = Utils::computeFont<uicontrol> (up, bb(3));
            QFontMetrics fm (font);

            QString s;
            QSize sz;

            if (str.is_string ())
              {
                s = QString::fromStdString (str.string_value ());
                sz = fm.size (Qt::TextSingleLine, s);
                ext(2) = sz.width ();
                ext(3) = sz.height ();
              }
            else if (str.iscellstr ())
              {
                string_vector sv = str.string_vector_value ();
                double wd = 0.0;
                double hg = 0.0;
                for (octave_idx_type ii = 0; ii < sv.numel (); ii++)
                  {
                    s = QString::fromStdString (sv(ii));
                    sz = fm.size (Qt::TextSingleLine, s);
                    wd = std::max (wd, static_cast<double> (sz.width ()));
                    hg = std::max (hg, static_cast<double> (sz.height ()));
                  }

                ext(2) = wd;
                // FIXME: Find a better way to determine the height of e.g.
                // listbox uicontrol objects
                ext(3) = hg * sv.numel ();
              }
          }
      }

    return ext;
  }

  Object*
  qt_graphics_toolkit::toolkitObject (const graphics_object& go)
  {
    ObjectProxy *proxy = toolkitObjectProxy (go);

    if (proxy)
      return proxy->object ();

    return nullptr;
  }

  ObjectProxy*
  qt_graphics_toolkit::toolkitObjectProxy (const graphics_object& go)
  {
    if (go)
      {
        octave_value ov = go.get (toolkitObjectProperty (go));

        if (ov.is_defined () && ! ov.isempty ())
          {
            OCTAVE_INTPTR_TYPE ptr = ov.OCTAVE_PTR_SCALAR ().value ();

            return reinterpret_cast<ObjectProxy *> (ptr);
          }
      }

    return nullptr;
  }

  void
  qt_graphics_toolkit::interpreter_event (const octave::fcn_callback& fcn)
  {
    octave::event_manager& evmgr = m_interpreter.get_event_manager ();

    evmgr.post_event (fcn);
  }

  void
  qt_graphics_toolkit::interpreter_event (const octave::meth_callback& meth)
  {
    octave::event_manager& evmgr = m_interpreter.get_event_manager ();

    evmgr.post_event (meth);
  }
};
