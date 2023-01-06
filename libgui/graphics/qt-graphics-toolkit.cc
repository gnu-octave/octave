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

#include <cstdint>

#include <QApplication>
#include <QFontMetrics>
#include <QThread>

#include "ButtonGroup.h"
#include "CheckBoxControl.h"
#include "ContextMenu.h"
#include "EditControl.h"
#include "Figure.h"
#include "ListBoxControl.h"
#include "Logger.h"
#include "Menu.h"
#include "Object.h"
#include "ObjectProxy.h"
#include "Panel.h"
#include "PopupMenuControl.h"
#include "PushButtonControl.h"
#include "PushTool.h"
#include "QtHandlesUtils.h"
#include "RadioButtonControl.h"
#include "SliderControl.h"
#include "Table.h"
#include "TextControl.h"
#include "ToggleButtonControl.h"
#include "ToggleTool.h"
#include "ToolBar.h"
#include "qt-graphics-toolkit.h"

#include "octave-qobject.h"

#include "event-manager.h"
#include "graphics.h"
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

OCTAVE_BEGIN_NAMESPACE(octave)

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
    qCritical ("octave::qt_graphics_toolkit: no __object__ property known for object "
               "of type %s", go.type ().c_str ());

  return "";
}

qt_graphics_toolkit::qt_graphics_toolkit (octave::interpreter& interp,
                                          octave::base_qobject& oct_qobj)
  : QObject (), base_graphics_toolkit ("qt"), m_interpreter (interp),
    m_octave_qobj (oct_qobj)
{
  // Implemented with a signal/slot connection in order to properly
  // cross from the interpreter thread (where requests to create
  // graphics object are initiated) to the GUI application thread
  // (where they are actually created and displayed).
  // We need to make sure the GUI Object and its proxy are properly
  // created before the initialize method returns, so we use a
  // BlockingQueuedConnection. After the signal is emitted, the interpreter
  // thread is locked until the slot has returned.

  connect (this, &qt_graphics_toolkit::create_object_signal,
           this, &qt_graphics_toolkit::create_object,
           Qt::BlockingQueuedConnection);
}

bool
qt_graphics_toolkit::initialize (const graphics_object& go)
{
  if (go.isa ("figure")
      || (go.isa ("uicontrol") && go.get ("style").string_value () != "frame")
      || go.isa ("uipanel")
      || go.isa ("uibuttongroup")
      || go.isa ("uimenu")
      || go.isa ("uicontextmenu")
      || go.isa ("uitable")
      || go.isa ("uitoolbar")
      || go.isa ("uipushtool")
      || go.isa ("uitoggletool"))
    {
      // FIXME: We need to unlock the mutex here but we have no way to know
      // if it was previously locked by this thread, and thus if we should
      // re-lock it.

      gh_manager& gh_mgr = m_interpreter.get_gh_manager ();

      gh_mgr.unlock ();

      Logger::debug ("qt_graphics_toolkit::initialize %s from thread %p",
                     go.type ().c_str (), QThread::currentThreadId ());

      ObjectProxy *proxy = new ObjectProxy ();
      graphics_object gObj (go);

      OCTAVE_PTR_TYPE tmp (reinterpret_cast<OCTAVE_INTPTR_TYPE> (proxy));
      gObj.get_properties ().set (toolkitObjectProperty (go), tmp);

      emit create_object_signal (go.get_handle ().value ());

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

  Logger::debug ("qt_graphics_toolkit::update %s(%d) from thread %p",
                 go.type ().c_str (), pId, QThread::currentThreadId ());

  ObjectProxy *proxy = toolkitObjectProxy (go);

  if (proxy)
    {
      if ((go.isa ("uicontrol")
           && pId == uicontrol::properties::ID_STYLE)
          || (go.isa ("uitable")
              && pId == uitable::properties::ID_DATA))
        {
          // Special case: we need to recreate the control widget
          // associated with the octave graphics_object
          // FIXME: For uitable, it would only be necessary to recreate
          // the table widget if the type of the displayed values changes
          // between Boolean and non-Boolean (bug #63388).

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

  gh_manager& gh_mgr = m_interpreter.get_gh_manager ();

  gh_mgr.unlock ();

  Logger::debug ("qt_graphics_toolkit::finalize %s from thread %p",
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

Object *
qt_graphics_toolkit::toolkitObject (const graphics_object& go)
{
  ObjectProxy *proxy = toolkitObjectProxy (go);

  if (proxy)
    return proxy->object ();

  return nullptr;
}

ObjectProxy *
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

void
qt_graphics_toolkit::create_object (double handle)
{
  gh_manager& gh_mgr = m_interpreter.get_gh_manager ();

  octave::autolock guard (gh_mgr.graphics_lock ());

  graphics_object go (gh_mgr.get_object (graphics_handle (handle)));

  if (! go.valid_object ())
    {
      qWarning ("qt_graphics_toolkit::create_object: invalid object for handle %g",
                handle);
      return;
    }

  if (go.get_properties ().is_beingdeleted ())
    {
      qWarning ("qt_graphics_toolkit::create_object: object is being deleted");
      return;
    }

  ObjectProxy *proxy = qt_graphics_toolkit::toolkitObjectProxy (go);

  if (! proxy)
    {
      qWarning ("qt_graphics_toolkit::create_object: no proxy for handle %g",
                handle);
      return;
    }

  Logger::debug ("qt_graphics_toolkit::create_object: "
                 "create %s from thread %p",
                 go.type ().c_str (), QThread::currentThreadId ());

  Object *obj = nullptr;

  if (go.isa ("figure"))
    obj = Figure::create (m_octave_qobj, m_interpreter, go);
  else if (go.isa ("uicontrol"))
    {
      uicontrol::properties& up =
        Utils::properties<uicontrol> (go);

      if (up.style_is ("pushbutton"))
        obj = PushButtonControl::create (m_octave_qobj, m_interpreter, go);
      else if (up.style_is ("edit"))
        obj = EditControl::create (m_octave_qobj, m_interpreter, go);
      else if (up.style_is ("checkbox"))
        obj = CheckBoxControl::create (m_octave_qobj, m_interpreter, go);
      else if (up.style_is ("radiobutton"))
        obj = RadioButtonControl::create (m_octave_qobj, m_interpreter, go);
      else if (up.style_is ("togglebutton"))
        obj = ToggleButtonControl::create (m_octave_qobj, m_interpreter, go);
      else if (up.style_is ("text"))
        obj = TextControl::create (m_octave_qobj, m_interpreter, go);
      else if (up.style_is ("popupmenu"))
        obj = PopupMenuControl::create (m_octave_qobj, m_interpreter, go);
      else if (up.style_is ("slider"))
        obj = SliderControl::create (m_octave_qobj, m_interpreter, go);
      else if (up.style_is ("listbox"))
        obj = ListBoxControl::create (m_octave_qobj, m_interpreter, go);
    }
  else if (go.isa ("uibuttongroup"))
    obj = ButtonGroup::create (m_octave_qobj, m_interpreter, go);
  else if (go.isa ("uipanel"))
    obj = Panel::create (m_octave_qobj, m_interpreter, go);
  else if (go.isa ("uimenu"))
    obj = Menu::create (m_octave_qobj, m_interpreter, go);
  else if (go.isa ("uicontextmenu"))
    obj = ContextMenu::create (m_octave_qobj, m_interpreter, go);
  else if (go.isa ("uitable"))
    obj = Table::create (m_octave_qobj, m_interpreter, go);
  else if (go.isa ("uitoolbar"))
    obj = ToolBar::create (m_octave_qobj, m_interpreter, go);
  else if (go.isa ("uipushtool"))
    obj = PushTool::create (m_octave_qobj, m_interpreter, go);
  else if (go.isa ("uitoggletool"))
    obj = ToggleTool::create (m_octave_qobj, m_interpreter, go);
  else
    qWarning ("qt_graphics_toolkit::create_object: unsupported type '%s'",
              go.type ().c_str ());

  if (obj)
    {
      proxy->setObject (obj);
      obj->do_connections (this);
    }
}

void qt_graphics_toolkit::gh_callback_event (const graphics_handle& h,
                                             const std::string& nm)
{
  gh_manager& gh_mgr = m_interpreter.get_gh_manager ();

  gh_mgr.post_callback (h, nm);
}

void qt_graphics_toolkit::gh_callback_event (const graphics_handle& h,
                                             const std::string& nm,
                                             const octave_value& data)
{
  gh_manager& gh_mgr = m_interpreter.get_gh_manager ();

  gh_mgr.post_callback (h, nm, data);
}

void qt_graphics_toolkit::gh_set_event (const graphics_handle& h,
                                        const std::string& nm,
                                        const octave_value& value)
{
  gh_manager& gh_mgr = m_interpreter.get_gh_manager ();

  gh_mgr.post_set (h, nm, value);
}

void qt_graphics_toolkit::gh_set_event (const graphics_handle& h,
                                        const std::string& nm,
                                        const octave_value& value,
                                        bool notify_toolkit)
{
  gh_manager& gh_mgr = m_interpreter.get_gh_manager ();

  gh_mgr.post_set (h, nm, value, notify_toolkit);
}

void qt_graphics_toolkit::gh_set_event (const graphics_handle& h,
                                        const std::string& nm,
                                        const octave_value& value,
                                        bool notify_toolkit,
                                        bool redraw_figure)
{
  gh_manager& gh_mgr = m_interpreter.get_gh_manager ();

  gh_mgr.post_set (h, nm, value, notify_toolkit, redraw_figure);
}

OCTAVE_END_NAMESPACE(octave);
