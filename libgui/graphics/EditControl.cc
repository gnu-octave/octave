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

#include <QLineEdit>

#include "Container.h"
#include "EditControl.h"
#include "TextEdit.h"
#include "QtHandlesUtils.h"

#include "octave-qobject.h"

OCTAVE_BEGIN_NAMESPACE(octave)

EditControl *
EditControl::create (octave::base_qobject& oct_qobj,
                     octave::interpreter& interp, const graphics_object& go)
{
  Object *parent = parentObject (interp, go);

  if (parent)
    {
      Container *container = parent->innerContainer ();

      if (container)
        {
          uicontrol::properties& up = Utils::properties<uicontrol> (go);

          if ((up.get_max () - up.get_min ()) > 1)
            return new EditControl (oct_qobj, interp, go,
                                    new TextEdit (container));
          else
            return new EditControl (oct_qobj, interp, go,
                                    new QLineEdit (container));
        }
    }

  return nullptr;
}

EditControl::EditControl (octave::base_qobject& oct_qobj,
                          octave::interpreter& interp,
                          const graphics_object& go, QLineEdit *edit)
  : BaseControl (oct_qobj, interp, go, edit), m_multiLine (false),
    m_textChanged (false)
{
  init (edit);
}

void
EditControl::init (QLineEdit *edit, bool callBase)
{
  if (callBase)
    BaseControl::init (edit, callBase);

  m_multiLine = false;
  initCommon (edit);

  uicontrol::properties& up = properties<uicontrol> ();

  if (up.enable_is ("inactive"))
    edit->setReadOnly (true);
  else
    edit->setEnabled (up.enable_is ("on"));
  edit->setText (Utils::fromStdString (up.get_string_string ()));
  edit->setAlignment (Utils::fromHVAlign (up.get_horizontalalignment (),
                                          up.get_verticalalignment ()));

  connect (edit, &QLineEdit::textEdited,
           this, &EditControl::textChanged);
  connect (edit, &QLineEdit::editingFinished,
           this, &EditControl::editingFinished);
  connect (edit, &QLineEdit::returnPressed,
           this, &EditControl::returnPressed);
}

EditControl::EditControl (octave::base_qobject& oct_qobj,
                          octave::interpreter& interp,
                          const graphics_object& go, TextEdit *edit)
  : BaseControl (oct_qobj, interp, go, edit), m_multiLine (true),
    m_textChanged (false)
{
  init (edit);
}

void
EditControl::init (TextEdit *edit, bool callBase)
{
  if (callBase)
    BaseControl::init (edit, callBase);

  m_multiLine = true;
  initCommon (edit);

  uicontrol::properties& up = properties<uicontrol> ();

  if (up.enable_is ("inactive"))
    edit->setReadOnly (true);
  else
    edit->setEnabled (up.enable_is ("on"));
  edit->setAcceptRichText (false);
  edit->setPlainText (Utils::fromStringVector
                      (up.get_string_vector ()).join ("\n"));
  edit->setAlignment (Utils::fromHVAlign (up.get_horizontalalignment (),
                                          up.get_verticalalignment ()));

  connect (edit, &TextEdit::textChanged,
           this, &EditControl::textChanged);
  connect (edit, &TextEdit::editingFinished,
           this, &EditControl::editingFinished);
  connect (edit, &TextEdit::returnPressed,
           this, &EditControl::returnPressed);
}

EditControl::~EditControl (void)
{ }

void
EditControl::initCommon (QWidget *)
{
  m_textChanged = false;
}

void
EditControl::update (int pId)
{
  bool handled = false;

  if (m_multiLine)
    handled = updateMultiLine (pId);
  else
    handled = updateSingleLine (pId);

  if (! handled)
    {
      switch (pId)
        {
        default:
          BaseControl::update (pId);
          break;
        }
    }
}

bool
EditControl::updateSingleLine (int pId)
{
  uicontrol::properties& up = properties<uicontrol> ();
  QLineEdit *edit = qWidget<QLineEdit> ();

  switch (pId)
    {
    case uicontrol::properties::ID_STRING:
      edit->setText (Utils::fromStdString (up.get_string_string ()));
      return true;

    case uicontrol::properties::ID_HORIZONTALALIGNMENT:
    case uicontrol::properties::ID_VERTICALALIGNMENT:
      edit->setAlignment (Utils::fromHVAlign (up.get_horizontalalignment (),
                                              up.get_verticalalignment ()));
      return true;

    case uicontrol::properties::ID_ENABLE:
      if (up.enable_is ("inactive"))
        edit->setReadOnly (true);
      else
        {
          edit->setReadOnly (false);
          edit->setEnabled (up.enable_is ("on"));
        }
      return true;

    case uicontrol::properties::ID_MIN:
    case uicontrol::properties::ID_MAX:
      if ((up.get_max () - up.get_min ()) > 1)
        {
          QWidget *container = edit->parentWidget ();

          delete edit;
          init (new TextEdit (container), true);
        }
      return true;

    default:
      break;
    }

  return false;
}

bool
EditControl::updateMultiLine (int pId)
{
  uicontrol::properties& up = properties<uicontrol> ();
  TextEdit *edit = qWidget<TextEdit> ();

  switch (pId)
    {
    case uicontrol::properties::ID_STRING:
      edit->setPlainText (Utils::fromStringVector
                          (up.get_string_vector ()).join ("\n"));
      return true;

    case uicontrol::properties::ID_HORIZONTALALIGNMENT:
    case uicontrol::properties::ID_VERTICALALIGNMENT:
      edit->setAlignment (Utils::fromHVAlign (up.get_horizontalalignment (),
                                              up.get_verticalalignment ()));
      return true;

    case uicontrol::properties::ID_ENABLE:
      if (up.enable_is ("inactive"))
        edit->setReadOnly (true);
      else
        {
          edit->setReadOnly (false);
          edit->setEnabled (up.enable_is ("on"));
        }
      return true;

    case uicontrol::properties::ID_MIN:
    case uicontrol::properties::ID_MAX:
      if ((up.get_max () - up.get_min ()) <= 1)
        {
          QWidget *container = edit->parentWidget ();

          delete edit;
          init (new QLineEdit (container), true);
        }
      return true;

    default:
      break;
    }

  return false;
}

void
EditControl::textChanged (void)
{
  m_textChanged = true;
}

void
EditControl::returnPressed (void)
{
  QString txt = (m_multiLine
                 ? qWidget<TextEdit> ()->toPlainText ()
                 : qWidget<QLineEdit> ()->text ());

  if (m_textChanged)
    {
      if (m_multiLine)
        emit gh_set_event (m_handle, "string",
                           Utils::toCellString (txt.split ("\n")), false);
      else
        emit gh_set_event (m_handle, "string",
                           Utils::toStdString (txt), false);

      m_textChanged = false;
    }

  if (txt.length () > 0)
    emit gh_callback_event (m_handle, "callback");
}

void
EditControl::editingFinished (void)
{
  if (m_textChanged)
    {
      QString txt = (m_multiLine
                     ? qWidget<TextEdit> ()->toPlainText ()
                     : qWidget<QLineEdit> ()->text ());
      if (m_multiLine)
        emit gh_set_event (m_handle, "string",
                           Utils::toCellString (txt.split ("\n")), false);
      else
        emit gh_set_event (m_handle, "string", Utils::toStdString (txt),
                           false);
      emit gh_callback_event (m_handle, "callback");

      m_textChanged = false;
    }
}

OCTAVE_END_NAMESPACE(octave)
