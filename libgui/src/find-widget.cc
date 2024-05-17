////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2024 The Octave Project Developers
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

#include "gui-settings.h"
#include "gui-preferences-sc.h"
#include "find-widget.h"


OCTAVE_BEGIN_NAMESPACE(octave)

// The documentation splitter, which is the main widget
// of the doc dock widget
find_widget::find_widget (bool x_button, QWidget *p)
  : QWidget (p),
    m_is_closeable (x_button),
    m_find_line_edit (new QLineEdit (this)),
    m_findnext_shortcut (new QShortcut (this)),
    m_findprev_shortcut (new QShortcut (this))
{
  gui_settings settings;

  QLabel *find_label = new QLabel (tr ("Find:"), this);

  m_find_line_edit->setClearButtonEnabled (true);

  connect (m_find_line_edit, &QLineEdit::returnPressed,
           this, &find_widget::find);
  connect (m_find_line_edit, &QLineEdit::textEdited,
           this, &find_widget::find_incremental);

  QToolButton *forward_button = new QToolButton (this);
  forward_button->setText (tr ("Search forward"));
  forward_button->setToolTip (tr ("Search forward"));
  forward_button->setIcon (settings.icon ("go-down"));
  connect (forward_button, &QToolButton::pressed,
           this, &find_widget::find);

  QToolButton *backward_button = new QToolButton (this);
  backward_button->setText (tr ("Search backward"));
  backward_button->setToolTip (tr ("Search backward"));
  backward_button->setIcon (settings.icon ("go-up"));
  connect (backward_button, &QToolButton::pressed,
           this, &find_widget::find_backward);

  QHBoxLayout *h_box_this = new QHBoxLayout (this);
  h_box_this->addWidget (find_label);
  h_box_this->addWidget (m_find_line_edit);
  h_box_this->addWidget (forward_button);
  h_box_this->addWidget (backward_button);

  if (x_button)
    {
      QToolButton *close_button = new QToolButton (this);
      close_button->setText (tr ("Close"));
      close_button->setToolTip (tr ("Close find dialog"));
      close_button->setIcon (settings.icon ("window-close"));
      connect (close_button, &QToolButton::pressed,
               this, [this] () { close (); });
      h_box_this->addWidget (close_button);
    }

  h_box_this->setContentsMargins (2, 2, 2, 2);
  this->setLayout (h_box_this);

  notice_settings ();

  m_findnext_shortcut->setContext (Qt::WidgetWithChildrenShortcut);
  connect (m_findnext_shortcut, &QShortcut::activated,
           this, &find_widget::find);

  m_findprev_shortcut->setContext (Qt::WidgetWithChildrenShortcut);
  connect (m_findprev_shortcut, &QShortcut::activated,
           this, &find_widget::find_backward);
}

void
find_widget::activate_find ()
{
  if (m_is_closeable && isVisible ())
    {
      hide ();
    }
  else
    {
      show ();
      m_find_line_edit->selectAll ();
      m_find_line_edit->setFocus ();
    }
}

void
find_widget::find ()
{
  Q_EMIT find_signal (m_find_line_edit->text (), false);
}

void
find_widget::find_backward ()
{
  Q_EMIT find_signal (m_find_line_edit->text (), true);
}

void
find_widget::find_incremental ()
{
  Q_EMIT find_incremental_signal (m_find_line_edit->text ());
}

void
find_widget::set_text (const QString& text)
{
  m_find_line_edit->setText (text);
}

QString
find_widget::text ()
{
  return m_find_line_edit->text ();
}

void
find_widget::notice_settings ()
{
  gui_settings settings;

  settings.shortcut (m_findnext_shortcut, sc_edit_edit_find_next);
  settings.shortcut (m_findprev_shortcut, sc_edit_edit_find_previous);
}

void
find_widget::save_settings ()
{
}

OCTAVE_END_NAMESPACE(octave)
