// Find dialog derived from an example from Qt Toolkit (license below (**))

////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2009-2023 The Octave Project Developers
//
// See the file COPYRIGHT.md in the top-level directory of this
// or <https://octave.org/copyright/>.
//
//  All rights reserved.
//  Contact: Nokia Corporation (qt-info@nokia.com)
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
// ** This file is part of the examples of the Qt Toolkit.
// **
// ** $QT_BEGIN_LICENSE:LGPL$
// ** Commercial Usage
// ** Licensees holding valid Qt Commercial licenses may use this file in
// ** accordance with the Qt Commercial License Agreement provided with the
// ** Software or, alternatively, in accordance with the terms contained in
// ** a written agreement between you and Nokia.
// **
// ** GNU Lesser General Public License Usage
// ** Alternatively, this file may be used under the terms of the GNU Lesser
// ** General Public License version 2.1 as published by the Free Software
// ** Foundation and appearing in the file LICENSE.LGPL included in the
// ** packaging of this file.  Please review the following information to
// ** ensure the GNU Lesser General Public License version 2.1 requirements
// ** will be met: https://www.gnu.org/licenses/old-licenses/lgpl-2.1.html.
// **
// ** In addition, as a special exception, Nokia gives you certain additional
// ** rights.  These rights are described in the Nokia Qt LGPL Exception
// ** version 1.1, included in the file LGPL_EXCEPTION.txt in this package.
// **
// ** GNU General Public License Usage
// ** Alternatively, this file may be used under the terms of the GNU
// ** General Public License version 3.0 as published by the Free Software
// ** Foundation and appearing in the file LICENSE.GPL included in the
// ** packaging of this file.  Please review the following information to
// ** ensure the GNU General Public License version 3.0 requirements will be
// ** met: https://www.gnu.org/copyleft/gpl.html.
// **
// ** If you have questions regarding the use of this file, please contact
// ** Nokia at qt-info@nokia.com.
// ** $QT_END_LICENSE$
//
////////////////////////////////////////////////////////////////////////

#if defined (HAVE_CONFIG_H)
#  include "config.h"
#endif

#if defined (HAVE_QSCINTILLA)

#include <QApplication>
#include <QCheckBox>
#include <QCheckBox>
#include <QCompleter>
#include <QDialogButtonBox>
#include <QGridLayout>
#include <QIcon>
#include <QLabel>
#include <QLineEdit>
#include <QMessageBox>
#include <QPushButton>
#include <QVBoxLayout>

#include "find-dialog.h"
#include "gui-preferences-ed.h"
#include "gui-utils.h"
#include "resource-manager.h"
#include "octave-qobject.h"

OCTAVE_BEGIN_NAMESPACE(octave)

find_dialog::find_dialog (base_qobject& oct_qobj,
                          octave_dock_widget *ed, QWidget *p)
: QDialog (p), m_octave_qobj (oct_qobj), m_editor (ed),
  m_in_sel (false), m_sel_beg (-1), m_sel_end (-1)
{
  setWindowTitle (tr ("Editor: Find and Replace"));

  m_search_label = new QLabel (tr ("Find &what:"));
  m_search_line_edit = new QComboBox (this);
  m_search_line_edit->setToolTip (tr ("Enter text to search for"));
  m_search_line_edit->setEditable (true);
  m_search_line_edit->setMaxCount (m_mru_length);
  m_search_line_edit->completer ()->setCaseSensitivity (Qt::CaseSensitive);
  m_search_label->setBuddy (m_search_line_edit);

  m_replace_label = new QLabel (tr ("Re&place with:"));
  m_replace_line_edit = new QComboBox (this);
  m_replace_line_edit->setToolTip (tr ("Enter new text replacing search hits"));
  m_replace_line_edit->setEditable (true);
  m_replace_line_edit->setMaxCount (m_mru_length);
  m_replace_line_edit->completer ()->setCaseSensitivity (Qt::CaseSensitive);
  m_replace_label->setBuddy (m_replace_line_edit);

  int width = QFontMetrics (m_search_line_edit->font ()).averageCharWidth();
  m_search_line_edit->setFixedWidth (20*width);
  m_replace_line_edit->setFixedWidth (20*width);

  m_case_check_box = new QCheckBox (tr ("Match &case"));
  m_from_start_check_box = new QCheckBox (tr ("Search from &start"));
  m_wrap_check_box = new QCheckBox (tr ("&Wrap while searching"));
  m_wrap_check_box->setChecked (true);
  m_find_next_button = new QPushButton (tr ("&Find Next"));
  m_find_prev_button = new QPushButton (tr ("Find &Previous"));
  m_replace_button = new QPushButton (tr ("&Replace"));
  m_replace_all_button = new QPushButton (tr ("Replace &All"));

  m_more_button = new QPushButton (tr ("&More..."));
  m_more_button->setCheckable (true);
  m_more_button->setAutoDefault (false);

  m_button_box = new QDialogButtonBox (Qt::Vertical);
  m_button_box->addButton (m_find_next_button, QDialogButtonBox::ActionRole);
  m_button_box->addButton (m_find_prev_button, QDialogButtonBox::ActionRole);
  m_button_box->addButton (m_replace_button, QDialogButtonBox::ActionRole);
  m_button_box->addButton (m_replace_all_button, QDialogButtonBox::ActionRole);
  m_button_box->addButton (m_more_button, QDialogButtonBox::ActionRole);
  m_button_box->addButton (QDialogButtonBox::Close);

  m_extension = new QWidget (this);
  m_whole_words_check_box = new QCheckBox (tr ("&Whole words"));
  m_regex_check_box = new QCheckBox (tr ("Regular E&xpressions"));
  m_backward_check_box = new QCheckBox (tr ("Search &backward"));
  m_search_selection_check_box = new QCheckBox (tr ("Search se&lection"));
  m_search_selection_check_box->setCheckable (true);

  connect (m_find_next_button, &QPushButton::clicked,
           this, &find_dialog::find_next);
  connect (m_find_prev_button, &QPushButton::clicked,
           this, &find_dialog::find_prev);
  connect (m_more_button, &QPushButton::toggled,
           m_extension, &QWidget::setVisible);
  connect (m_replace_button, &QPushButton::clicked,
           this, &find_dialog::replace);
  connect (m_replace_all_button, &QPushButton::clicked,
           this, &find_dialog::replace_all);
  connect (m_backward_check_box, &QCheckBox::stateChanged,
           this, &find_dialog::handle_backward_search_changed);
  connect (m_button_box, &QDialogButtonBox::rejected,
           this, &find_dialog::close);

  connect (m_search_selection_check_box, &QCheckBox::stateChanged,
           this, &find_dialog::handle_sel_search_changed);

  QVBoxLayout *extension_layout = new QVBoxLayout ();
  extension_layout->setMargin (0);
  extension_layout->addWidget (m_whole_words_check_box);
  extension_layout->addWidget (m_backward_check_box);
  extension_layout->addWidget (m_search_selection_check_box);
  m_extension->setLayout (extension_layout);

  QGridLayout *top_left_layout = new QGridLayout;
  top_left_layout->addWidget (m_search_label, 1, 1);
  top_left_layout->addWidget (m_search_line_edit, 1, 2);
  top_left_layout->addWidget (m_replace_label, 2, 1);
  top_left_layout->addWidget (m_replace_line_edit, 2, 2);

  QVBoxLayout *left_layout = new QVBoxLayout;
  left_layout->addLayout (top_left_layout);
  left_layout->insertStretch (1, 5);
  left_layout->addWidget (m_case_check_box);
  left_layout->addWidget (m_from_start_check_box);
  left_layout->addWidget (m_wrap_check_box);
  left_layout->addWidget (m_regex_check_box);

  QGridLayout *main_layout = new QGridLayout;
  main_layout->setSizeConstraint (QLayout::SetFixedSize);
  main_layout->addLayout (left_layout, 0, 0);
  main_layout->addWidget (m_button_box, 0, 1);
  main_layout->addWidget (m_extension, 1, 0);
  setLayout (main_layout);

  m_extension->hide ();
  m_find_next_button->setDefault (true);
  m_find_result_available = false;
  m_rep_all = 0;
  m_rep_active = false;

  // Connect required external signals
  connect (ed, SIGNAL (edit_area_changed (octave_qscintilla *)),
           this, SLOT (update_edit_area (octave_qscintilla *)));

  setWindowModality (Qt::NonModal);

  setAttribute(Qt::WA_ShowWithoutActivating);
  setAttribute(Qt::WA_DeleteOnClose);
}

// The edit_area has changed: update relevant data of the file dialog
void find_dialog::update_edit_area (octave_qscintilla *edit_area)
{
  m_edit_area = edit_area;
  m_search_selection_check_box->setEnabled (edit_area->hasSelectedText ());

  connect (m_edit_area, SIGNAL (copyAvailable (bool)),
           this,       SLOT (handle_selection_changed (bool)),
           Qt::UniqueConnection);
}

void find_dialog::save_settings ()
{
  resource_manager& rmgr = m_octave_qobj.get_resource_manager ();
  gui_settings *s = rmgr.get_settings ();

  // Save position
  QPoint dlg_pos = pos ();

#if defined (Q_OS_WIN32)
  int y = dlg_pos.y ();
#else
  int y = dlg_pos.y () - geometry ().height () + frameGeometry ().height ();
#endif

  m_last_position = QPoint (dlg_pos.x (), y);

  s->setValue (ed_fdlg_pos.key, m_last_position);

  // Is current search/replace text in the mru list?
  mru_update (m_search_line_edit);
  mru_update (m_replace_line_edit);

  // Store mru lists
  QStringList mru;
  for (int i = 0; i < m_search_line_edit->count (); i++)
    mru.append (m_search_line_edit->itemText (i));
  s->setValue (ed_fdlg_search.key, mru);

  mru.clear ();
  for (int i = 0; i < m_replace_line_edit->count (); i++)
    mru.append (m_replace_line_edit->itemText (i));
  s->setValue (ed_fdlg_replace.key, mru);

  // Store dialog's options
  int opts = 0
    + m_extension->isVisible () * FIND_DLG_MORE
    + m_case_check_box->isChecked () * FIND_DLG_CASE
    + m_from_start_check_box->isChecked () * FIND_DLG_START
    + m_wrap_check_box->isChecked () * FIND_DLG_WRAP
    + m_regex_check_box->isChecked () * FIND_DLG_REGX
    + m_whole_words_check_box->isChecked () * FIND_DLG_WORDS
    + m_backward_check_box->isChecked () * FIND_DLG_BACK
    + m_search_selection_check_box->isChecked () * FIND_DLG_SEL;
  s->setValue (ed_fdlg_opts.key, opts);

  s->sync ();
}

void find_dialog::restore_settings (QPoint ed_bottom_right)
{
  resource_manager& rmgr = m_octave_qobj.get_resource_manager ();
  gui_settings *s = rmgr.get_settings ();

  // Get mru lists for search and replace text
  QStringList mru = s->value (ed_fdlg_search.key).toStringList ();
  while (mru.length () > m_mru_length)
    mru.removeLast ();
  m_search_line_edit->addItems (mru);

  mru = s->value (ed_fdlg_replace.key).toStringList ();
  while (mru.length () > m_mru_length)
    mru.removeLast ();
  m_replace_line_edit->addItems (mru);

  // Get the dialog's options
  int opts = s->value (ed_fdlg_opts.key, ed_fdlg_opts.def).toInt ();

  m_extension->setVisible (FIND_DLG_MORE & opts);
  m_case_check_box->setChecked (FIND_DLG_CASE & opts);
  m_from_start_check_box->setChecked (FIND_DLG_START & opts);
  m_wrap_check_box->setChecked (FIND_DLG_WRAP & opts);
  m_regex_check_box->setChecked (FIND_DLG_REGX & opts);
  m_whole_words_check_box->setChecked (FIND_DLG_WORDS & opts);
  m_backward_check_box->setChecked (FIND_DLG_BACK & opts);
  m_search_selection_check_box->setChecked (FIND_DLG_SEL & opts);

  // Default position:  lower right of editor's position
  int xp = ed_bottom_right.x () - sizeHint ().width ();
  int yp = ed_bottom_right.y () - sizeHint ().height ();
  QRect default_geometry (xp, yp, sizeHint ().width (), sizeHint ().height ());

  // Last position from settings
  m_last_position = s->value (ed_fdlg_pos.key, QPoint (xp, yp)).toPoint ();
  QRect last_geometry (m_last_position,
                       QSize (sizeHint ().width (), sizeHint ().height ()));

  // Make sure we are on the screen
  adjust_to_screen (last_geometry, default_geometry);
  m_last_position = last_geometry.topLeft ();

  move (m_last_position);
}

// set text of "search from start" depending on backward search
void find_dialog::handle_backward_search_changed (int backward)
{
  if (backward)
    m_from_start_check_box->setText (tr ("Search from end"));
  else
    m_from_start_check_box->setText (tr ("Search from start"));
}

// search text has changed: reset the search
void find_dialog::handle_search_text_changed (void)
{
  // Return if nothing has changed
  if (m_search_line_edit->currentText () == m_search_line_edit->itemText (0))
    return;

  if (m_search_selection_check_box->isChecked ())
    m_find_result_available = false;

  mru_update (m_search_line_edit);
}

// replaced text has changed: reset the search
void find_dialog::handle_replace_text_changed (void)
{
  // Return if nothing has changed
  if (m_replace_line_edit->currentText () == m_replace_line_edit->itemText (0))
    return;

  mru_update (m_replace_line_edit);
}

// Update the mru list
void find_dialog::mru_update (QComboBox *mru)
{
  // Remove possible empty entries from the mru list
  int index;
  while ((index = mru->findText (QString ())) >= 0)
    mru->removeItem (index);

  // Get current text and return if it is empty
  QString text = mru->currentText ();

  if (text.isEmpty ())
    return;

  // Remove occurrences of the current text in the mru list
  while ((index = mru->findText (text)) >= 0)
    mru->removeItem (index);

  // Remove the last entry from the end if the list is full
  if (mru->count () == m_mru_length)
    mru->removeItem (m_mru_length -1);

  // Insert new item at the beginning and set it as current item
  mru->insertItem (0, text);
  mru->setCurrentIndex (0);
}

void find_dialog::handle_sel_search_changed (int selected)
{
  m_from_start_check_box->setEnabled (! selected);
  m_find_result_available = false;
}

void find_dialog::handle_selection_changed (bool has_selected)
{
  if (m_rep_active)
    return;

  m_search_selection_check_box->setEnabled (has_selected);
  m_find_result_available = false;
}

// initialize search text with selected text if this is in one single line
void find_dialog::init_search_text (void)
{
  if (m_edit_area && m_edit_area->hasSelectedText ())
    {
      int lbeg, lend, cbeg, cend;
      m_edit_area->getSelection (&lbeg, &cbeg, &lend, &cend);
      if (lbeg == lend)
        m_search_line_edit->setCurrentText (m_edit_area->selectedText ());
    }

  // set focus to "Find what" and select all text
  m_search_line_edit->setFocus ();
  m_search_line_edit->lineEdit ()->selectAll ();

  // Default to "find" next time.
  // Otherwise, it defaults to the last action, which may be "replace all".
  m_find_next_button->setDefault (true);
}

void find_dialog::find_next (void)
{
  find (! m_backward_check_box->isChecked ());
}

void find_dialog::find_prev (void)
{
  find (m_backward_check_box->isChecked ());
}

void find_dialog::find (bool forward)
{
  if (! m_edit_area)
    return;

  handle_search_text_changed ();

  // line adn col: -1 means search starts at current position
  int line = -1, col = -1;

  bool do_wrap = m_wrap_check_box->isChecked ();
  bool do_forward = forward;

  // Initialize the selection begin and end if it is the first search
  if (! m_find_result_available)
    {
      if (m_search_selection_check_box->isChecked ()
          && m_edit_area->hasSelectedText ())
        {
          int l1, c1, l2, c2;
          m_edit_area->getSelection (&l1, &c1, &l2, &c2);

          // Store the position of the selection
          m_sel_beg = m_edit_area->positionFromLineIndex (l1, c1);
          m_sel_end = m_edit_area->positionFromLineIndex (l2, c2);
          m_in_sel = true;
        }
      else
        m_in_sel = false;
    }

  // Get the correct line/col for beginning the search
  if (m_rep_all)
    {
      // Replace All
      if (m_rep_all == 1)
        {
          // Start at the beginning of file/sel if it is the first try
          if (m_in_sel)
            m_edit_area->lineIndexFromPosition (m_sel_beg, &line, &col);
          else
            {
              line = 0;
              col = 0;
            }
        }
      do_wrap = false;  // Never wrap when replacing all
    }
  else
    {
      // Normal search (not replace all): calculate start position of
      // search (in file or selection)
      if (m_from_start_check_box->isChecked ()
          || (m_in_sel && (! m_find_result_available)))
        {
          // From the beginning or the end of file/sel
          if (do_forward)
            {
              // From the beginning
              if (m_in_sel)
                m_edit_area->lineIndexFromPosition (m_sel_beg, &line, &col);
              else
                {
                  line = 0;
                  col = 0;
                }
            }
          else
            {
              // From the end
              if (m_in_sel)
                m_edit_area->lineIndexFromPosition (m_sel_end, &line, &col);
              else
                {
                  line = m_edit_area->lines () - 1;
                  col  = m_edit_area->text (line).length () - 1;
                  if (col == -1)
                    col = 0;
                }
            }
        }
      else if (! do_forward)
        {
          // Start from where the cursor is.  Fix QScintilla's cursor
          // positioning
          m_edit_area->getCursorPosition (&line, &col);
          if (m_find_result_available && m_edit_area->hasSelectedText ())
            {
              int currpos = m_edit_area->positionFromLineIndex (line, col);
              currpos -= (m_search_line_edit->currentText ().length ());
              if (currpos < 0)
                currpos = 0;
              m_edit_area->lineIndexFromPosition (currpos, &line, &col);
            }
        }
    }

  // Do the search
  m_find_result_available
    = m_edit_area->findFirst (m_search_line_edit->currentText (),
                              m_regex_check_box->isChecked (),
                              m_case_check_box->isChecked (),
                              m_whole_words_check_box->isChecked (),
                              do_wrap,
                              do_forward,
                              line, col,
                              true
#if defined (HAVE_QSCI_VERSION_2_6_0)
                              , true
#endif
                              );

  if (m_find_result_available)
    {
      // Search successful: reset search-from-start box and check for
      // the current selection
      m_from_start_check_box->setChecked (0);

      if (m_in_sel)
        {
          m_edit_area->getCursorPosition (&line, &col);
          int pos = m_edit_area->positionFromLineIndex (line, col);

          int l1, c1, l2, c2;
          m_edit_area->lineIndexFromPosition (m_sel_beg, &l1, &c1);
          m_edit_area->lineIndexFromPosition (m_sel_end, &l2, &c2);
          m_edit_area->show_selection_markers (l1, c1, l2, c2);

          // Check if new start position is still within the selection
          m_find_result_available =  pos >= m_sel_beg && pos <= m_sel_end;
        }
    }

  // No more search hits
  if (! m_find_result_available)
    {
      if (m_in_sel)
        {
          // Restore real selection and remove marker for selection
          int l1, c1, l2, c2;
          m_edit_area->lineIndexFromPosition (m_sel_beg, &l1, &c1);
          m_edit_area->lineIndexFromPosition (m_sel_end, &l2, &c2);
          m_edit_area->setSelection (l1, c1, l2, c2);
          m_edit_area->clear_selection_markers ();
        }

      // Display message if not replace all
      if (! m_rep_all)
        no_matches_message ();
    }

}

void find_dialog::do_replace (void)
{
  if (m_edit_area)
    {
      m_rep_active = true;  // changes in selection not made by the user

      m_edit_area->replace (m_replace_line_edit->currentText ());
      if (m_in_sel)
        {
          // Update the length of the selection
          m_sel_end = m_sel_end
            - m_search_line_edit->currentText ().toUtf8 ().size ()
            + m_replace_line_edit->currentText ().toUtf8 ().size ();
        }

      m_rep_active = false;
    }
}

void find_dialog::replace (void)
{
  if (m_edit_area)
    {
      handle_replace_text_changed ();

      // Do the replace if we have selected text
      if (m_find_result_available && m_edit_area->hasSelectedText ())
        do_replace ();

      find_next ();
    }
}

void find_dialog::replace_all (void)
{
  int line, col;

  if (m_edit_area)
    {
      handle_replace_text_changed ();

      m_edit_area->getCursorPosition (&line, &col);

      m_rep_all = 1;
      find_next ();  // find first occurrence (forward)

      m_edit_area->beginUndoAction ();
      while (m_find_result_available)   // while search string is found
        {
          do_replace ();
          m_rep_all++;                                          // inc counter
          find_next ();                                        // find next
        }
      m_edit_area->endUndoAction ();

      QMessageBox msg_box (QMessageBox::Information, tr ("Replace Result"),
                           tr ("%1 items replaced").arg (m_rep_all-1),
                           QMessageBox::Ok, this);
      msg_box.exec ();

      m_rep_all = 0;
      m_find_result_available = false;

      if (! m_search_selection_check_box->isChecked ())
        m_edit_area->setCursorPosition (line, col);
    }
}

void find_dialog::no_matches_message (void)
{
  QMessageBox msg_box (QMessageBox::Information, tr ("Find Result"),
                       tr ("No more matches found"), QMessageBox::Ok, this);
  msg_box.exec ();
}

void find_dialog::reject ()
{
  close ();
}

void find_dialog::closeEvent (QCloseEvent *e)
{
  save_settings ();
  e->accept ();
}

// Show and hide with (re-)storing position, otherwise there is always
// a small shift each time the dialog is shown again
void find_dialog::set_visible (bool visible)
{
  if (visible)
    {
      show ();
      move (m_last_position);
    }
  else
    {
      m_last_position = pos ();
      hide ();
    }
}

OCTAVE_END_NAMESPACE(octave)
#endif
