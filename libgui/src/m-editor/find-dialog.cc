/*

Find dialog derived from an example from Qt Toolkit (license below (**))

Copyright (C) 2012-2019 Torsten <ttl@justmail.de>
Copyright (C) 2009 Nokia Corporation and/or its subsidiary(-ies).
 All rights reserved.
 Contact: Nokia Corporation (qt-info@nokia.com)

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

** This file is part of the examples of the Qt Toolkit.
**
** $QT_BEGIN_LICENSE:LGPL$
** Commercial Usage
** Licensees holding valid Qt Commercial licenses may use this file in
** accordance with the Qt Commercial License Agreement provided with the
** Software or, alternatively, in accordance with the terms contained in
** a written agreement between you and Nokia.
**
** GNU Lesser General Public License Usage
** Alternatively, this file may be used under the terms of the GNU Lesser
** General Public License version 2.1 as published by the Free Software
** Foundation and appearing in the file LICENSE.LGPL included in the
** packaging of this file.  Please review the following information to
** ensure the GNU Lesser General Public License version 2.1 requirements
** will be met: https://www.gnu.org/licenses/old-licenses/lgpl-2.1.html.
**
** In addition, as a special exception, Nokia gives you certain additional
** rights.  These rights are described in the Nokia Qt LGPL Exception
** version 1.1, included in the file LGPL_EXCEPTION.txt in this package.
**
** GNU General Public License Usage
** Alternatively, this file may be used under the terms of the GNU
** General Public License version 3.0 as published by the Free Software
** Foundation and appearing in the file LICENSE.GPL included in the
** packaging of this file.  Please review the following information to
** ensure the GNU General Public License version 3.0 requirements will be
** met: https://www.gnu.org/copyleft/gpl.html.
**
** If you have questions regarding the use of this file, please contact
** Nokia at qt-info@nokia.com.
** $QT_END_LICENSE$
**

*/

#if defined (HAVE_CONFIG_H)
#  include "config.h"
#endif

#if defined (HAVE_QSCINTILLA)

#include <QCheckBox>
#include <QCheckBox>
#include <QDialogButtonBox>
#include <QGridLayout>
#include <QIcon>
#include <QLabel>
#include <QLineEdit>
#include <QMessageBox>
#include <QPushButton>
#include <QVBoxLayout>

#include "find-dialog.h"

namespace octave
{
  find_dialog::find_dialog (octave_qscintilla *edit_area,
                            QList<QAction *> find_actions, QWidget *p)
    : QDialog (p), m_in_sel (false), m_sel_beg (-1), m_sel_end (-1)
  {
    setWindowTitle (tr ("Find and Replace"));
    setWindowIcon (QIcon (":/actions/icons/find.png"));

    _search_label = new QLabel (tr ("Find &what:"));
    _search_line_edit = new QLineEdit;
    _search_label->setBuddy (_search_line_edit);
    _replace_label = new QLabel (tr ("Re&place with:"));
    _replace_line_edit = new QLineEdit;
    _replace_label->setBuddy (_replace_line_edit);

    _case_check_box = new QCheckBox (tr ("Match &case"));
    _from_start_check_box = new QCheckBox (tr ("Search from &start"));
    _wrap_check_box = new QCheckBox (tr ("&Wrap while searching"));
    _wrap_check_box->setChecked (true);
    _find_next_button = new QPushButton (tr ("&Find Next"));
    _find_prev_button = new QPushButton (tr ("Find &Previous"));
    _replace_button = new QPushButton (tr ("&Replace"));
    _replace_all_button = new QPushButton (tr ("Replace &All"));

    _more_button = new QPushButton (tr ("&More..."));
    _more_button->setCheckable (true);
    _more_button->setAutoDefault (false);

    _button_box = new QDialogButtonBox (Qt::Vertical);
    _button_box->addButton (_find_next_button, QDialogButtonBox::ActionRole);
    _button_box->addButton (_find_prev_button, QDialogButtonBox::ActionRole);
    _button_box->addButton (_replace_button, QDialogButtonBox::ActionRole);
    _button_box->addButton (_replace_all_button, QDialogButtonBox::ActionRole);
    _button_box->addButton (_more_button, QDialogButtonBox::ActionRole);
    _button_box->addButton (QDialogButtonBox::Close);

    _extension = new QWidget (this);
    _whole_words_check_box = new QCheckBox (tr ("&Whole words"));
    _regex_check_box = new QCheckBox (tr ("Regular E&xpressions"));
    _backward_check_box = new QCheckBox (tr ("Search &backward"));
    _search_selection_check_box = new QCheckBox (tr ("Search se&lection"));
    _search_selection_check_box->setCheckable (true);
    if (edit_area)
      _search_selection_check_box->setEnabled (edit_area->hasSelectedText ());

    _edit_area = edit_area;
    connect (_find_next_button,   SIGNAL (clicked ()),
             this,                SLOT (find_next ()));
    connect (_find_prev_button,   SIGNAL (clicked ()),
             this,                SLOT (find_prev ()));
    connect (_more_button,        SIGNAL (toggled (bool)),
             _extension,          SLOT (setVisible (bool)));
    connect (_replace_button,     SIGNAL (clicked ()),
             this,                SLOT (replace ()));
    connect (_replace_all_button, SIGNAL (clicked ()),
             this,                SLOT (replace_all ()));
    connect (_backward_check_box, SIGNAL (stateChanged (int)),
             this,                SLOT (handle_backward_search_changed (int)));
    connect (_button_box,         SIGNAL (rejected ()),
             this,                SLOT (close ()));
    connect (_search_line_edit,   SIGNAL (textChanged (QString)),
             this,                SLOT (handle_search_text_changed (QString)));

    connect (_edit_area, SIGNAL (copyAvailable (bool)),
             this,       SLOT (handle_selection_changed (bool)));
    connect (_search_selection_check_box, SIGNAL (stateChanged (int)),
             this,                        SLOT (handle_sel_search_changed (int)));

    QVBoxLayout *extension_layout = new QVBoxLayout ();
    extension_layout->setMargin (0);
    extension_layout->addWidget (_whole_words_check_box);
    extension_layout->addWidget (_backward_check_box);
    extension_layout->addWidget (_search_selection_check_box);
    _extension->setLayout (extension_layout);

    QGridLayout *top_left_layout = new QGridLayout;
    top_left_layout->addWidget (_search_label, 1, 1);
    top_left_layout->addWidget (_search_line_edit, 1, 2);
    top_left_layout->addWidget (_replace_label, 2, 1);
    top_left_layout->addWidget (_replace_line_edit, 2, 2);

    QVBoxLayout *left_layout = new QVBoxLayout;
    left_layout->addLayout (top_left_layout);
    left_layout->insertStretch (1, 5);
    left_layout->addWidget (_case_check_box);
    left_layout->addWidget (_from_start_check_box);
    left_layout->addWidget (_wrap_check_box);
    left_layout->addWidget (_regex_check_box);

    QGridLayout *main_layout = new QGridLayout;
    main_layout->setSizeConstraint (QLayout::SetFixedSize);
    main_layout->addLayout (left_layout, 0, 0);
    main_layout->addWidget (_button_box, 0, 1);
    main_layout->addWidget (_extension, 1, 0);
    setLayout (main_layout);

    _extension->hide ();
    _find_next_button->setDefault (true);
    _find_result_available = false;
    _rep_all = 0;
    _rep_active = false;

    // set the actions
    addActions (find_actions);

    // move dialog to side of the parent if there is room on the desktop to do so.
    int xp = p->x () +20;
    int yp = p->y () + p->frameGeometry ().height () - sizeHint ().height () -20;

    if (yp < 0)
      yp = 0;

    move (xp, yp);

  }

  void find_dialog::save_data (find_dialog_data *fdlg_data)
  {
    fdlg_data->text = _search_line_edit->text ();
    fdlg_data->replace_text = _replace_line_edit->text ();
    fdlg_data->geometry = geometry ();
    fdlg_data->is_visible = isVisible ();
    fdlg_data->options = 0
          + _extension->isVisible () * FIND_DLG_MORE
          + _case_check_box->isChecked () * FIND_DLG_CASE
          + _from_start_check_box->isChecked () * FIND_DLG_START
          + _wrap_check_box->isChecked () * FIND_DLG_WRAP
          + _regex_check_box->isChecked () * FIND_DLG_REGX
          + _whole_words_check_box->isChecked () * FIND_DLG_WORDS
          + _backward_check_box->isChecked () * FIND_DLG_BACK
          + _search_selection_check_box->isChecked () * FIND_DLG_SEL;
  }

  void find_dialog::restore_data (const find_dialog_data* fdlg_data)
  {
    setGeometry (fdlg_data->geometry);
    setVisible (fdlg_data->is_visible);
    _search_line_edit->setText (fdlg_data->text);
    _replace_line_edit->setText (fdlg_data->replace_text);
    _extension->setVisible (FIND_DLG_MORE & fdlg_data->options);
    _case_check_box->setChecked (FIND_DLG_CASE & fdlg_data->options);
    _from_start_check_box->setChecked (FIND_DLG_START & fdlg_data->options);
    _wrap_check_box->setChecked (FIND_DLG_WRAP & fdlg_data->options);
    _regex_check_box->setChecked (FIND_DLG_REGX & fdlg_data->options);
    _whole_words_check_box->setChecked (FIND_DLG_WORDS & fdlg_data->options);
    _backward_check_box->setChecked (FIND_DLG_BACK & fdlg_data->options);
    _search_selection_check_box->setChecked (FIND_DLG_SEL & fdlg_data->options);
  }

  // set text of "search from start" depending on backward search
  void find_dialog::handle_backward_search_changed (int backward)
  {
    if (backward)
      _from_start_check_box->setText (tr ("Search from end"));
    else
      _from_start_check_box->setText (tr ("Search from start"));
  }

  // search text has changed: reset the search
  void find_dialog::handle_search_text_changed (QString)
  {
    if (_search_selection_check_box->isChecked ())
      _find_result_available = false;
  }

  void find_dialog::handle_sel_search_changed (int selected)
  {
    _from_start_check_box->setEnabled (! selected);
    _find_result_available = false;
  }

  void find_dialog::handle_selection_changed (bool has_selected)
  {
    if (_rep_active)
      return;

    _search_selection_check_box->setEnabled (has_selected);
    _find_result_available = false;
  }

  // initialize search text with selected text if this is in one single line
  void find_dialog::init_search_text (void)
  {
    if (_edit_area && _edit_area->hasSelectedText ())
      {
        int lbeg, lend, cbeg, cend;
        _edit_area->getSelection (&lbeg,&cbeg,&lend,&cend);
        if (lbeg == lend)
          _search_line_edit->setText (_edit_area->selectedText ());
      }

    // set focus to "Find what" and select all text
    _search_line_edit->setFocus ();
    _search_line_edit->selectAll ();

    // Default to "find" next time.
    // Otherwise, it defaults to the last action, which may be "replace all".
    _find_next_button->setDefault (true);
  }

  void find_dialog::find_next (void)
  {
    find (! _backward_check_box->isChecked ());
  }

  void find_dialog::find_prev (void)
  {
    find (_backward_check_box->isChecked ());
  }

  void find_dialog::find (bool forward)
  {
    if (! _edit_area)
      return;

    // line adn col: -1 means search starts at current position
    int line = -1, col = -1;

    bool do_wrap = _wrap_check_box->isChecked ();
    bool do_forward = forward;

    // Initialize the selection begin and end if it is the first search
    if (! _find_result_available)
      {
        if (_search_selection_check_box->isChecked ()
            && _edit_area->hasSelectedText ())
          {
            int l1, c1, l2, c2;
            _edit_area->getSelection (&l1, &c1, &l2, &c2);

            // Store the position of the selection
            m_sel_beg = _edit_area->positionFromLineIndex (l1, c1);
            m_sel_end = _edit_area->positionFromLineIndex (l2, c2);
            m_in_sel = true;
          }
        else
          m_in_sel = false;
      }

    // Get the correct line/col for beginning the search
    if (_rep_all)
      {
        // Replace All
        if (_rep_all == 1)
          {
            // Start at the beginning of file/sel if it is the first try
            if (m_in_sel)
              _edit_area->lineIndexFromPosition (m_sel_beg, &line, &col);
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
        if (_from_start_check_box->isChecked ()
            || (m_in_sel && (! _find_result_available)))
          {
            // From the beginning or the end of file/sel
            if (do_forward)
              {
                // From the beginning
                if (m_in_sel)
                  _edit_area->lineIndexFromPosition (m_sel_beg, &line, &col);
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
                  _edit_area->lineIndexFromPosition (m_sel_end, &line, &col);
                else
                  {
                    line = _edit_area->lines () - 1;
                    col  = _edit_area->text (line).length () - 1;
                    if (col == -1)
                      col = 0;
                  }
              }
          }
        else if (! do_forward)
          {
            // Start from where the cursor is. Fix QScintilla's cursor
            // positioning
            _edit_area->getCursorPosition (&line,&col);
            if (_find_result_available && _edit_area->hasSelectedText ())
              {
                int currpos = _edit_area->positionFromLineIndex (line,col);
                currpos -= (_search_line_edit->text ().length ());
                if (currpos < 0)
                  currpos = 0;
                _edit_area->lineIndexFromPosition (currpos, &line, &col);
              }
          }
      }

    // Do the search
    _find_result_available = _edit_area->findFirst (
                                  _search_line_edit->text (),
                                  _regex_check_box->isChecked (),
                                  _case_check_box->isChecked (),
                                  _whole_words_check_box->isChecked (),
                                  do_wrap,
                                  do_forward,
                                  line,col,
                                  true
#if defined (HAVE_QSCI_VERSION_2_6_0)
                                , true
#endif
                              );

    if (_find_result_available)
      {
        // Search successful: reset search-from-start box and check for
        // the current selection
        _from_start_check_box->setChecked (0);

        if (m_in_sel)
          {
            _edit_area->getCursorPosition (&line,&col);
            int pos = _edit_area->positionFromLineIndex (line, col);

            int l1, c1, l2, c2;
            _edit_area->lineIndexFromPosition (m_sel_beg, &l1, &c1);
            _edit_area->lineIndexFromPosition (m_sel_end, &l2, &c2);
            _edit_area->show_selection_markers (l1, c1, l2, c2);

            // Check if new start position is still wihtin the selection
            _find_result_available =  pos >= m_sel_beg && pos <= m_sel_end;
          }
      }

    // No more search hits
    if (! _find_result_available)
      {
        if (m_in_sel)
          {
            // Restore real selection and remove marker for selection
            int l1, c1, l2, c2;
            _edit_area->lineIndexFromPosition (m_sel_beg, &l1, &c1);
            _edit_area->lineIndexFromPosition (m_sel_end, &l2, &c2);
            _edit_area->setSelection (l1, c1, l2, c2);
            _edit_area->clear_selection_markers ();
          }

        // Display message if not replace all
        if (! _rep_all)
          no_matches_message ();
      }

  }

  void find_dialog::do_replace (void)
  {
    if (_edit_area)
      {
        _rep_active = true;  // changes in selection not made by the user

        _edit_area->replace (_replace_line_edit->text ());
        if (m_in_sel)
          {
            // Update the length of the selection
            m_sel_end = m_sel_end
                        - _search_line_edit->text ().toUtf8 ().size ()
                        + _replace_line_edit->text ().toUtf8 ().size ();
          }

        _rep_active = false;
      }
  }

  void find_dialog::replace (void)
  {
    if (_edit_area)
      {
        // Do the replace if we have selected text
        if (_find_result_available && _edit_area->hasSelectedText ())
          do_replace ();

        find_next ();
      }
  }

  void find_dialog::replace_all (void)
  {
    int line, col;

    if (_edit_area)
      {
        _edit_area->getCursorPosition (&line,&col);

        _rep_all = 1;
        find_next ();  // find first occurence (forward)

        _edit_area->beginUndoAction ();
        while (_find_result_available)   // while search string is found
          {
            do_replace ();
            _rep_all++;                                          // inc counter
            find_next ();                                        // find next
          }
        _edit_area->endUndoAction ();

        QMessageBox msg_box (QMessageBox::Information, tr ("Replace Result"),
                             tr ("%1 items replaced").arg (_rep_all-1),
                             QMessageBox::Ok, this);
        msg_box.exec ();

        _rep_all = 0;
        _find_result_available = false;

        if (! _search_selection_check_box->isChecked ())
          _edit_area->setCursorPosition (line,col);
      }
  }

  void find_dialog::no_matches_message (void)
  {
    QMessageBox msg_box (QMessageBox::Information, tr ("Find Result"),
                         tr ("No more matches found"), QMessageBox::Ok, this);
    msg_box.exec ();
  }
}

#endif
