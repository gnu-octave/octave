/*

Copyright (C) 2014 Torsten <ttl@justmail.de>

This file is part of Octave.

Octave is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 3 of the License, or (at your
option) any later version.

Octave is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with Octave; see the file COPYING.  If not, see
<http://www.gnu.org/licenses/>.

*/

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <QMessageBox>
#include <QDebug>
#include <QGridLayout>
#include <QVBoxLayout>
#include <QDialogButtonBox>
#include <QPushButton>
#include <QLineEdit>
#include <QCheckBox>
#include <QHeaderView>
#include <QAction>

#include "error.h"
#include "resource-manager.h"
#include "shortcut-manager.h"
#include "singleton-cleanup.h"

shortcut_manager *shortcut_manager::instance = 0;

shortcut_manager::shortcut_manager ()
{
  setObjectName ("Shortcut_Manager");

  _settings = resource_manager::get_settings ();
}

shortcut_manager::~shortcut_manager ()
{
}

bool
shortcut_manager::instance_ok (void)
{
  bool retval = true;

  if (! instance)
    {
      instance = new shortcut_manager ();

      if (instance)
        singleton_cleanup_list::add (cleanup_instance);
    }

  if (! instance)
    {
      ::error ("unable to create shortcut_manager object!");

      retval = false;
    }

  return retval;
}

void
shortcut_manager::do_init_data ()
{
  // actions of the main window
  init (tr ("New File"), "main_file:new_file", QKeySequence::New );
  init (tr ("New Function"), "main_file:new_function", QKeySequence ("Ctrl+Shift+N") );
  init (tr ("New Figure"), "main_file:new_figure", QKeySequence () );
  init (tr ("Open File"), "main_file:open_file", QKeySequence::Open );
  init (tr ("Load Workspace"), "main_file:load_workspace", QKeySequence () );
  init (tr ("Save Workspace As"), "main_file:save_workspace", QKeySequence () );
  init (tr ("Preferences"), "main_file:preferences", QKeySequence () );
  init (tr ("Exit Octave"), "main_file:exit", QKeySequence::Quit );

  init (tr ("Copy"), "main_edit:copy", QKeySequence::Copy);
  init (tr ("Paste"), "main_edit:paste", QKeySequence::Paste);
  init (tr ("Undo"), "main_edit:undo", QKeySequence::Undo);
  init (tr ("Select All"), "main_edit:select_all", QKeySequence () );
  init (tr ("Clear Clipboard"), "main_edit:clear_clipboard", QKeySequence () );
  init (tr ("Find in Files"), "main_edit:find_in_files", QKeySequence (Qt::ControlModifier + Qt::ShiftModifier + Qt::Key_F) );
  init (tr ("Clear Command Window"), "main_edit:clear_command_window", QKeySequence () );
  init (tr ("Clear Command History"), "main_edit:clear_history", QKeySequence () );
  init (tr ("Clear Workspace"), "main_edit:clear_workspace", QKeySequence () );

  // actions of the editor
  init (tr ("Edit Function"), "editor_file:edit_function", QKeySequence (Qt::ControlModifier + Qt::Key_E) );
  init (tr ("Save File"), "editor_file:save", QKeySequence::Save );
  init (tr ("Save File As"), "editor_file:save_as", QKeySequence::SaveAs );
  init (tr ("Close"), "editor_file:close", QKeySequence::Close );
  init (tr ("Close All"), "editor_file:close_all", QKeySequence () );
  init (tr ("Close Other"), "editor_file:close_other",  QKeySequence () );
  init (tr ("Print"), "editor_file:print",  QKeySequence::Print );

  init (tr ("Undo"), "editor_edit:undo",  QKeySequence::Undo );
  init (tr ("Redo"), "editor_edit:redo",  QKeySequence::Redo );
  init (tr ("Copy"), "editor_edit:copy",  QKeySequence::Copy );
  init (tr ("Cuy"), "editor_edit:cut",  QKeySequence::Cut );
  init (tr ("Paste"), "editor_edit:paste",  QKeySequence::Paste );
  init (tr ("Select All"), "editor_edit:select_all",  QKeySequence::SelectAll );
  init (tr ("Find and Replace"), "editor_edit:find_replace",  QKeySequence::Find );

  init (tr ("Delete to Start of Word"), "editor_edit:delete_start_word",  QKeySequence::DeleteStartOfWord );
  init (tr ("Delete to End of Word"), "editor_edit:delete_end_word",  QKeySequence::DeleteEndOfWord );
  init (tr ("Delete to Start of Line"), "editor_edit:delete_start_line",  QKeySequence (Qt::ControlModifier + Qt::SHIFT + Qt::Key_Backspace) );
  init (tr ("Delete to End of Line"), "editor_edit:delete_end_line",  QKeySequence (Qt::ControlModifier + Qt::SHIFT + Qt::Key_Delete) );
  init (tr ("Delete Line"), "editor_edit:delete_line",  QKeySequence (Qt::ControlModifier + Qt::SHIFT + Qt::Key_L) );
  init (tr ("Copy Line"), "editor_edit:copy_line",  QKeySequence (Qt::ControlModifier + Qt::SHIFT + Qt::Key_C) );
  init (tr ("Cut Line"), "editor_edit:cut_line",  QKeySequence (Qt::ControlModifier + Qt::SHIFT + Qt::Key_X) );
  init (tr ("Duplicate Selection/Line"), "editor_edit:duplicate_selection",  QKeySequence (Qt::ControlModifier + Qt::Key_D) );
  init (tr ("Transpose Line"), "editor_edit:transpose_line",  QKeySequence (Qt::ControlModifier + Qt::Key_T) );

  init (tr ("Comment Selection"), "editor_edit:comment_selection",  QKeySequence (Qt::ControlModifier + Qt::Key_R) );
  init (tr ("Uncomment Selection"), "editor_edit:uncomment_selection",  QKeySequence (Qt::SHIFT + Qt::ControlModifier + Qt::Key_R) );
  init (tr ("Uppercase Selection"), "editor_edit:upper_case",  QKeySequence (Qt::ControlModifier + Qt::Key_U) );
  init (tr ("Lowercase Selection"), "editor_edit:lower_case",  QKeySequence (Qt::ControlModifier + Qt::AltModifier + Qt::Key_U) );
  init (tr ("Indent Selection"), "editor_edit:indent_selection",  QKeySequence (Qt::ControlModifier + Qt::Key_Tab) );
  init (tr ("Unindent Selection"), "editor_edit:unindent_selection",  QKeySequence (Qt::SHIFT + Qt::ControlModifier + Qt::Key_Tab) );

  init (tr ("Completion List"), "editor_edit:completion_list",  QKeySequence (Qt::ControlModifier + Qt::Key_Space) );
  init (tr ("Toggle Bookmark"), "editor_edit:toggle_bookmark",  QKeySequence (Qt::Key_F7) );
  init (tr ("Next Bookmark"), "editor_edit:next_bookmark",  QKeySequence (Qt::Key_F2) );
  init (tr ("Previous Bookmark"), "editor_edit:previous_bookmark",  QKeySequence (Qt::SHIFT + Qt::Key_F2) );
  init (tr ("Remove All Bookmark"), "editor_edit:remove_bookmark",  QKeySequence () );
  init (tr ("Goto Line"), "editor_edit:goto_line",  QKeySequence (Qt::ControlModifier+ Qt::Key_G) );
  init (tr ("Preferences"), "editor_edit:preferences",  QKeySequence () );
  init (tr ("Styles Preferences"), "editor_edit:styles_preferences",  QKeySequence () );
}

void
shortcut_manager::init (QString description, QString key, QKeySequence def_sc)
{
  QKeySequence actual = QKeySequence (_settings->value ("shortcuts/"+key, def_sc).toString ());

  // append the new shortcut to the list
  shortcut_t shortcut_info;
  shortcut_info.description = description;
  shortcut_info.settings_key = key;
  shortcut_info.actual_sc = actual;
  shortcut_info.default_sc = def_sc;
  _sc << shortcut_info;

  // insert shortcut prepended by widget in order check for duplicates later
  QString widget = key.section ('_',0,0);  // get widget that uses the shortcut
  if (! actual.isEmpty ())
    _shortcut_hash[widget + ":" + actual.toString ()] = _sc.count ();  // offset of 1 to avoid 0
  _action_hash[key] = _sc.count ();  // offset of 1 to avoid 0
}

void
shortcut_manager::do_fill_treewidget (QTreeWidget *tree_view)
{
  _dialog = 0;
  _level_hash.clear ();

  tree_view->header ()->setResizeMode (QHeaderView::ResizeToContents);

  QTreeWidgetItem *main = new QTreeWidgetItem (tree_view);
  main->setText (0, tr ("Main"));
  main->setExpanded (true);
  QTreeWidgetItem *main_file = new QTreeWidgetItem (main);
  main_file->setText (0, tr ("File"));
  QTreeWidgetItem *main_edit = new QTreeWidgetItem (main);
  main_edit->setText (0, tr ("Edit"));
  QTreeWidgetItem *main_debug = new QTreeWidgetItem (main);
  main_debug->setText (0, tr ("Debug"));
  QTreeWidgetItem *main_window = new QTreeWidgetItem (main);
  main_window->setText (0, tr ("Window"));
  QTreeWidgetItem *main_help = new QTreeWidgetItem (main);
  main_help->setText (0, tr ("Help"));
  QTreeWidgetItem *main_news = new QTreeWidgetItem (main);
  main_news->setText (0, tr ("News"));

  _level_hash["main_file"]   = main_file;
  _level_hash["main_edit"]   = main_edit;
  _level_hash["main_debug"]   = main_debug;
  _level_hash["main_window"]   = main_window;
  _level_hash["main_help"]   = main_help;
  _level_hash["main_news"]   = main_news;

  QTreeWidgetItem *editor = new QTreeWidgetItem (tree_view);
  editor->setText (0, tr ("Editor"));
  editor->setExpanded (true);
  QTreeWidgetItem *editor_file = new QTreeWidgetItem (editor);
  editor_file->setText (0, tr ("File"));
  QTreeWidgetItem *editor_edit = new QTreeWidgetItem (editor);
  editor_edit->setText (0, tr ("Edit"));
  QTreeWidgetItem *editor_view = new QTreeWidgetItem (editor);
  editor_view->setText (0, tr ("View"));
  QTreeWidgetItem *editor_debug = new QTreeWidgetItem (editor);
  editor_debug->setText (0, tr ("Debug"));
  QTreeWidgetItem *editor_run = new QTreeWidgetItem (editor);
  editor_run->setText (0, tr ("Run"));
  QTreeWidgetItem *editor_help = new QTreeWidgetItem (editor);
  editor_help->setText (0, tr ("Help"));

  _level_hash["editor_file"] = editor_file;
  _level_hash["editor_edit"] = editor_edit;
  _level_hash["editor_view"] = editor_view;
  _level_hash["editor_debug"] = editor_debug;
  _level_hash["editor_run"] = editor_run;
  _level_hash["editor_help"] = editor_help;

  connect (tree_view, SIGNAL (itemDoubleClicked (QTreeWidgetItem*, int)),
           this, SLOT (handle_double_clicked (QTreeWidgetItem*, int)));

  for (int i = 0; i < _sc.count (); i++)
    {
      shortcut_t sc = _sc.at (i);

      QTreeWidgetItem* section = _level_hash[sc.settings_key.section(':',0,0)];
      QTreeWidgetItem* tree_item = new QTreeWidgetItem (section);

      tree_item->setText (0, sc.description);
      tree_item->setText (1, sc.default_sc);
      tree_item->setText (2, sc.actual_sc);

      _item_index_hash[tree_item] = i + 1; // index+1 to avoid 0
      _index_item_hash[i] = tree_item;
    }

}

void
shortcut_manager::do_write_shortcuts ()
{
  for (int i = 0; i < _sc.count (); i++)
    _settings->setValue("shortcuts/"+_sc.at (i).settings_key, _sc.at (i).actual_sc.toString ());

  _settings->sync ();

  delete _dialog;
}

void
shortcut_manager::do_set_shortcut (QAction* action, const QString& key)
{
  int index = _action_hash[key] - 1;

  if (index > -1 && index < _sc.count ())
    action->setShortcut ( QKeySequence (
      _settings->value ("shortcuts/" + key, _sc.at (index).default_sc).toString ()));
  else
    qDebug () << "Key: " << key << " not found in _action_hash";
}

void
shortcut_manager::handle_double_clicked (QTreeWidgetItem* item, int)
{
  int i = _item_index_hash[item];
  if (i == 0)
    return;  // top-level-item clicked

  shortcut_dialog (i-1); // correct to index starting at 0
}

void
shortcut_manager::shortcut_dialog (int index)
{
  if (! _dialog)
    {
      _dialog = new QDialog (this);

      _dialog->setWindowTitle (tr ("Enter new Shortcut"));

      QVBoxLayout *box = new QVBoxLayout(_dialog);

      QLabel *help = new QLabel (tr ("Apply the desired shortcut or click "
                                     "on the right button to reset the "
                                     "shortcut to its default."));
      help->setWordWrap (true);
      box->addWidget (help);

      QCheckBox *direct = new QCheckBox (tr ("Enter shortcut directly by performing it"));
      direct->setCheckState (Qt::Checked);
      box->addWidget (direct);

      QGridLayout *grid = new QGridLayout();

      QLabel *actual = new QLabel (tr ("Actual shortcut"));
      _edit_actual = new enter_shortcut (_dialog);
      _edit_actual->setAlignment (Qt::AlignHCenter);
      grid->addWidget (actual, 0, 0);
      grid->addWidget (_edit_actual, 0, 1);

      QLabel *def = new QLabel (tr ("Default shortcut"));
      _label_default = new QLabel (_dialog);
      _label_default->setAlignment (Qt::AlignHCenter);
      grid->addWidget (def, 1, 0);
      grid->addWidget (_label_default, 1, 1);

      QPushButton *set_default = new QPushButton (tr ("Set to default"));
      grid->addWidget (set_default, 0, 2);
      connect (set_default, SIGNAL (clicked ()),
               this, SLOT (shortcut_dialog_set_default ()));

      box->addLayout (grid);

      QDialogButtonBox *button_box = new QDialogButtonBox (QDialogButtonBox::Ok
                                                   | QDialogButtonBox::Cancel);
      QList<QAbstractButton *> buttons = button_box->buttons ();
      for (int i = 0; i < buttons.count (); i++)
        buttons.at (i)->setShortcut (QKeySequence ());
      connect(button_box, SIGNAL (accepted ()), _dialog, SLOT (accept ()));
      connect(button_box, SIGNAL (rejected ()), _dialog, SLOT (reject ()));
      box->addWidget (button_box);

      _dialog->setLayout (box);

      connect (direct, SIGNAL (stateChanged (int)),
               _edit_actual, SLOT (handle_direct_shortcut (int)));
      connect (_dialog, SIGNAL (finished (int)),
               this, SLOT (shortcut_dialog_finished (int)));

    }

  _edit_actual->setText (_sc.at (index).actual_sc);
  _label_default->setText (_sc.at (index).default_sc);
  _handled_index = index;

  _edit_actual->setFocus ();
  _dialog->setFocusProxy (_edit_actual);
  _dialog->exec ();
}

void
shortcut_manager::shortcut_dialog_finished (int result)
{
  if (result == QDialog::Rejected)
    return;

  // check for duplicate

  // get the widget for which this shortcut is defined
  QString widget = _sc.at (_handled_index).settings_key.section ('_',0,0);
  // and look
  int double_index = _shortcut_hash[widget + ":" + _edit_actual->text()] - 1;

  if (double_index >= 0 && double_index != _handled_index)
    {
      int ret = QMessageBox::warning(this, tr("Double Shortcut"),
                  tr ("The chosen shortcut\n  \"%1\"\n"
                      "is already used for the action\n  \"%2\".\n"
                      "Do you want to use the shortcut anyhow removing it "
                      "from the previous action?")
                     .arg (_edit_actual->text())
                     .arg (_sc.at (double_index).description),
                  QMessageBox::Yes | QMessageBox::No, QMessageBox::Yes);

      if (ret == QMessageBox::Yes)
        {
          shortcut_t double_shortcut = _sc.at (double_index);
          double_shortcut.actual_sc = QKeySequence ();
          _sc.replace (double_index, double_shortcut);
          _index_item_hash[double_index]->setText (2, QKeySequence ());
        }
      else
        return;
    }

  shortcut_t shortcut = _sc.at (_handled_index);
  if (! shortcut.actual_sc.isEmpty ())
    _shortcut_hash.remove (widget + ":" + shortcut.actual_sc.toString ());
  shortcut.actual_sc = _edit_actual->text();
  _sc.replace (_handled_index, shortcut);

  _index_item_hash[_handled_index]->setText (2, shortcut.actual_sc);

  if (! shortcut.actual_sc.isEmpty ())
    _shortcut_hash[widget + ":" + shortcut.actual_sc.toString ()] = _handled_index + 1;
}

void
shortcut_manager::shortcut_dialog_set_default ()
{
  _edit_actual->setText (_label_default->text ());
}



enter_shortcut::enter_shortcut (QWidget *p) : QLineEdit (p)
{
  _direct_shortcut = true;
}

enter_shortcut::~enter_shortcut ()
{
}

void
enter_shortcut::handle_direct_shortcut (int state)
{
  if (state)
    _direct_shortcut = true;
  else
    _direct_shortcut = false;
}

void
enter_shortcut::keyPressEvent (QKeyEvent *e)
{
  if (! _direct_shortcut)
    {
      QLineEdit::keyPressEvent (e);
      return;
    }

  if (e->type () == QEvent::KeyPress)
    {
      int key = e->key ();

      if (key == Qt::Key_unknown || key == 0)
        return;

      Qt::KeyboardModifiers modifiers = e->modifiers ();

      if(modifiers & Qt::ShiftModifier)
        key += Qt::SHIFT;
      if(modifiers & Qt::ControlModifier)
        key += Qt::CTRL;
      if(modifiers & Qt::AltModifier)
        key += Qt::ALT;
      if(modifiers & Qt::MetaModifier)
        key += Qt::META;

      setText (QKeySequence(key));
    }
}

