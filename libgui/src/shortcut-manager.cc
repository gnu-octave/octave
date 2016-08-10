/*

Copyright (C) 2014-2015 Torsten <ttl@justmail.de>

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

#if defined (HAVE_CONFIG_H)
#  include "config.h"
#endif

#include <QtCore>
#include <QMessageBox>
#include <QDebug>
#include <QGridLayout>
#include <QVBoxLayout>
#include <QDialogButtonBox>
#include <QKeySequence>
#include <QPushButton>
#include <QLineEdit>
#include <QCheckBox>
#include <QHeaderView>
#include <QAction>
#include <QFileDialog>

#include "error.h"
#include "resource-manager.h"
#include "shortcut-manager.h"

shortcut_manager *shortcut_manager::instance = 0;

shortcut_manager::shortcut_manager ()
{
  setObjectName ("Shortcut_Manager");

  // Mac: don't let Qt interpret CMD key ("Meta" in Qt terminology) as Ctrl
#if defined (Q_OS_MAC)
  QCoreApplication::setAttribute (Qt::AA_MacDontSwapCtrlAndMeta, true);
#endif

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
      instance = new shortcut_manager ();

  if (! instance)
    {
      error ("unable to create shortcut_manager object!");

      retval = false;
    }

  return retval;
}

void
shortcut_manager::do_init_data ()
{
  Qt::KeyboardModifier ctrl;
  int prefix;
#if defined (Q_OS_MAC)
  // Use CMD key as an equivalent of Ctrl key on other platforms
  ctrl = Qt::MetaModifier;
  // Some of octave default shortcuts on windows/linux are already defined
  // as system wide shortcuts on Mac Os X (almost all Function keys).
  // Prefix those with Option (Alt) modifier to avoid conflicts.
  prefix = Qt::AltModifier;
#else
  ctrl = Qt::ControlModifier;
  prefix = Qt::NoModifier;
#endif

  Qt::KeyboardModifiers ctrl_shift = ctrl | Qt::ShiftModifier;
  Qt::KeyboardModifiers ctrl_alt = ctrl | Qt::AltModifier;

  // actions of the main window

  _settings->setValue ("shortcuts/main_ctrld",false); // reset use fo ctrl-d

  // file
  init (tr ("New File"), "main_file:new_file", QKeySequence::New);
  init (tr ("New Function"), "main_file:new_function",
        QKeySequence (ctrl_shift + Qt::Key_N));
  init (tr ("New Figure"), "main_file:new_figure", QKeySequence ());
  init (tr ("Open File"), "main_file:open_file", QKeySequence::Open);
  init (tr ("Load Workspace"), "main_file:load_workspace", QKeySequence ());
  init (tr ("Save Workspace As"), "main_file:save_workspace", QKeySequence ());
  init (tr ("Exit Octave"), "main_file:exit", QKeySequence::Quit);

  // edit
  init (tr ("Copy"), "main_edit:copy", QKeySequence::Copy);
  init (tr ("Paste"), "main_edit:paste", QKeySequence::Paste);
  init (tr ("Undo"), "main_edit:undo", QKeySequence::Undo);
  init (tr ("Select All"), "main_edit:select_all", QKeySequence ());
  init (tr ("Clear Clipboard"), "main_edit:clear_clipboard", QKeySequence ());
  init (tr ("Find in Files"), "main_edit:find_in_files",
        QKeySequence (ctrl_shift + Qt::Key_F));
  init (tr ("Clear Command Window"), "main_edit:clear_command_window",
        QKeySequence ());
  init (tr ("Clear Command History"), "main_edit:clear_history",
        QKeySequence ());
  init (tr ("Clear Workspace"), "main_edit:clear_workspace", QKeySequence ());
  init (tr ("Preferences"), "main_edit:preferences", QKeySequence ());

  // debug
  init (tr ("Step"), "main_debug:step_over",
        QKeySequence (prefix + Qt::Key_F10));
  init (tr ("Step Into"), "main_debug:step_into",
        QKeySequence (prefix + Qt::Key_F11));
  init (tr ("Step Out"), "main_debug:step_out",
        QKeySequence (prefix + Qt::ShiftModifier + Qt::Key_F11));
  init (tr ("Continue"), "main_debug:continue",
        QKeySequence (prefix + Qt::Key_F5));
  init (tr ("Quit Debug Mode"), "main_debug:quit",
        QKeySequence (prefix + Qt::ShiftModifier + Qt::Key_F5));

  // window
  init (tr ("Show Command Window"), "main_window:show_command",
        prefix + ctrl_shift + Qt::Key_0);
  init (tr ("Show Command History"), "main_window:show_history",
        prefix + ctrl_shift + Qt::Key_1);
  init (tr ("Show File Browser"), "main_window:show_file_browser",
        prefix + ctrl_shift + Qt::Key_2);
  init (tr ("Show Workspace"), "main_window:show_workspace",
        prefix + ctrl_shift + Qt::Key_3);
  init (tr ("Show Editor"), "main_window:show_editor",
        prefix + ctrl_shift + Qt::Key_4);
  init (tr ("Show Documentation"), "main_window:show_doc",
        prefix + ctrl_shift + Qt::Key_5);
  init (tr ("Command Window"), "main_window:command",
        prefix + ctrl + Qt::Key_0);
  init (tr ("Command History"), "main_window:history",
        prefix + ctrl + Qt::Key_1);
  init (tr ("File Browser"), "main_window:file_browser",
        prefix + ctrl + Qt::Key_2);
  init (tr ("Workspace"), "main_window:workspace",
        prefix + ctrl + Qt::Key_3);
  init (tr ("Editor"), "main_window:editor",
        prefix + ctrl + Qt::Key_4);
  init (tr ("Documentation"), "main_window:doc",
        prefix + ctrl + Qt::Key_5);
  init (tr ("Reset Default Window Layout"), "main_window:reset", QKeySequence ());

  // help
  init (tr ("Show Ondisk Documentation"), "main_help:ondisk_doc",
        QKeySequence ());
  init (tr ("Show Online Documentation"), "main_help:online_doc",
        QKeySequence ());
  init (tr ("Report Bug"), "main_help:report_bug", QKeySequence ());
  init (tr ("Octave Packages"), "main_help:packages", QKeySequence ());
  init (tr ("Share Code"), "main_help:agora", QKeySequence ());
  init (tr ("Contribute to Octave"), "main_help:contribute", QKeySequence ());
  init (tr ("Octave Developer Resources"), "main_help:developer",
        QKeySequence ());
  init (tr ("About Octave"), "main_help:about", QKeySequence ());

  // news
  init (tr ("Release Notes"), "main_news:release_notes", QKeySequence ());
  init (tr ("Community News"), "main_news:community_news", QKeySequence ());

  // actions of the editor

  // file
  init (tr ("Edit Function"), "editor_file:edit_function",
        QKeySequence (ctrl + Qt::Key_E));
  init (tr ("Save File"), "editor_file:save", QKeySequence::Save);
  init (tr ("Save File As"), "editor_file:save_as", QKeySequence::SaveAs);
  init (tr ("Close"), "editor_file:close", QKeySequence::Close);
  init (tr ("Close All"), "editor_file:close_all", QKeySequence ());
  init (tr ("Close Other Files"), "editor_file:close_other", QKeySequence ());
  init (tr ("Print"), "editor_file:print", QKeySequence::Print);

  // edit
  init (tr ("Redo"), "editor_edit:redo", QKeySequence::Redo);
  init (tr ("Cut"), "editor_edit:cut", QKeySequence::Cut);
  init (tr ("Find and Replace"), "editor_edit:find_replace",
        QKeySequence::Find);
  init (tr ("Find Next"), "editor_edit:find_next",
        QKeySequence::FindNext);
  init (tr ("Find Previous"), "editor_edit:find_previous",
        QKeySequence::FindPrevious);
  init (tr ("Delete to Start of Word"), "editor_edit:delete_start_word",
        QKeySequence::DeleteStartOfWord);
  init (tr ("Delete to End of Word"), "editor_edit:delete_end_word",
        QKeySequence::DeleteEndOfWord);
  init (tr ("Delete to Start of Line"), "editor_edit:delete_start_line",
        QKeySequence (ctrl_shift + Qt::Key_Backspace));
  init (tr ("Delete to End of Line"), "editor_edit:delete_end_line",
        QKeySequence (ctrl_shift + Qt::Key_Delete));
  init (tr ("Delete Line"), "editor_edit:delete_line",
        QKeySequence (ctrl_shift + Qt::Key_L));
  init (tr ("Copy Line"), "editor_edit:copy_line",
        QKeySequence (ctrl_shift + Qt::Key_C));
  init (tr ("Cut Line"), "editor_edit:cut_line",
        QKeySequence (ctrl_shift + Qt::Key_X));
  init (tr ("Duplicate Selection/Line"), "editor_edit:duplicate_selection",
        QKeySequence (ctrl + Qt::Key_D));
  init (tr ("Transpose Line"), "editor_edit:transpose_line",
        QKeySequence (ctrl + Qt::Key_T));
  init (tr ("Show Completion List"), "editor_edit:completion_list",
        QKeySequence (ctrl + Qt::Key_Space));

  init (tr ("Comment Selection"), "editor_edit:comment_selection",
        QKeySequence (ctrl + Qt::Key_R));
  init (tr ("Uncomment Selection"), "editor_edit:uncomment_selection",
        QKeySequence (ctrl_shift + Qt::Key_R));
  init (tr ("Uppercase Selection"), "editor_edit:upper_case",
        QKeySequence (ctrl + Qt::Key_U));
  init (tr ("Lowercase Selection"), "editor_edit:lower_case",
        QKeySequence (ctrl_alt + Qt::Key_U));

#if defined (Q_OS_MAC)
  init (tr ("Indent Selection"), "editor_edit:indent_selection",
        QKeySequence (prefix + Qt::Key_Tab));
  init (tr ("Unindent Selection"), "editor_edit:unindent_selection",
        QKeySequence (prefix + Qt::ShiftModifier + Qt::Key_Tab));
#else
  init (tr ("Indent Selection"), "editor_edit:indent_selection",
        QKeySequence (ctrl + Qt::Key_Tab));
  init (tr ("Unindent Selection"), "editor_edit:unindent_selection",
        QKeySequence (ctrl_shift + Qt::Key_Tab));
#endif

  init (tr ("Convert Line Endings to Windows"), "editor_edit:conv_eol_winows",
        QKeySequence ());
  init (tr ("Convert Line Endings to Unix"), "editor_edit:conv_eol_unix",
        QKeySequence ());
  init (tr ("Convert Line Endings to Mac"), "editor_edit:conv_eol_mac",
        QKeySequence ());

  init (tr ("Goto Line"), "editor_edit:goto_line",
        QKeySequence (ctrl + Qt::Key_L));
  init (tr ("Move to Matching Brace"), "editor_edit:move_to_brace",
        QKeySequence (ctrl + Qt::Key_M));
  init (tr ("Select to Matching Brace"), "editor_edit:select_to_brace",
        QKeySequence (ctrl_shift + Qt::Key_M));
  init (tr ("Toggle Bookmark"), "editor_edit:toggle_bookmark",
        QKeySequence (prefix + Qt::Key_F7));
  init (tr ("Next Bookmark"), "editor_edit:next_bookmark",
        QKeySequence (prefix + Qt::Key_F2));
  init (tr ("Previous Bookmark"), "editor_edit:previous_bookmark",
        QKeySequence (prefix + Qt::SHIFT + Qt::Key_F2));
  init (tr ("Remove All Bookmark"), "editor_edit:remove_bookmark",
        QKeySequence ());

  init (tr ("Preferences"), "editor_edit:preferences", QKeySequence ());
  init (tr ("Styles Preferences"), "editor_edit:styles_preferences",
        QKeySequence ());

  // view
  init (tr ("Show Line Numbers"), "editor_view:show_line_numbers",
        QKeySequence ());
  init (tr ("Show Whitespace Characters"), "editor_view:show_white_spaces",
        QKeySequence ());
  init (tr ("Show Line Endings"), "editor_view:show_eol_chars", QKeySequence ());
  init (tr ("Show Indentation Guides"), "editor_view:show_ind_guides",
        QKeySequence ());
  init (tr ("Show Long Line Marker"), "editor_view:show_long_line",
        QKeySequence ());
  init (tr ("Show Toolbar"), "editor_view:show_toolbar",
        QKeySequence ());
  init (tr ("Show Statusbar"), "editor_view:show_statusbar",
        QKeySequence ());
  init (tr ("Show Horizontal Scrollbar"), "editor_view:show_hscrollbar",
        QKeySequence ());
  init (tr ("Zoom In"), "editor_view:zoom_in", QKeySequence::ZoomIn);
  init (tr ("Zoom Out"), "editor_view:zoom_out", QKeySequence::ZoomOut);
#if defined (Q_OS_MAC)
  init (tr ("Zoom Normal"), "editor_view:zoom_normal",
        QKeySequence (ctrl + Qt::Key_Underscore));
#else
  init (tr ("Zoom Normal"), "editor_view:zoom_normal",
        QKeySequence (ctrl + Qt::Key_Period));
#endif

  // debug
  init (tr ("Toggle Breakpoint"), "editor_debug:toggle_breakpoint",
        QKeySequence ());
  init (tr ("Next Breakpoint"), "editor_debug:next_breakpoint",
        QKeySequence ());
  init (tr ("Previous Breakpoint"), "editor_debug:previous_breakpoint",
        QKeySequence ());
  init (tr ("Remove All Breakpoints"), "editor_debug:remove_breakpoints",
        QKeySequence ());

  // run
  init (tr ("Run File"), "editor_run:run_file",
        QKeySequence (prefix + Qt::Key_F5) );
  init (tr ("Run Selection"), "editor_run:run_selection",
        QKeySequence (prefix + Qt::Key_F9) );

  // help
  init (tr ("Help on Keyword"), "editor_help:help_keyword",
        QKeySequence::HelpContents);
  init (tr ("Document on Keyword"), "editor_help:doc_keyword",
        QKeySequence (Qt::SHIFT + Qt::Key_F1));

  // tab navigation
  init (tr ("Switch to Left Tab"), "editor_tabs:switch_left_tab",
        QKeySequence (ctrl + Qt::Key_PageUp));
  init (tr ("Switch to Right Tab"), "editor_tabs:switch_right_tab",
        QKeySequence (ctrl + Qt::Key_PageDown));
  init (tr ("Move Tab Left"), "editor_tabs:move_tab_left",
        QKeySequence (Qt::AltModifier + Qt::Key_PageUp));
  init (tr ("Move Tab Right"), "editor_tabs:move_tab_right",
        QKeySequence (Qt::AltModifier + Qt::Key_PageDown));

}

void
shortcut_manager::init (QString description, QString key, QKeySequence def_sc)
{
  QKeySequence actual
    = QKeySequence (_settings->value ("shortcuts/"+key, def_sc).toString ());

  // append the new shortcut to the list
  shortcut_t shortcut_info;
  shortcut_info.description = description;
  shortcut_info.settings_key = key;
  shortcut_info.actual_sc = actual;
  shortcut_info.default_sc = def_sc;
  _sc << shortcut_info;

  // insert shortcut in order check for duplicates later
  if (! actual.isEmpty ())
    _shortcut_hash[actual.toString ()] = _sc.count ();
  _action_hash[key] = _sc.count ();

  // check whether ctrl+d is used from main window, i.e. is a global shortcut
  if (key.startsWith ("main_")
      && actual == QKeySequence (Qt::ControlModifier+Qt::Key_D))
    _settings->setValue ("shortcuts/main_ctrld",true);
}

void
shortcut_manager::do_fill_treewidget (QTreeWidget *tree_view)
{
  _dialog = 0;
  _level_hash.clear ();

#if defined (HAVE_QT4)
  tree_view->header ()->setResizeMode (QHeaderView::ResizeToContents);
#else
  tree_view->header ()->setSectionResizeMode (QHeaderView::ResizeToContents);
#endif

  QTreeWidgetItem *main = new QTreeWidgetItem (tree_view);
  main->setText (0, tr ("Global"));
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
  QTreeWidgetItem *editor_tabs = new QTreeWidgetItem (editor);
  editor_tabs->setText (0, tr ("Tabs"));

  _level_hash["editor_file"] = editor_file;
  _level_hash["editor_edit"] = editor_edit;
  _level_hash["editor_view"] = editor_view;
  _level_hash["editor_debug"] = editor_debug;
  _level_hash["editor_run"] = editor_run;
  _level_hash["editor_help"] = editor_help;
  _level_hash["editor_tabs"] = editor_tabs;

  connect (tree_view, SIGNAL (itemDoubleClicked (QTreeWidgetItem*, int)),
           this, SLOT (handle_double_clicked (QTreeWidgetItem*, int)));

  for (int i = 0; i < _sc.count (); i++)
    {
      shortcut_t sc = _sc.at (i);

      QTreeWidgetItem* section = _level_hash[sc.settings_key.section(':',0,0)];
      QTreeWidgetItem* tree_item = new QTreeWidgetItem (section);

      // set a slightly transparent foreground for default columns
      QColor fg = QColor (tree_item->foreground (1).color ());
      fg.setAlpha (128);
      tree_item->setForeground (1, QBrush (fg));

      // write the shortcuts
      tree_item->setText (0, sc.description);
      tree_item->setText (1, sc.default_sc.toString ());
      tree_item->setText (2, sc.actual_sc.toString ());

      _item_index_hash[tree_item] = i + 1; // index+1 to avoid 0
      _index_item_hash[i] = tree_item;
    }

}

// write one or all actual shortcut set(s) into a settings file
void
shortcut_manager::do_write_shortcuts (QSettings* settings,
                                      bool closing)
{
  bool sc_ctrld = false;

  for (int i = 0; i < _sc.count (); i++)  // loop over all shortcuts
    {
      settings->setValue("shortcuts/"+_sc.at (i).settings_key,
                             _sc.at (i).actual_sc.toString ());
      // special: check main-window for Ctrl-D (Terminal)
      if (_sc.at (i).settings_key.startsWith ("main_")
          && _sc.at (i).actual_sc == QKeySequence (Qt::ControlModifier+Qt::Key_D))
        sc_ctrld = true;
    }

    settings->setValue ("shortcuts/main_ctrld",sc_ctrld);

  if (closing)
    {
      delete _dialog;  // the dialog for key sequences can be removed now
      _dialog = 0;     // make sure it is zero again
    }

  settings->sync ();    // sync the settings file
}

void
shortcut_manager::do_set_shortcut (QAction* action, const QString& key)
{
  int index;

  index = _action_hash[key] - 1;

  if (index > -1 && index < _sc.count ())
    action->setShortcut (QKeySequence (
      _settings->value ("shortcuts/" + key, _sc.at (index).default_sc).toString ()));
  else
    qDebug () << "Key: " << key << " not found in _action_hash";
}

void
shortcut_manager::handle_double_clicked (QTreeWidgetItem* item, int col)
{
  if (col != 2)
    return;

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

      QCheckBox *direct = new QCheckBox (
        tr ("Enter shortcut directly by performing it"));
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

  _edit_actual->setText (_sc.at (index).actual_sc.toString ());
  _label_default->setText (_sc.at (index).default_sc.toString ());
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
  int double_index = _shortcut_hash[_edit_actual->text()] - 1;

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
          _index_item_hash[double_index]->setText (2, QString ());
        }
      else
        return;
    }

  shortcut_t shortcut = _sc.at (_handled_index);
  if (! shortcut.actual_sc.isEmpty ())
    _shortcut_hash.remove (shortcut.actual_sc.toString ());
  shortcut.actual_sc = _edit_actual->text();
  _sc.replace (_handled_index, shortcut);

  _index_item_hash[_handled_index]->setText (2, shortcut.actual_sc.toString ());

  if (! shortcut.actual_sc.isEmpty ())
    _shortcut_hash[shortcut.actual_sc.toString ()] = _handled_index + 1;
}

void
shortcut_manager::shortcut_dialog_set_default ()
{
  _edit_actual->setText (_label_default->text ());
}

// import a shortcut set from a given settings file or reset to
// the defaults (settings = 0) and refresh the tree view
void
shortcut_manager::import_shortcuts (QSettings *settings)
{
  for (int i = 0; i < _sc.count (); i++)
    {
      // update the list of all shortcuts
      shortcut_t sc = _sc.at (i);           // make a copy

      if (settings)
        sc.actual_sc = QKeySequence (         // get new shortcut from settings
          settings->value ("shortcuts/"+sc.settings_key,sc.actual_sc).
                          toString ());       // and use the old one as default
      else
        sc.actual_sc = QKeySequence (sc.default_sc); // get default shortcut

      _sc.replace (i,sc);                   // replace the old with the new one

      // update the tree view
      QTreeWidgetItem* tree_item = _index_item_hash[i]; // get related tree item
      tree_item->setText (2, sc.actual_sc.toString ()); // display new shortcut
    }
}

// ask the user whether to save the current shortcut set;
// returns true to proceed with import action, false to abort it
bool
shortcut_manager::overwrite_all_shortcuts ()
{
  QMessageBox msg_box;
  msg_box.setWindowTitle(tr ("Overwriting Shortcuts"));
  msg_box.setIcon (QMessageBox::Warning);
  msg_box.setText(tr ("You are about to overwrite all shortcuts.\n"
     "Would you like to save the current shortcut set or cancel the action?"));
  msg_box.setStandardButtons(QMessageBox::Save | QMessageBox::Cancel);
  QPushButton *discard = msg_box.addButton (tr ("Don't save"),
                                            QMessageBox::DestructiveRole);
  msg_box.setDefaultButton(QMessageBox::Save);

  int ret = msg_box.exec ();

  if (msg_box.clickedButton () == discard)
    return true;  // do not save and go ahead

  if (ret == QMessageBox::Save)
    {
      if (do_import_export (OSC_EXPORT))
        return true;  // go ahead
    }

  return false; // abort the import
}

// import or export of shortcut sets,
// called from settings dialog when related buttons are clicked;
// returns true on success, false otherwise
bool
shortcut_manager::do_import_export (int action)
{
  // ask to save the current shortcuts, maybe abort import
  if (action == OSC_DEFAULT || action == OSC_IMPORT)
    {
      if (! overwrite_all_shortcuts ())
        return false;
    }

  // get the filename to read or write the shortcuts,
  // the default extension is .osc (octave shortcuts)
  if (action != OSC_DEFAULT)
    {
      QString file;

      if (action == OSC_IMPORT)
        file = QFileDialog::getOpenFileName (this,
                    tr ("Import shortcuts from file ..."), QString (),
                    tr ("Octave Shortcut Files (*.osc);;All Files (*)"),
                    0,QFileDialog::DontUseNativeDialog);
      else if (action == OSC_EXPORT)
        file = QFileDialog::getSaveFileName (this,
                    tr ("Export shortcuts into file ..."), QString (),
                    tr ("Octave Shortcut Files (*.osc);;All Files (*)"),
                    0,QFileDialog::DontUseNativeDialog);

      if (file.isEmpty ())
          return false;

      QSettings *osc_settings = new QSettings (file, QSettings::IniFormat);

      if (! osc_settings)
        {
          qWarning () << tr ("Failed to open %1 as octave shortcut file")
                         .arg (file);
          return false;
        }
      else
        {
          if (action == OSC_IMPORT)
            import_shortcuts (osc_settings);   // import (special action)
          else if (action == OSC_EXPORT)
            do_write_shortcuts (osc_settings, false); // export, (save settings)
        }
    }
  else
    {
      import_shortcuts (0);
    }

  return true;
}

// enter_shortcut:
// class derived from QLineEdit for directly entering key sequences which
enter_shortcut::enter_shortcut (QWidget *p) : QLineEdit (p)
{
  _direct_shortcut = true;      // the shortcut is directly entered
}

enter_shortcut::~enter_shortcut ()
{
}

// slot for checkbox whether the shortcut is directly entered or not
void
enter_shortcut::handle_direct_shortcut (int state)
{
  if (state)
    _direct_shortcut = true;  // the shortcut is directly entered
  else
    _direct_shortcut = false; // the shortcut has to be written as text
}

// new keyPressEvent
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

      if (modifiers & Qt::ShiftModifier)
        key += Qt::SHIFT;
      if (modifiers & Qt::ControlModifier)
        key += Qt::CTRL;
      if (modifiers & Qt::AltModifier)
        key += Qt::ALT;
      if (modifiers & Qt::MetaModifier)
        key += Qt::META;

      setText (QKeySequence(key).toString ());
    }
}
