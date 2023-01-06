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

#if ! defined (octave_files_dock_widget_h)
#define octave_files_dock_widget_h 1

#include <QAction>
#include <QComboBox>
#include <QDate>
#include <QFileSystemModel>
#include <QList>
#include <QListView>
#include <QListWidget>
#include <QMouseEvent>
#include <QObject>
#include <QSignalMapper>
#include <QToolBar>
#include <QToolButton>
#include <QTreeView>
#include <QVBoxLayout>
#include <QWidget>

#include "octave-dock-widget.h"

OCTAVE_BEGIN_NAMESPACE(octave)

class base_qobject;

//!  Dock widget to display files in the current directory.

class files_dock_widget : public octave_dock_widget
{
  Q_OBJECT

public:

  files_dock_widget (QWidget *parent, base_qobject& oct_qobj);

  ~files_dock_widget (void) = default;

signals:

  //! Emitted, whenever the user requested to open a file.

  void open_file (const QString& fileName);

  //! Emitted, whenever the currently displayed directory changed.

  void displayed_directory_changed (const QString& dir);

  //! Emitted, whenever the user requested to load a file in the text editor.

  void load_file_signal (const QString& fileName);

  //! Emitted, whenever the user requested to open an unknown type file.

  void open_any_signal (const QString& fileName);

  //! Emitted, whenever the user requested to run a file.

  void run_file_signal (const QFileInfo& info);

  //! Emitted, whenever wants to search for a file .

  void find_files_signal (const QString& startdir);

  //! Emitted, whenever the user removes or renames a file.

  void file_remove_signal (const QString& old_name, const QString& new_name);

  //! Emitted, when a file or directory is renamed.

  void file_renamed_signal (bool);

  //! Emitted, when the path has to be modified

  void modify_path_signal (const QStringList& dir_list, bool rm,
                           bool subdirs);

public slots:

  //! Slot for handling a change in directory via double click.

  void item_double_clicked (const QModelIndex& index);

  //! Slot for handling the up-directory button in the toolbar.

  void change_directory_up (void);

  //! Slot for handling the sync octave directory button in the toolbar.

  void do_sync_octave_directory (void);

  //! Slot for handling the sync browser directory button in the toolbar.

  void do_sync_browser_directory (void);

  //! Sets the current directory being displayed.

  void set_current_directory (const QString& dir);

  //! Accepts user input a the line edit for the current directory.

  void accept_directory_line_edit (void);

  //! Set the internal variable that holds the actual octave variable.

  void update_octave_directory (const QString& dir);

  //! Tells the widget to react on changed settings.

  void notice_settings (const gui_settings *settings);

  void save_settings (void);

private slots:

  void headercontextmenu_requested (const QPoint& pos);
  void toggle_header (int col);

  //! Context menu wanted.

  void contextmenu_requested (const QPoint& pos);

  //! Context menu actions.
  //!@{
  void contextmenu_open (bool);
  void contextmenu_open_in_editor (bool);
  void contextmenu_open_in_app (bool);
  void contextmenu_copy_selection (bool);
  void contextmenu_run (bool);
  void contextmenu_load (bool);
  void contextmenu_rename (bool);
  void contextmenu_delete (bool);
  void contextmenu_newfile (bool);
  void contextmenu_newdir (bool);
  void contextmenu_setcurrentdir (bool);
  void contextmenu_add_to_path (bool, bool rm=false, bool subdirs=false);
  void contextmenu_add_to_path_subdirs (bool);
  void contextmenu_rm_from_path (bool);
  void contextmenu_rm_from_path_subdirs (bool);
  void contextmenu_findfiles (bool);
  //!@}

  //! Popdown menu options.
  //!@{
  void popdownmenu_newfile (bool);
  void popdownmenu_newdir (bool);
  void popdownmenu_search_dir (bool);
  void popdownmenu_findfiles (bool);
  void popdownmenu_home (bool);
  //!@}

  //! Inherited from octave_doc_widget.
  //!@{
  void copyClipboard ();
  void pasteClipboard ();
  void selectAll ();
  //!@}

private:

  //! Get currently selected QFileInfo object.

  QList<QFileInfo> get_selected_items_info (bool);

  //! Process new file/directory actions

  void process_new_file (const QString& parent_name);
  void process_new_dir (const QString& parent_name);

  //! Process setting current dir or find in files

  void process_set_current_dir (const QString& parent_name);
  void process_find_files (const QString& dir_name);

  //! set a new directory or open a file

  void display_directory (const QString& dir, bool set_octave_dir = true);

  void open_item_in_app (const QModelIndex& index);

  //! Variables for the actions

  QToolBar *m_navigation_tool_bar;
  QAction *m_sync_octave_directory_action;
  QAction *m_sync_browser_directory_action;
  QAction *m_rename_action;

  //! The file system model.

  QFileSystemModel *m_file_system_model;

  //! The file system view.
  //!@{
  QTreeView *m_file_tree_view;
  QComboBox *m_current_directory;
  //!@}

  //! Flag if syncing with Octave.

  bool m_sync_octave_dir;

  //! The actual Octave directory.

  QString m_octave_dir;

  enum { MaxMRUDirs = 10 };

  QStringList m_columns_shown;
  QStringList m_columns_shown_keys;
  QList <QVariant> m_columns_shown_defs;
  QSignalMapper *m_sig_mapper;
};

OCTAVE_END_NAMESPACE(octave)

#endif
