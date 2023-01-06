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

#include <QApplication>
#include <QClipboard>
#include <QCompleter>
#include <QDebug>
#include <QDesktopServices>
#include <QFileDialog>
#include <QFileInfo>
#include <QHeaderView>
#include <QInputDialog>
#include <QLineEdit>
#include <QMenu>
#include <QMessageBox>
#include <QProcess>
#include <QSizePolicy>
#include <QStyledItemDelegate>
#include <QTimer>
#include <QToolButton>
#include <QUrl>

#include "files-dock-widget.h"
#include "gui-preferences-fb.h"
#include "gui-preferences-global.h"
#include "octave-qobject.h"
#include "octave-qtutils.h"
#include "qt-interpreter-events.h"

#include "oct-env.h"

OCTAVE_BEGIN_NAMESPACE(octave)

class FileTreeViewer : public QTreeView
{
public:

  FileTreeViewer (QWidget *p) : QTreeView (p) { }

  ~FileTreeViewer (void) = default;

  void mousePressEvent (QMouseEvent *e)
  {
    if (e->button () != Qt::RightButton)
      QTreeView::mousePressEvent (e);
  }
};

// to have file renamed in the file tree, it has to be renamed in
// QFileSystemModel::setData.
// For the editor to behave correctly, some signals must be sent before
// and after the rename
class file_system_model : public QFileSystemModel
{
public:
  file_system_model (files_dock_widget *p) : QFileSystemModel (p) {}

  ~file_system_model () = default;

  bool setData (const QModelIndex &idx, const QVariant &value,
                int role) override
  {
    if (!idx.isValid () || idx.column () != 0 || role != Qt::EditRole
        || (flags (idx) & Qt::ItemIsEditable) == 0)
      {
        return false;
      }

    QString new_name = value.toString ();
    QString old_name = idx.data ().toString ();
    if (new_name == old_name)
      return true;
    if (new_name.isEmpty ()
        || QDir::toNativeSeparators (new_name).contains (QDir::separator ()))
      {
        display_rename_failed_message (old_name, new_name);
        return false;
      }

    auto parent_dir = QDir(filePath (parent (idx)));

    files_dock_widget *fdw = static_cast<files_dock_widget*>(parent());

    fdw->file_remove_signal(parent_dir.filePath(old_name), parent_dir.filePath(new_name));

    if (!parent_dir.rename (old_name, new_name))
      {
        display_rename_failed_message (old_name, new_name);
        fdw->file_renamed_signal(false);
        return false;
      }

    fdw->file_renamed_signal(true);

    emit fileRenamed(parent_dir.absolutePath(), old_name, new_name);
    revert();

    return true;
  }

private:
  void display_rename_failed_message (const QString &old_name,
                                      const QString &new_name)
  {
    const QString message =

      files_dock_widget::tr ("Could not rename file \"%1\" to \"%2\".")
      .arg (old_name)
      .arg (new_name);
    QMessageBox::information (static_cast<QWidget *> (parent ()),
                              QFileSystemModel::tr ("Invalid filename"),
                              message, QMessageBox::Ok);
  }
};

// Delegate to improve ergonomy of file renaming by pre-selecting the text
// before the extension.
class RenameItemDelegate : public QStyledItemDelegate
{
public:
  RenameItemDelegate (QObject *parent = nullptr)
    : QStyledItemDelegate{ parent }
  {
  }

  void setEditorData (QWidget *editor,
                      const QModelIndex &index) const override
  {
    QLineEdit *line_edit = qobject_cast<QLineEdit *> (editor);

    if (!line_edit)
      {
        QStyledItemDelegate::setEditorData (editor, index);
        return;
      }

    QString filename = index.data (Qt::EditRole).toString ();

    int select_len = filename.indexOf (QChar ('.'));
    if (select_len == -1)
      select_len = filename.size ();

    line_edit->setText (filename);

    // Qt calls QLineEdit::selectAll after this function is called, so to
    // actually restrict the selection, we have to post the modification at
    // the end of the event loop.
    // QTimer allows this easily with 0 as timeout.
    QTimer::singleShot (0, [=] () {
      line_edit->setSelection (0, select_len);
    });
  }
};

files_dock_widget::files_dock_widget (QWidget *p, base_qobject& oct_qobj)
  : octave_dock_widget ("FilesDockWidget", p, oct_qobj)
{
  set_title (tr ("File Browser"));
  setToolTip (tr ("Browse your files"));

  m_sig_mapper = nullptr;

  m_columns_shown = QStringList ();
  m_columns_shown.append (tr ("File size"));
  m_columns_shown.append (tr ("File type"));
  m_columns_shown.append (tr ("Date modified"));
  m_columns_shown.append (tr ("Show hidden"));
  m_columns_shown.append (tr ("Alternating row colors"));

  m_columns_shown_keys = QStringList ();
  m_columns_shown_keys.append (fb_show_size.key);
  m_columns_shown_keys.append (fb_show_type.key);
  m_columns_shown_keys.append (fb_show_date.key);
  m_columns_shown_keys.append (fb_show_hidden.key);
  m_columns_shown_keys.append (fb_show_altcol.key);

  m_columns_shown_defs = QList <QVariant> ();
  m_columns_shown_defs.append (fb_show_size.def);
  m_columns_shown_defs.append (fb_show_type.def);
  m_columns_shown_defs.append (fb_show_date.def);
  m_columns_shown_defs.append (fb_show_hidden.def);
  m_columns_shown_defs.append (fb_show_altcol.def);

  QWidget *container = new QWidget (this);

  setWidget (container);

  // Create a toolbar
  m_navigation_tool_bar = new QToolBar ("", container);
  m_navigation_tool_bar->setAllowedAreas (Qt::TopToolBarArea);
  m_navigation_tool_bar->setMovable (false);

  m_current_directory = new QComboBox (m_navigation_tool_bar);
  m_current_directory->setToolTip (tr ("Enter the path or filename"));
  m_current_directory->setEditable (true);
  m_current_directory->setMaxCount (MaxMRUDirs);
  m_current_directory->setInsertPolicy (QComboBox::NoInsert);
  m_current_directory->setSizeAdjustPolicy (QComboBox::AdjustToMinimumContentsLengthWithIcon);
  QSizePolicy sizePol (QSizePolicy::Expanding, QSizePolicy::Preferred);
  m_current_directory->setSizePolicy (sizePol);

  resource_manager& rmgr = m_octave_qobj.get_resource_manager ();

  QAction *directory_up_action
    = new QAction (rmgr.icon ("folder-up", false, "go-up"), "", m_navigation_tool_bar);
  directory_up_action->setToolTip (tr ("One directory up"));

  m_sync_browser_directory_action
    = new QAction (rmgr.icon ("go-first"), tr ("Show Octave directory"),
                   m_navigation_tool_bar);
  m_sync_browser_directory_action->setToolTip (tr ("Go to current Octave directory"));
  m_sync_browser_directory_action->setEnabled (false);

  m_sync_octave_directory_action
    = new QAction (rmgr.icon ("go-last"), tr ("Set Octave directory"),
                   m_navigation_tool_bar);
  m_sync_octave_directory_action->setToolTip (tr ("Set Octave directory to current browser directory"));
  m_sync_octave_directory_action->setEnabled (false);

  QToolButton *popdown_button = new QToolButton ();
  popdown_button->setToolTip (tr ("Actions on current directory"));
  QMenu *popdown_menu = new QMenu ();
  popdown_menu->addAction (rmgr.icon ("user-home"),
                           tr ("Show Home Directory"), this,
                           SLOT (popdownmenu_home (bool)));
  popdown_menu->addAction (m_sync_browser_directory_action);
  popdown_menu->addAction (m_sync_octave_directory_action);
  popdown_button->setMenu (popdown_menu);
  popdown_button->setPopupMode (QToolButton::InstantPopup);
  popdown_button->setDefaultAction (
                                    new QAction (rmgr.icon ("folder-settings", false, "applications-system"),
                                                 "", m_navigation_tool_bar));

  popdown_menu->addSeparator ();
  popdown_menu->addAction (rmgr.icon ("folder"),
                           tr ("Set Browser Directory..."),
                           this, &files_dock_widget::popdownmenu_search_dir);
  popdown_menu->addSeparator ();
  popdown_menu->addAction (rmgr.icon ("edit-find"),
                           tr ("Find Files..."),
                           this, &files_dock_widget::popdownmenu_findfiles);
  popdown_menu->addSeparator ();
  popdown_menu->addAction (rmgr.icon ("document-new"),
                           tr ("New File..."),
                           this, &files_dock_widget::popdownmenu_newfile);
  popdown_menu->addAction (rmgr.icon ("folder-new"),
                           tr ("New Directory..."),
                           this, &files_dock_widget::popdownmenu_newdir);

  m_navigation_tool_bar->addWidget (m_current_directory);
  m_navigation_tool_bar->addAction (directory_up_action);
  m_navigation_tool_bar->addWidget (popdown_button);

  connect (directory_up_action, &QAction::triggered,
           this, &files_dock_widget::change_directory_up);
  connect (m_sync_octave_directory_action, &QAction::triggered,
           this, &files_dock_widget::do_sync_octave_directory);
  connect (m_sync_browser_directory_action, &QAction::triggered,
           this, &files_dock_widget::do_sync_browser_directory);

  gui_settings *settings = rmgr.get_settings ();
  // FIXME: what should happen if settings is 0?

  // Create the QFileSystemModel starting in the desired directory
  QDir startup_dir;  // take current dir

  if (settings->value (fb_restore_last_dir).toBool ())
    {
      // restore last dir from previous session
      QStringList last_dirs
        = settings->value (fb_mru_list.key).toStringList ();
      if (last_dirs.length () > 0)
        startup_dir = QDir (last_dirs.at (0));  // last dir in previous session
    }
  else if (! settings->value (fb_startup_dir).toString ().isEmpty ())
    {
      // do not restore but there is a startup dir configured
      startup_dir = QDir (settings->value (fb_startup_dir.key).toString ());
    }

  if (! startup_dir.exists ())
    {
      // the configured startup dir does not exist, take actual one
      startup_dir = QDir ();
    }

  m_file_system_model = new file_system_model (this);
  m_file_system_model->setResolveSymlinks (false);
  m_file_system_model->setFilter (
                                  QDir::System | QDir::NoDotAndDotDot | QDir::AllEntries);
  QModelIndex rootPathIndex
    = m_file_system_model->setRootPath (startup_dir.absolutePath ());

  // Attach the model to the QTreeView and set the root index
  m_file_tree_view = new FileTreeViewer (container);
  m_file_tree_view->setSelectionMode (QAbstractItemView::ExtendedSelection);
  m_file_tree_view->setModel (m_file_system_model);
  m_file_tree_view->setRootIndex (rootPathIndex);
  m_file_tree_view->setSortingEnabled (true);
  m_file_tree_view->setAlternatingRowColors (true);
  m_file_tree_view->setAnimated (true);
  m_file_tree_view->setToolTip (tr ("Double-click to open file/folder, right click for alternatives"));

  // allow renaming directly in the tree view with
  // m_file_tree_view->edit(index)
  m_file_system_model->setReadOnly (false);
  // delegate to improve rename ergonomy by pre-selecting text up to the
  // extension
  auto *rename_delegate = new RenameItemDelegate (this);
  m_file_tree_view->setItemDelegateForColumn (0, rename_delegate);
  // prevent the tree view to override Octave's double-click behavior
  m_file_tree_view->setEditTriggers (QAbstractItemView::NoEditTriggers);
  // create the rename action (that will be added to context menu)
  // and associate to F2 key shortcut
  m_rename_action = new QAction (tr ("Rename..."), this);
  m_rename_action->setShortcut (Qt::Key_F2);
  m_rename_action->setShortcutContext(Qt::WidgetWithChildrenShortcut);
  connect (m_rename_action, &QAction::triggered, this,
           &files_dock_widget::contextmenu_rename);
  addAction(m_rename_action);

  // get sort column and order as well as column state (order and width)

  m_file_tree_view->sortByColumn
    (settings->value (fb_sort_column).toInt (),
     static_cast<Qt::SortOrder> (settings->value (fb_sort_order).toUInt ()));
  // FIXME: use value<Qt::SortOrder> instead of static cast after
  //        dropping support of Qt 5.4

  if (settings->contains (fb_column_state.key))
    m_file_tree_view->header ()->restoreState
      (settings->value (fb_column_state.key).toByteArray ());

  // Set header properties for sorting
  m_file_tree_view->header ()->setSectionsClickable (true);
  m_file_tree_view->header ()->setSectionsMovable (true);
  m_file_tree_view->header ()->setSortIndicatorShown (true);

  QStringList mru_dirs =
    settings->value (fb_mru_list.key).toStringList ();
  m_current_directory->addItems (mru_dirs);

  m_current_directory->setEditText
    (m_file_system_model->fileInfo (rootPathIndex). absoluteFilePath ());

  connect (m_file_tree_view, &FileTreeViewer::activated,
           this, &files_dock_widget::item_double_clicked);

  // add context menu to tree_view
  m_file_tree_view->setContextMenuPolicy (Qt::CustomContextMenu);
  connect (m_file_tree_view, &FileTreeViewer::customContextMenuRequested,
           this, &files_dock_widget::contextmenu_requested);

  m_file_tree_view->header ()->setContextMenuPolicy (Qt::CustomContextMenu);
  connect (m_file_tree_view->header (),
           &QHeaderView::customContextMenuRequested,
           this, &files_dock_widget::headercontextmenu_requested);

  // Layout the widgets vertically with the toolbar on top
  QVBoxLayout *vbox_layout = new QVBoxLayout ();
  vbox_layout->setSpacing (0);
  vbox_layout->addWidget (m_navigation_tool_bar);
  vbox_layout->addWidget (m_file_tree_view);
  vbox_layout->setMargin (1);

  container->setLayout (vbox_layout);

  // FIXME: Add right-click contextual menus for copying, pasting,
  //        deleting files (and others).

  connect (m_current_directory->lineEdit (), &QLineEdit::returnPressed,
           this, &files_dock_widget::accept_directory_line_edit);

  // FIXME: We could use
  //
  //    connect (m_current_directory,
  //             QOverload<const QString&>::of (&QComboBox::activated),
  //             this, &files_dock_widget::set_current_directory);
  //
  // but referring to QComboBox::activated will generate deprecated
  // function warnings from GCC.  We could also use
  //
  //    connect (m_current_directory, &QComboBox::textActivated,
  //             this, &files_dock_widget::set_current_directory);
  //
  // but the function textActivated was not introduced until Qt 5.14
  // so we'll need a feature test.

  connect (m_current_directory, SIGNAL (activated (const QString&)),
           this, SLOT (set_current_directory (const QString&)));

  QCompleter *completer = new QCompleter (m_file_system_model, this);
  m_current_directory->setCompleter (completer);

  setFocusProxy (m_current_directory);

  m_sync_octave_dir = true;   // default, overwritten with notice_settings ()
  m_octave_dir = "";

  if (! p)
    make_window ();
}

void files_dock_widget::save_settings (void)
{
  resource_manager& rmgr = m_octave_qobj.get_resource_manager ();
  gui_settings *settings = rmgr.get_settings ();

  if (! settings)
    return;

  int sort_column = m_file_tree_view->header ()->sortIndicatorSection ();
  Qt::SortOrder sort_order = m_file_tree_view->header ()->sortIndicatorOrder ();
  settings->setValue (fb_sort_column.key, sort_column);
  settings->setValue (fb_sort_order.key, sort_order);
  settings->setValue (fb_column_state.key,
                      m_file_tree_view->header ()->saveState ());

  QStringList dirs;
  for (int i=0; i< m_current_directory->count (); i++)
    {
      dirs.append (m_current_directory->itemText (i));
    }
  settings->setValue (fb_mru_list.key, dirs);

  settings->sync ();

  octave_dock_widget::save_settings ();

  if (m_sig_mapper)
    delete m_sig_mapper;
}

void files_dock_widget::item_double_clicked (const QModelIndex& index)
{
  // Retrieve the file info associated with the model index.
  QFileInfo fileInfo = m_file_system_model->fileInfo (index);
  set_current_directory (fileInfo.absoluteFilePath ());
}

void files_dock_widget::set_current_directory (const QString& dir)
{
  display_directory (dir);
}

void files_dock_widget::accept_directory_line_edit (void)
{
  display_directory (m_current_directory->currentText ());
}

void files_dock_widget::change_directory_up (void)
{
  QDir dir
    = QDir (m_file_system_model->filePath (m_file_tree_view->rootIndex ()));

  dir.cdUp ();
  display_directory (dir.absolutePath ());
}

void files_dock_widget::do_sync_octave_directory (void)
{
  QDir dir
    = QDir (m_file_system_model->filePath (m_file_tree_view->rootIndex ()));

  emit displayed_directory_changed (dir.absolutePath ());
}

void files_dock_widget::do_sync_browser_directory (void)
{
  display_directory (m_octave_dir, false); // false: no sync of octave dir
}

void files_dock_widget::update_octave_directory (const QString& dir)
{
  m_octave_dir = dir;
  if (m_sync_octave_dir)
    display_directory (m_octave_dir, false); // false: no sync of octave dir
}

void files_dock_widget::display_directory (const QString& dir,
                                           bool set_octave_dir)
{
  QFileInfo fileInfo (dir);
  if (fileInfo.exists ())
    {
      if (fileInfo.isDir ())
        {
          m_file_tree_view->setRootIndex (m_file_system_model->
                                          index (fileInfo.absoluteFilePath ()));
          m_file_system_model->setRootPath (fileInfo.absoluteFilePath ());
          if (m_sync_octave_dir && set_octave_dir)
            process_set_current_dir (fileInfo.absoluteFilePath ());

          // see if it's in the list, and if it is,
          // remove it and then put at top of the list
          int index
            = m_current_directory->findText (fileInfo.absoluteFilePath ());
          if (index != -1)
            {
              m_current_directory->removeItem (index);
            }
          m_current_directory->insertItem (0, fileInfo.absoluteFilePath ());
          m_current_directory->setCurrentIndex (0);
        }
      else
        {
          QString abs_fname = fileInfo.absoluteFilePath ();

          QString suffix = fileInfo.suffix ().toLower ();
          resource_manager& rmgr = m_octave_qobj.get_resource_manager ();
          gui_settings *settings = rmgr.get_settings ();
          QString ext = settings->value (fb_txt_file_ext).toString ();
#if defined (HAVE_QT_SPLITBEHAVIOR_ENUM)
          QStringList extensions = ext.split (";", Qt::SkipEmptyParts);
#else
          QStringList extensions = ext.split (";", QString::SkipEmptyParts);
#endif
          if (QFile::exists (abs_fname))
            {
              if (extensions.contains (suffix))
                emit open_file (fileInfo.absoluteFilePath ());
              else
                emit open_any_signal (abs_fname);
            }
        }
    }
}

void files_dock_widget::open_item_in_app (const QModelIndex& index)
{
  // Retrieve the file info associated with the model index.
  QFileInfo fileInfo = m_file_system_model->fileInfo (index);

  QString file = fileInfo.absoluteFilePath ();

  QDesktopServices::openUrl (QUrl::fromLocalFile (file));
}

void files_dock_widget::toggle_header (int col)
{
  resource_manager& rmgr = m_octave_qobj.get_resource_manager ();
  gui_settings *settings = rmgr.get_settings ();

  QString key = m_columns_shown_keys.at (col);
  bool shown = settings->value (key, false).toBool ();
  settings->setValue (key, ! shown);
  settings->sync ();

  switch (col)
    {
    case 0:
    case 1:
    case 2:
      // toggle column visibility
      m_file_tree_view->setColumnHidden (col + 1, shown);
      break;
    case 3:
    case 4:
      // other actions depending on new settings
      notice_settings (settings);
      break;
    }
}

void files_dock_widget::headercontextmenu_requested (const QPoint& mpos)
{
  QMenu menu (this);

  if (m_sig_mapper)
    delete m_sig_mapper;
  m_sig_mapper = new QSignalMapper (this);

  resource_manager& rmgr = m_octave_qobj.get_resource_manager ();
  gui_settings *settings = rmgr.get_settings ();

  for (int i = 0; i < m_columns_shown.size (); i++)
    {
      QAction *action = menu.addAction (m_columns_shown.at (i),
                                        m_sig_mapper, SLOT (map ()));
      m_sig_mapper->setMapping (action, i);
      action->setCheckable (true);
      action->setChecked
        (settings->value (m_columns_shown_keys.at (i),
                          m_columns_shown_defs.at (i)).toBool ());
    }

  // FIXME: We could use
  //
  //   connect (&m_sig_mapper, QOverload<int>::of (&QSignalMapper::mapped),
  //            this, &workspace_view::toggle_header);
  //
  // but referring to QSignalMapper::mapped will generate deprecated
  // function warnings from GCC.  We could also use
  //
  //   connect (&m_sig_mapper, &QSignalMapper::mappedInt,
  //            this, &workspace_view::toggle_header);
  //
  // but the function mappedInt was not introduced until Qt 5.15 so
  // we'll need a feature test.

  connect (m_sig_mapper, SIGNAL (mapped (int)),
           this, SLOT (toggle_header (int)));

  menu.exec (m_file_tree_view->mapToGlobal (mpos));
}

void files_dock_widget::contextmenu_requested (const QPoint& mpos)
{

  QMenu menu (this);

  QModelIndex index = m_file_tree_view->indexAt (mpos);

  if (index.isValid ())
    {
      QFileInfo info = m_file_system_model->fileInfo (index);

      QItemSelectionModel *m = m_file_tree_view->selectionModel ();
      QModelIndexList sel = m->selectedRows ();

      // check if item at mouse position is seleccted
      if (! sel.contains (index))
        {
          // is not selected -> clear actual selection and select this item
          m->setCurrentIndex (index,
                              QItemSelectionModel::Clear
                              | QItemSelectionModel::Select
                              | QItemSelectionModel::Rows);
        }

      resource_manager& rmgr = m_octave_qobj.get_resource_manager ();

      // construct the context menu depending on item
      menu.addAction (rmgr.icon ("document-open"), tr ("Open"),
                      this, &files_dock_widget::contextmenu_open);

      if (info.isDir ())
        {
          menu.addAction (tr ("Open in System File Explorer"),
                          this, &files_dock_widget::contextmenu_open_in_app);
        }

      if (info.isFile ())
        menu.addAction (tr ("Open in Text Editor"),
                        this, &files_dock_widget::contextmenu_open_in_editor);

      menu.addAction (tr ("Copy Selection to Clipboard"),
                      this, &files_dock_widget::contextmenu_copy_selection);

      if (info.isFile () && info.suffix () == "m")
        menu.addAction (rmgr.icon ("media-playback-start"), tr ("Run"),
                        this, &files_dock_widget::contextmenu_run);

      if (info.isFile ())
        menu.addAction (tr ("Load Data"),
                        this, &files_dock_widget::contextmenu_load);

      if (info.isDir ())
        {
          menu.addSeparator ();
          menu.addAction (rmgr.icon ("go-first"), tr ("Set Current Directory"),
                          this, &files_dock_widget::contextmenu_setcurrentdir);

          QMenu *add_path_menu = menu.addMenu (tr ("Add to Path"));

          add_path_menu->addAction (tr ("Selected Directories"),
                                    this, [=] (bool checked) { contextmenu_add_to_path (checked); });
          add_path_menu->addAction (tr ("Selected Directories and Subdirectories"),
                                    this, &files_dock_widget::contextmenu_add_to_path_subdirs);

          QMenu *rm_path_menu = menu.addMenu (tr ("Remove from Path"));

          rm_path_menu->addAction (tr ("Selected Directories"),
                                   this, &files_dock_widget::contextmenu_rm_from_path);
          rm_path_menu->addAction (tr ("Selected Directories and Subdirectories"),
                                   this, &files_dock_widget::contextmenu_rm_from_path_subdirs);

          menu.addSeparator ();

          menu.addAction (rmgr.icon ("edit-find"), tr ("Find Files..."),
                          this, &files_dock_widget::contextmenu_findfiles);
        }

      menu.addSeparator ();
      menu.addAction (m_rename_action);
      menu.addAction (rmgr.icon ("edit-delete"), tr ("Delete..."),
                      this, &files_dock_widget::contextmenu_delete);

      if (info.isDir ())
        {
          menu.addSeparator ();
          menu.addAction (rmgr.icon ("document-new"), tr ("New File..."),
                          this, &files_dock_widget::contextmenu_newfile);
          menu.addAction (rmgr.icon ("folder-new"), tr ("New Directory..."),
                          this, &files_dock_widget::contextmenu_newdir);
        }

      // show the menu
      menu.exec (m_file_tree_view->mapToGlobal (mpos));

    }
}

void files_dock_widget::contextmenu_open (bool)
{

  QItemSelectionModel *m = m_file_tree_view->selectionModel ();
  QModelIndexList rows = m->selectedRows ();

  for (auto it = rows.begin (); it != rows.end (); it++)
    {
      QFileInfo file = m_file_system_model->fileInfo (*it);
      if (file.exists ())
        display_directory (file.absoluteFilePath ());
    }
}

void files_dock_widget::contextmenu_open_in_editor (bool)
{

  QItemSelectionModel *m = m_file_tree_view->selectionModel ();
  QModelIndexList rows = m->selectedRows ();

  for (auto it = rows.begin (); it != rows.end (); it++)
    {
      QFileInfo file = m_file_system_model->fileInfo (*it);
      if (file.exists ())
        emit open_file (file.absoluteFilePath ());
    }
}

void files_dock_widget::contextmenu_open_in_app (bool)
{
  QItemSelectionModel *m = m_file_tree_view->selectionModel ();
  QModelIndexList rows = m->selectedRows ();

  for (auto it = rows.begin (); it != rows.end (); it++)
    open_item_in_app (*it);
}

void files_dock_widget::contextmenu_copy_selection (bool)
{
  QItemSelectionModel *m = m_file_tree_view->selectionModel ();
  QModelIndexList rows = m->selectedRows ();

  QStringList selection;

  for (auto it = rows.begin (); it != rows.end (); it++)
    {
      QFileInfo info = m_file_system_model->fileInfo (*it);

      selection << info.fileName ();
    }

  QClipboard *clipboard = QApplication::clipboard ();

  clipboard->setText (selection.join ("\n"));
}

void files_dock_widget::contextmenu_load (bool)
{
  QItemSelectionModel *m = m_file_tree_view->selectionModel ();
  QModelIndexList rows = m->selectedRows ();

  if (rows.size () > 0)
    {
      QModelIndex index = rows[0];

      QFileInfo info = m_file_system_model->fileInfo (index);

      emit load_file_signal (info.fileName ());
    }
}

void files_dock_widget::contextmenu_run (bool)
{
  QItemSelectionModel *m = m_file_tree_view->selectionModel ();
  QModelIndexList rows = m->selectedRows ();

  if (rows.size () > 0)
    {
      QModelIndex index = rows[0];

      QFileInfo info = m_file_system_model->fileInfo (index);
      emit run_file_signal (info);
    }
}

void files_dock_widget::contextmenu_rename (bool)
{
  QItemSelectionModel *m = m_file_tree_view->selectionModel ();
  QModelIndexList rows = m->selectedRows ();
  if (rows.size () > 0)
    {
      QModelIndex index = rows[0];
      m_file_tree_view->edit(index);
    }
}

void files_dock_widget::contextmenu_delete (bool)
{
  QItemSelectionModel *m = m_file_tree_view->selectionModel ();
  QModelIndexList rows = m->selectedRows ();

  int file_cnt = rows.size ();
  bool multiple_files = (file_cnt > 1);

  for (auto it = rows.begin (); it != rows.end (); it++)
    {
      QModelIndex index = *it;

      QFileInfo info = m_file_system_model->fileInfo (index);

      QMessageBox::StandardButton dlg_answer;
      if (multiple_files)
        if (it == rows.begin ())
          {
            dlg_answer = QMessageBox::question (this,
                                                tr ("Delete file/directory"),
                                                tr ("Are you sure you want to delete all %1 selected files?\n").arg (file_cnt),
                                                QMessageBox::Yes | QMessageBox::No);
            if (dlg_answer != QMessageBox::Yes)
              return;
          }
        else
          dlg_answer = QMessageBox::Yes;
      else
        {
          dlg_answer = QMessageBox::question (this,
                                              tr ("Delete file/directory"),
                                              tr ("Are you sure you want to delete\n")
                                              + info.filePath (),
                                              QMessageBox::Yes | QMessageBox::No);
        }

      if (dlg_answer == QMessageBox::Yes)
        {
          if (info.isDir ())
            {
              // see if directory is empty
              QDir path (info.absoluteFilePath ());
              QList<QFileInfo> fileLst = path.entryInfoList (
                                                             QDir::Hidden | QDir::AllEntries |
                                                             QDir::NoDotAndDotDot | QDir::System);

              if (fileLst.count () != 0)
                QMessageBox::warning (this, tr ("Delete file/directory"),
                                      tr ("Can not delete a directory that is not empty"));
              else
                m_file_system_model->rmdir (index);
            }
          else
            {
              // Close the file in the editor if open
              emit file_remove_signal (info.filePath (), QString ());
              // Remove the file.
              bool st = m_file_system_model->remove (index);
              if (! st)
                {
                  QMessageBox::warning (this, tr ("Deletion error"),
                                        tr ("Could not delete file \"%1\".").
                                        arg (info.filePath ()));
                  // Reload the old file
                }
              emit file_renamed_signal (st);
            }

          m_file_system_model->revert ();

        }
    }
}

// Get the currently selected files/dirs and return their file info
// in a list.
QList<QFileInfo> files_dock_widget::get_selected_items_info (bool dir)
{
  QItemSelectionModel *m = m_file_tree_view->selectionModel ();
  QModelIndexList rows = m->selectedRows ();

  QList<QFileInfo> infos;

  for (auto it = rows.begin (); it != rows.end (); it++)
    {
      QModelIndex index = *it;

      QFileInfo info = m_file_system_model->fileInfo (index);

      if (info.exists () &&
          ((dir & info.isDir ()) || (! dir && info.isFile ())))
        infos.append (info);
    }

  return infos;
}

void files_dock_widget::contextmenu_newfile (bool)
{
  QItemSelectionModel *m = m_file_tree_view->selectionModel ();
  QModelIndexList rows = m->selectedRows ();

  if (rows.size () > 0)
    {
      QModelIndex index = rows[0];

      QFileInfo info = m_file_system_model->fileInfo (index);
      QString parent_dir = info.filePath ();

      process_new_file (parent_dir);
    }
}

void files_dock_widget::contextmenu_newdir (bool)
{
  QItemSelectionModel *m = m_file_tree_view->selectionModel ();
  QModelIndexList rows = m->selectedRows ();

  if (rows.size () > 0)
    {
      QModelIndex index = rows[0];

      QFileInfo info = m_file_system_model->fileInfo (index);
      QString parent_dir = info.filePath ();

      process_new_dir (parent_dir);
    }
}

void files_dock_widget::contextmenu_setcurrentdir (bool)
{
  QList<QFileInfo> infos = get_selected_items_info (true);

  if (infos.length () > 0 && infos.first ().isDir ())
    process_set_current_dir (infos.first ().absoluteFilePath ());
}

void files_dock_widget::contextmenu_add_to_path (bool, bool rm, bool subdirs)
{
  QList<QFileInfo> infos = get_selected_items_info (true);

  QStringList dir_list;

  for (int i = 0; i < infos.length (); i++)
    dir_list.append (infos.at (i).absoluteFilePath ());

  if (infos.length () > 0)
    emit modify_path_signal (dir_list, rm, subdirs);
}

void files_dock_widget::contextmenu_add_to_path_subdirs (bool)
{
  contextmenu_add_to_path (true, false, true);
}

void files_dock_widget::contextmenu_rm_from_path (bool)
{
  contextmenu_add_to_path (true, true, false);
}

void files_dock_widget::contextmenu_rm_from_path_subdirs (bool)
{
  contextmenu_add_to_path (true, true, true);
}

void files_dock_widget::contextmenu_findfiles (bool)
{
  QItemSelectionModel *m = m_file_tree_view->selectionModel ();
  QModelIndexList rows = m->selectedRows ();

  if (rows.size () > 0)
    {
      QModelIndex index = rows[0];

      QFileInfo info = m_file_system_model->fileInfo (index);

      if (info.isDir ())
        {
          process_find_files (info.absoluteFilePath ());
        }
    }
}

void files_dock_widget::notice_settings (const gui_settings *settings)
{
  // QSettings pointer is checked before emitting.

  int size_idx = settings->value (global_icon_size).toInt ();
  size_idx = (size_idx > 0) - (size_idx < 0) + 1;  // Make valid index from 0 to 2

  QStyle *st = style ();
  int icon_size = st->pixelMetric (global_icon_sizes[size_idx]);
  m_navigation_tool_bar->setIconSize (QSize (icon_size, icon_size));

  // filenames are always shown, other columns can be hidden by settings
  for (int i = 0; i < 3; i++)
    m_file_tree_view->setColumnHidden (i + 1,
                                       ! settings->value (m_columns_shown_keys.at (i),false).toBool ());

  QDir::Filters current_filter = m_file_system_model->filter ();
  if (settings->value (m_columns_shown_keys.at (3), false).toBool ())
    m_file_system_model->setFilter (current_filter | QDir::Hidden);
  else
    m_file_system_model->setFilter (current_filter & (~QDir::Hidden));

  m_file_tree_view->setAlternatingRowColors
    (settings->value (m_columns_shown_keys.at (4),true).toBool ());
  m_file_tree_view->setModel (m_file_system_model);

  // enable the buttons to sync octave/browser dir
  // only if this is not done by default
  m_sync_octave_dir
    = settings->value (fb_sync_octdir).toBool ();
  m_sync_octave_directory_action->setEnabled (! m_sync_octave_dir);
  m_sync_browser_directory_action->setEnabled (! m_sync_octave_dir);

  // If m_sync_octave_dir is enabled, then we want the file browser to
  // update to match the current working directory of the
  // interpreter.  We don't want to queue any signal to change the
  // interpreter's current working directory.  In this case, we just
  // want the GUI to match the state of the interpreter.

  if (m_sync_octave_dir)
    do_sync_browser_directory ();
}

void files_dock_widget::popdownmenu_home (bool)
{
  QString dir = QString::fromStdString (sys::env::get_home_directory ());

  if (dir.isEmpty ())
    dir = QDir::homePath ();

  set_current_directory (dir);
}

void files_dock_widget::popdownmenu_search_dir (bool)
{
  // FIXME: Remove, if for all common KDE versions (bug #54607) is resolved.
  int opts = QFileDialog::ShowDirsOnly;
  resource_manager& rmgr = m_octave_qobj.get_resource_manager ();
  gui_settings *settings = rmgr.get_settings ();
  if (! settings->value (global_use_native_dialogs).toBool ())
    opts |= QFileDialog::DontUseNativeDialog;

  QString dir = QFileDialog::getExistingDirectory (this,
                                                   tr ("Set directory of file browser"),
                                                   m_file_system_model->rootPath (),
                                                   QFileDialog::Option (opts));
  set_current_directory (dir);
}

void files_dock_widget::popdownmenu_findfiles (bool)
{
  process_find_files (m_file_system_model->rootPath ());
}

void files_dock_widget::popdownmenu_newdir (bool)
{
  process_new_dir (m_file_system_model->rootPath ());
}

void files_dock_widget::popdownmenu_newfile (bool)
{
  process_new_file (m_file_system_model->rootPath ());
}

void files_dock_widget::process_new_file (const QString& parent_dir)
{
  bool ok;

  QString name = QInputDialog::getText (this, tr ("Create File"),
                                        tr ("Create file in\n", "String ends with \\n!") + parent_dir,
                                        QLineEdit::Normal,
                                        tr ("New File.txt"), &ok);
  if (ok && name.length () > 0)
    {
      name = parent_dir + '/' + name;

      QFile file (name);
      file.open (QIODevice::WriteOnly);
      m_file_system_model->revert ();
    }
}

void files_dock_widget::process_new_dir (const QString& parent_dir)
{
  bool ok;

  QString name = QInputDialog::getText (this, tr ("Create Directory"),
                                        tr ("Create folder in\n", "String ends with \\n!") + parent_dir,
                                        QLineEdit::Normal,
                                        tr ("New Directory"), &ok);
  if (ok && name.length () > 0)
    {
      QDir dir (parent_dir);
      dir.mkdir (name);
      m_file_system_model->revert ();
    }
}

void files_dock_widget::process_set_current_dir (const QString& dir)
{
  emit displayed_directory_changed (dir);
}

void files_dock_widget::process_find_files (const QString& dir)
{
  emit find_files_signal (dir);
}

void files_dock_widget::copyClipboard ()
{
  if (m_file_tree_view->hasFocus ())
    contextmenu_copy_selection (true);
  if (m_current_directory->hasFocus ())
    {
      QClipboard *clipboard = QApplication::clipboard ();

      QLineEdit *edit = m_current_directory->lineEdit ();
      if (edit && edit->hasSelectedText ())
        {
          clipboard->setText (edit->selectedText ());
        }
    }
}

void files_dock_widget::pasteClipboard ()
{
  if (m_current_directory->hasFocus ())
    {
      QClipboard *clipboard = QApplication::clipboard ();
      QString str = clipboard->text ();
      QLineEdit *edit = m_current_directory->lineEdit ();
      if (edit && str.length () > 0)
        edit->insert (str);
    }
}

void files_dock_widget::selectAll ()
{
  if (m_file_tree_view->hasFocus ())
    m_file_tree_view->selectAll ();
  if (m_current_directory->hasFocus ())
    {
      QLineEdit *edit = m_current_directory->lineEdit ();
      if (edit)
        {
          edit->selectAll ();
        }
    }
}

OCTAVE_END_NAMESPACE(octave)
