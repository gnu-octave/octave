////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2011-2025 The Octave Project Developers
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

#include <map>

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
#include <QMimeDatabase>
#include <QMimeType>
#include <QProcess>
#include <QSizePolicy>
#include <QStyledItemDelegate>
#include <QTimer>
#include <QToolButton>
#include <QUrl>

#include "files-dock-widget.h"
#include "gui-preferences-fb.h"
#include "gui-preferences-global.h"
#include "gui-settings.h"
#include "gui-utils.h"
#include "qt-interpreter-events.h"

#include "oct-env.h"

OCTAVE_BEGIN_NAMESPACE(octave)

// ----------------------------------------------------------------------
//  Class file_tree_viewer

class FileTreeViewer : public QTreeView
{
public:

  FileTreeViewer (QWidget *p) : QTreeView (p) { }

  ~FileTreeViewer () = default;

  void mousePressEvent (QMouseEvent *e)
  {
    if (e->button () != Qt::RightButton)
      QTreeView::mousePressEvent (e);
  }
};


// ----------------------------------------------------------------------
//  Class file_system_model

// to have file renamed in the file tree, it has to be renamed in
// QFileSystemModel::setData.
// For the editor to behave correctly, some signals must be sent before
// and after the rename

class file_system_model : public QFileSystemModel
{
public:
  file_system_model (file_system_browser *p) : QFileSystemModel (p) {}

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

    auto parent_dir = QDir (filePath (parent (idx)));

    file_system_browser *fsb = static_cast<file_system_browser *>(parent ());

    fsb->file_remove_signal (parent_dir.filePath (old_name), parent_dir.filePath (new_name));

    if (!parent_dir.rename (old_name, new_name))
      {
        display_rename_failed_message (old_name, new_name);
        fsb->file_renamed_signal (false);
        return false;
      }

    fsb->file_renamed_signal (true);

    Q_EMIT fileRenamed (parent_dir.absolutePath (), old_name, new_name);
    revert ();

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
    QTimer::singleShot (0, [line_edit, select_len] () { line_edit->setSelection (0, select_len); });
  }
};


// ----------------------------------------------------------------------
//  Class cache_file_icon_provider

class cache_file_icon_provider : public QFileIconProvider
{
public:
  cache_file_icon_provider ()
  {
    m_null_icon = QIcon ();
    m_file_icon = m_file_icon_provider.icon (QFileIconProvider::File);
    m_folder_icon = m_file_icon_provider.icon (QFileIconProvider::Folder);
  }

  QIcon icon (IconType ict) const
  {
    if (ict == QFileIconProvider::File)
      return m_file_icon;
    else if (ict == QFileIconProvider::Folder)
      return m_folder_icon;
    else
      return m_null_icon;
  }

  QIcon icon (const QFileInfo &fi) const
  {
    static bool no_platform_theme = QIcon::themeName ().isEmpty ();

    if (no_platform_theme)
      return m_file_icon_provider.icon (fi);

    QMimeType mime_type = m_db.mimeTypeForFile (fi.absoluteFilePath ());
    QString icon_name = mime_type.iconName ();
    auto mime_type_iter = m_icon_cache.find (icon_name);
    if (mime_type_iter != m_icon_cache.end ())
      return mime_type_iter->second;

    QIcon icon = QIcon::fromTheme (icon_name);
    m_icon_cache.insert ({icon_name, icon});
    return icon;
  }

private:
  QIcon m_null_icon;
  QIcon m_file_icon;
  QIcon m_folder_icon;

  QFileIconProvider m_file_icon_provider;
  QMimeDatabase m_db;
  static std::map<QString, QIcon> m_icon_cache;
};

std::map<QString, QIcon> cache_file_icon_provider::m_icon_cache;


// ----------------------------------------------------------------------
//  Class file_dock_widget

files_dock_widget::files_dock_widget (QWidget *p)
  : octave_dock_widget ("FilesDockWidget", p)
{
  set_title (tr ("File Browser"));
  setToolTip (tr ("Browse your files"));

  QTabWidget *files_tab_widget = new QTabWidget (this);
  files_tab_widget->setTabPosition (QTabWidget::South);

  setWidget (files_tab_widget);

  m_file_browser = new file_system_browser (files_tab_widget);
  m_editor_files = new editor_files_browser (files_tab_widget);

  files_tab_widget->addTab (m_file_browser, tr ("File System"));
  files_tab_widget->addTab (m_editor_files, tr ("Editor Files"));

#if ! defined (HAVE_QSCINTILLA)
  files_tab_widget->tabBar ()->hide ();
#endif

  if (! p)
    make_window ();
}

void
files_dock_widget::save_settings ()
{
  m_file_browser->save_settings ();
  m_editor_files->save_settings ();

  gui_settings settings;

  QTabWidget *tab_widget = findChild<QTabWidget *>();
  if (tab_widget)
    {
      int tab = tab_widget->currentIndex ();
      settings.setValue (fb_active_tab.settings_key (), tab);
    }

  octave_dock_widget::save_settings ();
}

void
files_dock_widget::notice_settings ()
{
  gui_settings settings;

  QTabWidget *tab_widget = findChild<QTabWidget *>();
  if (tab_widget)
    {
#if defined (HAVE_QSCINTILLA)
      int tab = settings.value (fb_active_tab.settings_key ()).toInt ();
      tab_widget->setCurrentIndex (tab);
#else
      tab_widget->setCurrentIndex (tab_widget->indexOf (m_file_browser));
#endif
    }

  m_file_browser->notice_settings ();
  m_editor_files->notice_settings ();
}



// ----------------------------------------------------------------------
//  Class file_system_browser

file_system_browser::file_system_browser (QWidget *p)
  : QWidget (p),
    m_first (true),
    m_header_settings_only (false)
{
  m_sig_mapper = nullptr;

  m_columns_shown = QStringList ();
  m_columns_shown.append (tr ("File size"));
  m_columns_shown.append (tr ("File type"));
  m_columns_shown.append (tr ("Date modified"));
  m_columns_shown.append (tr ("Show hidden"));
  m_columns_shown.append (tr ("Alternating row colors"));

  m_columns_shown_keys = QStringList ();
  m_columns_shown_keys.append (fb_show_size.settings_key ());
  m_columns_shown_keys.append (fb_show_type.settings_key ());
  m_columns_shown_keys.append (fb_show_date.settings_key ());
  m_columns_shown_keys.append (fb_show_hidden.settings_key ());
  m_columns_shown_keys.append (fb_show_altcol.settings_key ());

  m_columns_shown_defs = QList <QVariant> ();
  m_columns_shown_defs.append (fb_show_size.def ());
  m_columns_shown_defs.append (fb_show_type.def ());
  m_columns_shown_defs.append (fb_show_date.def ());
  m_columns_shown_defs.append (fb_show_hidden.def ());
  m_columns_shown_defs.append (fb_show_altcol.def ());

  // Create a toolbar
  m_navigation_tool_bar = new QToolBar ("", this);
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

  gui_settings settings;

  QAction *directory_up_action
    = new QAction (settings.icon ("folder-up", false, "go-up"), "",
                   m_navigation_tool_bar);
  directory_up_action->setToolTip (tr ("One directory up"));

  m_sync_browser_directory_action
    = new QAction (settings.icon ("go-first"), tr ("Show Octave directory"),
                   m_navigation_tool_bar);
  m_sync_browser_directory_action->setToolTip (tr ("Go to current Octave directory"));
  m_sync_browser_directory_action->setEnabled (false);

  m_sync_octave_directory_action
    = new QAction (settings.icon ("go-last"), tr ("Set Octave directory"),
                   m_navigation_tool_bar);
  m_sync_octave_directory_action->setToolTip (tr ("Set Octave directory to current browser directory"));
  m_sync_octave_directory_action->setEnabled (false);

  QToolButton *popdown_button = new QToolButton ();
  popdown_button->setToolTip (tr ("Actions on current directory"));
  QMenu *popdown_menu = new QMenu ();
  popdown_menu->addAction (settings.icon ("user-home"),
                           tr ("Show Home Directory"), this,
                           SLOT (popdownmenu_home (bool)));
  popdown_menu->addAction (m_sync_browser_directory_action);
  popdown_menu->addAction (m_sync_octave_directory_action);
  popdown_button->setMenu (popdown_menu);
  popdown_button->setPopupMode (QToolButton::InstantPopup);
  popdown_button->setDefaultAction
    (new QAction (settings.icon ("folder-settings", false,
                                 "applications-system"),
                  "", m_navigation_tool_bar));

  popdown_menu->addSeparator ();
  popdown_menu->addAction (settings.icon ("folder"),
                           tr ("Set Browser Directory..."),
                           this, &file_system_browser::popdownmenu_search_dir);
  popdown_menu->addSeparator ();
  popdown_menu->addAction (settings.icon ("edit-find"),
                           tr ("Find Files..."),
                           this, &file_system_browser::popdownmenu_findfiles);
  popdown_menu->addSeparator ();
  popdown_menu->addAction (settings.icon ("document-new"),
                           tr ("New File..."),
                           this, &file_system_browser::popdownmenu_newfile);
  popdown_menu->addAction (settings.icon ("folder-new"),
                           tr ("New Directory..."),
                           this, &file_system_browser::popdownmenu_newdir);

  m_navigation_tool_bar->addWidget (m_current_directory);
  m_navigation_tool_bar->addAction (directory_up_action);
  m_navigation_tool_bar->addWidget (popdown_button);

  connect (directory_up_action, &QAction::triggered,
           this, &file_system_browser::change_directory_up);
  connect (m_sync_octave_directory_action, &QAction::triggered,
           this, &file_system_browser::do_sync_octave_directory);
  connect (m_sync_browser_directory_action, &QAction::triggered,
           this, &file_system_browser::do_sync_browser_directory);

  // Create the QFileSystemModel starting in the desired directory
  QDir startup_dir;  // take current dir

  if (settings.bool_value (fb_restore_last_dir))
    {
      // restore last dir from previous session
      QStringList last_dirs
        = settings.value (fb_mru_list.settings_key ()).toStringList ();
      if (last_dirs.length () > 0)
        startup_dir = QDir (last_dirs.at (0));  // last dir in previous session
    }
  else if (! settings.string_value (fb_startup_dir).isEmpty ())
    {
      // do not restore but there is a startup dir configured
      startup_dir = QDir (settings.value (fb_startup_dir.settings_key ()).toString ());
    }

  if (! startup_dir.exists ())
    {
      // the configured startup dir does not exist, take actual one
      startup_dir = QDir ();
    }

  m_file_system_model = new file_system_model (this);
  m_file_system_model->setResolveSymlinks (false);
  m_file_system_model->setFilter
    (QDir::System | QDir::NoDotAndDotDot | QDir::AllEntries);
  QModelIndex rootPathIndex
    = m_file_system_model->setRootPath (startup_dir.absolutePath ());

  m_file_icon_provider = new cache_file_icon_provider ();
  m_file_system_model->setIconProvider (m_file_icon_provider);

  // Attach the model to the QTreeView and set the root index
  m_file_tree_view = new FileTreeViewer (this);
  m_file_tree_view->setSelectionMode (QAbstractItemView::ExtendedSelection);
  m_file_tree_view->setModel (m_file_system_model);
  m_file_tree_view->setRootIndex (rootPathIndex);
  m_file_tree_view->setSortingEnabled (true);
  m_file_tree_view->setAlternatingRowColors (true);
  m_file_tree_view->setAnimated (true);
  m_file_tree_view->setToolTip (tr ("Double-click to open file/folder, right click for alternatives"));

  // allow renaming directly in the tree view with
  // m_file_tree_view->edit (index)
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
  m_rename_action->setShortcutContext (Qt::WidgetWithChildrenShortcut);
  connect (m_rename_action, &QAction::triggered, this,
           &file_system_browser::contextmenu_rename);
  addAction (m_rename_action);

  // get sort column and order as well as column state (order and width)

  m_file_tree_view->sortByColumn
    (settings.int_value (fb_sort_column),
     static_cast<Qt::SortOrder> (settings.uint_value (fb_sort_order)));
     // FIXME: use value<Qt::SortOrder> instead of static cast after
     //        dropping support of Qt 5.4

  // Set header properties for sorting
  m_file_tree_view->header ()->setSectionsClickable (true);
  m_file_tree_view->header ()->setSectionsMovable (true);
  m_file_tree_view->header ()->setSortIndicatorShown (true);

  QStringList mru_dirs =
    settings.value (fb_mru_list.settings_key ()).toStringList ();
  m_current_directory->addItems (mru_dirs);

  m_current_directory->setEditText
    (m_file_system_model->fileInfo (rootPathIndex). absoluteFilePath ());

  connect (m_file_tree_view, &FileTreeViewer::activated,
           this, &file_system_browser::item_double_clicked);

  // add context menu to tree_view
  m_file_tree_view->setContextMenuPolicy (Qt::CustomContextMenu);
  connect (m_file_tree_view, &FileTreeViewer::customContextMenuRequested,
           this, &file_system_browser::contextmenu_requested);

  m_file_tree_view->header ()->setContextMenuPolicy (Qt::CustomContextMenu);
  connect (m_file_tree_view->header (),
           &QHeaderView::customContextMenuRequested,
           this, &file_system_browser::headercontextmenu_requested);

  // Layout the widgets vertically with the toolbar on top
  QVBoxLayout *vbox_layout = new QVBoxLayout ();
  vbox_layout->setSpacing (0);
  vbox_layout->addWidget (m_navigation_tool_bar);
  vbox_layout->addWidget (m_file_tree_view);
  vbox_layout->setContentsMargins (1, 1, 1, 1);

  setLayout (vbox_layout);

  // FIXME: Add right-click contextual menus for copying, pasting,
  //        deleting files (and others).

  connect (m_current_directory->lineEdit (), &QLineEdit::returnPressed,
           this, &file_system_browser::accept_directory_line_edit);

#if QT_VERSION >= QT_VERSION_CHECK(6, 0, 0)
  connect (m_current_directory, &QComboBox::textActivated,
           this, &file_system_browser::set_current_directory);
#else
  connect (m_current_directory, SIGNAL (activated (const QString&)),
           this, SLOT (set_current_directory (const QString&)));
#endif

  QCompleter *completer = new QCompleter (m_file_system_model, this);
  m_current_directory->setCompleter (completer);

  setFocusProxy (m_current_directory);

  m_sync_octave_dir = true;   // default, overwritten with notice_settings ()
  m_octave_dir = "";
}

void
file_system_browser::restore_header_state ()
{
  gui_settings settings;

  if (settings.contains (fb_column_state.settings_key ()))
    m_file_tree_view->header ()->restoreState
      (settings.value (fb_column_state.settings_key ()).toByteArray ());
}

void
file_system_browser::save_settings ()
{
  gui_settings settings;

  int sort_column = m_file_tree_view->header ()->sortIndicatorSection ();
  Qt::SortOrder sort_order = m_file_tree_view->header ()->sortIndicatorOrder ();
  settings.setValue (fb_sort_column.settings_key (), sort_column);
  settings.setValue (fb_sort_order.settings_key (), sort_order);
  settings.setValue (fb_column_state.settings_key (),
                     m_file_tree_view->header ()->saveState ());

  QStringList dirs;
  for (int i=0; i< m_current_directory->count (); i++)
    {
      dirs.append (m_current_directory->itemText (i));
    }
  settings.setValue (fb_mru_list.settings_key (), dirs);

  settings.sync ();

  if (m_sig_mapper)
    delete m_sig_mapper;
}

void
file_system_browser::item_double_clicked (const QModelIndex& index)
{
  // Retrieve the file info associated with the model index.
  QFileInfo fileInfo = m_file_system_model->fileInfo (index);
  set_current_directory (fileInfo.absoluteFilePath ());
}

void
file_system_browser::set_current_directory (const QString& dir)
{
  display_directory (dir);
}

void
file_system_browser::accept_directory_line_edit ()
{
  display_directory (m_current_directory->currentText ());
}

void
file_system_browser::change_directory_up ()
{
  QDir dir
    = QDir (m_file_system_model->filePath (m_file_tree_view->rootIndex ()));

  dir.cdUp ();
  display_directory (dir.absolutePath ());
}

void
file_system_browser::do_sync_octave_directory ()
{
  QDir dir
    = QDir (m_file_system_model->filePath (m_file_tree_view->rootIndex ()));

  Q_EMIT displayed_directory_changed (dir.absolutePath ());
}

void
file_system_browser::do_sync_browser_directory ()
{
  display_directory (m_octave_dir, false); // false: no sync of octave dir
}

void
file_system_browser::update_octave_directory (const QString& dir)
{
  m_octave_dir = dir;
  if (m_sync_octave_dir)
    display_directory (m_octave_dir, false); // false: no sync of octave dir
}

void
file_system_browser::display_directory (const QString& dir,
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
          combobox_insert_current_item (m_current_directory,
                                        fileInfo.absoluteFilePath ());
        }
      else
        {
          QString abs_fname = fileInfo.absoluteFilePath ();

          QString suffix = fileInfo.suffix ().toLower ();

          gui_settings settings;

          QString ext = settings.string_value (fb_txt_file_ext);
#if defined (HAVE_QT_SPLITBEHAVIOR_ENUM)
          QStringList extensions = ext.split (";", Qt::SkipEmptyParts);
#else
          QStringList extensions = ext.split (";", QString::SkipEmptyParts);
#endif
          if (QFile::exists (abs_fname))
            {
              if (extensions.contains (suffix))
                Q_EMIT open_file (fileInfo.absoluteFilePath ());
              else
                Q_EMIT open_any_signal (abs_fname);
            }
        }
    }
}

void
file_system_browser::open_item_in_app (const QModelIndex& index)
{
  // Retrieve the file info associated with the model index.
  QFileInfo fileInfo = m_file_system_model->fileInfo (index);

  QString file = fileInfo.absoluteFilePath ();

  QDesktopServices::openUrl (QUrl::fromLocalFile (file));
}

void
file_system_browser::toggle_header (int col)
{
  gui_settings settings;

  if (col <= 2)
    {
      // Toggle column visibility
      m_file_tree_view->setColumnHidden (col + 1,
                                ! m_file_tree_view->isColumnHidden (col +1));
    }
  else
    {
      // Other actions depending on new settings
      QString key = m_columns_shown_keys.at (col);
      bool active = settings.value (key, false).toBool ();

      // Toggle the settings in the settings file
      settings.setValue (key, ! active);
      settings.sync ();

      // Reload header related settings only
      m_header_settings_only = true;
      notice_settings ();
    }
}

void
file_system_browser::headercontextmenu_requested (const QPoint& mpos)
{
  QMenu menu (this);

  if (m_sig_mapper)
    delete m_sig_mapper;
  m_sig_mapper = new QSignalMapper (this);

  gui_settings settings;

  for (int i = 0; i < m_columns_shown.size (); i++)
    {
      QAction *action = menu.addAction (m_columns_shown.at (i),
                                        m_sig_mapper, SLOT (map ()));
      m_sig_mapper->setMapping (action, i);
      action->setCheckable (true);
      if (i <= 2)
        {
          // Column visibility
          action->setChecked (! m_file_tree_view->isColumnHidden (i +1));
        }
      else
        {
          // Other actions depending on settings
          action->setChecked (settings.value (m_columns_shown_keys.at (i),
                              m_columns_shown_defs.at (i)).toBool ());
        }
    }

#if QT_VERSION >= QT_VERSION_CHECK(6, 0, 0)
  connect (m_sig_mapper, &QSignalMapper::mappedInt,
           this, &file_system_browser::toggle_header);
#else
  connect (m_sig_mapper, SIGNAL (mapped (int)),
           this, SLOT (toggle_header (int)));
#endif

  menu.exec (m_file_tree_view->mapToGlobal (mpos));
}

void
file_system_browser::contextmenu_requested (const QPoint& mpos)
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

      gui_settings settings;

      // construct the context menu depending on item
      menu.addAction (settings.icon ("document-open"), tr ("Open"),
                      this, &file_system_browser::contextmenu_open);

      if (info.isDir ())
        {
          menu.addAction (tr ("Open in System File Explorer"),
                          this, &file_system_browser::contextmenu_open_in_app);
        }

      if (info.isFile ())
        menu.addAction (tr ("Open in Text Editor"),
                        this, &file_system_browser::contextmenu_open_in_editor);

      menu.addAction (tr ("Copy Selection to Clipboard"),
                      this, &file_system_browser::contextmenu_copy_selection);

      if (info.isFile () && info.suffix () == "m")
        menu.addAction (settings.icon ("system-run"), tr ("Run"),
                        this, &file_system_browser::contextmenu_run);

      if (info.isFile ())
        menu.addAction (tr ("Load Data"),
                        this, &file_system_browser::contextmenu_load);

      if (info.isDir ())
        {
          menu.addSeparator ();
          menu.addAction (settings.icon ("go-first"), tr ("Set Current Directory"),
                          this, &file_system_browser::contextmenu_setcurrentdir);

          QMenu *add_path_menu = menu.addMenu (tr ("Add to Path"));

          add_path_menu->addAction (tr ("Selected Directories"),
                                    this, [this] (bool checked) { contextmenu_add_to_path (checked); });
          add_path_menu->addAction (tr ("Selected Directories and Subdirectories"),
                                    this, &file_system_browser::contextmenu_add_to_path_subdirs);

          QMenu *rm_path_menu = menu.addMenu (tr ("Remove from Path"));

          rm_path_menu->addAction (tr ("Selected Directories"),
                                   this, &file_system_browser::contextmenu_rm_from_path);
          rm_path_menu->addAction (tr ("Selected Directories and Subdirectories"),
                                   this, &file_system_browser::contextmenu_rm_from_path_subdirs);

          menu.addSeparator ();

          menu.addAction (settings.icon ("edit-find"), tr ("Find Files..."),
                          this, &file_system_browser::contextmenu_findfiles);
        }

      menu.addSeparator ();
      menu.addAction (m_rename_action);
      menu.addAction (settings.icon ("edit-delete"), tr ("Delete..."),
                      this, &file_system_browser::contextmenu_delete);

      if (info.isDir ())
        {
          menu.addSeparator ();
          menu.addAction (settings.icon ("document-new"), tr ("New File..."),
                          this, &file_system_browser::contextmenu_newfile);
          menu.addAction (settings.icon ("folder-new"), tr ("New Directory..."),
                          this, &file_system_browser::contextmenu_newdir);
        }

      // show the menu
      menu.exec (m_file_tree_view->mapToGlobal (mpos));

    }
}

void
file_system_browser::contextmenu_open (bool)
{

  QItemSelectionModel *m = m_file_tree_view->selectionModel ();
  QModelIndexList rows = m->selectedRows ();

  for (const auto& it : rows)
    {
      QFileInfo file = m_file_system_model->fileInfo (it);
      if (file.exists ())
        display_directory (file.absoluteFilePath ());
    }
}

void
file_system_browser::contextmenu_open_in_editor (bool)
{

  QItemSelectionModel *m = m_file_tree_view->selectionModel ();
  QModelIndexList rows = m->selectedRows ();

  for (const auto& it : rows)
    {
      QFileInfo file = m_file_system_model->fileInfo (it);
      if (file.exists ())
        Q_EMIT open_file (file.absoluteFilePath ());
    }
}

void
file_system_browser::contextmenu_open_in_app (bool)
{
  QItemSelectionModel *m = m_file_tree_view->selectionModel ();
  QModelIndexList rows = m->selectedRows ();

  for (const auto& it : rows)
    open_item_in_app (it);
}

void
file_system_browser::contextmenu_copy_selection (bool)
{
  QItemSelectionModel *m = m_file_tree_view->selectionModel ();
  QModelIndexList rows = m->selectedRows ();

  QStringList selection;

  for (const auto& it : rows)
    {
      QFileInfo info = m_file_system_model->fileInfo (it);
      selection << info.fileName ();
    }

  QClipboard *clipboard = QApplication::clipboard ();

  clipboard->setText (selection.join ("\n"));
}

void
file_system_browser::contextmenu_load (bool)
{
  QItemSelectionModel *m = m_file_tree_view->selectionModel ();
  QModelIndexList rows = m->selectedRows ();

  if (rows.size () > 0)
    {
      QModelIndex index = rows[0];

      QFileInfo info = m_file_system_model->fileInfo (index);

      Q_EMIT load_file_signal (info.fileName ());
    }
}

void
file_system_browser::contextmenu_run (bool)
{
  QItemSelectionModel *m = m_file_tree_view->selectionModel ();
  QModelIndexList rows = m->selectedRows ();

  if (rows.size () > 0)
    {
      QModelIndex index = rows[0];

      QFileInfo info = m_file_system_model->fileInfo (index);
      Q_EMIT run_file_signal (info, ED_RUN_FILE);
    }
}

void
file_system_browser::contextmenu_rename (bool)
{
  QItemSelectionModel *m = m_file_tree_view->selectionModel ();
  QModelIndexList rows = m->selectedRows ();
  if (rows.size () > 0)
    {
      QModelIndex index = rows[0];
      m_file_tree_view->edit (index);
    }
}

void
file_system_browser::contextmenu_delete (bool)
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
              QList<QFileInfo> fileLst
                = path.entryInfoList (QDir::Hidden | QDir::AllEntries
                                      | QDir::NoDotAndDotDot | QDir::System);

              if (fileLst.count () != 0)
                QMessageBox::warning (this, tr ("Delete file/directory"),
                                      tr ("Can not delete a directory that is not empty"));
              else
                m_file_system_model->rmdir (index);
            }
          else
            {
              // Close the file in the editor if open
              Q_EMIT file_remove_signal (info.filePath (), QString ());
              // Remove the file.
              bool st = m_file_system_model->remove (index);
              if (! st)
                {
                  QMessageBox::warning (this, tr ("Deletion error"),
                                        tr ("Could not delete file \"%1\".").
                                        arg (info.filePath ()));
                  // Reload the old file
                }
              Q_EMIT file_renamed_signal (st);
            }

          m_file_system_model->revert ();

        }
    }
}

// Get the currently selected files/dirs and return their file info
// in a list.
QList<QFileInfo>
file_system_browser::get_selected_items_info (bool dir)
{
  QItemSelectionModel *m = m_file_tree_view->selectionModel ();
  QModelIndexList rows = m->selectedRows ();

  QList<QFileInfo> infos;

  for (const auto& idx : rows)
    {
      QFileInfo info = m_file_system_model->fileInfo (idx);

      if (info.exists () &&
          ((dir & info.isDir ()) || (! dir && info.isFile ())))
        infos.append (info);
    }

  return infos;
}

void
file_system_browser::contextmenu_newfile (bool)
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

void
file_system_browser::contextmenu_newdir (bool)
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

void
file_system_browser::contextmenu_setcurrentdir (bool)
{
  QList<QFileInfo> infos = get_selected_items_info (true);

  if (infos.length () > 0 && infos.first ().isDir ())
    process_set_current_dir (infos.first ().absoluteFilePath ());
}

void
file_system_browser::contextmenu_add_to_path (bool, bool rm, bool subdirs)
{
  QList<QFileInfo> infos = get_selected_items_info (true);

  QStringList dir_list;

  for (int i = 0; i < infos.length (); i++)
    dir_list.append (infos.at (i).absoluteFilePath ());

  if (infos.length () > 0)
    Q_EMIT modify_path_signal (dir_list, rm, subdirs);
}

void
file_system_browser::contextmenu_add_to_path_subdirs (bool)
{
  contextmenu_add_to_path (true, false, true);
}

void
file_system_browser::contextmenu_rm_from_path (bool)
{
  contextmenu_add_to_path (true, true, false);
}

void
file_system_browser::contextmenu_rm_from_path_subdirs (bool)
{
  contextmenu_add_to_path (true, true, true);
}

void
file_system_browser::contextmenu_findfiles (bool)
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

void
file_system_browser::notice_settings ()
{
  gui_settings settings;

  if (m_first)
    m_first = false;
  else
    {
      // Save current state in case some settings are messing up the state
      settings.setValue (fb_column_state.settings_key (),
                         m_file_tree_view->header ()->saveState ());
      settings.sync ();
    }

  QDir::Filters current_filter = m_file_system_model->filter ();
  if (settings.value (m_columns_shown_keys.at (3), false).toBool ())
    m_file_system_model->setFilter (current_filter | QDir::Hidden);
  else
    m_file_system_model->setFilter (current_filter & (~QDir::Hidden));

  m_file_tree_view->setAlternatingRowColors
    (settings.value (m_columns_shown_keys.at (4), true).toBool ());
  m_file_tree_view->setModel (m_file_system_model);

  // Done if only settings changed by toggle:header were requested
  if (m_header_settings_only)
    {
      m_header_settings_only = false;
      return;
    }

  int size_idx = settings.int_value (global_icon_size);
  size_idx = (size_idx > 0) - (size_idx < 0) + 1;  // Make valid index from 0 to 2

  QStyle *st = style ();
  int icon_size = st->pixelMetric (global_icon_sizes[size_idx]);
  m_navigation_tool_bar->setIconSize (QSize (icon_size, icon_size));

  // enable the buttons to sync octave/browser dir
  // only if this is not done by default
  m_sync_octave_dir
    = settings.bool_value (fb_sync_octdir);
  m_sync_octave_directory_action->setEnabled (! m_sync_octave_dir);
  m_sync_browser_directory_action->setEnabled (! m_sync_octave_dir);

  // If m_sync_octave_dir is enabled, then we want the file browser to
  // update to match the current working directory of the
  // interpreter.  We don't want to queue any signal to change the
  // interpreter's current working directory.  In this case, we just
  // want the GUI to match the state of the interpreter.

  if (m_sync_octave_dir)
    do_sync_browser_directory ();

  // Initialize column order, visibility and width of the file browser. From this post,
  // https://www.qtcentre.org/threads/26675-QTableView-saving-restoring-columns-widths
  // this might fail if done directly after other actions. This effect shows
  // up in the GUI since Qt 6.6.x. As a solution, the following timer ensures
  // that the header is restored when the event loop is idle.

  QTimer::singleShot (0, this, SLOT(restore_header_state ()));
}

void
file_system_browser::popdownmenu_home (bool)
{
  QString dir = QString::fromStdString (sys::env::get_home_directory ());

  if (dir.isEmpty ())
    dir = QDir::homePath ();

  set_current_directory (dir);
}

void
file_system_browser::popdownmenu_search_dir (bool)
{
  // FIXME: Remove, if for all common KDE versions (bug #54607) is resolved.
  int opts = QFileDialog::ShowDirsOnly;

  gui_settings settings;

  if (! settings.bool_value (global_use_native_dialogs))
    opts |= QFileDialog::DontUseNativeDialog;

  QString dir = QFileDialog::getExistingDirectory (this,
                  tr ("Set directory of file browser"),
                  m_file_system_model->rootPath (),
                  QFileDialog::Option (opts));
  set_current_directory (dir);
}

void
file_system_browser::popdownmenu_findfiles (bool)
{
  process_find_files (m_file_system_model->rootPath ());
}

void
file_system_browser::popdownmenu_newdir (bool)
{
  process_new_dir (m_file_system_model->rootPath ());
}

void
file_system_browser::popdownmenu_newfile (bool)
{
  process_new_file (m_file_system_model->rootPath ());
}

void
file_system_browser::process_new_file (const QString& parent_dir)
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

void
file_system_browser::process_new_dir (const QString& parent_dir)
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

void
file_system_browser::process_set_current_dir (const QString& dir)
{
  Q_EMIT displayed_directory_changed (dir);
}

void
file_system_browser::process_find_files (const QString& dir)
{
  Q_EMIT find_files_signal (dir);
}

void
file_system_browser::copyClipboard ()
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

void
file_system_browser::pasteClipboard ()
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

void
file_system_browser::selectAll ()
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



// ----------------------------------------------------------------------
//  Class for the editor files browser

editor_files_browser::editor_files_browser (QWidget *p)
  : QWidget (p)
{
  m_editor_files_model = new QStandardItemModel (this);
  QStringList header_text;
  header_text.append (tr ("Open Editor Files"));

  m_editor_files_model->setHorizontalHeaderLabels (header_text);

  m_file_icon_provider = new cache_file_icon_provider ();

  m_parent_item = m_editor_files_model->invisibleRootItem();

  m_home_dir = QString::fromStdString (sys::env::get_home_directory ());

  // Attach the model to the QTreeView and set the root index
  m_editor_tree_view = new QTreeView (this);
  m_editor_tree_view->setModel (m_editor_files_model);
  m_editor_tree_view->setTextElideMode (Qt::ElideMiddle);
  m_editor_tree_view->setSortingEnabled (true);
  m_editor_tree_view->setAlternatingRowColors (true);
  m_editor_tree_view->setAnimated (false);
  m_editor_tree_view->setToolTip (tr ("Click to focus file in editor"));

  connect (m_editor_tree_view, &QTreeView::clicked, this,
           &editor_files_browser::clicked);

  // add context menu to tree_view
  m_editor_tree_view->setContextMenuPolicy (Qt::CustomContextMenu);
  connect (m_editor_tree_view, &editor_files_browser::customContextMenuRequested,
           this, &editor_files_browser::contextmenu_requested);

  // the layout
  QVBoxLayout *vbox_layout = new QVBoxLayout ();
  vbox_layout->setSpacing (0);
  vbox_layout->addWidget (m_editor_tree_view);
  vbox_layout->setContentsMargins (1, 1, 1, 1);

  setLayout (vbox_layout);
  QSizePolicy sizePol (QSizePolicy::Expanding, QSizePolicy::Preferred);
}


// -------------------------------------------------------------------------

QFileInfo editor_files_browser::get_file_info_from_item (QStandardItem *item)
{
  if (! item)
    return QFileInfo ();

  QString file;
  QString dir = item->text ();

  if (! item->hasChildren ())
    {
      // item is a file
      file = dir;
      dir = item->parent ()->text ();
    }

  dir.replace (QRegularExpression ("^~"), m_home_dir);

  return QFileInfo (QDir (dir), file);
}


// -------------------------------------------------------------------------

QStringList editor_files_browser::get_dir_file_from_string (const QString& path)
{
  QStringList dir_and_file;

  if (! path.isEmpty ())
    {
      QFileInfo f_info (path);
      QString dir = f_info.absoluteDir ().absolutePath ();
      dir.replace (QRegularExpression ("^" + m_home_dir), "~");

      dir_and_file.append (dir);
      dir_and_file.append (f_info.fileName ());
    }

  return dir_and_file;
}


// -------------------------------------------------------------------------

void editor_files_browser::contextmenu_requested (const QPoint& mpos)
{
  QMenu menu (this);

  QModelIndex index = m_editor_tree_view->indexAt (mpos);

  if (index.isValid ())
    {
      QItemSelectionModel *m = m_editor_tree_view->selectionModel ();
      QModelIndexList sel = m->selectedRows ();

       m->setCurrentIndex (index,
                           QItemSelectionModel::Clear
                           | QItemSelectionModel::Select
                           | QItemSelectionModel::Rows);

      QModelIndexList rows = m->selectedRows ();

      if (rows.isEmpty ())
        return;

      QStandardItem *item = m_editor_files_model->itemFromIndex (rows[0]);
      QFileInfo info = get_file_info_from_item (item);

      gui_settings settings;

      // close actions
      menu.addAction (settings.icon ("window-close"), tr ("&Close"),
                      this, &editor_files_browser::ctx_menu_close);

      menu.addAction (settings.icon ("window-close"), tr ("Close &All"),
                      this, &editor_files_browser::ctx_menu_close_all);

      // run action
      if (info.isFile () && info.suffix () == "m")
        {
          menu.addSeparator ();
          menu.addAction (settings.icon ("system-run"), tr ("Run"),
                          this, &editor_files_browser::ctx_menu_run);
        }

      // set current dir, if directory
      if (info.isDir ())
        {
          menu.addSeparator ();
          menu.addAction (settings.icon ("go-first"), tr ("Set Current &Directory"),
                          this, &editor_files_browser::ctx_menu_setcurrentdir);
        }

      menu.exec (m_editor_tree_view->mapToGlobal (mpos));
    }
}


// -------------------------------------------------------------------------

void editor_files_browser::clicked (const QModelIndex& index)
{
  QStandardItem *item = m_editor_files_model->itemFromIndex (index);

  if (item->hasChildren ())
    return;     // directory clicked, nothing to do

  QFileInfo file = get_file_info_from_item (item);
  Q_EMIT focus_editor_file_signal (file.absoluteFilePath ());
}


// -------------------------------------------------------------------------

void editor_files_browser::add_editor_file (const QString& file)
{
  QStringList f = get_dir_file_from_string (file);

  if (f.isEmpty ())
    return;

  QString dir_name = f[0];
  QString file_name = f[1];

  QStandardItem *dir_item;

  QList <QStandardItem *> dir_items = m_editor_files_model->findItems (dir_name);
  if (dir_items.isEmpty ())
    {
      // The directory does not yet exist
      dir_item =
          new QStandardItem (m_file_icon_provider->icon (QFileIconProvider::Folder), dir_name);
      dir_item->setEditable (false);
      m_parent_item->appendRow(dir_item);
    }
  else
    dir_item = dir_items[0];  // The directory does already exist

  m_editor_tree_view->expand (dir_item->index ());  // expand file's directory

  QStandardItem *file_item =
      new QStandardItem (m_file_icon_provider->icon (QFileIconProvider::File), file_name);
  file_item->setEditable (false);
  dir_item->appendRow(file_item);   // add file item

  m_parent_item->sortChildren (0);
}


// -------------------------------------------------------------------------

void editor_files_browser::rename_editor_file (const QString& file,
                                               const QString& newfile)
{
  if (file.isEmpty ())
    {
      add_editor_file (newfile);
      return;
    }

  QStringList f = get_dir_file_from_string (file);

  if (f.isEmpty ())
    return;

  QString dir_name = f[0];
  QString file_name = f[1];

  QStandardItem *dir_item;

  QList <QStandardItem *> dir_items = m_editor_files_model->findItems (dir_name);
    if (! dir_items.isEmpty ())
    {
      dir_item = dir_items[0];
      int i = 0;
      for (i = 0; i < dir_item->rowCount (); i++)
        {
          if (dir_item->child(i)->text () == file_name)
            {
              if (newfile.isEmpty())
                {
                  dir_item->removeRow (i);
                  if (! dir_item->hasChildren ())
                    {
                      m_parent_item->removeRow (dir_item->row ());
                      break;  // dir_item does point to a valid object anymore
                    }
                }
              else
                {
                  QFileInfo new_f_info (newfile);
                  dir_item->child(i)->setText (new_f_info.fileName ());
                }
            }
        }
    }
}


// -------------------------------------------------------------------------

void editor_files_browser::remove_editor_file (const QString& file)
{
  rename_editor_file (file, QString ());
}


// -------------------------------------------------------------------------

void editor_files_browser::ctx_menu_close (bool)
{
  QItemSelectionModel *m = m_editor_tree_view->selectionModel ();
  QModelIndexList rows = m->selectedRows ();

  if (rows.isEmpty ())
    return;

  QStringList files_to_remove;

  QStandardItem *item = m_editor_files_model->itemFromIndex (rows[0]);
  if (item->hasChildren ())
    {
      for (int i = 0; i < item->rowCount (); i++)
        {
          files_to_remove.append (get_file_info_from_item (item->child (i)).
                                  absoluteFilePath ());
        }
    }
  else
    {
      files_to_remove.append (get_file_info_from_item (item).absoluteFilePath ());
    }

  Q_EMIT close_editor_file_signal (files_to_remove);
}


// -------------------------------------------------------------------------

void editor_files_browser::ctx_menu_close_all (bool)
{
  QStringList files_to_remove;
  QStandardItem *item;

  for (int i = 0; i < m_parent_item->rowCount (); i++)
    {
      item = m_parent_item->child (i);
      for (int j = 0; j < item->rowCount (); j++)
        {
          files_to_remove.append (get_file_info_from_item (item->child (j)).
                                  absoluteFilePath ());
        }
    }

  Q_EMIT close_editor_file_signal (files_to_remove);
}


// -------------------------------------------------------------------------

void editor_files_browser::ctx_menu_run (bool)
{
  QItemSelectionModel *m = m_editor_tree_view->selectionModel ();
  QModelIndexList rows = m->selectedRows ();

  if (rows.isEmpty ())
    return;

  QStandardItem *item = m_editor_files_model->itemFromIndex (rows[0]);
  QFileInfo info = get_file_info_from_item (item);

  Q_EMIT run_file_signal (info, ED_RUN_FILE);
}


// -------------------------------------------------------------------------

void editor_files_browser::ctx_menu_setcurrentdir (bool)
{
  QItemSelectionModel *m = m_editor_tree_view->selectionModel ();
  QModelIndexList rows = m->selectedRows ();

  if (rows.isEmpty ())
    return;

  QStandardItem *item = m_editor_files_model->itemFromIndex (rows[0]);
  QFileInfo info = get_file_info_from_item (item);

  if (info.isDir ())
    Q_EMIT displayed_directory_changed (info.absoluteFilePath ());
}


// -------------------------------------------------------------------------

void editor_files_browser::save_settings ()
{ }


// -------------------------------------------------------------------------

void editor_files_browser::notice_settings ()
{ }


OCTAVE_END_NAMESPACE(octave)
