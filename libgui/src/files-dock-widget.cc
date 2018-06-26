/*

Copyright (C) 2013-2018 John P. Swensen
Copyright (C) 2011-2018 Jacob Dawid

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

*/

#if defined (HAVE_CONFIG_H)
#  include "config.h"
#endif

#include "resource-manager.h"
#include "files-dock-widget.h"

#include <QApplication>
#include <QClipboard>
#include <QFileInfo>
#include <QCompleter>
#include <QProcess>
#include <QDebug>
#include <QHeaderView>
#include <QLineEdit>
#include <QSizePolicy>
#include <QMenu>
#include <QInputDialog>
#include <QMessageBox>
#include <QToolButton>
#include <QUrl>
#include <QDesktopServices>
#include <QFileDialog>

#include "load-save.h"
#include "oct-env.h"

namespace octave
{
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

  files_dock_widget::files_dock_widget (QWidget *p)
    : octave_dock_widget ("FilesDockWidget", p)
  {
    setWindowIcon (QIcon (":/actions/icons/logo.png"));
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
    m_columns_shown_keys.append ("filesdockwidget/showFileSize");
    m_columns_shown_keys.append ("filesdockwidget/showFileType");
    m_columns_shown_keys.append ("filesdockwidget/showLastModified");
    m_columns_shown_keys.append ("filesdockwidget/showHiddenFiles");
    m_columns_shown_keys.append ("filesdockwidget/useAlternatingRowColors");

    QWidget *container = new QWidget (this);

    setWidget (container);

    connect (this, SIGNAL (open_file (const QString&)),
             main_win (), SLOT (open_file (const QString&)));

    connect (this, SIGNAL (displayed_directory_changed (const QString&)),
             main_win (),
             SLOT (set_current_working_directory (const QString&)));

    // Create a toolbar
    m_navigation_tool_bar = new QToolBar ("", container);
    m_navigation_tool_bar->setAllowedAreas (Qt::TopToolBarArea);
    m_navigation_tool_bar->setMovable (false);

    m_current_directory = new QComboBox (m_navigation_tool_bar);
    m_current_directory->setToolTip (tr ("Enter the path or filename"));
    m_current_directory->setEditable (true);
    m_current_directory->setMaxCount (MaxMRUDirs);
    m_current_directory->setInsertPolicy (QComboBox::NoInsert);
    m_current_directory->setSizeAdjustPolicy (
                                              QComboBox::AdjustToMinimumContentsLengthWithIcon);
    QSizePolicy sizePol (QSizePolicy::Expanding, QSizePolicy::Preferred);
    m_current_directory->setSizePolicy (sizePol);

    QAction *directory_up_action = new QAction (resource_manager::icon ("go-up"),
                                                "", m_navigation_tool_bar);
    directory_up_action->setToolTip (tr ("One directory up"));

    m_sync_browser_directory_action
      = new QAction (resource_manager::icon ("go-first"),
                     tr ("Show Octave directory"), m_navigation_tool_bar);
    m_sync_browser_directory_action->setToolTip (
                                                 tr ("Go to current Octave directory"));
    m_sync_browser_directory_action->setEnabled ("false");

    m_sync_octave_directory_action
      = new QAction (resource_manager::icon ("go-last"),
                     tr ("Set Octave directory"), m_navigation_tool_bar);
    m_sync_octave_directory_action->setToolTip (
                                                tr ("Set Octave directory to current browser directory"));
    m_sync_octave_directory_action->setEnabled ("false");

    QToolButton *popdown_button = new QToolButton ();
    popdown_button->setToolTip (tr ("Actions on current directory"));
    QMenu *popdown_menu = new QMenu ();
    popdown_menu->addAction (resource_manager::icon ("user-home"),
                             tr ("Show Home Directory"),
                             this, SLOT (popdownmenu_home (bool)));
    popdown_menu->addAction (m_sync_browser_directory_action);
    popdown_menu->addAction (m_sync_octave_directory_action);
    popdown_button->setMenu (popdown_menu);
    popdown_button->setPopupMode (QToolButton::InstantPopup);
    popdown_button->setDefaultAction (new QAction (
                                                   resource_manager::icon ("applications-system"), "",
                                                   m_navigation_tool_bar));

    popdown_menu->addSeparator ();
    popdown_menu->addAction (resource_manager::icon ("folder"),
                             tr ("Set Browser Directory..."),
                             this, SLOT (popdownmenu_search_dir (bool)));
    popdown_menu->addSeparator ();
    popdown_menu->addAction (resource_manager::icon ("edit-find"),
                             tr ("Find Files..."),
                             this, SLOT (popdownmenu_findfiles (bool)));
    popdown_menu->addSeparator ();
    popdown_menu->addAction (resource_manager::icon ("document-new"),
                             tr ("New File..."),
                             this, SLOT (popdownmenu_newfile (bool)));
    popdown_menu->addAction (resource_manager::icon ("folder-new"),
                             tr ("New Directory..."),
                             this, SLOT (popdownmenu_newdir (bool)));

    m_navigation_tool_bar->addWidget (m_current_directory);
    m_navigation_tool_bar->addAction (directory_up_action);
    m_navigation_tool_bar->addWidget (popdown_button);

    connect (directory_up_action, SIGNAL (triggered ()), this,
             SLOT (change_directory_up ()));
    connect (m_sync_octave_directory_action, SIGNAL (triggered ()), this,
             SLOT (do_sync_octave_directory ()));
    connect (m_sync_browser_directory_action, SIGNAL (triggered ()), this,
             SLOT (do_sync_browser_directory ()));

    QSettings *settings = resource_manager::get_settings ();
    // FIXME: what should happen if settings is 0?

    // Create the QFileSystemModel starting in the desired directory
    QDir startup_dir;  // take current dir

    if (settings->value ("filesdockwidget/restore_last_dir",false).toBool ())
      {
        // restore last dir from previous session
        QStringList last_dirs
          = settings->value ("filesdockwidget/mru_dir_list").toStringList ();
        if (last_dirs.length () > 0)
          startup_dir = QDir (last_dirs.at (0));  // last dir in previous session
      }
    else if (! settings->value ("filesdockwidget/startup_dir").toString ().isEmpty ())
      {
        // do not restore but there is a startup dir configured
        startup_dir
          = QDir (settings->value ("filesdockwidget/startup_dir").toString ());
      }

    if (! startup_dir.exists ())
      {
        // the configured startup dir does not exist, take actual one
        startup_dir = QDir ();
      }

    m_file_system_model = new QFileSystemModel (this);
    QModelIndex rootPathIndex = m_file_system_model->setRootPath (
                                                                  startup_dir.absolutePath ());

    // Attach the model to the QTreeView and set the root index
    m_file_tree_view = new FileTreeViewer (container);
    m_file_tree_view->setSelectionMode (QAbstractItemView::ExtendedSelection);
    m_file_tree_view->setModel (m_file_system_model);
    m_file_tree_view->setRootIndex (rootPathIndex);
    m_file_tree_view->setSortingEnabled (true);
    m_file_tree_view->setAlternatingRowColors (true);
    m_file_tree_view->setAnimated (true);
    m_file_tree_view->setToolTip (tr ("Double click to open file/folder, right click for alternatives"));

    // get sort column and order as well as cloumn state (order and width)

    m_file_tree_view->sortByColumn (
                                    settings->value ("filesdockwidget/sort_files_by_column",0).toInt (),
                                    static_cast<Qt::SortOrder>
                                    (settings->value ("filesdockwidget/sort_files_by_order",
                                                      Qt::AscendingOrder).toUInt ())
                                    );
    m_file_tree_view->header ()->restoreState (
                                               settings->value ("filesdockwidget/column_state").toByteArray ());

    QStringList mru_dirs =
      settings->value ("filesdockwidget/mru_dir_list").toStringList ();
    m_current_directory->addItems (mru_dirs);

    m_current_directory->setEditText (
                                      m_file_system_model->fileInfo (rootPathIndex).  absoluteFilePath ());

    connect (m_file_tree_view, SIGNAL (activated (const QModelIndex &)),
             this, SLOT (item_double_clicked (const QModelIndex &)));

    // add context menu to tree_view
    m_file_tree_view->setContextMenuPolicy (Qt::CustomContextMenu);
    connect (m_file_tree_view,
             SIGNAL (customContextMenuRequested (const QPoint &)),
             this, SLOT (contextmenu_requested (const QPoint &)));

    m_file_tree_view->header ()->setContextMenuPolicy (Qt::CustomContextMenu);
    connect (m_file_tree_view->header (),
             SIGNAL (customContextMenuRequested (const QPoint &)),
             this, SLOT (headercontextmenu_requested (const QPoint &)));

    // Layout the widgets vertically with the toolbar on top
    QVBoxLayout *vbox_layout = new QVBoxLayout ();
    vbox_layout->setSpacing (0);
    vbox_layout->addWidget (m_navigation_tool_bar);
    vbox_layout->addWidget (m_file_tree_view);
    vbox_layout->setMargin (1);

    container->setLayout (vbox_layout);

    // FIXME: Add right-click contextual menus for copying, pasting,
    //        deleting files (and others).

    connect (m_current_directory->lineEdit (), SIGNAL (returnPressed ()),
             this, SLOT (accept_directory_line_edit ()));

    connect (m_current_directory, SIGNAL (activated (const QString &)),
             this, SLOT (set_current_directory (const QString &)));

    connect (this, SIGNAL (run_file_signal (const QFileInfo&)),
             main_win (), SLOT (run_file_in_terminal (const QFileInfo&)));

    QCompleter *completer = new QCompleter (m_file_system_model, this);
    m_current_directory->setCompleter (completer);

    setFocusProxy (m_current_directory);

    m_sync_octave_dir = true;   // default, overwirtten with notice_settings ()
    m_octave_dir = "";
  }

  void files_dock_widget::save_settings (void)
  {
    QSettings *settings = resource_manager::get_settings ();

    if (! settings)
      return;

    int sort_column = m_file_tree_view->header ()->sortIndicatorSection ();
    Qt::SortOrder sort_order = m_file_tree_view->header ()->sortIndicatorOrder ();
    settings->setValue ("filesdockwidget/sort_files_by_column", sort_column);
    settings->setValue ("filesdockwidget/sort_files_by_order", sort_order);
    settings->setValue ("filesdockwidget/column_state",
                        m_file_tree_view->header ()->saveState ());

    QStringList dirs;
    for (int i=0; i< m_current_directory->count (); i++)
      {
        dirs.append (m_current_directory->itemText (i));
      }
    settings->setValue ("filesdockwidget/mru_dir_list", dirs);

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
    display_directory (m_octave_dir,false);  // false: no sync of octave dir
  }

  void files_dock_widget::update_octave_directory (const QString& dir)
  {
    m_octave_dir = dir;
    if (m_sync_octave_dir)
      display_directory (m_octave_dir,false);  // false: no sync of octave dir
  }

  void files_dock_widget::display_directory (const QString& dir, bool set_octave_dir)
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

            // see if its in the list, and if it is,
            // remove it and then, put at top of the list
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
            QSettings *settings = resource_manager::get_settings ();
            QString ext = settings->value ("filesdockwidget/txt_file_extensions",
                                           "m;c;cc;cpp;h;txt").toString ();
            QStringList extensions = ext.split (";", QString::SkipEmptyParts);

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
    QSettings *settings = resource_manager::get_settings ();

    QString key = m_columns_shown_keys.at (col);
    bool shown = settings->value (key,false).toBool ();
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

    QSettings *settings = resource_manager::get_settings ();

    for (int i = 0; i < m_columns_shown.size (); i++)
      {
        QAction *action = menu.addAction (m_columns_shown.at (i),
                                          m_sig_mapper, SLOT (map ()));
        m_sig_mapper->setMapping (action, i);
        action->setCheckable (true);
        action->setChecked (
                            settings->value (m_columns_shown_keys.at (i),true).toBool ());
      }

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

        // construct the context menu depending on item
        menu.addAction (resource_manager::icon ("document-open"), tr ("Open"),
                        this, SLOT (contextmenu_open (bool)));

        if (info.isDir ())
          {
            menu.addAction (tr ("Open in System File Explorer"),
                            this, SLOT (contextmenu_open_in_app (bool)));
          }

        if (info.isFile ())
          menu.addAction (tr ("Open in Text Editor"),
                          this, SLOT (contextmenu_open_in_editor (bool)));

        menu.addAction (tr ("Copy Selection to Clipboard"),
                        this, SLOT (contextmenu_copy_selection (bool)));

        if (info.isFile () && info.suffix () == "m")
          menu.addAction (resource_manager::icon ("media-playback-start"),
                          tr ("Run"), this, SLOT (contextmenu_run (bool)));

        if (info.isFile ())
          menu.addAction (tr ("Load Data"), this, SLOT (contextmenu_load (bool)));

        if (info.isDir ())
          {
            menu.addSeparator ();
            menu.addAction (resource_manager::icon ("go-first"),
                            tr ("Set Current Directory"),
                            this, SLOT (contextmenu_setcurrentdir (bool)));
            menu.addSeparator ();
            menu.addAction (resource_manager::icon ("edit-find"),
                            tr ("Find Files..."), this,
                            SLOT (contextmenu_findfiles (bool)));
          }

        menu.addSeparator ();
        menu.addAction (tr ("Rename..."), this, SLOT (contextmenu_rename (bool)));
        menu.addAction (resource_manager::icon ("edit-delete"),
                        tr ("Delete..."), this, SLOT (contextmenu_delete (bool)));

        if (info.isDir ())
          {
            menu.addSeparator ();
            menu.addAction (resource_manager::icon ("document-new"),
                            tr ("New File..."),
                            this, SLOT (contextmenu_newfile (bool)));
            menu.addAction (resource_manager::icon ("folder-new"),
                            tr ("New Directory..."),
                            this, SLOT (contextmenu_newdir (bool)));
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

        QFileInfo info = m_file_system_model->fileInfo (index);
        QDir path = info.absoluteDir ();
        QString old_name = info.fileName ();
        bool ok;

        QString new_name
          = QInputDialog::getText (this, tr ("Rename file/directory"),
                                   tr ("Rename file/directory:\n")
                                   + old_name + tr ("\n to: "),
                                   QLineEdit::Normal, old_name, &ok);
        if (ok && new_name.length () > 0)
          {
            new_name = path.absolutePath () + '/' + new_name;
            old_name = path.absolutePath () + '/' + old_name;
            // editor: close old
            emit file_remove_signal (old_name, new_name);
            // Do the renaming
            bool st = path.rename (old_name, new_name);
            // editor: load new/old file depending on success
            emit file_renamed_signal (st);
            // Clear cache of file browser
            m_file_system_model->revert ();
          }
      }

  }

  void files_dock_widget::contextmenu_delete (bool)
  {
    QItemSelectionModel *m = m_file_tree_view->selectionModel ();
    QModelIndexList rows = m->selectedRows ();

    for (auto it = rows.begin (); it != rows.end (); it++)
      {
        QModelIndex index = *it;

        QFileInfo info = m_file_system_model->fileInfo (index);

        if (QMessageBox::question (this, tr ("Delete file/directory"),
                                   tr ("Are you sure you want to delete\n")
                                   + info.filePath (),
                                   QMessageBox::Yes | QMessageBox::No)
            == QMessageBox::Yes)
          {
            if (info.isDir ())
              {
                // see if direcory is empty
                QDir path (info.absoluteFilePath ());
                QList<QFileInfo> fileLst = path.entryInfoList (QDir::AllEntries |
                                                               QDir::NoDotAndDotDot);

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
                // reload the old file if removing was not successful
                if (! st)
                  emit file_renamed_signal (false);
              }

            m_file_system_model->revert ();

          }
      }
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
    QItemSelectionModel *m = m_file_tree_view->selectionModel ();
    QModelIndexList rows = m->selectedRows ();

    if (rows.size () > 0)
      {
        QModelIndex index = rows[0];

        QFileInfo info = m_file_system_model->fileInfo (index);

        if (info.isDir ())
          {
            process_set_current_dir (info.absoluteFilePath ());
          }
      }
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

  void files_dock_widget::notice_settings (const QSettings *settings)
  {
    // Qsettings pointer is checked before emitting.

    int icon_size_settings = settings->value ("toolbar_icon_size",0).toInt ();
    QStyle *st = style ();
    int icon_size = st->pixelMetric (QStyle::PM_ToolBarIconSize);

    if (icon_size_settings == 1)
      icon_size = st->pixelMetric (QStyle::PM_LargeIconSize);
    else if (icon_size_settings == -1)
      icon_size = st->pixelMetric (QStyle::PM_SmallIconSize);

    m_navigation_tool_bar->setIconSize (QSize (icon_size,icon_size));

    // filenames are always shown, other columns can be hidden by settings
    for (int i = 0; i < 3; i++)
      m_file_tree_view->setColumnHidden (i + 1,
                                         ! settings->value (m_columns_shown_keys.at (i),false).toBool ());

    if (settings->value (m_columns_shown_keys.at (3),false).toBool ())
      m_file_system_model->setFilter (QDir::NoDotAndDotDot | QDir::AllEntries
                                      | QDir::Hidden);
    else
      m_file_system_model->setFilter (QDir::NoDotAndDotDot | QDir::AllEntries);

    m_file_tree_view->setAlternatingRowColors (
                                               settings->value (m_columns_shown_keys.at (4),true).toBool ());
    m_file_tree_view->setModel (m_file_system_model);

    // enable the buttons to sync octave/browser dir
    // only if this is not done by default
    m_sync_octave_dir
      = settings->value ("filesdockwidget/sync_octave_directory",true).toBool ();
    m_sync_octave_directory_action->setEnabled (! m_sync_octave_dir);
    m_sync_browser_directory_action->setEnabled (! m_sync_octave_dir);

    if (m_sync_octave_dir)
      display_directory (m_octave_dir);  // sync browser to octave dir

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
    QString dir = QFileDialog::getExistingDirectory (this,
                     tr ("Set directory of file browser"),
                     m_file_system_model->rootPath (),
                     QFileDialog::ShowDirsOnly
                     | QFileDialog::DontUseNativeDialog);
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
                                          tr ("Create file in\n","String ends with \\n!") + parent_dir,
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
                                          tr ("Create folder in\n","String ends with \\n!") + parent_dir,
                                          QLineEdit::Normal,
                                          tr ("New Directory"), &ok);
    if (ok && name.length () > 0)
      {
        QDir dir (parent_dir);
        dir.mkdir (name);
        m_file_system_model->revert ();
      }
  }

  void files_dock_widget::process_set_current_dir (const QString & dir)
  {
    emit displayed_directory_changed (dir);
  }

  void files_dock_widget::process_find_files (const QString & dir)
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
}
