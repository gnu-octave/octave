/*

Copyright (C) 2011-2012 Jacob Dawid

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

class FileTreeViewer : public QTreeView
{
public:

  FileTreeViewer (QWidget *p) : QTreeView (p) { }

  void mousePressEvent (QMouseEvent *e)
  {
    if (e->button () != Qt::RightButton)
      QTreeView::mousePressEvent (e);
  }
};

files_dock_widget::files_dock_widget (QWidget *p)
  : octave_dock_widget (p)
{
  setObjectName ("FilesDockWidget");
  setWindowIcon (QIcon(":/actions/icons/logo.png"));
  set_title (tr ("File Browser"));
  setToolTip (tr ("Browse your files."));

  QWidget *container = new QWidget (this);

  setWidget (container);

  connect (this, SIGNAL (open_file (const QString&)),
           main_win (), SLOT (open_file (const QString&)));

  connect (this, SIGNAL (displayed_directory_changed (const QString&)),
           main_win (), SLOT (set_current_working_directory (const QString&)));

  // Create a toolbar
  _navigation_tool_bar = new QToolBar ("", container);
  _navigation_tool_bar->setAllowedAreas (Qt::TopToolBarArea);
  _navigation_tool_bar->setMovable (false);

  _current_directory = new QComboBox (_navigation_tool_bar);
  _current_directory->setToolTip (tr ("Enter the path or filename"));
  _current_directory->setEditable(true);
  _current_directory->setMaxCount(MaxMRUDirs);
  _current_directory->setInsertPolicy(QComboBox::NoInsert);
  _current_directory->setSizeAdjustPolicy (QComboBox::AdjustToMinimumContentsLengthWithIcon);
  QSizePolicy sizePol(QSizePolicy::Expanding, QSizePolicy::Preferred);
  _current_directory->setSizePolicy(sizePol);

  QAction *directory_up_action = new QAction (QIcon(":/actions/icons/up.png"),
                                              "", _navigation_tool_bar);
  directory_up_action->setToolTip (tr ("Move up one directory"));

  _sync_browser_directory_action = new QAction (QIcon(":/actions/icons/reload.png"),
                                                tr("Show octave directory"), _navigation_tool_bar);
  _sync_browser_directory_action->setToolTip (tr ("Goto current octave directory"));
  _sync_browser_directory_action->setEnabled ("false");

  _sync_octave_directory_action = new QAction (QIcon(":/actions/icons/ok.png"),
                                               tr("Set octave directory"), _navigation_tool_bar);
  _sync_octave_directory_action->setToolTip (tr ("Set octave directroy to current browser directory"));
  _sync_octave_directory_action->setEnabled ("false");

  QToolButton * popdown_button = new QToolButton();
  popdown_button->setToolTip(tr ("Actions on current directory"));
  QMenu * popdown_menu = new QMenu();
  popdown_menu->addAction (QIcon (":/actions/icons/home.png"),
                           tr ("Show Home directory"),
                           this, SLOT (popdownmenu_home (bool)));
  popdown_menu->addAction(_sync_browser_directory_action);
  popdown_menu->addAction(_sync_octave_directory_action);
  popdown_button->setMenu(popdown_menu);
  popdown_button->setPopupMode(QToolButton::InstantPopup);
  popdown_button->setDefaultAction(new QAction(QIcon(":/actions/icons/gear.png"),"", _navigation_tool_bar));

  popdown_menu->addSeparator();
  popdown_menu->addAction (QIcon (":/actions/icons/search.png"),
                           tr ("Search directory"),
                           this, SLOT (popdownmenu_search_dir (bool)));
  popdown_menu->addSeparator();
  popdown_menu->addAction( tr ("Find Files ..."),
                          this, SLOT(popdownmenu_findfiles(bool)));
  popdown_menu->addSeparator();
  popdown_menu->addAction(QIcon(":/actions/icons/filenew.png"),
                          tr ("New File"),
                          this, SLOT(popdownmenu_newfile(bool)));
  popdown_menu->addAction(QIcon(":/actions/icons/folder_new.png"),
                          tr ("New Directory"),
                          this, SLOT(popdownmenu_newdir(bool)));

  _navigation_tool_bar->addWidget (_current_directory);
  _navigation_tool_bar->addAction (directory_up_action);
  _navigation_tool_bar->addWidget (popdown_button);

  connect (directory_up_action, SIGNAL (triggered ()), this,
           SLOT (change_directory_up ()));
  connect (_sync_octave_directory_action, SIGNAL (triggered ()), this,
           SLOT (do_sync_octave_directory ()));
  connect (_sync_browser_directory_action, SIGNAL (triggered ()), this,
           SLOT (do_sync_browser_directory ()));

  // Create the QFileSystemModel starting in the actual directory
  QDir curr_dir;
  _file_system_model = new QFileSystemModel (this);
  _file_system_model->setFilter (QDir::NoDotAndDotDot | QDir::AllEntries);
  QModelIndex rootPathIndex = _file_system_model->setRootPath (
                                                  curr_dir.absolutePath ());

  // Attach the model to the QTreeView and set the root index
  _file_tree_view = new FileTreeViewer (container);
  _file_tree_view->setSelectionMode (QAbstractItemView::ExtendedSelection);
  _file_tree_view->setModel (_file_system_model);
  _file_tree_view->setRootIndex (rootPathIndex);
  _file_tree_view->setSortingEnabled (true);
  _file_tree_view->setAlternatingRowColors (true);
  _file_tree_view->setAnimated (true);
  _file_tree_view->setToolTip (tr ("Doubleclick a file to open it"));

  // get sort column and order as well as cloumn state (order and width)
  QSettings *settings = resource_manager::get_settings ();
  // FIXME -- what should happen if settings is 0?
  _file_tree_view->sortByColumn (
              settings->value ("filesdockwidget/sort_files_by_column",0).toInt (),
              static_cast<Qt::SortOrder>(settings->value ("filesdockwidget/sort_files_by_order",Qt::AscendingOrder).toUInt ())
  );
  _file_tree_view->header ()->restoreState (settings->value ("filesdockwidget/column_state").toByteArray ());
  
  QStringList mru_dirs = settings->value ("filesdockwidget/mru_dir_list").toStringList ();
  _current_directory->addItems(mru_dirs);

  _current_directory->setEditText(_file_system_model->fileInfo (rootPathIndex).
                              absoluteFilePath ());

  connect (_file_tree_view, SIGNAL (doubleClicked (const QModelIndex &)),
           this, SLOT (item_double_clicked (const QModelIndex &)));

  // add context menu to tree_view
  _file_tree_view->setContextMenuPolicy(Qt::CustomContextMenu);
  connect(_file_tree_view, SIGNAL(customContextMenuRequested(const QPoint &)), 
           this, SLOT(contextmenu_requested(const QPoint &)));

  // Layout the widgets vertically with the toolbar on top
  QVBoxLayout *vbox_layout = new QVBoxLayout ();
  vbox_layout->setSpacing (0);
  vbox_layout->addWidget (_navigation_tool_bar);
  vbox_layout->addWidget (_file_tree_view);
  vbox_layout->setMargin (1);

  container->setLayout (vbox_layout);

  // TODO: Add right-click contextual menus for copying, pasting, deleting files (and others)

  connect (_current_directory->lineEdit(), SIGNAL (returnPressed ()),
            this, SLOT (accept_directory_line_edit ()));

  connect (_current_directory, SIGNAL (activated (const QString &)),
           this, SLOT (set_current_directory (const QString &)));

  connect (this, SIGNAL (run_file_signal (const QFileInfo&)),
           main_win (), SLOT (run_file_in_terminal (const QFileInfo&)));

  QCompleter *completer = new QCompleter (_file_system_model, this);
  _current_directory->setCompleter (completer);

  setFocusProxy (_current_directory);
  
  _sync_octave_dir = true;   // default, overwirtten with notice_settings ()
  _octave_dir = "";
}

files_dock_widget::~files_dock_widget ()
{
  QSettings *settings = resource_manager::get_settings ();
  int sort_column = _file_tree_view->header ()->sortIndicatorSection ();
  Qt::SortOrder sort_order = _file_tree_view->header ()->sortIndicatorOrder ();
  settings->setValue ("filesdockwidget/sort_files_by_column", sort_column);
  settings->setValue ("filesdockwidget/sort_files_by_order", sort_order);
  settings->setValue ("filesdockwidget/column_state", _file_tree_view->header ()->saveState ());

  QStringList dirs;
  for(int i=0; i< _current_directory->count(); i++)
  {
    dirs.append(_current_directory->itemText(i));
  }
  settings->setValue ("filesdockwidget/mru_dir_list", dirs);

  settings->sync ();
}

void
files_dock_widget::item_double_clicked (const QModelIndex& index)
{
  // Retrieve the file info associated with the model index.
  QFileInfo fileInfo = _file_system_model->fileInfo (index);
  set_current_directory (fileInfo.absoluteFilePath ());
}

void
files_dock_widget::set_current_directory (const QString& dir)
{
  display_directory (dir);
}

void
files_dock_widget::accept_directory_line_edit (void)
{
  display_directory (_current_directory->currentText ());
}

void
files_dock_widget::change_directory_up (void)
{
  QDir dir = QDir (_file_system_model->filePath (_file_tree_view->rootIndex ()));
  dir.cdUp ();
  display_directory (dir.absolutePath ());
}

void
files_dock_widget::do_sync_octave_directory (void)
{
  QDir dir = QDir (_file_system_model->filePath (_file_tree_view->rootIndex ()));
  emit displayed_directory_changed (dir.absolutePath ());
}

void
files_dock_widget::do_sync_browser_directory (void)
{
  display_directory (_octave_dir,false);  // false: no sync of octave dir
}

void
files_dock_widget::update_octave_directory (const QString& dir)
{
  _octave_dir = dir;
  if (_sync_octave_dir)
    display_directory (_octave_dir,false);  // false: no sync of octave dir
}

void
files_dock_widget::display_directory (const QString& dir, bool set_octave_dir)
{
  QFileInfo fileInfo (dir);
  if (fileInfo.exists ())
    {
      if (fileInfo.isDir ())
        {
          _file_tree_view->setRootIndex (_file_system_model->
                                         index (fileInfo.absoluteFilePath ()));
          _file_system_model->setRootPath (fileInfo.absoluteFilePath ());
          _file_system_model->sort (0, Qt::AscendingOrder);
          if (_sync_octave_dir && set_octave_dir)
            process_set_current_dir(fileInfo.absoluteFilePath ());

          // see if its in the list, and if it is, remove it and then, put at top of the list
          int index = _current_directory->findText(fileInfo.absoluteFilePath ());
          if(index != -1)
          {
             _current_directory->removeItem(index);
          }
          _current_directory->insertItem(0, fileInfo.absoluteFilePath ());
          _current_directory->setCurrentIndex(0);
        }
      else
        {
          QString abs_fname = fileInfo.absoluteFilePath ();

          if (QFile::exists (abs_fname))
            {
              if (is_octave_data_file (abs_fname.toStdString ()))
                emit load_file_signal (abs_fname);
              else
                emit open_file (fileInfo.absoluteFilePath ());
            }
        }
    }
}

void
files_dock_widget::open_item_in_app (const QModelIndex& index)
{
  // Retrieve the file info associated with the model index.
  QFileInfo fileInfo = _file_system_model->fileInfo (index);

  QString file = fileInfo.absoluteFilePath ();

  QDesktopServices::openUrl (QUrl::fromLocalFile (file));
}

void 
files_dock_widget::contextmenu_requested (const QPoint& mpos)
{

  QMenu menu(this);

  QModelIndex index = _file_tree_view->indexAt (mpos);
  //QAbstractItemModel *m = _file_tree_view->model ();

  if (index.isValid())
    { 
      QFileInfo info = _file_system_model->fileInfo(index);

      menu.addAction (QIcon (":/actions/icons/fileopen.png"), tr("Open"),
                     this, SLOT(contextmenu_open(bool)));

      menu.addAction (tr("Open in Default Application"),
                      this, SLOT (contextmenu_open_in_app (bool)));

      menu.addAction (tr("Copy Selection to Clipboard"),
                      this, SLOT (contextmenu_copy_selection (bool)));

      if (info.isFile () && info.suffix () == "m")
        menu.addAction (QIcon (":/actions/icons/artsbuilderexecute.png"),
                        tr("Run"), this, SLOT(contextmenu_run(bool)));

      if (info.isFile ())
        menu.addAction (tr("Load Data"), this, SLOT(contextmenu_load(bool)));

      if (info.isDir ())
        {
          menu.addSeparator ();
          menu.addAction (QIcon (":/actions/icons/ok.png"),
                          tr ("Set Current Directory"),
                          this, SLOT (contextmenu_setcurrentdir (bool)));
          menu.addSeparator ();
          menu.addAction (tr ("Find Files ..."), this, SLOT(contextmenu_findfiles(bool)));
        }

      menu.addSeparator();
      menu.addAction (tr ("Rename"), this, SLOT(contextmenu_rename(bool)));
      menu.addAction (QIcon(":/actions/icons/editdelete.png"), tr("Delete"),
                      this, SLOT(contextmenu_delete(bool)));

      if (info.isDir())
        {
          menu.addSeparator();
          menu.addAction (QIcon (":/actions/icons/filenew.png"),
                          tr ("New File"),
                          this, SLOT(contextmenu_newfile(bool)));
          menu.addAction (QIcon (":/actions/icons/folder_new.png"),
                          tr("New Directory"),
                          this, SLOT(contextmenu_newdir(bool)));
        }

      menu.exec(_file_tree_view->mapToGlobal(mpos));

    }
}

void
files_dock_widget::contextmenu_open (bool)
{

  QItemSelectionModel *m = _file_tree_view->selectionModel ();
  QModelIndexList rows = m->selectedRows ();

  for( QModelIndexList::iterator it = rows.begin (); it != rows.end (); it++)
    {
      item_double_clicked(*it);
    }
}

void
files_dock_widget::contextmenu_open_in_app (bool)
{
  QItemSelectionModel *m = _file_tree_view->selectionModel ();
  QModelIndexList rows = m->selectedRows ();

  for (QModelIndexList::iterator it = rows.begin (); it != rows.end (); it++)
    open_item_in_app (*it);
}

void
files_dock_widget::contextmenu_copy_selection (bool)
{
  QItemSelectionModel *m = _file_tree_view->selectionModel ();
  QModelIndexList rows = m->selectedRows ();

  QStringList selection;

  for (QModelIndexList::iterator it = rows.begin (); it != rows.end (); it++)
    {
      QFileInfo info = _file_system_model->fileInfo (*it);

      selection << info.fileName ();
    }

  QClipboard *clipboard = QApplication::clipboard ();

  clipboard->setText (selection.join ("\n"));
}

void
files_dock_widget::contextmenu_load (bool)
{
  QItemSelectionModel *m = _file_tree_view->selectionModel ();
  QModelIndexList rows = m->selectedRows ();

  if (rows.size () > 0)
    {
      QModelIndex index = rows[0];

      QFileInfo info = _file_system_model->fileInfo(index);

      emit load_file_signal (info.fileName ());
    }
}

void
files_dock_widget::contextmenu_run (bool)
{
  QItemSelectionModel *m = _file_tree_view->selectionModel ();
  QModelIndexList rows = m->selectedRows ();

  if (rows.size () > 0)
    {
      QModelIndex index = rows[0];

      QFileInfo info = _file_system_model->fileInfo(index);
      emit run_file_signal (info);
    }
}

void 
files_dock_widget::contextmenu_rename (bool)
{
  QItemSelectionModel *m = _file_tree_view->selectionModel ();
  QModelIndexList rows = m->selectedRows ();
  if(rows.size() > 0)
    {
      QModelIndex index = rows[0];

      QFileInfo info = _file_system_model->fileInfo(index);
      QDir path = info.absoluteDir();
      QString old_name = info.fileName();
      bool ok;

      QString new_name = QInputDialog::getText (this, tr("Rename file/directory"), 
                                                tr("Rename file/directory:\n") + old_name + tr("\n to: "),
                                                QLineEdit::Normal, old_name, &ok);
      if(ok && new_name.length()>0)
        {
          new_name = path.absolutePath() + "/" + new_name;
          old_name = path.absolutePath() + "/" + old_name;
          path.rename(old_name, new_name);
          _file_system_model->revert();
        }
    }

}

void 
files_dock_widget::contextmenu_delete (bool)
{
  QItemSelectionModel *m = _file_tree_view->selectionModel ();
  QModelIndexList rows = m->selectedRows ();

  for( QModelIndexList::iterator it = rows.begin (); it != rows.end (); it++)
    {
      QModelIndex index = *it;

      QFileInfo info = _file_system_model->fileInfo(index);

      if(QMessageBox::question(this, tr("Delete file/directory"), 
                               tr("Are you sre you want to delete\n") + info.filePath(),
                               QMessageBox::Yes|QMessageBox::No) == QMessageBox::Yes) 
        {
           if(info.isDir())
             {
               // see if direcory is empty
               QDir path(info.absoluteFilePath());
               QList<QFileInfo> fileLst = path.entryInfoList(QDir::AllEntries | QDir::NoDotAndDotDot);

               if(fileLst.count() != 0)
                 QMessageBox::warning(this, tr("Delete file/directory"),
                                      tr("Can not delete a directory that is not empty"));
               else
                 _file_system_model->rmdir(index);
             }
           else
             {
               _file_system_model->remove(index);
             }

           _file_system_model->revert();

        }
    }
}

void 
files_dock_widget::contextmenu_newfile (bool)
{
  QItemSelectionModel *m = _file_tree_view->selectionModel ();
  QModelIndexList rows = m->selectedRows ();

  if(rows.size() > 0)
    {
      QModelIndex index = rows[0];

      QFileInfo info = _file_system_model->fileInfo(index);
      QString parent_dir = info.filePath();

      process_new_file(parent_dir);
    }
}

void 
files_dock_widget::contextmenu_newdir (bool)
{
  QItemSelectionModel *m = _file_tree_view->selectionModel ();
  QModelIndexList rows = m->selectedRows ();

  if(rows.size() > 0)
    {
      QModelIndex index = rows[0];

      QFileInfo info = _file_system_model->fileInfo(index);
      QString parent_dir = info.filePath();

      process_new_dir(parent_dir);
    }
}

void 
files_dock_widget::contextmenu_setcurrentdir (bool)
{
  QItemSelectionModel *m = _file_tree_view->selectionModel ();
  QModelIndexList rows = m->selectedRows ();

  if(rows.size() > 0)
    {
      QModelIndex index = rows[0];

      QFileInfo info = _file_system_model->fileInfo(index);

      if(info.isDir())
        {
          process_set_current_dir(info.absoluteFilePath ());
        }
    }
}

void 
files_dock_widget::contextmenu_findfiles (bool)
{
  QItemSelectionModel *m = _file_tree_view->selectionModel ();
  QModelIndexList rows = m->selectedRows ();

  if(rows.size() > 0)
    {
      QModelIndex index = rows[0];

      QFileInfo info = _file_system_model->fileInfo(index);

      if(info.isDir())
        {
          process_find_files(info.absoluteFilePath ());
        }
    }
}

void
files_dock_widget::notice_settings (const QSettings *settings)
{
  // Qsettings pointer is checked before emitting.

  int icon_size = settings->value ("toolbar_icon_size",24).toInt ();
  if (icon_size > 16)
    icon_size = icon_size - 4;
  _navigation_tool_bar->setIconSize (QSize (icon_size,icon_size));

  // file names are always shown, other columns can be hidden by settings
  _file_tree_view->setColumnHidden (0, false);
  _file_tree_view->setColumnHidden (1, !settings->value ("filesdockwidget/showFileSize",false).toBool ());
  _file_tree_view->setColumnHidden (2, !settings->value ("filesdockwidget/showFileType",false).toBool ());
  _file_tree_view->setColumnHidden (3, !settings->value ("filesdockwidget/showLastModified",false).toBool ());
  _file_tree_view->setAlternatingRowColors (settings->value ("filesdockwidget/useAlternatingRowColors",true).toBool ());
  if (settings->value ("filesdockwidget/showHiddenFiles",false).toBool ())
    {
      // TODO: React on option for hidden files.
    }
  // enalbe the buttons to sync octave/browser dir only if this is not done by default
  _sync_octave_dir = settings->value ("filesdockwidget/sync_octave_directory",false).toBool ();
  _sync_octave_directory_action->setEnabled (!_sync_octave_dir);
  _sync_browser_directory_action->setEnabled (!_sync_octave_dir);

  if (_sync_octave_dir)
    display_directory (_octave_dir);  // sync browser to octave dir

}

void
files_dock_widget::popdownmenu_home (bool)
{
  QString dir = QDir::homePath ();
  set_current_directory (dir);
}

void
files_dock_widget::popdownmenu_search_dir (bool)
{
  QString dir = QFileDialog::getExistingDirectory
    (this, tr ("Set directory of file browser"),_file_system_model->rootPath());
  set_current_directory (dir);
}

void
files_dock_widget::popdownmenu_findfiles (bool)
{
      process_find_files(_file_system_model->rootPath());
}

void
files_dock_widget::popdownmenu_newdir (bool)
{
      process_new_dir(_file_system_model->rootPath());
}

void
files_dock_widget::popdownmenu_newfile (bool)
{
      process_new_file(_file_system_model->rootPath());
}

void
files_dock_widget::process_new_file (const QString &parent_dir)
{
  bool ok;

  QString name = QInputDialog::getText (this, tr("Create File"), tr("Create file in\n") + parent_dir,
                                       QLineEdit::Normal, "New File.txt", &ok);
  if(ok && name.length()>0)
    {
      name = parent_dir + "/" + name;

      QFile file(name);
      file.open(QIODevice::WriteOnly);
      _file_system_model->revert();
    }
}

void
files_dock_widget::process_new_dir (const QString &parent_dir)
{
  bool ok;

  QString name = QInputDialog::getText (this, tr("Create Directory"), tr("Create folder in\n") + parent_dir,
                                       QLineEdit::Normal, "New Directory", &ok);
  if(ok && name.length()>0)
    {
     QDir dir(parent_dir);
      dir.mkdir(name);
      _file_system_model->revert();
    }
}

void files_dock_widget::process_set_current_dir(const QString & dir)
{
  emit displayed_directory_changed (dir);
}

void files_dock_widget::process_find_files(const QString & dir)
{
  emit find_files_signal(dir);
}

