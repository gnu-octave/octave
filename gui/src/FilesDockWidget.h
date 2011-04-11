#ifndef FILESDOCKWIDGET_H
#define FILESDOCKWIDGET_H

#include <QListView>
#include <QDate>
#include <QObject>
#include <QWidget>
#include <QListWidget>
#include <QFileSystemModel>
#include <QToolBar>
#include <QToolButton>
#include <QVBoxLayout>
#include <QAction>
#include <QTreeView>

#include <vector>
#include <string>

#undef PACKAGE_BUGREPORT
#undef PACKAGE_NAME
#undef PACKAGE_STRING
#undef PACKAGE_TARNAME
#undef PACKAGE_VERSION
#include "octave/config.h"
#include "octave/octave.h"
#include "octave/str-vec.h"
#include "octave/cmd-hist.h"
#include <QDockWidget>

class FilesDockWidget : public QDockWidget {
  Q_OBJECT
public :
  FilesDockWidget(QWidget *parent = 0);
  void setDirectory(QString dir);
  
public slots:
  /** Slot for handling a change in directory via double click. */
  void itemDoubleClicked(const QModelIndex &index);

  /** Slot for handling the up-directory button in the toolbar. */
  void onUpDirectory();
    
private:

  // TODO: Add toolbar with buttons for navigating the path, creating dirs, etc

  /** Toolbar for file and directory manipulation. */
  QToolBar *toolbar;

  /** Variables for the up-directory action. */
  QIcon dirIcon;
  QAction *dirAction;
  QToolButton *upDirectoryButton;

  /** The file system model. */
  QFileSystemModel *fileSystemModel;

  /** The file system view. */
  QTreeView *fileTreeView;
};

#endif // FILESDOCKWIDGET_H
