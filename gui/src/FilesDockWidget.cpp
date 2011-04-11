#include "FilesDockWidget.h"

#include <QApplication>

FilesDockWidget::FilesDockWidget(QWidget *parent)
  : QDockWidget(parent)
{
    setWidget(new QWidget(this));

    // Create a toolbar
    toolbar = new QToolBar("", widget());
    toolbar->setAllowedAreas(Qt::TopToolBarArea);
    toolbar->setMovable(false);
    toolbar->setIconSize(QSize (20,20));

    // Add a button to the toolbar with the QT standard icon for up-directory
    // TODO: Maybe change this to be an up-directory icon that is OS specific???
    QStyle *style = QApplication::style();
    dirIcon = style->standardIcon(QStyle::SP_FileDialogToParent);
    dirAction = new QAction(dirIcon, "", toolbar);

    toolbar->addAction(dirAction);
    connect(dirAction, SIGNAL(triggered()), this, SLOT(onUpDirectory()));

    // TODO: Add other buttons for creating directories

    // Create the QFileSystemModel starting in the home directory
    QString homePath = QDir::homePath();
    // TODO: This should occur after Octave has been initialized and the startup directory of Octave is established

    fileSystemModel = new QFileSystemModel(this);
    fileSystemModel->setFilter(QDir::NoDotAndDotDot | QDir::AllEntries);
    QModelIndex rootPathIndex = fileSystemModel->setRootPath(homePath);

    // Attach the model to the QTreeView and set the root index
    fileTreeView = new QTreeView(widget());
    fileTreeView->setModel(fileSystemModel);
    fileTreeView->setRootIndex(rootPathIndex);
    fileTreeView->setSortingEnabled(true);

    connect(fileTreeView, SIGNAL(doubleClicked(const QModelIndex &)), this, SLOT(itemDoubleClicked(const QModelIndex &)));

    // Layout the widgets vertically with the toolbar on top
    QVBoxLayout *layout = new QVBoxLayout();
    layout->setSpacing(0);
    layout->addWidget(toolbar);
    layout->addWidget(fileTreeView);
    widget()->setLayout(layout);
    // TODO: Add right-click contextual menus for copying, pasting, deleting files (and others)
}

void FilesDockWidget::itemDoubleClicked(const QModelIndex &index)
{
    QFileInfo fileInfo = fileSystemModel->fileInfo(index);
    if (fileInfo.isDir()) {
      fileSystemModel->setRootPath(fileInfo.absolutePath());
      fileTreeView->setRootIndex(index);
    } else {
      // TODO: Open the file appropriately based on the mime type
    }
}

void FilesDockWidget::onUpDirectory(void)
{
    // Move up an index node
    QDir dir = QDir(fileSystemModel->filePath(fileTreeView->rootIndex()));
    dir.cdUp();
    fileTreeView->setRootIndex(fileSystemModel->index(dir.absolutePath()));
}

