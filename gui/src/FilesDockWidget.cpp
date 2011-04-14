#include "FilesDockWidget.h"

#include <QApplication>
#include <QFileInfo>

FilesDockWidget::FilesDockWidget(QWidget *parent)
  : QDockWidget(parent) {
    setObjectName("FilesDockWidget");
    setWindowTitle("Current Folder");
    setWidget(new QWidget(this));

    // Create a toolbar
    m_navigationToolBar = new QToolBar("", widget());
    m_navigationToolBar->setAllowedAreas(Qt::TopToolBarArea);
    m_navigationToolBar->setMovable(false);
    m_navigationToolBar->setIconSize(QSize (20,20));

    // Add a button to the toolbar with the QT standard icon for up-directory
    // TODO: Maybe change this to be an up-directory icon that is OS specific???
    QStyle *style = QApplication::style();
    m_directoryIcon = style->standardIcon(QStyle::SP_FileDialogToParent);
    m_directoryUpAction = new QAction(m_directoryIcon, "", m_navigationToolBar);
    m_currentDirectory = new QLineEdit(m_navigationToolBar);

    m_navigationToolBar->addAction(m_directoryUpAction);
    m_navigationToolBar->addWidget(m_currentDirectory);
    connect(m_directoryUpAction, SIGNAL(triggered()), this, SLOT(onUpDirectory()));

    // TODO: Add other buttons for creating directories

    // Create the QFileSystemModel starting in the home directory
    QString homePath = QDir::homePath();
    // TODO: This should occur after Octave has been initialized and the startup directory of Octave is established

    m_fileSystemModel = new QFileSystemModel(this);
    m_fileSystemModel->setFilter(QDir::NoDotAndDotDot | QDir::AllEntries);
    QModelIndex rootPathIndex = m_fileSystemModel->setRootPath(homePath);

    // Attach the model to the QTreeView and set the root index
    m_fileTreeView = new QTreeView(widget());
    m_fileTreeView->setModel(m_fileSystemModel);
    m_fileTreeView->setRootIndex(rootPathIndex);
    m_fileTreeView->setSortingEnabled(true);
    m_fileTreeView->setAlternatingRowColors(true);
    m_fileTreeView->setAnimated(true);
    setCurrentDirectory(m_fileSystemModel->fileInfo(rootPathIndex).absoluteFilePath());

    connect(m_fileTreeView, SIGNAL(doubleClicked(const QModelIndex &)), this, SLOT(itemDoubleClicked(const QModelIndex &)));

    // Layout the widgets vertically with the toolbar on top
    QVBoxLayout *layout = new QVBoxLayout();
    layout->setSpacing(0);
    layout->addWidget(m_navigationToolBar);
    layout->addWidget(m_fileTreeView);
    widget()->setLayout(layout);
    // TODO: Add right-click contextual menus for copying, pasting, deleting files (and others)

    connect(m_currentDirectory, SIGNAL(returnPressed()), this, SLOT(currentDirectoryEntered()));
    //m_currentDirectory->setEnabled(false);
}

void FilesDockWidget::itemDoubleClicked(const QModelIndex &index)
{
    QFileInfo fileInfo = m_fileSystemModel->fileInfo(index);
    if (fileInfo.isDir()) {
        m_fileSystemModel->setRootPath(fileInfo.absolutePath());
        m_fileTreeView->setRootIndex(index);
        setCurrentDirectory(m_fileSystemModel->fileInfo(index).absoluteFilePath());
    } else {
        QFileInfo fileInfo = m_fileSystemModel->fileInfo(index);
        emit openFile(fileInfo.filePath());
    }
}

void FilesDockWidget::setCurrentDirectory(QString currentDirectory) {
    m_currentDirectory->setText(currentDirectory);
}

void FilesDockWidget::onUpDirectory(void) {
    // Move up an inm_fileTreeView->setRootIndex(m_fileSystemModel->index(dir.absolutePath()));dex node
    QDir dir = QDir(m_fileSystemModel->filePath(m_fileTreeView->rootIndex()));
    dir.cdUp();
    m_fileSystemModel->setRootPath(dir.absolutePath());
    m_fileTreeView->setRootIndex(m_fileSystemModel->index(dir.absolutePath()));
    setCurrentDirectory(dir.absolutePath());
}

void FilesDockWidget::currentDirectoryEntered() {
    QFileInfo fileInfo(m_currentDirectory->text());
    if (fileInfo.isDir()) {
        m_fileTreeView->setRootIndex(m_fileSystemModel->index(fileInfo.absolutePath()));
        m_fileSystemModel->setRootPath(fileInfo.absolutePath());
        setCurrentDirectory(fileInfo.absoluteFilePath());
    } else {
        if(QFile::exists(fileInfo.absoluteFilePath()))
            emit openFile(fileInfo.absoluteFilePath());
    }
}
