#include "ImageViewerMdiSubWindow.h"
#include <QLabel>
#include <QPixmap>
#include <QScrollArea>

ImageViewerMdiSubWindow::ImageViewerMdiSubWindow(QPixmap pixmap, QWidget *parent)
    : QMdiSubWindow(parent),
      m_pixmap(pixmap) {
    construct();
}

void ImageViewerMdiSubWindow::construct() {
    QLabel *label = new QLabel();
    label->setBackgroundRole(QPalette::Base);
    label->setSizePolicy(QSizePolicy::Ignored, QSizePolicy::Ignored);
    label->setScaledContents(true);
    label->setPixmap(m_pixmap);

    QScrollArea *scrollArea = new QScrollArea(this);
    scrollArea->setBackgroundRole(QPalette::Dark);
    scrollArea->setWidget(label);
    setWidget(scrollArea);
}
