#include "ImageViewerDockWidget.h"
#include <QLabel>
#include <QPixmap>
#include <QScrollArea>

ImageViewerDockWidget::ImageViewerDockWidget(QPixmap pixmap, QWidget *parent)
    : QDockWidget(parent),
      m_pixmap(pixmap) {
    construct();
}

void ImageViewerDockWidget::construct() {
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
