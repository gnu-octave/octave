#ifndef IMAGEVIEWERDOCKWIDGET_H
#define IMAGEVIEWERDOCKWIDGET_H

#include <QDockWidget>

class ImageViewerDockWidget : public QDockWidget {
public:
    ImageViewerDockWidget(QPixmap pixmap, QWidget *parent = 0);

private:
    void construct();
    QPixmap m_pixmap;
};

#endif // IMAGEVIEWERDOCKWIDGET_H
