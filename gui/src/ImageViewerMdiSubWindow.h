#ifndef IMAGEVIEWERMDISUBWINDOW_H
#define IMAGEVIEWERMDISUBWINDOW_H

#include <QMdiSubWindow>

class ImageViewerMdiSubWindow : public QMdiSubWindow
{
public:
    ImageViewerMdiSubWindow(QPixmap pixmap, QWidget *parent = 0);

private:
    void construct();
    QPixmap m_pixmap;
};

#endif // IMAGEVIEWERMDISUBWINDOW_H
