#ifndef PLOT2DWIDGET_H
#define PLOT2DWIDGET_H

#include <QWidget>
#include <QGLWidget>

class Plot2dView : public QGLWidget {
public:
    explicit Plot2dView(QWidget *parent = 0);

protected:
    void initializeGL();
    void paintGL();
    void resizeGL(int w, int h);

private:
    void construct();
};

class Plot2dWidget : public QWidget
{
    Q_OBJECT
public:
    explicit Plot2dWidget(QWidget *parent = 0);

signals:

public slots:

private:
    void construct();

    Plot2dView *m_plot2dView;
};

#endif // PLOT2DWIDGET_H
