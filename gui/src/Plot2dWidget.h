#ifndef PLOT2DWIDGET_H
#define PLOT2DWIDGET_H

#include <QWidget>
#include <QGLWidget>
#include <QTabWidget>
#include <QStackedWidget>
#include <QComboBox>
#include <QWheelEvent>
#include <QMouseEvent>
#include <QLineEdit>
#include <QPushButton>

class Plot2dView : public QGLWidget {
    Q_OBJECT
public:
    explicit Plot2dView(QWidget *parent = 0);

protected:
    void initializeGL();
    void paintGL();
    void resizeGL(int w, int h);
    void wheelEvent(QWheelEvent *wheelEvent);
    void mousePressEvent(QMouseEvent *mouseEvent);
    void mouseReleaseEvent(QMouseEvent *mouseEvent);
    void mouseMoveEvent(QMouseEvent *mouseEvent);

private slots:
    void animate();

private:
    void construct();
    bool m_leftMouseButtonDown;
    double m_lastMouseButtonDownX;
    double m_lastMouseButtonDownY;
    double m_scrollX;
    double m_scrollY;
    double m_zoom;
    double m_zoomAcceleration;
};

class Plot2dWidget : public QWidget
{
    Q_OBJECT
public:
    explicit Plot2dWidget(QWidget *parent = 0);

signals:

public slots:
    void dataSourceTypeChanged(QString type);

private:
    void construct();

    Plot2dView *m_plot2dView;
    QTabWidget *m_tabWidget;
    QWidget *m_generalTab;
    QWidget *m_dataSourceTab;
    QWidget *m_verticalAxisTab;
    QWidget *m_horizontalAxisTab;
    QComboBox *m_dataSourceTypeComboBox;
    QStackedWidget *m_dataSourceStackedWidget;
    QLineEdit *m_sampledFromLineEdit;
    QLineEdit *m_sampledToLineEdit;
    QLineEdit *m_parameterizedFromLineEdit;
    QLineEdit *m_parameterizedToLineEdit;
    QPushButton *m_refreshDataRangeButton;
};

#endif // PLOT2DWIDGET_H
