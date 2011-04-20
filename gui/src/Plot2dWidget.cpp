#include "Plot2dWidget.h"
#include <QVBoxLayout>
#include <QHBoxLayout>
#include <QPushButton>
#include <QTimer>
#include <math.h>

Plot2dView::Plot2dView(QWidget *parent)
    : QGLWidget(parent) {
    construct();
}

void Plot2dView::construct() {
    QTimer *animationTimer = new QTimer(this);
    animationTimer->setInterval(20);
    animationTimer->start();
    m_zoom = 1.0;
    m_scrollX = 0.0;
    m_scrollY = 0.0;
    m_leftMouseButtonDown = false;
    connect(animationTimer, SIGNAL(timeout()), this, SLOT(animate()));
}

void Plot2dView::initializeGL() {
    glClearColor(0.0,0.0, 0.0, 0.0);
    glEnable(GL_POINT_SMOOTH);
    // glEnable(GL_LINE_SMOOTH);
    glEnable(GL_POLYGON_SMOOTH);
    glEnable(GL_BLEND);
    glBlendFunc(GL_SRC_ALPHA,GL_ONE_MINUS_SRC_ALPHA);
}

void Plot2dView::paintGL() {
    glMatrixMode(GL_MODELVIEW_MATRIX);
    glLoadIdentity();
    glScaled(m_zoom, m_zoom, 0.0);
    glTranslated(-0.5 - m_scrollX, -0.5 - m_scrollY, 0.0);

    glClear(GL_COLOR_BUFFER_BIT);
    glBegin(GL_LINES);
        glColor3d(1.0, 1.0, 1.0);
        glVertex2d(0.1, 0.1);
        glVertex2d(0.9, 0.1);
        glVertex2d(0.1, 0.1);
        glVertex2d(0.1, 0.9);
    glEnd();

    glBegin(GL_POLYGON);
        glVertex2d(0.092, 0.9);
        glVertex2d(0.108, 0.9);
        glVertex2d(0.1, 0.92);
    glEnd();
    glBegin(GL_POLYGON);
        glVertex2d(0.9, 0.092);
        glVertex2d(0.9, 0.108);
        glVertex2d(0.92, 0.1);
    glEnd();

    renderText(0.8, 0.05, 0.0, "axis");
}

void Plot2dView::resizeGL(int w, int h) {
    glViewport(0, 0, w, h);
    glMatrixMode(GL_MODELVIEW_MATRIX);
    glLoadIdentity();

    glMatrixMode(GL_PROJECTION_MATRIX);
    glLoadIdentity();
    glOrtho(-1.0, 1.0, -1.0, 1.0, 0.0, 100.0);
}

void Plot2dView::wheelEvent(QWheelEvent *wheelEvent) {
    m_zoomAcceleration += ((double)wheelEvent->delta()) / 5000;
    wheelEvent->accept();
    updateGL();
}

void Plot2dView::mousePressEvent(QMouseEvent *mouseEvent) {
    if(mouseEvent->button() == Qt::LeftButton) {
        m_leftMouseButtonDown = true;
        m_lastMouseButtonDownX = mouseEvent->x();
        m_lastMouseButtonDownY = mouseEvent->y();
        mouseEvent->accept();
    }
}

void Plot2dView::mouseReleaseEvent(QMouseEvent *mouseEvent) {
    if(mouseEvent->button() == Qt::LeftButton) {
        m_leftMouseButtonDown = false;
        mouseEvent->accept();
    }
}

void Plot2dView::mouseMoveEvent(QMouseEvent *mouseEvent) {
    if(m_leftMouseButtonDown) {
        m_scrollX -= ((double)mouseEvent->x() - m_lastMouseButtonDownX) / 100;
        m_scrollY += ((double)mouseEvent->y() - m_lastMouseButtonDownY) / 100;
        m_lastMouseButtonDownX = (double)mouseEvent->x();
        m_lastMouseButtonDownY = (double)mouseEvent->y();
    }
    updateGL();
}

void Plot2dView::animate() {
    m_zoom += m_zoomAcceleration;
    if(m_zoom < 0)
        m_zoom = 0;
    m_zoomAcceleration *= 0.2;
    if(abs(m_zoomAcceleration) < 0.01)
        m_zoomAcceleration = 0;
    updateGL();
}

Plot2dWidget::Plot2dWidget(QWidget *parent)
    : QWidget(parent) {
    construct();
}

void Plot2dWidget::construct() {
    QVBoxLayout *layout = new QVBoxLayout();
    m_plot2dView = new Plot2dView(this);
    m_plot2dView->setSizePolicy(QSizePolicy::Expanding, QSizePolicy::Expanding);
    layout->addWidget(m_plot2dView);

    m_tabWidget = new QTabWidget(this);
    m_tabWidget->setSizePolicy(QSizePolicy::Expanding, QSizePolicy::Minimum);
    layout->addWidget(m_tabWidget);

    m_dataSourceTab = new QWidget(this);
    m_verticalAxisTab = new QWidget(this);
    m_horizontalAxisTab = new QWidget(this);
    m_tabWidget->addTab(m_dataSourceTab, tr("Data Source"));
    m_tabWidget->addTab(m_verticalAxisTab, tr("Vertical Axis"));
    m_tabWidget->addTab(m_horizontalAxisTab, tr("Horizontal Axis"));

        // Build data source tab.
        QHBoxLayout *dataSourceTabLayout = new QHBoxLayout();

        m_dataSourceTypeComboBox = new QComboBox(this);
        m_dataSourceTypeComboBox->addItem(tr("Parameterized"));
        m_dataSourceTypeComboBox->addItem(tr("Sampled"));
        dataSourceTabLayout->addWidget(m_dataSourceTypeComboBox);
        dataSourceTabLayout->addStretch();
        m_dataSourceTab->setLayout(dataSourceTabLayout);

    layout->setMargin(0);
    setLayout(layout);

}
