#include "Plot2dWidget.h"
#include <QVBoxLayout>
#include <QHBoxLayout>
#include <QPushButton>

Plot2dView::Plot2dView(QWidget *parent)
    : QGLWidget(parent) {
    construct();
}

void Plot2dView::construct() {
}

void Plot2dView::initializeGL() {
    glClearColor(0.9, 0.9, 0.9, 0.0);
    glEnable(GL_POINT_SMOOTH);
    // glEnable(GL_LINE_SMOOTH);
    glEnable(GL_POLYGON_SMOOTH);
    glEnable(GL_BLEND);
    glBlendFunc(GL_SRC_ALPHA,GL_ONE_MINUS_SRC_ALPHA);
}

void Plot2dView::paintGL() {
    glClear(GL_COLOR_BUFFER_BIT);
    glBegin(GL_LINES);
        glColor3d(0.0, 0.0, 0.0);
        glVertex2d(0.1, 0.1);
        glVertex2d(0.9, 0.1);
        glVertex2d(0.1, 0.1);
        glVertex2d(0.1, 0.9);
    glEnd();

    glBegin(GL_POLYGON);
        glVertex2d(0.092, 0.9);
        glVertex2d(0.108, 0.9);
        glVertex2d(0.1, 0.93);
    glEnd();
    glBegin(GL_POLYGON);
        glVertex2d(0.9, 0.092);
        glVertex2d(0.9, 0.108);
        glVertex2d(0.93, 0.1);
    glEnd();

    renderText(0.8, 0.05, 0.0, "axis");
}

void Plot2dView::resizeGL(int w, int h) {
    glViewport(0, 0, w, h);
    glMatrixMode(GL_MODELVIEW_MATRIX);
    glLoadIdentity();

    glMatrixMode(GL_PROJECTION_MATRIX);
    glLoadIdentity();
    glOrtho(0.0, 1.0, 0.0, 1.0, 0.0, 100.0);
}

Plot2dWidget::Plot2dWidget(QWidget *parent) :
    QWidget(parent) {
    construct();
}

void Plot2dWidget::construct() {
    QVBoxLayout *layout = new QVBoxLayout();
    m_plot2dView = new Plot2dView(this);
    m_plot2dView->setSizePolicy(QSizePolicy::Expanding, QSizePolicy::Expanding);
    layout->addWidget(m_plot2dView);
        QWidget *buttonBar = new QWidget(this);
        QHBoxLayout *buttonBarLayout = new QHBoxLayout(this);
        QPushButton *exportButton = new QPushButton(tr("Export"), this);
        exportButton->setEnabled(false);
        buttonBarLayout->addWidget(exportButton);
        buttonBarLayout->addStretch();
        buttonBarLayout->setMargin(1);
        buttonBar->setLayout(buttonBarLayout);
        buttonBar->setSizePolicy(QSizePolicy::Expanding, QSizePolicy::Minimum);
    layout->addWidget(buttonBar);
    layout->setMargin(0);
    setLayout(layout);
}
