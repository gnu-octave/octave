#include "Plot2dWidget.h"
#include <QVBoxLayout>
#include <QHBoxLayout>
#include <QPushButton>
#include <QTimer>
#include <QLabel>
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
    m_zoomAcceleration = 0.0;
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
    glScaled(m_zoom * 1.5, m_zoom * 1.5, 0.0);
    glTranslated(-0.5 - m_scrollX, -0.5 - m_scrollY, 0.0);

    glClear(GL_COLOR_BUFFER_BIT);
    glBegin(GL_LINES);
        glColor3d(1.0, 1.0, 1.0);
        glVertex2d(0.0, 0.0);
        glVertex2d(1.0, 0.0);
        glVertex2d(0.0, 0.0);
        glVertex2d(0.0, 1.0);
    glEnd();

    for(double phi = 0.0; phi < 2*3.141; phi += 2*3.141 / 3) {
        glBegin(GL_LINES);
            glColor3d(phi / (2 * 3.141), 1.0, 1.0 - phi / (2 * 3.141));
            for(double d = 0.0; d < 1.0; d +=0.01)
                glVertex2d(d, sin(d*2*3.141 + phi) / 2 + 0.5);
        glEnd();
    }

    glMatrixMode(GL_MODELVIEW_MATRIX);
    glLoadIdentity();

    glColor3d(1.0, 1.0, 1.0);
    renderText(-0.9, -0.9, 0.0, QString("Scaling: %1%, Translation: (%2/%3)")
                                .arg(m_zoom * 100)
                                .arg(m_scrollX)
                                .arg(m_scrollY));
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
    m_zoomAcceleration += ((double)wheelEvent->delta()) / 1000;
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
        m_scrollX -= ((double)mouseEvent->x() - m_lastMouseButtonDownX) / (100 * m_zoom);
        m_scrollY += ((double)mouseEvent->y() - m_lastMouseButtonDownY) / (100 * m_zoom);
        m_lastMouseButtonDownX = (double)mouseEvent->x();
        m_lastMouseButtonDownY = (double)mouseEvent->y();
    }
    updateGL();
}

void Plot2dView::animate() {
    m_zoom += m_zoomAcceleration;
    if(m_zoom < 0)
        m_zoom = 0;
    m_zoomAcceleration *= 0.5;
    //if(abs(m_zoomAcceleration) < 0.001)
    //    m_zoomAcceleration = 0;
    updateGL();
}

Plot2dWidget::Plot2dWidget(QWidget *parent)
    : QWidget(parent) {
    construct();
}

void Plot2dWidget::dataSourceTypeChanged(QString type) {
    if(type == "Sampled") {
        m_dataSourceStackedWidget->setCurrentIndex(0);
    } else if(type == "Parameterized") {
        m_dataSourceStackedWidget->setCurrentIndex(1);
    }
}

void Plot2dWidget::construct() {
    QVBoxLayout *layout = new QVBoxLayout();
    m_plot2dView = new Plot2dView(this);
    m_plot2dView->setSizePolicy(QSizePolicy::Expanding, QSizePolicy::Expanding);
    layout->addWidget(m_plot2dView);

    m_tabWidget = new QTabWidget(this);
    m_tabWidget->setSizePolicy(QSizePolicy::Expanding, QSizePolicy::Minimum);
    layout->addWidget(m_tabWidget);

    m_generalTab = new QWidget(this);
    m_dataSourceTab = new QWidget(this);
    m_verticalAxisTab = new QWidget(this);
    m_horizontalAxisTab = new QWidget(this);
    m_tabWidget->addTab(m_generalTab, tr("General"));
    m_tabWidget->addTab(m_dataSourceTab, tr("Data Source"));
    m_tabWidget->addTab(m_verticalAxisTab, tr("Vertical Axis"));
    m_tabWidget->addTab(m_horizontalAxisTab, tr("Horizontal Axis"));

        // Build general tab.
        QHBoxLayout *generalTabLayout = new QHBoxLayout();
        m_generalTab->setLayout(generalTabLayout);

        // Build data source tab.
        QHBoxLayout *dataSourceTabLayout = new QHBoxLayout();
        m_dataSourceTypeComboBox = new QComboBox(this);
        m_dataSourceTypeComboBox->addItem(tr("Sampled"));
        m_dataSourceTypeComboBox->addItem(tr("Parameterized"));

        m_dataSourceStackedWidget = new QStackedWidget(this);
        m_sampledFromLineEdit = new QLineEdit("0", this);
        m_sampledToLineEdit = new QLineEdit("4096", this);
        m_parameterizedFromLineEdit = new QLineEdit("0.0", this);
        m_parameterizedToLineEdit = new QLineEdit("1.0", this);

        m_sampledFromLineEdit->setAlignment(Qt::AlignRight);
        m_sampledToLineEdit->setAlignment(Qt::AlignRight);
        m_parameterizedFromLineEdit->setAlignment(Qt::AlignRight);
        m_parameterizedToLineEdit->setAlignment(Qt::AlignRight);

        QWidget *sampledDataSourceRange = new QWidget(this);
        QHBoxLayout *sampledDataSourceLayout = new QHBoxLayout();
        sampledDataSourceLayout->addWidget(new QLabel(tr("From sample"), this));
        sampledDataSourceLayout->addWidget(m_sampledFromLineEdit);
        sampledDataSourceLayout->addWidget(new QLabel(tr("to sample"), this));
        sampledDataSourceLayout->addWidget(m_sampledToLineEdit);
        sampledDataSourceLayout->addWidget(new QLabel(".", this));
        sampledDataSourceLayout->setMargin(0);
        sampledDataSourceRange->setLayout(sampledDataSourceLayout);

        QWidget *parameterizedDataSourceRange = new QWidget(this);
        QHBoxLayout *parameterizedDataSourceLayout = new QHBoxLayout();
        parameterizedDataSourceLayout->addWidget(new QLabel(tr("From value"), this));
        parameterizedDataSourceLayout->addWidget(m_parameterizedFromLineEdit);
        parameterizedDataSourceLayout->addWidget(new QLabel(tr("to value"), this));
        parameterizedDataSourceLayout->addWidget(m_parameterizedToLineEdit);
        parameterizedDataSourceLayout->addWidget(new QLabel(".", this));
        parameterizedDataSourceLayout->setMargin(0);
        parameterizedDataSourceRange->setLayout(parameterizedDataSourceLayout);

        m_dataSourceStackedWidget->addWidget(sampledDataSourceRange);
        m_dataSourceStackedWidget->addWidget(parameterizedDataSourceRange);

        m_refreshDataRangeButton = new QPushButton(tr("Refresh"), this);
        dataSourceTabLayout->addWidget(new QLabel(tr("Type:"), this));
        dataSourceTabLayout->addWidget(m_dataSourceTypeComboBox);
        dataSourceTabLayout->addWidget(m_dataSourceStackedWidget);
        dataSourceTabLayout->addWidget(m_refreshDataRangeButton);
        m_dataSourceTab->setLayout(dataSourceTabLayout);

    layout->setMargin(0);
    setLayout(layout);

    connect(m_dataSourceTypeComboBox, SIGNAL(currentIndexChanged(QString)), this, SLOT(dataSourceTypeChanged(QString)));
}
