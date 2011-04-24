#include "PlotterWidget.h"
#include "Plot2dWidget.h"
#include <QVBoxLayout>
#include <QHBoxLayout>
#include <QPushButton>
#include <QMdiSubWindow>

PlotterWidget::PlotterWidget(QWidget *parent)
    : QWidget(parent) {
    construct();
}

void PlotterWidget::addNew2dPlot() {
    QMdiSubWindow *mdiSubWindow = new QMdiSubWindow(this);
    mdiSubWindow->setWidget(new Plot2dWidget(this));
    mdiSubWindow->setWindowTitle("2d Plot");
    m_mdiArea->addSubWindow(mdiSubWindow);
    mdiSubWindow->resize(400, 300);
    mdiSubWindow->showMaximized();
}

void PlotterWidget::construct() {
    QVBoxLayout *layout = new QVBoxLayout();
    m_mdiArea = new QMdiArea(this);
        layout->addWidget(m_mdiArea);
        QWidget *buttonBar = new QWidget(this);
        QHBoxLayout *buttonBarLayout = new QHBoxLayout(this);
        QPushButton *createPlot2dButton = new QPushButton(tr("Create 2d Plot"), this);
        QPushButton *createPlot3dButton = new QPushButton(tr("Create 3d Plot"), this);
        createPlot3dButton->setEnabled(false);
        buttonBarLayout->addWidget(createPlot2dButton);
        buttonBarLayout->addWidget(createPlot3dButton);
        buttonBarLayout->addStretch();
        buttonBarLayout->setMargin(1);
        buttonBar->setLayout(buttonBarLayout);
    layout->addWidget(buttonBar);
    layout->setMargin(0);
    setLayout(layout);

    connect(createPlot2dButton, SIGNAL(clicked()), this, SLOT(addNew2dPlot()));
}
