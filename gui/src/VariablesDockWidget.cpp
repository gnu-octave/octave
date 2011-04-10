#include "VariablesDockWidget.h"
#include <QHBoxLayout>

VariablesDockWidget::VariablesDockWidget(QWidget *parent)
    : QDockWidget(parent) {
    construct();
}

void VariablesDockWidget::construct() {
    m_variablesTreeView = new QTreeView(this);

    QHBoxLayout *layout = new QHBoxLayout();

    setWindowTitle("Variables");
    setWidget(new QWidget());

    layout->addWidget(m_variablesTreeView);
    layout->setMargin(2);

    widget()->setLayout(layout);
}
