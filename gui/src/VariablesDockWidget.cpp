#include "VariablesDockWidget.h"
#include <QHBoxLayout>

VariablesDockWidget::VariablesDockWidget(QWidget *parent)
    : QDockWidget(parent) {
    construct();
}

void VariablesDockWidget::construct() {
    QStringList headerLabels;
    headerLabels << "Name" << "Type" << "Value";
    m_variablesTreeWidget = new QTreeWidget(this);
    m_variablesTreeWidget->setHeaderHidden(false);
    m_variablesTreeWidget->setHeaderLabels(headerLabels);
    QHBoxLayout *layout = new QHBoxLayout();

    setWindowTitle("Workspace");
    setWidget(new QWidget());

    layout->addWidget(m_variablesTreeWidget);
    layout->setMargin(2);

    widget()->setLayout(layout);

    QTreeWidgetItem *treeWidgetItem = new QTreeWidgetItem();
    treeWidgetItem->setData(0, 0, QString("Global"));
    m_variablesTreeWidget->insertTopLevelItem(0, treeWidgetItem);
    m_variablesTreeWidget->expandAll();
}

void VariablesDockWidget::setVariablesList(QList<OctaveLink::VariableMetaData> variablesList) {
    // This method may be a little bit confusing; variablesList is a complete list of all
    // variables that are in the workspace currently.
    QTreeWidgetItem *topLevelItem = m_variablesTreeWidget->topLevelItem(0);

    // First we check, if any variables that exist in the model tree have to be updated
    // or created. So we walk the variablesList check against the tree.
    foreach(OctaveLink::VariableMetaData variable, variablesList) {
        int childCount = topLevelItem->childCount();
        bool alreadyExists = false;
        QTreeWidgetItem *child;

        // Search for the corresponding item in the tree. If it has been found, child
        // will contain the appropriate QTreeWidgetItem* pointing at it.
        for(int i = 0; i < childCount; i++) {
            child = topLevelItem->child(i);
            if(child->data(0, 0).toString() == variable.variableName) {
                alreadyExists = true;
                break;
            }
        }

        // If it already exists, just update it.
        if(alreadyExists) {
            child->setData(0, 0, variable.variableName);
            child->setData(1, 0, variable.typeName);
        } else {
            // It does not exist, so create a new one and set the right values.
            child = new QTreeWidgetItem();
            child->setData(0, 0, variable.variableName);
            child->setData(1, 0, variable.typeName);
            topLevelItem->addChild(child);
        }
    }

    // Check the tree against the list for deleted variables.
    for(int i = 0; i < topLevelItem->childCount(); i++) {
        bool existsInVariableList = false;
        QTreeWidgetItem *child = topLevelItem->child(i);
        foreach(OctaveLink::VariableMetaData variable, variablesList) {
            if(variable.variableName == child->data(0, 0).toString()) {
                existsInVariableList = true;
            }
        }

        if(!existsInVariableList) {
            topLevelItem->removeChild(child);
            delete child;
            i--;
        }
    }
}
