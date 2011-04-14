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
    treeWidgetItem->setData(0, 0, QString("Persistent"));
    m_variablesTreeWidget->insertTopLevelItem(0, treeWidgetItem);
    m_variablesTreeWidget->expandAll();
}

void VariablesDockWidget::updateTreeEntry(QTreeWidgetItem *treeItem, SymbolRecord symbolRecord) {
    treeItem->setData(0, 0, QString(symbolRecord.name().c_str()));
    treeItem->setData(1, 0, QString(symbolRecord.varval().type_name().c_str()));

    QString type = QString(symbolRecord.varval().type_name().c_str());
    if(type == "string") {
        QString stringValue(symbolRecord.varval().string_value().c_str());
        treeItem->setData(2, 0, stringValue);
    } else if(type == "scalar") {
        double scalarValue = symbolRecord.varval().scalar_value();
        treeItem->setData(2, 0, QString("%1").arg(scalarValue));
    } else if(type == "matrix") {
        Matrix matrixValue = symbolRecord.varval().matrix_value();
        // TODO: Display matrix.
    } else {
        treeItem->setData(2, 0, QString("<Type not recognized>"));
    }
}

void VariablesDockWidget::setVariablesList(QList<SymbolRecord> symbolTable) {
    // This method may be a little bit confusing; variablesList is a complete list of all
    // variables that are in the workspace currently.
    QTreeWidgetItem *topLevelItem = m_variablesTreeWidget->topLevelItem(0);

    // First we check, if any variables that exist in the model tree have to be updated
    // or created. So we walk the variablesList check against the tree.
    foreach(SymbolRecord symbolRecord, symbolTable) {
        int childCount = topLevelItem->childCount();
        bool alreadyExists = false;
        QTreeWidgetItem *child;

        // Search for the corresponding item in the tree. If it has been found, child
        // will contain the appropriate QTreeWidgetItem* pointing at it.
        for(int i = 0; i < childCount; i++) {
            child = topLevelItem->child(i);
            if(child->data(0, 0).toString() == QString(symbolRecord.name().c_str())) {
                alreadyExists = true;
                break;
            }
        }

        // If it already exists, just update it.
        if(alreadyExists) {
            updateTreeEntry(child, symbolRecord);
        } else {
            // It does not exist, so create a new one and set the right values.
            child = new QTreeWidgetItem();
            updateTreeEntry(child, symbolRecord);
            topLevelItem->addChild(child);
        }
    }

    // Check the tree against the list for deleted variables.
    for(int i = 0; i < topLevelItem->childCount(); i++) {
        bool existsInVariableList = false;
        QTreeWidgetItem *child = topLevelItem->child(i);
        foreach(SymbolRecord symbolRecord, symbolTable) {
            if(QString(symbolRecord.name().c_str()) == child->data(0, 0).toString()) {
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
