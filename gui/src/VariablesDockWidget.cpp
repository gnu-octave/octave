#include "VariablesDockWidget.h"
#include <QHBoxLayout>
#include <QVBoxLayout>
#include <QPushButton>

VariablesDockWidget::VariablesDockWidget(QWidget *parent)
    : QDockWidget(parent) {
    setObjectName("VariablesDockWidget");
    construct();
}

void VariablesDockWidget::construct() {
    m_updateSemaphore = new QSemaphore(1);
    QStringList headerLabels;
    headerLabels << tr("Name") << tr("Type") << tr("Value");
    m_variablesTreeWidget = new QTreeWidget(this);
    m_variablesTreeWidget->setHeaderHidden(false);
    m_variablesTreeWidget->setHeaderLabels(headerLabels);
    QVBoxLayout *layout = new QVBoxLayout();

    setWindowTitle(tr("Workspace"));
    setWidget(new QWidget());

    layout->addWidget(m_variablesTreeWidget);
    QWidget *buttonBar = new QWidget(this);
    layout->addWidget(buttonBar);

        QHBoxLayout *buttonBarLayout = new QHBoxLayout();
        QPushButton *saveWorkspaceButton = new QPushButton(tr("Save"), buttonBar);
        QPushButton *loadWorkspaceButton = new QPushButton(tr("Load"), buttonBar);
        QPushButton *clearWorkspaceButton = new QPushButton(tr("Clear"), buttonBar);
        buttonBarLayout->addWidget(saveWorkspaceButton);
        buttonBarLayout->addWidget(loadWorkspaceButton);
        buttonBarLayout->addWidget(clearWorkspaceButton);
        buttonBarLayout->setMargin(2);
        buttonBar->setLayout(buttonBarLayout);

    layout->setMargin(2);
    widget()->setLayout(layout);

    connect(saveWorkspaceButton, SIGNAL(clicked()), this, SLOT(emitSaveWorkspace()));
    connect(loadWorkspaceButton, SIGNAL(clicked()), this, SLOT(emitLoadWorkspace()));
    connect(clearWorkspaceButton, SIGNAL(clicked()), this, SLOT(emitClearWorkspace()));

    QTreeWidgetItem *treeWidgetItem = new QTreeWidgetItem();
    treeWidgetItem->setData(0, 0, QString(tr("Local")));
    m_variablesTreeWidget->insertTopLevelItem(0, treeWidgetItem);

    treeWidgetItem = new QTreeWidgetItem();
    treeWidgetItem->setData(0, 0, QString(tr("Global")));
    m_variablesTreeWidget->insertTopLevelItem(1, treeWidgetItem);

    treeWidgetItem = new QTreeWidgetItem();
    treeWidgetItem->setData(0, 0, QString(tr("Persistent")));
    m_variablesTreeWidget->insertTopLevelItem(2, treeWidgetItem);

    treeWidgetItem = new QTreeWidgetItem();
    treeWidgetItem->setData(0, 0, QString(tr("Hidden")));
    m_variablesTreeWidget->insertTopLevelItem(3, treeWidgetItem);

    m_variablesTreeWidget->expandAll();
    m_variablesTreeWidget->setAlternatingRowColors(true);
    m_variablesTreeWidget->setAnimated(true);    
}

void VariablesDockWidget::updateTreeEntry(QTreeWidgetItem *treeItem, SymbolRecord symbolRecord) {
    treeItem->setData(0, 0, QString(symbolRecord.name().c_str()));
    treeItem->setData(1, 0, QString(symbolRecord.varval().type_name().c_str()));
    treeItem->setData(2, 0, OctaveLink::octaveValueAsQString(symbolRecord.varval()));
}

void VariablesDockWidget::setVariablesList(QList<SymbolRecord> symbolTable) {
    m_updateSemaphore->acquire();
    // Split the symbol table into its different scopes.
    QList<SymbolRecord> localSymbolTable;
    QList<SymbolRecord> globalSymbolTable;
    QList<SymbolRecord> persistentSymbolTable;
    QList<SymbolRecord> hiddenSymbolTable;

    foreach(SymbolRecord symbolRecord, symbolTable) {
        // It's true that being global or hidden includes it's can mean it's also locally visible,
        // but we want to distinguish that here.
        if(symbolRecord.is_local() && !symbolRecord.is_global() && !symbolRecord.is_hidden()) {
            localSymbolTable.append(symbolRecord);
        }

        if(symbolRecord.is_global()) {
            globalSymbolTable.append(symbolRecord);
        }

        if(symbolRecord.is_persistent()) {
            persistentSymbolTable.append(symbolRecord);
        }

        if(symbolRecord.is_hidden()) {
            hiddenSymbolTable.append(symbolRecord);
        }
    }

    updateScope(0, localSymbolTable);
    updateScope(1, globalSymbolTable);
    updateScope(2, persistentSymbolTable);
    updateScope(3, hiddenSymbolTable);
    m_updateSemaphore->release();
}

void VariablesDockWidget::updateScope(int topLevelItemIndex, QList<SymbolRecord> symbolTable) {
    // This method may be a little bit confusing; variablesList is a complete list of all
    // variables that are in the workspace currently.
    QTreeWidgetItem *topLevelItem = m_variablesTreeWidget->topLevelItem(topLevelItemIndex);

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

void VariablesDockWidget::emitSaveWorkspace() {
    emit saveWorkspace();
}

void VariablesDockWidget::emitLoadWorkspace() {
    emit loadWorkspace();
}

void VariablesDockWidget::emitClearWorkspace() {
    emit clearWorkspace();
}
