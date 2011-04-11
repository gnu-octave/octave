#include "HistoryDockWidget.h"
#include <QHBoxLayout>

HistoryDockWidget::HistoryDockWidget(QWidget *parent)
    : QDockWidget(parent) {
    construct();
}

void HistoryDockWidget::construct() {
    m_historyListModel = new QStringListModel();
    m_historyListView = new QListView(this);
    m_historyListView->setModel(m_historyListModel);

    QHBoxLayout *layout = new QHBoxLayout();

    setWindowTitle("History");
    setWidget(new QWidget());

    layout->addWidget(m_historyListView);
    layout->setMargin(2);

    widget()->setLayout(layout);
}


void HistoryDockWidget::updateHistory(string_vector historyEntries) {
    QStringList stringList = m_historyListModel->stringList();
    for(size_t i = 0; i < historyEntries.length(); i++) {
        QString command(historyEntries[i].c_str());
        if(!command.startsWith("#"))
            stringList.push_front(QString("%1: ").arg(stringList.size() + 1) + command);
    }
    m_historyListModel->setStringList(stringList);
    emit information("History updated.");
}
