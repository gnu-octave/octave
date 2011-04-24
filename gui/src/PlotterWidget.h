#ifndef PLOTTERWIDGET_H
#define PLOTTERWIDGET_H

#include <QWidget>
#include <QMdiArea>

class PlotterWidget : public QWidget {
    Q_OBJECT
public:
    PlotterWidget(QWidget *parent = 0);

public slots:
    void addNew2dPlot();

private:
    void construct();
    QMdiArea *m_mdiArea;
};

#endif // PLOTTERWIDGET_H
