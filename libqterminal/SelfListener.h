#ifndef SELFLISTENER_H
#define SELFLISTENER_H

#include <QThread>

class SelfListener : public QThread
{
    Q_OBJECT
public:
    explicit SelfListener(int a, QObject *parent = 0);

signals:
    void recvData(const char* stdOutBuffer, int stdOutlen);

public slots:

protected:
    void run();
    int _a;
};

#endif // SELFLISTENER_H
