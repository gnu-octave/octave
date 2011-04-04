#ifndef CLIENTMANAGER_H
#define CLIENTMANAGER_H

#include <QList>
#include <QObject>
#include "client.h"

class Client;
class ClientManager : public QObject {
    Q_OBJECT
public:
    static ClientManager& clientManager();
    Client *startProcess(QString command);

private:
    ClientManager();
    static ClientManager m_clientManager;
    QList<Client*> m_activeClients;
};

#endif // CLIENTMANAGER_H
