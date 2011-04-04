#include "clientmanager.h"

ClientManager ClientManager::m_clientManager;

ClientManager::ClientManager() {
}

ClientManager& ClientManager::clientManager() {
    return ClientManager::m_clientManager;
}

Client *ClientManager::startProcess(QString command) {
    Client *client = new Client(command);
    m_activeClients.push_back(client);
    return client;
}
