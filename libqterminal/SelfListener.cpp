#include "SelfListener.h"

SelfListener::SelfListener(int a, QObject *parent) :
    QThread(parent) {
    _a = a;
}

void SelfListener::run() {
    char buf[1025];
    int len;

    while(1) {
        len = ::read(_a, buf, 1024);
        if (len > 0) {
           buf[len] = 0; // Just in case.
           emit recvData(buf, len);
        }
    }
}
