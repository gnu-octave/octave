/****************************************************************************
** Meta object code from reading C++ file 'Pty.h'
**
** Created: Thu Apr 7 10:08:47 2011
**      by: The Qt Meta Object Compiler version 62 (Qt 4.7.2)
**
** WARNING! All changes made in this file will be lost!
*****************************************************************************/

#include "../lib/Pty.h"
#if !defined(Q_MOC_OUTPUT_REVISION)
#error "The header file 'Pty.h' doesn't include <QObject>."
#elif Q_MOC_OUTPUT_REVISION != 62
#error "This file was generated using the moc from 4.7.2. It"
#error "cannot be used with the include files from this version of Qt."
#error "(The moc has changed too much.)"
#endif

QT_BEGIN_MOC_NAMESPACE
static const uint qt_meta_data_Konsole__Pty[] = {

 // content:
       5,       // revision
       0,       // classname
       0,    0, // classinfo
      10,   14, // methods
       0,    0, // properties
       0,    0, // enums/sets
       0,    0, // constructors
       0,       // flags
       3,       // signalCount

 // signals: signature, parameters, type, tag, flags
      23,   14,   13,   13, 0x05,
      47,   33,   13,   13, 0x05,
      77,   13,   13,   13, 0x05,

 // slots: signature, parameters, type, tag, flags
      94,   91,   13,   13, 0x0a,
     117,  112,   13,   13, 0x0a,
     131,   33,   13,   13, 0x0a,
     157,   13,   13,   13, 0x08,
     182,  167,   13,   13, 0x08,
     217,   13,   13,   13, 0x08,
     230,   13,   13,   13, 0x08,

       0        // eod
};

static const char qt_meta_stringdata_Konsole__Pty[] = {
    "Konsole::Pty\0\0exitCode\0done(int)\0"
    "buffer,length\0receivedData(const char*,int)\0"
    "bufferEmpty()\0on\0setUtf8Mode(bool)\0"
    "lock\0lockPty(bool)\0sendData(const char*,int)\0"
    "donePty()\0,buffer,length\0"
    "dataReceived(K3Process*,char*,int)\0"
    "doSendJobs()\0writeReady()\0"
};

const QMetaObject Konsole::Pty::staticMetaObject = {
    { &K3Process::staticMetaObject, qt_meta_stringdata_Konsole__Pty,
      qt_meta_data_Konsole__Pty, 0 }
};

#ifdef Q_NO_DATA_RELOCATION
const QMetaObject &Konsole::Pty::getStaticMetaObject() { return staticMetaObject; }
#endif //Q_NO_DATA_RELOCATION

const QMetaObject *Konsole::Pty::metaObject() const
{
    return QObject::d_ptr->metaObject ? QObject::d_ptr->metaObject : &staticMetaObject;
}

void *Konsole::Pty::qt_metacast(const char *_clname)
{
    if (!_clname) return 0;
    if (!strcmp(_clname, qt_meta_stringdata_Konsole__Pty))
        return static_cast<void*>(const_cast< Pty*>(this));
    return K3Process::qt_metacast(_clname);
}

int Konsole::Pty::qt_metacall(QMetaObject::Call _c, int _id, void **_a)
{
    _id = K3Process::qt_metacall(_c, _id, _a);
    if (_id < 0)
        return _id;
    if (_c == QMetaObject::InvokeMetaMethod) {
        switch (_id) {
        case 0: done((*reinterpret_cast< int(*)>(_a[1]))); break;
        case 1: receivedData((*reinterpret_cast< const char*(*)>(_a[1])),(*reinterpret_cast< int(*)>(_a[2]))); break;
        case 2: bufferEmpty(); break;
        case 3: setUtf8Mode((*reinterpret_cast< bool(*)>(_a[1]))); break;
        case 4: lockPty((*reinterpret_cast< bool(*)>(_a[1]))); break;
        case 5: sendData((*reinterpret_cast< const char*(*)>(_a[1])),(*reinterpret_cast< int(*)>(_a[2]))); break;
        case 6: donePty(); break;
        case 7: dataReceived((*reinterpret_cast< K3Process*(*)>(_a[1])),(*reinterpret_cast< char*(*)>(_a[2])),(*reinterpret_cast< int(*)>(_a[3]))); break;
        case 8: doSendJobs(); break;
        case 9: writeReady(); break;
        default: ;
        }
        _id -= 10;
    }
    return _id;
}

// SIGNAL 0
void Konsole::Pty::done(int _t1)
{
    void *_a[] = { 0, const_cast<void*>(reinterpret_cast<const void*>(&_t1)) };
    QMetaObject::activate(this, &staticMetaObject, 0, _a);
}

// SIGNAL 1
void Konsole::Pty::receivedData(const char * _t1, int _t2)
{
    void *_a[] = { 0, const_cast<void*>(reinterpret_cast<const void*>(&_t1)), const_cast<void*>(reinterpret_cast<const void*>(&_t2)) };
    QMetaObject::activate(this, &staticMetaObject, 1, _a);
}

// SIGNAL 2
void Konsole::Pty::bufferEmpty()
{
    QMetaObject::activate(this, &staticMetaObject, 2, 0);
}
QT_END_MOC_NAMESPACE
