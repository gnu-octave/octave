/****************************************************************************
** Meta object code from reading C++ file 'k3process.h'
**
** Created: Thu Apr 7 10:08:48 2011
**      by: The Qt Meta Object Compiler version 62 (Qt 4.7.2)
**
** WARNING! All changes made in this file will be lost!
*****************************************************************************/

#include "../lib/k3process.h"
#if !defined(Q_MOC_OUTPUT_REVISION)
#error "The header file 'k3process.h' doesn't include <QObject>."
#elif Q_MOC_OUTPUT_REVISION != 62
#error "This file was generated using the moc from 4.7.2. It"
#error "cannot be used with the include files from this version of Qt."
#error "(The moc has changed too much.)"
#endif

QT_BEGIN_MOC_NAMESPACE
static const uint qt_meta_data_K3Process[] = {

 // content:
       5,       // revision
       0,       // classname
       0,    0, // classinfo
       8,   14, // methods
       0,    0, // properties
       0,    0, // enums/sets
       0,    0, // constructors
       0,       // flags
       5,       // signalCount

 // signals: signature, parameters, type, tag, flags
      16,   11,   10,   10, 0x05,
      61,   42,   10,   10, 0x05,
     105,   98,   10,   10, 0x05,
     130,   42,   10,   10, 0x05,
     167,   11,   10,   10, 0x05,

 // slots: signature, parameters, type, tag, flags
     195,  190,   10,   10, 0x09,
     216,  190,   10,   10, 0x09,
     242,  236,   10,   10, 0x09,

       0        // eod
};

static const char qt_meta_stringdata_K3Process[] = {
    "K3Process\0\0proc\0processExited(K3Process*)\0"
    "proc,buffer,buflen\0"
    "receivedStdout(K3Process*,char*,int)\0"
    "fd,len\0receivedStdout(int,int&)\0"
    "receivedStderr(K3Process*,char*,int)\0"
    "wroteStdin(K3Process*)\0fdno\0"
    "slotChildOutput(int)\0slotChildError(int)\0"
    "dummy\0slotSendData(int)\0"
};

const QMetaObject K3Process::staticMetaObject = {
    { &QObject::staticMetaObject, qt_meta_stringdata_K3Process,
      qt_meta_data_K3Process, 0 }
};

#ifdef Q_NO_DATA_RELOCATION
const QMetaObject &K3Process::getStaticMetaObject() { return staticMetaObject; }
#endif //Q_NO_DATA_RELOCATION

const QMetaObject *K3Process::metaObject() const
{
    return QObject::d_ptr->metaObject ? QObject::d_ptr->metaObject : &staticMetaObject;
}

void *K3Process::qt_metacast(const char *_clname)
{
    if (!_clname) return 0;
    if (!strcmp(_clname, qt_meta_stringdata_K3Process))
        return static_cast<void*>(const_cast< K3Process*>(this));
    return QObject::qt_metacast(_clname);
}

int K3Process::qt_metacall(QMetaObject::Call _c, int _id, void **_a)
{
    _id = QObject::qt_metacall(_c, _id, _a);
    if (_id < 0)
        return _id;
    if (_c == QMetaObject::InvokeMetaMethod) {
        switch (_id) {
        case 0: processExited((*reinterpret_cast< K3Process*(*)>(_a[1]))); break;
        case 1: receivedStdout((*reinterpret_cast< K3Process*(*)>(_a[1])),(*reinterpret_cast< char*(*)>(_a[2])),(*reinterpret_cast< int(*)>(_a[3]))); break;
        case 2: receivedStdout((*reinterpret_cast< int(*)>(_a[1])),(*reinterpret_cast< int(*)>(_a[2]))); break;
        case 3: receivedStderr((*reinterpret_cast< K3Process*(*)>(_a[1])),(*reinterpret_cast< char*(*)>(_a[2])),(*reinterpret_cast< int(*)>(_a[3]))); break;
        case 4: wroteStdin((*reinterpret_cast< K3Process*(*)>(_a[1]))); break;
        case 5: slotChildOutput((*reinterpret_cast< int(*)>(_a[1]))); break;
        case 6: slotChildError((*reinterpret_cast< int(*)>(_a[1]))); break;
        case 7: slotSendData((*reinterpret_cast< int(*)>(_a[1]))); break;
        default: ;
        }
        _id -= 8;
    }
    return _id;
}

// SIGNAL 0
void K3Process::processExited(K3Process * _t1)
{
    void *_a[] = { 0, const_cast<void*>(reinterpret_cast<const void*>(&_t1)) };
    QMetaObject::activate(this, &staticMetaObject, 0, _a);
}

// SIGNAL 1
void K3Process::receivedStdout(K3Process * _t1, char * _t2, int _t3)
{
    void *_a[] = { 0, const_cast<void*>(reinterpret_cast<const void*>(&_t1)), const_cast<void*>(reinterpret_cast<const void*>(&_t2)), const_cast<void*>(reinterpret_cast<const void*>(&_t3)) };
    QMetaObject::activate(this, &staticMetaObject, 1, _a);
}

// SIGNAL 2
void K3Process::receivedStdout(int _t1, int & _t2)
{
    void *_a[] = { 0, const_cast<void*>(reinterpret_cast<const void*>(&_t1)), const_cast<void*>(reinterpret_cast<const void*>(&_t2)) };
    QMetaObject::activate(this, &staticMetaObject, 2, _a);
}

// SIGNAL 3
void K3Process::receivedStderr(K3Process * _t1, char * _t2, int _t3)
{
    void *_a[] = { 0, const_cast<void*>(reinterpret_cast<const void*>(&_t1)), const_cast<void*>(reinterpret_cast<const void*>(&_t2)), const_cast<void*>(reinterpret_cast<const void*>(&_t3)) };
    QMetaObject::activate(this, &staticMetaObject, 3, _a);
}

// SIGNAL 4
void K3Process::wroteStdin(K3Process * _t1)
{
    void *_a[] = { 0, const_cast<void*>(reinterpret_cast<const void*>(&_t1)) };
    QMetaObject::activate(this, &staticMetaObject, 4, _a);
}
static const uint qt_meta_data_K3ShellProcess[] = {

 // content:
       5,       // revision
       0,       // classname
       0,    0, // classinfo
       0,    0, // methods
       0,    0, // properties
       0,    0, // enums/sets
       0,    0, // constructors
       0,       // flags
       0,       // signalCount

       0        // eod
};

static const char qt_meta_stringdata_K3ShellProcess[] = {
    "K3ShellProcess\0"
};

const QMetaObject K3ShellProcess::staticMetaObject = {
    { &K3Process::staticMetaObject, qt_meta_stringdata_K3ShellProcess,
      qt_meta_data_K3ShellProcess, 0 }
};

#ifdef Q_NO_DATA_RELOCATION
const QMetaObject &K3ShellProcess::getStaticMetaObject() { return staticMetaObject; }
#endif //Q_NO_DATA_RELOCATION

const QMetaObject *K3ShellProcess::metaObject() const
{
    return QObject::d_ptr->metaObject ? QObject::d_ptr->metaObject : &staticMetaObject;
}

void *K3ShellProcess::qt_metacast(const char *_clname)
{
    if (!_clname) return 0;
    if (!strcmp(_clname, qt_meta_stringdata_K3ShellProcess))
        return static_cast<void*>(const_cast< K3ShellProcess*>(this));
    return K3Process::qt_metacast(_clname);
}

int K3ShellProcess::qt_metacall(QMetaObject::Call _c, int _id, void **_a)
{
    _id = K3Process::qt_metacall(_c, _id, _a);
    if (_id < 0)
        return _id;
    return _id;
}
QT_END_MOC_NAMESPACE
