/****************************************************************************
** Meta object code from reading C++ file 'Emulation.h'
**
** Created: Thu Apr 7 10:08:43 2011
**      by: The Qt Meta Object Compiler version 62 (Qt 4.7.2)
**
** WARNING! All changes made in this file will be lost!
*****************************************************************************/

#include "../lib/Emulation.h"
#if !defined(Q_MOC_OUTPUT_REVISION)
#error "The header file 'Emulation.h' doesn't include <QObject>."
#elif Q_MOC_OUTPUT_REVISION != 62
#error "This file was generated using the moc from 4.7.2. It"
#error "cannot be used with the include files from this version of Qt."
#error "(The moc has changed too much.)"
#endif

QT_BEGIN_MOC_NAMESPACE
static const uint qt_meta_data_Konsole__Emulation[] = {

 // content:
       5,       // revision
       0,       // classname
       0,    0, // classinfo
      21,   14, // methods
       0,    0, // properties
       0,    0, // enums/sets
       0,    0, // constructors
       0,       // flags
      11,       // signalCount

 // signals: signature, parameters, type, tag, flags
      29,   20,   19,   19, 0x05,
      63,   55,   19,   19, 0x05,
      84,   19,   19,   19, 0x05,
     111,  105,   19,   19, 0x05,
     125,   19,   19,   19, 0x05,
     148,  142,   19,   19, 0x05,
     189,  179,   19,   19, 0x05,
     219,   19,   19,   19, 0x05,
     250,  235,   19,   19, 0x05,
     298,  276,   19,   19, 0x05,
     329,  324,   19,   19, 0x05,

 // slots: signature, parameters, type, tag, flags
     381,  367,   19,   19, 0x0a,
     403,  324,   19,   19, 0x0a,
     421,   19,   19,   19, 0x0a,
     476,  446,   19,   19, 0x0a,
     522,  508,   19,   19, 0x0a,
     557,  550,   19,   19, 0x2a,
     592,  581,   19,   19, 0x0a,
     621,   19,   19,   19, 0x09,
     638,   19,   19,   19, 0x08,
     649,  179,   19,   19, 0x08,

       0        // eod
};

static const char qt_meta_stringdata_Konsole__Emulation[] = {
    "Konsole::Emulation\0\0data,len\0"
    "sendData(const char*,int)\0suspend\0"
    "lockPtyRequest(bool)\0useUtf8Request(bool)\0"
    "state\0stateSet(int)\0zmodemDetected()\0"
    "color\0changeTabTextColorRequest(int)\0"
    "usesMouse\0programUsesMouseChanged(bool)\0"
    "outputChanged()\0title,newTitle\0"
    "titleChanged(int,QString)\0"
    "lineCount,columnCount\0imageSizeChanged(int,int)\0"
    "text\0profileChangeCommandReceived(QString)\0"
    "lines,columns\0setImageSize(int,int)\0"
    "sendText(QString)\0sendKeyEvent(QKeyEvent*)\0"
    "buttons,column,line,eventType\0"
    "sendMouseEvent(int,int,int,int)\0"
    "string,length\0sendString(const char*,int)\0"
    "string\0sendString(const char*)\0"
    "buffer,len\0receiveData(const char*,int)\0"
    "bufferedUpdate()\0showBulk()\0"
    "usesMouseChanged(bool)\0"
};

const QMetaObject Konsole::Emulation::staticMetaObject = {
    { &QObject::staticMetaObject, qt_meta_stringdata_Konsole__Emulation,
      qt_meta_data_Konsole__Emulation, 0 }
};

#ifdef Q_NO_DATA_RELOCATION
const QMetaObject &Konsole::Emulation::getStaticMetaObject() { return staticMetaObject; }
#endif //Q_NO_DATA_RELOCATION

const QMetaObject *Konsole::Emulation::metaObject() const
{
    return QObject::d_ptr->metaObject ? QObject::d_ptr->metaObject : &staticMetaObject;
}

void *Konsole::Emulation::qt_metacast(const char *_clname)
{
    if (!_clname) return 0;
    if (!strcmp(_clname, qt_meta_stringdata_Konsole__Emulation))
        return static_cast<void*>(const_cast< Emulation*>(this));
    return QObject::qt_metacast(_clname);
}

int Konsole::Emulation::qt_metacall(QMetaObject::Call _c, int _id, void **_a)
{
    _id = QObject::qt_metacall(_c, _id, _a);
    if (_id < 0)
        return _id;
    if (_c == QMetaObject::InvokeMetaMethod) {
        switch (_id) {
        case 0: sendData((*reinterpret_cast< const char*(*)>(_a[1])),(*reinterpret_cast< int(*)>(_a[2]))); break;
        case 1: lockPtyRequest((*reinterpret_cast< bool(*)>(_a[1]))); break;
        case 2: useUtf8Request((*reinterpret_cast< bool(*)>(_a[1]))); break;
        case 3: stateSet((*reinterpret_cast< int(*)>(_a[1]))); break;
        case 4: zmodemDetected(); break;
        case 5: changeTabTextColorRequest((*reinterpret_cast< int(*)>(_a[1]))); break;
        case 6: programUsesMouseChanged((*reinterpret_cast< bool(*)>(_a[1]))); break;
        case 7: outputChanged(); break;
        case 8: titleChanged((*reinterpret_cast< int(*)>(_a[1])),(*reinterpret_cast< const QString(*)>(_a[2]))); break;
        case 9: imageSizeChanged((*reinterpret_cast< int(*)>(_a[1])),(*reinterpret_cast< int(*)>(_a[2]))); break;
        case 10: profileChangeCommandReceived((*reinterpret_cast< const QString(*)>(_a[1]))); break;
        case 11: setImageSize((*reinterpret_cast< int(*)>(_a[1])),(*reinterpret_cast< int(*)>(_a[2]))); break;
        case 12: sendText((*reinterpret_cast< const QString(*)>(_a[1]))); break;
        case 13: sendKeyEvent((*reinterpret_cast< QKeyEvent*(*)>(_a[1]))); break;
        case 14: sendMouseEvent((*reinterpret_cast< int(*)>(_a[1])),(*reinterpret_cast< int(*)>(_a[2])),(*reinterpret_cast< int(*)>(_a[3])),(*reinterpret_cast< int(*)>(_a[4]))); break;
        case 15: sendString((*reinterpret_cast< const char*(*)>(_a[1])),(*reinterpret_cast< int(*)>(_a[2]))); break;
        case 16: sendString((*reinterpret_cast< const char*(*)>(_a[1]))); break;
        case 17: receiveData((*reinterpret_cast< const char*(*)>(_a[1])),(*reinterpret_cast< int(*)>(_a[2]))); break;
        case 18: bufferedUpdate(); break;
        case 19: showBulk(); break;
        case 20: usesMouseChanged((*reinterpret_cast< bool(*)>(_a[1]))); break;
        default: ;
        }
        _id -= 21;
    }
    return _id;
}

// SIGNAL 0
void Konsole::Emulation::sendData(const char * _t1, int _t2)
{
    void *_a[] = { 0, const_cast<void*>(reinterpret_cast<const void*>(&_t1)), const_cast<void*>(reinterpret_cast<const void*>(&_t2)) };
    QMetaObject::activate(this, &staticMetaObject, 0, _a);
}

// SIGNAL 1
void Konsole::Emulation::lockPtyRequest(bool _t1)
{
    void *_a[] = { 0, const_cast<void*>(reinterpret_cast<const void*>(&_t1)) };
    QMetaObject::activate(this, &staticMetaObject, 1, _a);
}

// SIGNAL 2
void Konsole::Emulation::useUtf8Request(bool _t1)
{
    void *_a[] = { 0, const_cast<void*>(reinterpret_cast<const void*>(&_t1)) };
    QMetaObject::activate(this, &staticMetaObject, 2, _a);
}

// SIGNAL 3
void Konsole::Emulation::stateSet(int _t1)
{
    void *_a[] = { 0, const_cast<void*>(reinterpret_cast<const void*>(&_t1)) };
    QMetaObject::activate(this, &staticMetaObject, 3, _a);
}

// SIGNAL 4
void Konsole::Emulation::zmodemDetected()
{
    QMetaObject::activate(this, &staticMetaObject, 4, 0);
}

// SIGNAL 5
void Konsole::Emulation::changeTabTextColorRequest(int _t1)
{
    void *_a[] = { 0, const_cast<void*>(reinterpret_cast<const void*>(&_t1)) };
    QMetaObject::activate(this, &staticMetaObject, 5, _a);
}

// SIGNAL 6
void Konsole::Emulation::programUsesMouseChanged(bool _t1)
{
    void *_a[] = { 0, const_cast<void*>(reinterpret_cast<const void*>(&_t1)) };
    QMetaObject::activate(this, &staticMetaObject, 6, _a);
}

// SIGNAL 7
void Konsole::Emulation::outputChanged()
{
    QMetaObject::activate(this, &staticMetaObject, 7, 0);
}

// SIGNAL 8
void Konsole::Emulation::titleChanged(int _t1, const QString & _t2)
{
    void *_a[] = { 0, const_cast<void*>(reinterpret_cast<const void*>(&_t1)), const_cast<void*>(reinterpret_cast<const void*>(&_t2)) };
    QMetaObject::activate(this, &staticMetaObject, 8, _a);
}

// SIGNAL 9
void Konsole::Emulation::imageSizeChanged(int _t1, int _t2)
{
    void *_a[] = { 0, const_cast<void*>(reinterpret_cast<const void*>(&_t1)), const_cast<void*>(reinterpret_cast<const void*>(&_t2)) };
    QMetaObject::activate(this, &staticMetaObject, 9, _a);
}

// SIGNAL 10
void Konsole::Emulation::profileChangeCommandReceived(const QString & _t1)
{
    void *_a[] = { 0, const_cast<void*>(reinterpret_cast<const void*>(&_t1)) };
    QMetaObject::activate(this, &staticMetaObject, 10, _a);
}
QT_END_MOC_NAMESPACE
