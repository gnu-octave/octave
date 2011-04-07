/****************************************************************************
** Meta object code from reading C++ file 'Session.h'
**
** Created: Thu Apr 7 10:08:49 2011
**      by: The Qt Meta Object Compiler version 62 (Qt 4.7.2)
**
** WARNING! All changes made in this file will be lost!
*****************************************************************************/

#include "../lib/Session.h"
#if !defined(Q_MOC_OUTPUT_REVISION)
#error "The header file 'Session.h' doesn't include <QObject>."
#elif Q_MOC_OUTPUT_REVISION != 62
#error "This file was generated using the moc from 4.7.2. It"
#error "cannot be used with the include files from this version of Qt."
#error "(The moc has changed too much.)"
#endif

QT_BEGIN_MOC_NAMESPACE
static const uint qt_meta_data_Konsole__Session[] = {

 // content:
       5,       // revision
       0,       // classname
       0,    0, // classinfo
      23,   14, // methods
       4,  129, // properties
       0,    0, // enums/sets
       0,    0, // constructors
       0,       // flags
      13,       // signalCount

 // signals: signature, parameters, type, tag, flags
      18,   17,   17,   17, 0x05,
      28,   17,   17,   17, 0x05,
      44,   39,   17,   17, 0x05,
      66,   17,   17,   17, 0x05,
      89,   81,   17,   17, 0x05,
     119,  113,   17,   17, 0x05,
     145,  137,   17,   17, 0x05,
     166,   17,   17,   17, 0x05,
     197,   17,   17,   17, 0x05,
     238,  234,   17,   17, 0x05,
     267,  262,   17,   17, 0x05,
     288,   39,   17,   17, 0x05,
     334,  326,   17,   17, 0x05,

 // slots: signature, parameters, type, tag, flags
     366,   17,   17,   17, 0x0a,
     372,   17,   17,   17, 0x0a,
     389,  380,   17,   17, 0x0a,
     415,   17,   17,   17, 0x08,
     436,  425,   17,   17, 0x08,
     468,   17,   17,   17, 0x08,
     500,  487,   17,   17, 0x08,
     540,  526,   17,   17, 0x08,
     571,   17,   17,   17, 0x08,
     598,  593,   17,   17, 0x08,

 // properties: name, type, flags
     630,  622, 0x0a095001,
     639,  635, 0x02095001,
     649,  622, 0x0a095103,
     262,  661, 0x15095103,

       0        // eod
};

static const char qt_meta_stringdata_Konsole__Session[] = {
    "Konsole::Session\0\0started()\0finished()\0"
    "text\0receivedData(QString)\0titleChanged()\0"
    "profile\0profileChanged(QString)\0state\0"
    "stateChanged(int)\0message\0"
    "bellRequest(QString)\0"
    "changeTabTextColorRequest(int)\0"
    "changeBackgroundColorRequest(QColor)\0"
    "url\0openUrlRequest(QString)\0size\0"
    "resizeRequest(QSize)\0"
    "profileChangeCommandReceived(QString)\0"
    "enabled\0flowControlEnabledChanged(bool)\0"
    "run()\0close()\0,caption\0setUserTitle(int,QString)\0"
    "done(int)\0buffer,len\0"
    "onReceiveBlock(const char*,int)\0"
    "monitorTimerDone()\0height,width\0"
    "onViewSizeChange(int,int)\0lines,columns\0"
    "onEmulationSizeChange(int,int)\0"
    "activityStateSet(int)\0view\0"
    "viewDestroyed(QObject*)\0QString\0name\0"
    "int\0processId\0keyBindings\0QSize\0"
};

const QMetaObject Konsole::Session::staticMetaObject = {
    { &QObject::staticMetaObject, qt_meta_stringdata_Konsole__Session,
      qt_meta_data_Konsole__Session, 0 }
};

#ifdef Q_NO_DATA_RELOCATION
const QMetaObject &Konsole::Session::getStaticMetaObject() { return staticMetaObject; }
#endif //Q_NO_DATA_RELOCATION

const QMetaObject *Konsole::Session::metaObject() const
{
    return QObject::d_ptr->metaObject ? QObject::d_ptr->metaObject : &staticMetaObject;
}

void *Konsole::Session::qt_metacast(const char *_clname)
{
    if (!_clname) return 0;
    if (!strcmp(_clname, qt_meta_stringdata_Konsole__Session))
        return static_cast<void*>(const_cast< Session*>(this));
    return QObject::qt_metacast(_clname);
}

int Konsole::Session::qt_metacall(QMetaObject::Call _c, int _id, void **_a)
{
    _id = QObject::qt_metacall(_c, _id, _a);
    if (_id < 0)
        return _id;
    if (_c == QMetaObject::InvokeMetaMethod) {
        switch (_id) {
        case 0: started(); break;
        case 1: finished(); break;
        case 2: receivedData((*reinterpret_cast< const QString(*)>(_a[1]))); break;
        case 3: titleChanged(); break;
        case 4: profileChanged((*reinterpret_cast< const QString(*)>(_a[1]))); break;
        case 5: stateChanged((*reinterpret_cast< int(*)>(_a[1]))); break;
        case 6: bellRequest((*reinterpret_cast< const QString(*)>(_a[1]))); break;
        case 7: changeTabTextColorRequest((*reinterpret_cast< int(*)>(_a[1]))); break;
        case 8: changeBackgroundColorRequest((*reinterpret_cast< const QColor(*)>(_a[1]))); break;
        case 9: openUrlRequest((*reinterpret_cast< const QString(*)>(_a[1]))); break;
        case 10: resizeRequest((*reinterpret_cast< const QSize(*)>(_a[1]))); break;
        case 11: profileChangeCommandReceived((*reinterpret_cast< const QString(*)>(_a[1]))); break;
        case 12: flowControlEnabledChanged((*reinterpret_cast< bool(*)>(_a[1]))); break;
        case 13: run(); break;
        case 14: close(); break;
        case 15: setUserTitle((*reinterpret_cast< int(*)>(_a[1])),(*reinterpret_cast< const QString(*)>(_a[2]))); break;
        case 16: done((*reinterpret_cast< int(*)>(_a[1]))); break;
        case 17: onReceiveBlock((*reinterpret_cast< const char*(*)>(_a[1])),(*reinterpret_cast< int(*)>(_a[2]))); break;
        case 18: monitorTimerDone(); break;
        case 19: onViewSizeChange((*reinterpret_cast< int(*)>(_a[1])),(*reinterpret_cast< int(*)>(_a[2]))); break;
        case 20: onEmulationSizeChange((*reinterpret_cast< int(*)>(_a[1])),(*reinterpret_cast< int(*)>(_a[2]))); break;
        case 21: activityStateSet((*reinterpret_cast< int(*)>(_a[1]))); break;
        case 22: viewDestroyed((*reinterpret_cast< QObject*(*)>(_a[1]))); break;
        default: ;
        }
        _id -= 23;
    }
#ifndef QT_NO_PROPERTIES
      else if (_c == QMetaObject::ReadProperty) {
        void *_v = _a[0];
        switch (_id) {
        case 0: *reinterpret_cast< QString*>(_v) = nameTitle(); break;
        case 1: *reinterpret_cast< int*>(_v) = processId(); break;
        case 2: *reinterpret_cast< QString*>(_v) = keyBindings(); break;
        case 3: *reinterpret_cast< QSize*>(_v) = size(); break;
        }
        _id -= 4;
    } else if (_c == QMetaObject::WriteProperty) {
        void *_v = _a[0];
        switch (_id) {
        case 2: setKeyBindings(*reinterpret_cast< QString*>(_v)); break;
        case 3: setSize(*reinterpret_cast< QSize*>(_v)); break;
        }
        _id -= 4;
    } else if (_c == QMetaObject::ResetProperty) {
        _id -= 4;
    } else if (_c == QMetaObject::QueryPropertyDesignable) {
        _id -= 4;
    } else if (_c == QMetaObject::QueryPropertyScriptable) {
        _id -= 4;
    } else if (_c == QMetaObject::QueryPropertyStored) {
        _id -= 4;
    } else if (_c == QMetaObject::QueryPropertyEditable) {
        _id -= 4;
    } else if (_c == QMetaObject::QueryPropertyUser) {
        _id -= 4;
    }
#endif // QT_NO_PROPERTIES
    return _id;
}

// SIGNAL 0
void Konsole::Session::started()
{
    QMetaObject::activate(this, &staticMetaObject, 0, 0);
}

// SIGNAL 1
void Konsole::Session::finished()
{
    QMetaObject::activate(this, &staticMetaObject, 1, 0);
}

// SIGNAL 2
void Konsole::Session::receivedData(const QString & _t1)
{
    void *_a[] = { 0, const_cast<void*>(reinterpret_cast<const void*>(&_t1)) };
    QMetaObject::activate(this, &staticMetaObject, 2, _a);
}

// SIGNAL 3
void Konsole::Session::titleChanged()
{
    QMetaObject::activate(this, &staticMetaObject, 3, 0);
}

// SIGNAL 4
void Konsole::Session::profileChanged(const QString & _t1)
{
    void *_a[] = { 0, const_cast<void*>(reinterpret_cast<const void*>(&_t1)) };
    QMetaObject::activate(this, &staticMetaObject, 4, _a);
}

// SIGNAL 5
void Konsole::Session::stateChanged(int _t1)
{
    void *_a[] = { 0, const_cast<void*>(reinterpret_cast<const void*>(&_t1)) };
    QMetaObject::activate(this, &staticMetaObject, 5, _a);
}

// SIGNAL 6
void Konsole::Session::bellRequest(const QString & _t1)
{
    void *_a[] = { 0, const_cast<void*>(reinterpret_cast<const void*>(&_t1)) };
    QMetaObject::activate(this, &staticMetaObject, 6, _a);
}

// SIGNAL 7
void Konsole::Session::changeTabTextColorRequest(int _t1)
{
    void *_a[] = { 0, const_cast<void*>(reinterpret_cast<const void*>(&_t1)) };
    QMetaObject::activate(this, &staticMetaObject, 7, _a);
}

// SIGNAL 8
void Konsole::Session::changeBackgroundColorRequest(const QColor & _t1)
{
    void *_a[] = { 0, const_cast<void*>(reinterpret_cast<const void*>(&_t1)) };
    QMetaObject::activate(this, &staticMetaObject, 8, _a);
}

// SIGNAL 9
void Konsole::Session::openUrlRequest(const QString & _t1)
{
    void *_a[] = { 0, const_cast<void*>(reinterpret_cast<const void*>(&_t1)) };
    QMetaObject::activate(this, &staticMetaObject, 9, _a);
}

// SIGNAL 10
void Konsole::Session::resizeRequest(const QSize & _t1)
{
    void *_a[] = { 0, const_cast<void*>(reinterpret_cast<const void*>(&_t1)) };
    QMetaObject::activate(this, &staticMetaObject, 10, _a);
}

// SIGNAL 11
void Konsole::Session::profileChangeCommandReceived(const QString & _t1)
{
    void *_a[] = { 0, const_cast<void*>(reinterpret_cast<const void*>(&_t1)) };
    QMetaObject::activate(this, &staticMetaObject, 11, _a);
}

// SIGNAL 12
void Konsole::Session::flowControlEnabledChanged(bool _t1)
{
    void *_a[] = { 0, const_cast<void*>(reinterpret_cast<const void*>(&_t1)) };
    QMetaObject::activate(this, &staticMetaObject, 12, _a);
}
static const uint qt_meta_data_Konsole__SessionGroup[] = {

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

static const char qt_meta_stringdata_Konsole__SessionGroup[] = {
    "Konsole::SessionGroup\0"
};

const QMetaObject Konsole::SessionGroup::staticMetaObject = {
    { &QObject::staticMetaObject, qt_meta_stringdata_Konsole__SessionGroup,
      qt_meta_data_Konsole__SessionGroup, 0 }
};

#ifdef Q_NO_DATA_RELOCATION
const QMetaObject &Konsole::SessionGroup::getStaticMetaObject() { return staticMetaObject; }
#endif //Q_NO_DATA_RELOCATION

const QMetaObject *Konsole::SessionGroup::metaObject() const
{
    return QObject::d_ptr->metaObject ? QObject::d_ptr->metaObject : &staticMetaObject;
}

void *Konsole::SessionGroup::qt_metacast(const char *_clname)
{
    if (!_clname) return 0;
    if (!strcmp(_clname, qt_meta_stringdata_Konsole__SessionGroup))
        return static_cast<void*>(const_cast< SessionGroup*>(this));
    return QObject::qt_metacast(_clname);
}

int Konsole::SessionGroup::qt_metacall(QMetaObject::Call _c, int _id, void **_a)
{
    _id = QObject::qt_metacall(_c, _id, _a);
    if (_id < 0)
        return _id;
    return _id;
}
QT_END_MOC_NAMESPACE
