#-------------------------------------------------
#
# Project created by QtCreator 2013-05-07T13:40:57
#
#-------------------------------------------------

TARGET = qte
TEMPLATE = lib
CONFIG += staticlib

QT += widgets uitools

include(proxy/proxy.pri)

SOURCES += qte.cpp \
    qtelink.cpp \
    qteevent.cpp \
    qteeventloadui.cpp \
    qteeventconnect.cpp \
    qteeventinvoke.cpp \
    qteabstractstate.cpp \
    qteargument.cpp \
    qteobjectproxy.cpp

HEADERS += qte.h \
    qtelink.h \
    qteevent.h \
    qteeventloadui.h \
    qteeventconnect.h \
    qteeventinvoke.h \
    qteabstractstate.h \
    qteargument.h \
    qteobjectproxy.h
unix:!symbian {
    maemo5 {
        target.path = /opt/usr/lib
    } else {
        target.path = /usr/lib
    }
    INSTALLS += target
}

OTHER_FILES += \
    wrap/wrap.pri
