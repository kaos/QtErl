#option(host_build)

TEMPLATE = app
CONFIG += console

DEFINES += QT_MOC QT_NO_CAST_FROM_ASCII QT_NO_CAST_FROM_BYTEARRAY QT_NO_COMPRESS

#INCLUDEPATH += $$QT_BUILD_TREE/src/corelib/global

include(owc.pri)
#HEADERS += qdatetime_p.h
SOURCES += main.cpp

#load(qt_tool)
