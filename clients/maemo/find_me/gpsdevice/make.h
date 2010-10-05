#ifndef __GPSDEVICE_MAKE_H
#define __GPSDEVICE_MAKE_H

#include "gpsdevice.h"
#include <QSettings>

namespace GPSDevice
{
   GPSDevice * make(QSettings & cfg);
}

#endif // __GPSDEVICE_MAKE_H
