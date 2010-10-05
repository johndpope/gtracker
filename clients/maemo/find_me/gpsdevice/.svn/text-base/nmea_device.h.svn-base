#ifndef __GPSDEVICE_NMEA_DEVICE_H
#define __GPSDEVICE_NMEA_DEVICE_H

#include "gpsdevice.h"
#include <QStringList>

namespace GPSDevice
{
   class NMEADevice : public GPSDevice
   {
      public:
         NMEADevice(QObject * parent = 0);
         virtual ~NMEADevice();

         void setSkipDataWithZeroSpd(bool value);

      protected:
         void parseLine(QString line);

      private:
         void parseGPRMC(QStringList fields);
         void parseGPGSA(QStringList fields);
         void parseGPGSV(QStringList fields);

      private:
         bool m_check_speed;
   };
}

#endif // __GPSDEVICE_NMEA_DEVICE_H
