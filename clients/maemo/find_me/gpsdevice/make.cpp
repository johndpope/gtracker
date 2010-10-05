#include "make.h"
#include "gpsd.h"
#include "maemo_internal.h"

namespace GPSDevice
{
   GPSDevice * make(QSettings & cfg)
   {
      QString type = cfg.value("GPS/type", "Internal").toString();
      GPSDevice * ptr = 0;
      if (type == "GPSD")
      {
         ptr = new GPSD(cfg.value("GPSD/hostname", GPSD::DefaultHost).toString(),
               cfg.value("GPSD/port", GPSD::DefaultPort).toUInt());
         dynamic_cast<NMEADevice *>(ptr)->setSkipDataWithZeroSpd(cfg.value("GPSD/skip_zero", "false").toString() == "true");
      }
      else if (type == "Internal")
      {
         ptr = new MaemoInternal(cfg.value("Internal/device", MaemoInternal::DefaultDevice).toString());
         dynamic_cast<NMEADevice *>(ptr)->setSkipDataWithZeroSpd(cfg.value("Internal/skip_zero", "false").toString() == "true");
      }
      else
      {
         ptr = new GPSDevice();
      }

      return ptr;
   }
}
