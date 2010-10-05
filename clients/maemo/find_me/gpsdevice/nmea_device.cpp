#include "nmea_device.h"
#include <QDebug>

namespace GPSDevice
{
   namespace
   {
      double ndegreeToDegree(double value)
      {
         double degree = static_cast<int>(value / 100);
         return degree + (value - degree * 100) / 60;
      }

      GPSDevice::Fix convertToFix(char fix)
      {
         switch (fix)
         {
            case '2':
               return GPSDevice::Fix2D;
            case '3':
               return GPSDevice::Fix3D;
         }

         return GPSDevice::FixNo;
      }
   }

   NMEADevice::NMEADevice(QObject * parent) :
      GPSDevice(parent),
      m_check_speed(false)
   {
   }

   NMEADevice::~NMEADevice()
   {
   }

   void NMEADevice::setSkipDataWithZeroSpd(bool value)
   {
      m_check_speed = value;
   }

   void NMEADevice::parseLine(QString line)
   {
      //qDebug() << "NMEA: " << line;
      QStringList fields = line.split(',');
      if (fields.at(0) == "$GPRMC")
         parseGPRMC(fields);
      else if (fields.at(0) == "$GPGSA")
         parseGPGSA(fields);
      else if (fields.at(0) == "$GPGSV")
         parseGPGSV(fields);
   }

   void NMEADevice::parseGPRMC(QStringList fields)
   {
      if (fields.at(2) == "A")
      {
         setSpeed(int(fields.at(7).toDouble() * 1.852)); // 1 Knots == 1.85200 km/h
         setState(GPSDevice::LinkReady);

         if (!m_check_speed || speed() > 0)
         {
            double lat = ndegreeToDegree(fields.at(3).toDouble()) * (fields.at(4) == "S" ? -1 : 1);
            double lon = ndegreeToDegree(fields.at(5).toDouble()) * (fields.at(6) == "W" ? -1 : 1);

            QDateTime timestamp(QDate::fromString(fields.at(9), "ddMMyy").addYears(100),
                  QTime::fromString(fields.at(1), "hhmmss.z"), Qt::UTC);

            emit positionChanged(lat, lon, timestamp);
         }
      }
      else
      {
         setState(GPSDevice::NoLink);
         setSpeed(0);
      }
   }

   void NMEADevice::parseGPGSA(QStringList fields)
   {
      const QString & fix = fields.at(2);
      if (!fix.isEmpty())
      {
         setFix(convertToFix(fix[0].toAscii()));
      }

      int count = 0;
      for (int i = 3; i < 3+12; ++i)
      {
         if (!fields.at(i).isEmpty())
         {
            count++;
         }
      }

      setSattelitesUsed(count);
   }

   void NMEADevice::parseGPGSV(QStringList fields)
   {
      setSattelitesInView(fields.at(3).toInt());
   }
}
