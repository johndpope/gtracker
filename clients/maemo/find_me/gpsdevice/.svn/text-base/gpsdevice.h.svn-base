#ifndef __GPSDEVICE_GPSDEVICE_H
#define __GPSDEVICE_GPSDEVICE_H

#include <QObject>
#include <QDateTime>

namespace GPSDevice
{
   class GPSDevice : public QObject
   {
      Q_OBJECT
      Q_ENUMS(State)
      Q_ENUMS(Fix)

      public:
         enum State { Disconnected, Connecting, Connected, NoLink, LinkReady };
         enum Fix { FixNo = 1, Fix2D, Fix3D };

      public:
         GPSDevice(QObject * parent = 0);
         virtual ~GPSDevice();

         virtual void openDevice();
         virtual void closeDevice();

         State state() const;
         int satellitesUsed() const;
         int satellitesInView() const;
         Fix fix() const;
         int speed() const;

         virtual QString type() const;

      protected:
         void setState(State state);
         void setSattelitesUsed(int count);
         void setSattelitesInView(int count);
         void setFix(Fix fix);
         void setSpeed(int speed);

      signals:
         void positionChanged(double lat, double lon, QDateTime stamp);
         void stateChanged(GPSDevice::GPSDevice::State state);
         void satellitesChanged(int inView, int used);
         void fixChanged(GPSDevice::GPSDevice::Fix fix);
         void speedChanged(int speed);

      private:
         State m_state;
         int m_satellites_used;
         int m_satellites_in_view;
         Fix m_fix;
         int m_speed;
   };
}

#endif // __GPSDEVICE_GPSDEVICE_H
