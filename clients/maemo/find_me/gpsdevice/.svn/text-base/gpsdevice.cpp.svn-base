#include "gpsdevice.h"

namespace GPSDevice
{
   GPSDevice::GPSDevice(QObject * parent) :
      QObject(parent),
      m_state(GPSDevice::Disconnected),
      m_satellites_used(0),
      m_satellites_in_view(0),
      m_fix(FixNo),
      m_speed(0)
   {
   }

   GPSDevice::~GPSDevice()
   {
   }

   void GPSDevice::openDevice()
   {
   }

   void GPSDevice::closeDevice()
   {
   }

   GPSDevice::State GPSDevice::state() const
   {
      return m_state;
   }

   int GPSDevice::satellitesUsed() const
   {
      return m_satellites_used;
   }

   int GPSDevice::satellitesInView() const
   {
      return m_satellites_in_view;
   }

   GPSDevice::Fix GPSDevice::fix() const
   {
      return m_fix;
   }

   int GPSDevice::speed() const
   {
      return m_speed;
   }

   QString GPSDevice::type() const
   {
      return "Fake";
   }

   void GPSDevice::setState(GPSDevice::State state)
   {
      if (m_state != state)
      {
         m_state = state;
         emit stateChanged(m_state);
      }
   }

   void GPSDevice::setSattelitesUsed(int count)
   {
      if (m_satellites_used != count)
      {
         m_satellites_used = count;
         emit satellitesChanged(m_satellites_in_view, m_satellites_used);
      }
   }

   void GPSDevice::setSattelitesInView(int count)
   {
      if (m_satellites_in_view != count)
      {
         m_satellites_in_view = count;
         emit satellitesChanged(m_satellites_in_view, m_satellites_used);
      }
   }

   void GPSDevice::setFix(Fix fix)
   {
      if (m_fix != fix)
      {
         m_fix = fix;
         emit fixChanged(m_fix);
      }
   }

   void GPSDevice::setSpeed(int speed)
   {
      if (m_speed != speed)
      {
         m_speed = speed;
         emit speedChanged(m_speed);
      }
   }
}
