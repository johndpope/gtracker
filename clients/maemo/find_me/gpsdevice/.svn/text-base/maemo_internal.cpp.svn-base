#include <QLocalSocket>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/ioctl.h>
#include <fcntl.h>
#include <errno.h>
#include "maemo_internal.h"
#include "../log.h"

namespace GPSDevice
{
   namespace
   {
      const char * GPS_on = "P 3\n";
      const char * GPS_off = "P 0\n";
   }

   const QString MaemoInternal::DefaultDevice = "/dev/pgps";

   MaemoInternal::MaemoInternal(QString device, bool autoEnable, QObject * parent) :
      NMEADevice(parent),
      m_device(device),
      m_auto_enable(autoEnable),
      m_fd(-1),
      m_notifier(0)

   {
      if (m_auto_enable)
      {
         DEBUG("Internal| enabling GPS on maemo");
         enableGPS(true);
      }
   }

   MaemoInternal::~MaemoInternal()
   {
      close();
      if (m_auto_enable)
      {
         DEBUG("Internal| disabling GPS on maemo");
         enableGPS(false);
      }
   }

   void MaemoInternal::openDevice()
   {
      DEBUG("Internal| openDevice = { device %s }", m_device.toAscii().data());
      close();

      setState(GPSDevice::Connecting);
      m_fd = ::open(m_device.toAscii().data(), O_RDWR);
      if (m_fd == -1)
      {
         setState(GPSDevice::Disconnected);
      }
      else
      {
         ::tcgetattr(m_fd, &m_prev);
         termios new_opt;
         ::memset(&new_opt, 0, sizeof(new_opt));
         new_opt.c_cflag |= (CLOCAL | CREAD);
         ::cfsetspeed(&new_opt, B9600);
         new_opt.c_cflag &= ~PARENB;
         new_opt.c_cflag &= ~CSIZE;
         new_opt.c_cflag |= CS8;
         new_opt.c_cflag &= ~CSTOPB;
         ::tcsetattr(m_fd, TCSANOW, &new_opt);
         ::tcflush(m_fd, TCIOFLUSH);

         if (::write(m_fd, "R=1\n", 4) == -1)
         {
            DEBUG("Internal|    write error");
            closeDevice();
            return ;
         }

         m_notifier = new QSocketNotifier(m_fd, QSocketNotifier::Read);
         connect(m_notifier, SIGNAL(activated(int)), this, SLOT(activated(int)));
         m_notifier->setEnabled(true);
         setState(GPSDevice::Connected);
      }
   }

   void MaemoInternal::closeDevice()
   {
      DEBUG("Internal| closeDevice");
      close();
      setState(GPSDevice::Disconnected);
   }

   QString MaemoInternal::type() const
   {
      return "Internal";
   }

   void MaemoInternal::enableGPS(bool val)
   {
      DEBUG("Internal| enableGPS = { %s }", val ? "True" : "False");

      QLocalSocket socket;
      socket.connectToServer("/var/lib/gps/gps_driver_ctrl");
      if (socket.waitForConnected(10000))
      {
         const char * ptr = val ? GPS_on : GPS_off;
         socket.write(ptr);
         socket.flush();
         socket.disconnectFromServer();
      }
      else
      {
         DEBUG("Internal|   waitForConnected() failed");
      }
   }

   void MaemoInternal::close()
   {
      if (m_notifier != 0)
      {
         delete m_notifier;
         m_notifier = 0;
      }

      if (m_fd != -1)
      {
         ::tcsetattr(m_fd, TCSANOW, &m_prev);
         ::close(m_fd);
         m_fd = -1;
      }
   }

   void MaemoInternal::parseBuffer()
   {
      int index;
      while ((index = m_buffer.indexOf('\n')) != -1)
      {
         parseLine(m_buffer.left(index + 1));
         m_buffer.remove(0, index + 1); //all with \n
      }
   }

   ssize_t MaemoInternal::bytesAvailable()
   {
      int bytes = 0;
      if (::ioctl(m_fd, FIONREAD, (char *) &bytes) == -1)
      {
         return -1;
      }

      return bytes;
   }

   void MaemoInternal::activated(int fd)
   {
      Q_UNUSED(fd);
      QByteArray buffer;
      ssize_t count = bytesAvailable();
      if (count == -1)
      {
         closeDevice();
         return ;
      }

      buffer.resize(count);
      ssize_t result = ::read(m_fd, buffer.data(), count);
      if (result == -1 && errno != EAGAIN)
      {
         closeDevice();
      }
      else
      {
         m_buffer.append(buffer);
         parseBuffer();
      }
   }
}
