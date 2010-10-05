#ifndef __GTRACKER_MESSAGES_H
#define __GTRACKER_MESSAGES_H

#include <stdint.h>
#include <string.h>

namespace GTracker
{
   const size_t   IDSize            = 12;
   const uint8_t  ProtocolVersion   = 0x01;

   enum MessageType
   {
      Authenticate                  = 0x41,
      Coordinate                    = 0x43,
      AuthenticateResponse          = 0x42,
      Error                         = 0x44,
      Heartbeat                     = 0x45
   };

   enum ErrorType
   {
      ERROR_NOT_AUTH                = 0x01,
      ERROR_WRONG_MSG               = 0x02,
      ERROR_FUCK_OFF                = 0x03,
      ERROR_SERVER_UNAVAILABLE      = 0x04,
      ERROR_WRONG_CID               = 0x05,
      ERROR_ALREADY_AUTH            = 0x06
   };

   typedef uint8_t Length;
   typedef uint8_t Type;

#pragma pack(push, 1)

   struct AuthenticateMessage
   {
      uint8_t  type;
      uint8_t  version;
      uint8_t  id[IDSize];
   };

   struct CoordinateMessage
   {
      uint8_t  type;
      int16_t  lat;
      uint32_t lat_e;
      int16_t  lon;
      uint32_t lon_e;
      uint32_t timestamp;
   };

   struct AuthenticateResponseMessage
   {
      uint8_t  type;
      uint8_t  id[IDSize];
   };

   struct ErrorMessage
   {
      uint8_t  type;
      uint8_t  code;
   };

   struct HeartbeatMessage
   {
      uint8_t  type;
   };

#pragma pack(pop)
}

#endif // __GTRACKER_MESSAGES_H
