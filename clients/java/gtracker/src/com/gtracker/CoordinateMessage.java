/*
 * Copyright (C) 04/20/2010 Dmitry S. Melnikov (dmitryme@gmail.com)
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License
 * as published by the Free Software Foundation; either version 2
 * of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
 */

package com.gtracker;

import java.io.ByteArrayOutputStream;

public class CoordinateMessage implements IMessage, IOutMessage
{
   public CoordinateMessage(double latitude, double longitude, long timestamp)
   {
      latitude_ = latitude;
      longitude_ = longitude;
      timestamp_ = timestamp;
   }
   public void process(GtClient c) throws Exception
   {
      c.process(this);
   }
   public double getLongitude()
   {
      return longitude_;
   }
   public double getlatitude()
   {
      return latitude_;
   }
   public long getTimestamp()
   {
      return timestamp_;
   }
   public byte[] toBytes()
   {
      byte buf[] = new byte[17];
      buf[0] = 0x43;

      buf[1] = (byte)latitude_;                 // latitude
      buf[2] = (byte)((short)latitude_ >> 8);  // latitude
      int latExp = getExponent(latitude_);
      buf[3] = (byte)latExp;                    // latitude exponent
      buf[4] = (byte)(latExp >> 8);             // latitude exponent
      buf[5] = (byte)(latExp >> 16);            // latitude exponent
      buf[6] = (byte)(latExp >> 24);            // latitude exponent

      buf[7] = (byte)longitude_;                 // longitude
      buf[8] = (byte)((short)longitude_ >> 8);
      int lonExp = getExponent(longitude_);
      buf[9] = (byte)lonExp;                    // longitude exponent
      buf[10] = (byte)(lonExp >> 8);            // longitude exponent
      buf[11] = (byte)(lonExp >> 16);           // longitude exponent
      buf[12] = (byte)(lonExp >> 24);           // longitude exponent

      int tm = (int)(timestamp_ / 1000); // got a seconds
      buf[13] =  (byte)tm;                       // timestamp
      buf[14] = (byte)(tm >> 8);                 // timestamp
      buf[15] = (byte)(tm >> 16);                // timestamp
      buf[16] = (byte)(tm >> 24);                // timestamp

      return buf;
   }

   private int getExponent(double val)
   {
      long mantissa = (long)val;
      return (int)((val - mantissa) * 1000000);
   }
   private double latitude_;
   private double longitude_;
   private long   timestamp_;
}
