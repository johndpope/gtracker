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

public class AuthMessage implements IOutMessage
{
   public AuthMessage(String did)
   {
      if (did == null || (did.length() > 0 && did.length() != 12))
      {
         throw new RuntimeException("wrong device ID");
      }

      if (did.compareTo("") == 0)
      {
         buff = new byte[2];
      } else
      {
         buff = new byte[2 + 12];
         System.arraycopy(did.getBytes(), 0, buff, 2, did.getBytes().length);
      }
      buff[0] = 0x41;
      buff[1] = 1;
   }

   public byte[] toBytes()
   {
      return buff;
   }
   byte[] buff;
}
