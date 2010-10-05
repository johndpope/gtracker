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

package com.gtracker.optional.locationservices;

import com.gtracker.IClient;
import com.gtracker.ServerStateMessage;

public abstract class OptionalLocationProvider
{
   public abstract void setListener(ILocationListener ll);
   
   public static OptionalLocationProvider getProvider() throws ClassNotFoundException
   {
      OptionalLocationProvider olp = null;
      try
      {
         // this will throw an exception if JSR-179 is missing
         Class.forName("javax.microedition.location.Location");

         // more about this class later
         Class c = Class.forName(
                 "com.gtracker.optional.locationservices.OptionalLocationProviderImpl");
         olp = (OptionalLocationProvider)(c.newInstance());
      }
      catch (Exception e)
      {
         throw new ClassNotFoundException("Internal GPS not supported.");
      }
      return olp;
   }
}