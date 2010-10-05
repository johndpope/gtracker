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

public class State
{
   public State(IClient client, int initialState)
   {
      client_ = client;
      setState(initialState);

   }
   public int getState()
   {
      return state_;
   }
   public void setText(String newTxt)
   {
      client_.onMessage(new ServerStateMessage(state_, newTxt));
   }
   public boolean setState(int newState)
   {
      if(state_ != newState)
      {
         state_ = newState;
         client_.onMessage(new ServerStateMessage(state_));
         return true;
      }
      return false;
   }
   public boolean setState(int newState, String txt)
   {
      if(state_ != newState)
      {
         state_ = newState;
         client_.onMessage(new ServerStateMessage(state_, txt));
         return true;
      }
      return false;
   }

   private int state_;
   private IClient client_ = null;
}