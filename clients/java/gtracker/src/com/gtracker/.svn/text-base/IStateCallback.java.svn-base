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

public interface IStateCallback {

public class StateEnum
{
    public static final int ServerConnecting    = 1;
    public static final int ServerDisconnected  = 2;
    public static final int ServerConnected     = 3;
    public static final int ServerAuthCheck     = 4;
    public static final int ServerAuthOk        = 5;
    public static final int DeviceWrongDID      = 6;
    public static final int DeviceAlreadyInUse  = 7;
    public static final int GpsConnecting       = 9;
    public static final int GpsConnected        = 10;
    public static final int GpsNotFound         = 11;
    public static final int GpsDisconnected     = 12;
}
    void onState(int state, String txt);
    void onBytesInOut(int in, int out);
}
