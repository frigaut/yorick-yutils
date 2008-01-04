/*
 * TWS_Grid.I Grid widgets for the Tiny Widget Set. See tws.i
 *
 * $Id: tws_grid.i,v 1.1 2008-01-04 15:04:37 frigaut Exp $
 *
 * This file is part of Yutils
 * Copyright (C) 2007  Thibaut Paumard <paumard@users.sourceforge.net>
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along
 * with this program; if not, write to the Free Software Foundation, Inc.,
 * 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
 *
 * $Log: tws_grid.i,v $
 * Revision 1.1  2008-01-04 15:04:37  frigaut
 * - added tws*.i from thibaut
 *
 *
 */


struct TWS_Grid
/* DOCUMENT TWS_Grid
*/
{
  string type,uname;
  pointer children,root,parent;
  double cur_gridx,cur_gridy,buttons_xsize, buttons_ysize;
  long cur_index;
  double position(4);
  long frame_id;    // id of frame for pledit
  GraphK frame_keywords;
}

func tws_grid(self,parent=,cols=,lines=,uname=,position=,action=,mouse=,frame_keywords=,height=)
/* DOCUMENT tws_init,wid,cols,lines [,uname=uname]

   Creates  a TWS  base widget  in  window WID.  This widget  will contain  the
   buttons in  a grid with COLS columns  and LINES lines. A  uname (=user name)
   may be given to this widget  for later reference, which is currently useless
   since the base widget doesn't return any event to your handler.

   After   having   called   TWS_INIT   once,   you  can   had   buttons   with
   TWS_BUTTON. Buttons  will be drawn  bottom to top  and left to right  in the
   grid.
*/
{
  if (is_void(self)) action="Create";
  if (action=="Create") {
    self=&TWS_Grid();
    self->root=parent->root;
    self->parent=parent;
    self->type="tws_grid";
    if (structof(frame_keywords)==GraphK) self->frame_keywords=frame_keywords;
    if (!is_void(uname)) self->uname=uname;
    if (is_void(position)) position=parent->position;
    if (is_void(lines)) lines=1.;
    if (is_void(height)) height=lines;
    self->position=tws_action(self->parent)(self->parent,action="GetPosition",position=position,height=height);
    self->root->cur_plid++;
    self->frame_id=self->root->cur_plid;
    if (is_void(cols)) cols=1.;
    self->buttons_xsize=(self->position(3)-self->position(1))/cols;
    self->buttons_ysize=(self->position(4)-self->position(2))/lines;
    self->cur_index=0;
    tws_addtoparent,parent,self;
    return self;
  } else if (action=="Realize") {
    plrect,self->position,keywords=self->frame_keywords;
    for (i=1;i<=numberof(*self->children);i++) rien=tws_action((*self->children)(i))((*self->children)(i),action="Realize");
  } else if (action=="GetPosition") {
    if (is_void(height)) height=1;
    xsize=self->position(3)-self->position(1);
    ysize=self->position(4)-self->position(2);
    X=self->position(1);
    Y=self->position(2);
    y0=self->cur_index*self->buttons_ysize;
    col=floor(y0/ysize);
    y0=y0-col*ysize;
    x0=col*self->buttons_xsize;
    x1=x0+self->buttons_xsize;
    y1=y0+height*self->buttons_ysize;
    self->cur_index=self->cur_index+height;
    return [X+x0,Y+y0,X+x1,Y+y1];
  } else if (action=="GetEvent") {
    Event=[];
    if (tws_isinrect(self->position,mouse)) {
      i=1;
      Event=[];
      while (is_void(Event) && i<=numberof(*self->children)) {
        Event=tws_action((*self->children)(i))((*self->children)(i),action="GetEvent",mouse=mouse);
        i++;
      }
    }
    return Event;
  }
}
