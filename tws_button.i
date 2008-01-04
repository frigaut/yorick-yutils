/*
 * TWS_Button.I Tiny Widget Set Button.
 * Buttons for the Tiny Widget Set. See tws.i
 *
 * $Id: tws_button.i,v 1.1 2008-01-04 15:04:37 frigaut Exp $
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
 * $Log: tws_button.i,v $
 * Revision 1.1  2008-01-04 15:04:37  frigaut
 * - added tws*.i from thibaut
 *
 *
 */


struct TWS_Button
/* DOCUMENT TWS_Button
*/
{
  string type,uname;
  pointer root,parent; // A button has no children
  double position(4);
  string label;
  long label_id,frame_id;
}

struct TWS_ButtonEvent
{
  pointer widget;
  double mouse(11);
  long button;
}

func tws_button(self,parent=,label=,uname=,action=,position=,mouse=)
/* DOCUMENT tws_button,label=label,parent=parent,uname=uname

   Creates  a TWS button,  labeled LABEL,  with uname  (=user name)  UNAME. The
   uname should be used by your handler. See tws_handler.
*/
{
  if (is_void(self)) action="Create";
  if (action=="Create") {
    if (is_void(position)) position=parent->position;
    if (is_void(uname)) uname="";
    if (is_void(label)) label="";
    self=&TWS_Button();
    self->root=parent->root;
    self->parent=parent;
    self->type="tws_button";
    self->label=label;
    self->uname=uname;
    self->frame_id=tws_plid(self->root);
    self->label_id=tws_plid(self->root);
    self->position=tws_action(self->parent)(self->parent,action="GetPosition",position=position);
    tws_addtoparent,parent,self;
    return self;
  } else if (action=="Realize") {
    plrect,self->position;
    plt,self->label,self->position(1)+(self->position(3)-self->position(1))/10.,self->position(2)+(self->position(4)-self->position(2))/2.,tosys=1,justify="LH";
  } else if (action=="GetEvent") {
    Event=[];
    if (tws_isinrect(self->position,mouse)) {
      Event=TWS_ButtonEvent();
      Event.widget=self;
      Event.mouse=mouse;
      Event.button=long(mouse(10));
    }
    return Event;
  } else if (action=="Activate") {
    pledit,self->label_id,color="blue";
    redraw;
  } else if (action=="Deactivate") {
    pledit,self->label_id,color="black";
    redraw;
  }
}
