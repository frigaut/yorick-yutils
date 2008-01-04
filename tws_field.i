/*
 * TWS_FIELD.I Field widgets for the Tiny Widget Set. See tws.i.
 *
 * $Id: tws_field.i,v 1.1 2008-01-04 15:04:37 frigaut Exp $
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
 * $Log: tws_field.i,v $
 * Revision 1.1  2008-01-04 15:04:37  frigaut
 * - added tws*.i from thibaut
 *
 *
 */


struct TWS_Field
/* DOCUMENT TWS_Button
*/
{
  string type,uname;
  pointer root,parent; // A button has no children
  double position(4),value;
  string label,prompt;
  long label_id,frame_id,value_id,frame;
}

struct TWS_FieldEvent
{
  pointer widget;
  double mouse(11);
  long button;
  double value;
}

func tws_field(self,parent=,label=,uname=,action=,position=,mouse=,value=,frame=,prompt=)
/* DOCUMENT tws_button,label,uname=uname

   Creates  a TWS button,  labeled LABEL,  with uname  (=user name)  UNAME. The
   uname should be used by your handler. See tws_handler.
*/
{
  if (is_void(self)) action="Create";
  if (action=="Create") {
    if (is_void(position)) position=parent->position;
    if (is_void(prompt) && !is_void(label)) prompt=label;
    self=&TWS_Field(uname=uname,value=value,label=label,parent=parent,prompt=prompt);
    if (is_void(frame) || frame != 0) self->frame=1;
    self->root=parent->root;
    self->type="tws_field";
    if (self->frame) self->frame_id=tws_plid(self->root);
    self->label_id=tws_plid(self->root);
    self->value_id=tws_plid(self->root);
    self->position=tws_action(self->parent)(self->parent,action="GetPosition",position=position);
    tws_addtoparent,parent,self;
    return self;
  } else if (action=="Realize") {
    if (self->frame) plrect,self->position;
    plt,self->label,self->position(1)+(self->position(3)-self->position(1))/10.,self->position(2)+(self->position(4)-self->position(2))/2.,tosys=1,justify="LH";
    plt,swrite(self->value),self->position(3)-(self->position(3)-self->position(1))/10.,self->position(2)+(self->position(4)-self->position(2))/2.,tosys=1,justify="RH";
  } else if (action=="GetEvent") {
    Event=[];
    if (tws_isinrect(self->position,mouse)) {
      Event=TWS_FieldEvent();
      Event.widget=self;
      Event.mouse=mouse;
      Event.button=long(mouse(10));
      value=self->value;
      pledit,self->label_id,color="blue";
      pledit,self->value_id,color="red";
      redraw;
      n=read(prompt=self->prompt,format="%g",value);
      redraw;
      self->value=value;
      Event.value=value;
      tws_realize,self->root,nokill=1; // quick ugly way to update
    }
    return Event;
  } else if (action=="Activate") {
    redraw;
  } else if (action=="Deactivate") {
    pledit,self->label_id,color="black";
    pledit,self->value_id,color="black";
    redraw;
  }
}
