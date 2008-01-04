/*
 * TWS_Button.I Tiny Widget Set Button.
 *
 * Buttons for the Tiny Widget Set. See tws.i
 *
 * $Id: tws_radio.i,v 1.1 2008-01-04 15:04:37 frigaut Exp $
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
 * $Log: tws_radio.i,v $
 * Revision 1.1  2008-01-04 15:04:37  frigaut
 * - added tws*.i from thibaut
 *
 * 
 */


struct TWS_Radio
/* DOCUMENT TWS_Button
*/
{
  string type,uname;
  pointer root,parent; // A button has no children
  double position(4);
  string label;
  int selected;
  long label_id,selected_id;
}

struct TWS_RadioEvent
{
  pointer widget;
  double mouse(11);
  long button;
  long selected;
}

func tws_radio(self,parent=,label=,uname=,action=,position=,mouse=)
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
    self=&TWS_Radio();
    self->root=parent->root;
    self->parent=parent;
    self->type="tws_radio";
    self->label=label;
    self->uname=uname;
    self->selected=0;
    self->selected_id=tws_plid(self->root);
    self->label_id=tws_plid(self->root);
    self->position=tws_action(self->parent)(self->parent,action="GetPosition",position=position);
    tws_addtoparent,parent,self;
    return self;
  } else if (action=="Realize") {
    plt,"*",self->position(1)+(self->position(3)-self->position(1))/10.,self->position(2)+(self->position(4)-self->position(2))/2.,tosys=1,justify="CH";
    plt,self->label,self->position(1)+(self->position(3)-self->position(1))/5.,self->position(2)+(self->position(4)-self->position(2))/2.,tosys=1,justify="LH";
    if (self->selected) pledit,self->selected_id,color="red",font="helveticaB";
  } else if (action=="GetEvent") {
    Event=[];
    if (tws_isinrect(self->position,mouse)) {
      Event=TWS_RadioEvent();
      Event.widget=self;
      Event.mouse=mouse;
      Event.button=long(mouse(10));
      Event.selected=1;
      rien=tws_action(self)(self,action="Select");
      for (i=1;i<=numberof(*self->parent->children);i++) {
        if ((*self->parent->children)(i)->type=="tws_radio" && (*self->parent->children)(i)!=self) {
          rien=tws_action(self)((*self->parent->children)(i),action="Deselect");
        }
      }
    }
    return Event;
  } else if (action=="Activate") {
    pledit,self->label_id,color="blue";
    redraw;
  } else if (action=="Deactivate") {
    pledit,self->label_id,color="black";
    redraw;
  } else if (action=="Select") {
    pledit,self->selected_id,color="red",font="helveticaB";
    self->selected=1;
  } else if (action=="Deselect") {
    pledit,self->selected_id,color="black",font="helvetica";
    self->selected=0;
  }
}

func tws_radiotesthandler(event)
{
  event;
  *event.widget;
  return 0;
}

func tws_radiotest
{
  extern root,grid,choix1,choix2,choix3;
  root=tws_root();
  grid=tws_grid(parent=root,lines=3);
  choix1=tws_radio(parent=grid,uname="choix1",label="Choix 1");
  choix2=tws_radio(parent=grid,uname="choix2",label="Choix 2");
  choix3=tws_radio(parent=grid,uname="choix3",label="Choix 3");
  tws_realize,root;
  tws_action(choix3)(choix3,action="Select");
  tws_handler,root,"tws_radiotesthandler";
}
