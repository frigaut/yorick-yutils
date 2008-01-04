/*
 *  TWS_ROOT.I Root widgets for the Tiny Widget Set. See tws.i.
 *
 * $Id: tws_root.i,v 1.1 2008-01-04 15:04:37 frigaut Exp $
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
 * $Log: tws_root.i,v $
 * Revision 1.1  2008-01-04 15:04:37  frigaut
 * - added tws*.i from thibaut
 *
 *
 */


struct TWS_Root
/* DOCUMENT TWS_Root
*/
{
  string type;        // Type of widget, like "Root", "Grid" or "Button"
  string uname;       // Application name of the widget
  pointer children;   // Pointer to an array of pointers to widget structs.
  pointer root;       // Pointer to root widget (self in this case)
  pointer parent;     // Pointer to parent widget (nil in this case)
  double position(4); // [x0,y0,x1,y1] defining rectangle of this widget. Always [0,0,1,1] for a Root.
  long wid;           // Only for Root widgets : Window ID.
  long cur_plid;      // Only for Root widgets : number of objects defined up to now in the window
  long dpi;           // Root widget should accept any keyword of WINDOW
  long height;
  long width;
  string style;
}

func tws_root(self,wid=,uname=,width=,height=,style=,action=,dpi=,position=,mouse=,nokill=)
/* DOCUMENT tws_root,wid,cols,lines [,uname=uname]

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
    // Defaults
    if (is_void(wid)) wid=0;
    if (is_void(dpi)) dpi=75;
    if (is_void(width)) {
      if (dpi==75) width=450; else width=600;
    }
    if (is_void(height)) {
      if (dpi==75) height=450; else height=600;
    }
    if (is_void(uname)) uname="Root";
    if (is_void(style)) style="nobox.gs";

    // Do.
    self=&TWS_Root();
    self->root=self;
    self->position=[0,0,1,1];
    self->wid=wid;
    self->dpi=dpi;
    self->type="tws_root";
    self->uname=uname;
    self->height=height;
    self->width=width;
    self->style=style;
    return self;
  } else if (action=="Realize") {
    if (!nokill) {
        winkill,self->wid;
        window,self->wid,height=self->height,width=self->width,style=self->style,dpi=self->dpi;
    } else window,self->wid;
    // Here I define  in a general way  the viewport in a full  window of given
    // width and  height.  The  three numeric parameters  xc,yc and  maybe fact
    // (=0.0013/2) can probably  be improved. Now I would just  like to now how
    // to remove the "system" line...
    require,"style.i";
    get_style, land, sys, leg, cleg;
    xc=0.397;
    yc=0.639;
    fact=0.00065;
    sys(1).viewport=[xc,xc,yc,yc]+[-self->width,self->width,-self->height,self->height]*fact;
    set_style, land, sys, leg, cleg;
    fma;
    for (i=1;i<=numberof(*self->children);i++) rien=tws_action((*self->children)(i))((*self->children)(i),action="Realize");
  } else if (action=="GetPosition") {
    return position;
  } else if (action=="GetEvent") {
    i=1;
    event=[];
    while (is_void(event) && i<=numberof(*self->children)) {
      event=tws_action((*self->children)(i))((*self->children)(i),action="GetEvent",mouse=mouse);
      i++;
    }
    return event;
  }
}
