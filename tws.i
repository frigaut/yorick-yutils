/*
 * TWS.I Tiny Widget Set.
 *
 * $Id: tws.i,v 1.1 2008-01-04 15:04:37 frigaut Exp $
 *
 * Provides  simplistic routines  to  build simplistic,  almost graphical  user
 * interfaces. Currently supports simplisitic "buttons".
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
 * $Log: tws.i,v $
 * Revision 1.1  2008-01-04 15:04:37  frigaut
 * - added tws*.i from thibaut
 *
 *
 */

require,"graphk.i";
require,"tws_root.i";
require,"tws_grid.i";
require,"tws_button.i";
require,"tws_field.i";
require,"tws_radio.i";
require,"tws_label.i";

extern tws;
/* DOCUMENT tws_init, tws_button, tws_handler are the useful routines.

   Tiny Widget Set.

   Provides  simplistic routines  to  build simplistic,  almost graphical  user
   interfaces. Currently supports simplisitic "buttons".

   See  Cubeview  for sample  usage.  You  normally  call tws_init  once,  then
   tws_button for each button, then tws_handler.

   Basic sample usage:

   // Define your handler:
   func my_handler(uname,button){
     if (button==1) {
       // mouse button 1 performs action
       if (uname=="hello") write,"Hello World!";
       if (uname=="quit") return 2;
       return 0;
     } else {
       // Any other button performs contextual help
       if (uname=="hello") write,"Click on this button to write \"Hello World!\"";
       if (uname=="quit") write,"Click on this button to quit";
       return 0;
     }
   }
   
   // Create base widget:
   tws_init,0,1,2;
   
   // Create two buttons:
   tws_button,"Hello World!",uname="hello";
   tws_button,"Quit",uname="quit";
   
   // Call the handler:
   tws_handler,"my_handler";

   // Enjoy.
*/

func tws_addtoparent(parent,self)
{
  a=*(parent->children);
  if (is_void(a)) a=[self]; else grow,a,[self];
  parent->children=&a;  
}

func tws_isinrect(position,point)
{
  llx=min(position(1),position(3));
  urx=max(position(1),position(3));
  lly=min(position(2),position(4));
  ury=max(position(2),position(4));
  return ((point(1) > llx) && (point(1) < urx) && (point(2) > lly) && (point(2) < ury));
}

func plrect(x0,y0,x1,y1,keywords=)
    // need to call plgk instead of plhk for more eye candy. Move plrect into graphk.i
{
  if (is_void(keywords)) keywords=GraphK(closed=&long(1),marks=&long(0));
  else if (keywords.closed==pointer()) keywords.closed=&long(1);
  if (is_void(y0)) {
    pos=x0;
    x0=pos(1);
    y0=pos(2);
    x1=pos(3);
    y1=pos(4);
  } else if (is_void(x1)) {
    y1=y0(2);
    x1=y0(1);
    y0=x0(2);
    x0=x0(1);
  }
  plgk,[y0,y0,y1,y1],[x0,x1,x1,x0],keywords=keywords;
}

func tws_plid(widget)
{
  widget->cur_plid++;
  return widget->cur_plid;
}

func tws_action(widget)
{
  return symbol_def(widget->type);
}

func tws_handler(root,handler)
/*  DOCUMENT tws_handler,handler

    Once your  control panel  has been created  using TWS_INIT  and TWS_BUTTON,
    call TWS_HANDLER to start event loop. HANDLER must be a string, the name of
    a routine written by you that will do the real job.

    Sample HANDLER routine:

    func my_handler(uname,button)
    {
       write,"Hello world!";
       return 0;
    }

    (See "help,tws"  for a slightly more  complete example, see  Cubeview for a
    useful one.)
    
    Each time a button is pressed  in the control panel, your home made handler
    is called  with two arguments  (which your handler  must accept even  if it
    doesn't use them):

      UNAME: the user name of the tws_button which has been pressed.
      BUTTON: the ID of the mouse button that has been used. (See MOUSE)

    From these to informations, your handler  must do its job, for instance run
    contextual  help if  right button  has been  used, and  run  the approriate
    action if left button has been pressed.

    The tws_button which has been pressed is blue untill your handler returns a
    value:  this is  to let  the  user know  that the  corresponding action  is
    running. Pressing another button at that time does normally nothing.

    Your handler must return  a value. The loop lasts as long  as this value is
    0. Any other value  makes TWS_HANDLER exit, a value  of 2 makes TWS_HANDLER
    kill the command window and exit, which is nice. Think of it as "quit", but
    keep  in mind  that  YOUR handler  must  do any  house  keeping job  before
    returning this value.
*/
{
  res=0;
  window,(root->wid);
  while (res==0) {
    result=mouse(1,0,"");
    button=long(result(10));
    if (button != 0) {
      event=tws_action(root)(root,action="GetEvent",mouse=result);
      if (!is_void(event)) {
        rien=tws_action(event.widget)(event.widget,action="Activate");
        res=symbol_def(handler)(event);
        window,(root->wid);
        rien=tws_action(event.widget)(event.widget,action="Deactivate");
      }
    }
  }
  if (res==2) winkill,root->wid;
}

func tws_realize(root,nokill=)
{
  rien=tws_action(root)(root,action="Realize",nokill=nokill);
}
