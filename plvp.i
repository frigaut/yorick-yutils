/* Easily manipulate viewport properties (2up, 4up, split, merge viewports)
 *
 *
 * Author: Francois Rigaut
 * Written 2004
 * last revision/addition: 2007
 *
 * Copyright (c) 2003-2007, Francois Rigaut
 *
 * This program is free software; you can redistribute it and/or  modify it
 * under the terms of the GNU General Public License  as  published  by the
 * Free Software Foundation; either version 2 of the License,  or  (at your
 * option) any later version.
 *
 * This program is distributed in the hope  that  it  will  be  useful, but
 * WITHOUT  ANY   WARRANTY;   without   even   the   implied   warranty  of
 * MERCHANTABILITY or  FITNESS  FOR  A  PARTICULAR  PURPOSE.   See  the GNU
 * General Public License for more details (to receive a  copy  of  the GNU
 * General Public License, write to the Free Software Foundation, Inc., 675
 * Mass Ave, Cambridge, MA 02139, USA).
 *
 *
*/   


require,"style.i";

func plsplit(nx,ny,win=,margin=,style=,dpi=,sys=,square=,save=,vp=,height=,width=)
/* DOCUMENT plsplit,nx,ny,margin=,style=,dpi=,sys=
   Split the viewport into nx x ny viewports
   nx = number of horizontal viewports
   ny = number of vertical viewports
   margin = margin correction for each viewport (+/- a few hundreth)
   style = if style is set, the given style is loaded before the
           split occurs. style can be a string vector of styles, one
           per viewport to be created.
   dpi = if dpi is set, the current window is re-created with said dpi
   sys = system number on which the split operation is to be performed.
         sys can be used to create complex, multi-level viewports.
   square = if set, created viewports will be square.
   vp = a 4 element vector to get a specific viewport, as returned by :
        get_style,landscape, systems, legends, clegends;
        write,systems.viewport;
   height and width as in the call to window.
        
   examples:
   plsplit,2,2,style="work.gs"
     define a 2x2 viewport with style "work.gs"

   plsplit,2,2,style=["work.gs","work.gs","work.gs","nobox.gs"]
     define a 2x2 viewport with style "work.gs", except for viewport#4
     which gets "nobox.gs"

   plsplit,1,2,sys=2
     start from graphic system with >1 viewport.
     viewport#2 will be split in 1x2 sub-viewports

   plsplit,1,2,sys=2,style=["boxed.gs","nobox.gs"]
     start from graphic system with >1 viewport.
     viewport#2 will be split in 1x2 sub-viewports with styles
     as in the style keyword.
     
   plsplit,3,3,style=_(array("boxed.gs",7),"nobox.gs","boxed.gs"),\
     dpi=100,margin=0.005; pljoin,[2,3,5,6]; testViewports;

   SEE ALSO:
*/
{
  extern pltitle_height_vp,pltitle_margin,xytitle_margin;

  n = current_window();

  if (sys && (n==-1))
    error,"sys keyword: No window to draw system from";
  
  if (sys) get_style,landscape, systems, legends, clegends;

  // if dpi is set, have to kill the current window, if it exist
  if (win) {
    winkill,win;
    n=win;
  } else {
    if (n!=-1) winkill;
    if (n==-1) n=0;  
  }
    
  
  // create new window
  window,n,dpi=dpi,wait=1,height=height,width=width;

  keep_sys=keep_pltitle_height=keep_pltitle_margin=keep_xytitle_margin=[];

  if (sys) {
    // we reload the old systems
    set_style,landscape, systems, legends, clegends;
    // keep all but the one we want to modify
    w = where(indgen(numberof(systems))!=sys);
    keep_sys = systems(w);
    keep_pltitle_height = pltitle_height_vp(w);
    keep_pltitle_margin = pltitle_margin(w);
    keep_xytitle_margin = xytitle_margin(,w);
    systems = systems(sys);
  } else {
    // we get the current system
    get_style,landscape, systems, legends, clegends;
  }
  cvp = systems(1).viewport;
  if (vp!=[]) cvp = vp;

  if (!is_void(style)) {
    systems = [];
    // load all styles
    for (i=1;i<=numberof(style);i++) {
      read_style,style(i),landscape, tmp, legends, clegends;
      tmp.viewport=cvp;
      grow,systems,tmp;
    }
  }
  
  nx = clip(nx,1,);
  ny = clip(ny,1,);
  nvp = nx*ny;
  ex = 0.75;

  if (numberof(systems)!=nvp) { // complete using last
    grow,systems,array(systems(0),nvp-numberof(systems));
  }

  pltitle_height_vp = array(float,nvp);
  pltitle_margin = array(float,nvp);
  xytitle_margin = array(float,[2,2,nvp]);
  plmargin = array(float,[2,2,nvp]);

  // now systems contains the system array to tweak

  for (i=1;i<=nx;i++) {
    for (j=1;j<=ny;j++) {

      ns = (j-1)*nx+i;

      // set each VP margin:
      plmargin(,ns) = [0.026,0.026];
      // if tick project outward of viewport, adjust default margin:
      plticks = [systems(ns).ticks.horiz.flags(1),
                 systems(ns).ticks.vert.flags(1)];
      if (plticks(1) & 0x010) plmargin(2,ns)=0.032;
      if (plticks(2) & 0x010) plmargin(1,ns)=0.032;
      if (!is_void(margin)) plmargin(,ns)+=margin;

      marginx = plmargin(1,ns);
      //  marginy = (cvp(4)-cvp(3))/(cvp(2)-cvp(1))*plmargin(2);
      marginy = plmargin(2,ns);
  
      x1 = cvp(1)-marginx;
      x2 = cvp(2)+marginx;
      y1 = cvp(3)-marginy;
      y2 = cvp(4)+marginy;
      xsize = (x2-x1)/nx;
      ysize = (y2-y1)/ny;

      if (square) {
        // find max margin:
        if (xsize>ysize) {
          x1 = x1+(xsize-ysize)/2.;
          xsize=ysize;
        } else {
          y1 = y1+(ysize-xsize)/2.;
          ysize=xsize;
        }      
      }  
  
      vp = array(double,4);
      vp(1) = x1+(i-1)*xsize+marginx;
      vp(2) = x1+i*xsize-marginx;
      vp(3) = y1+(j-1)*ysize+marginy;
      vp(4) = y1+j*ysize-marginy;
      systems(ns).viewport = vp;

      xs = vp(dif:1:2);
      ys = vp(dif:3:4);
      
      // adjust xytitle_margins
      xytitle_margin(1,ns)= 0.015/xs^0.25-
        max(systems(ns).ticks.vert.tickLen)*((plticks(2) & 0x010) > 0);
      xytitle_margin(2,ns)= 0.021/ys^0.20-
        max(systems(ns).ticks.horiz.tickLen)*((plticks(1) & 0x010) > 0);
      
      // adjust tick lenght:
      systems(ns).ticks.horiz.tickLen /= ny^ex;
      systems(ns).ticks.vert.tickLen /= nx^ex;

      // adjust text height:
      systems(ns).ticks.horiz.textStyle.height= \
        systems(ns).ticks.vert.textStyle.height =
        max([0.008,0.055*min([xs,ys])^1]);

      // adjust xytitle/pltitle text height:
      pltitle_height_vp(ns)= systems(ns).ticks.horiz.textStyle.height*900;

      //  adjust pltitle_margin
      if ((plticks(1) & 0x010) > 0) {
        pltitle_margin(ns)= max(systems(ns).ticks.horiz.tickLen);
      } else {
        pltitle_margin(ns) = 0.;
      }

      // adjust distance to labels
      systems(ns).ticks.horiz.labelOff /= ny^0.7;
      systems(ns).ticks.vert.labelOff /= nx^0.7;

    }
  }
  grow,keep_pltitle_height,pltitle_height_vp;
  pltitle_height_vp = keep_pltitle_height;
  pltitle_height_vp = max(pltitle_height_vp,8);

  grow,keep_pltitle_margin,pltitle_margin;
  pltitle_margin = keep_pltitle_margin;

  grow,keep_xytitle_margin,xytitle_margin;
  xytitle_margin = keep_xytitle_margin;
  
  grow,keep_sys,systems;
  set_style,landscape, keep_sys, legends, clegends;

  if (save) write_style, save, landscape, keep_sys, legends, clegends;
}

func pltitle_vp(title, adjust, pos=)
/* DOCUMENT pltitle_vp, title, deltay
     Plot TITLE centered above the coordinate system for any of the
     standard Gist styles.  You may want to customize this for other
     plot styles.
     pos = -1 (left), 0 (default, centered) or 1 (right)
   SEE ALSO: plt, xytitles
 */
{
  if (is_void(pltitle_margin)) {
    marg=0.;
    csys=1;
  } else {
    csys = min(plsys(),numberof(pltitle_margin));
    marg=pltitle_margin(csys);
  }

  //pltitle_height is always set
  if (numberof(pltitle_height_vp)>1) {
    csys = min(plsys(),numberof(pltitle_height_vp));
    _pltheight = pltitle_height_vp(csys);
  } else _pltheight = pltitle_height_vp(1);


  if (is_void(adjust)) adjust=0.;
  adjust +=marg;
  
  port= viewport();
  if (!pos) {
    plt, title, port(zcen:1:2)(1), port(4)+adjust,
      font=pltitle_font, justify="CB", height=_pltheight;
  } else if (pos==-1) {
    plt, title, port(1), port(4)+adjust,
      font=pltitle_font, justify="LB", height=_pltheight;
  } else if (pos==1) {
    plt, title, port(2), port(4)+adjust,
      font=pltitle_font, justify="RB", height=_pltheight;
  } else {
    error,"pos keyword can only be -1,0 or 1";
  }
}

func xytitles_vp(xtitle, ytitle, adjust)
/* DOCUMENT xytitles_vp, xtitle, ytitle
       -or- xytitles_vp, xtitle, ytitle, [deltax,deltay]
     Plot XTITLE horizontally under the viewport and YTITLE vertically
     to the left of the viewport.  If the tick numbers interfere with
     the labels, you can specify the [DELTAX,DELTAY] in NDC units to
     displace the labels.  (Especially for the y title, the adjustment
     may depend on how many digits the numbers on your scale actually
     have.)  Note that DELTAX moves YTITLE and DELTAY moves XTITLE.
     WARNING: There is no easy way to ensure that this type of title
              will not interfere with the tick numbering.  Interference
              may make the numbers or the title or both illegible.
   SEE ALSO: plt, pltitle
 */
{
  // multi viewport margins
  if (is_void(xytitle_margin)) {
    marg=0.;
  } else if ((_d=dimsof(xytitle_margin))(1)!=2) {
    marg=xytitle_margin(1);
  } else {
    // this is if the mutli viewport was created by other means than
    // plsplit and xytitle_margin has not been set for each viewport
    csys = min(plsys(),_d(3));
    marg=xytitle_margin(,csys);
  }

  //pltitle_height is always set
  if (numberof(pltitle_height_vp)>1) {
    csys = min(plsys(),numberof(pltitle_height_vp));
    _pltheight = pltitle_height_vp(csys);
  } else if (numberof(pltitle_height_vp)==0) {
    pltitle_height_vp = pltitle_height;
    _pltheight = pltitle_height;
  } else _pltheight = pltitle_height_vp(1);

  if (is_void(adjust)) adjust=[0.,0.];
  adjust +=marg;
  
  port= viewport();
  if (xtitle && strlen(xtitle))
    plt, xtitle, port(zcen:1:2)(1), port(3)-0.050+adjust(2),
      font=pltitle_font, justify="CT", height=_pltheight;
  if (ytitle && strlen(ytitle))
    plt, ytitle, port(1)-0.050+adjust(1), port(zcen:3:4)(1),
      font=pltitle_font, justify="CB", height=_pltheight, orient=1;
}

if (pltitle_orig==[]) pltitle_orig = pltitle;
if (xytitles_orig==[]) xytitles_orig = xytitles;

func pltitle(title, adjust, pos=)
{
  get_style,landscape,systems;
  if (numberof(systems)==1) pltitle_orig,title;
  else pltitle_vp,title,adjust,pos=pos;
}

func xytitles(xtitle, ytitle, adjust)
{
  get_style,landscape,systems;
  if (numberof(systems)==1) xytitles_orig,xtitle, ytitle, adjust;
  else xytitles_vp,xtitle,ytitle,adjust;
}


func pljoin(sys2join)
{
  get_style,landscape, systems, legends, clegends;

  sys = [];

  nsys = numberof(systems);
  //w=1 where valid, i.e. keep:
  w = where(!(indgen(nsys)==sys2join(-,))(,sum));
  nonw = where((indgen(nsys)==sys2join(-,))(,sum)); //w=1 where invalid

  sys = systems(w);
  _pltheight = pltitle_height_vp(w);
  _pltmargin = pltitle_margin(w);
  _xytmargin = xytitle_margin(,w);

  grow,sys,systems(sys2join(1));
  sys(0).viewport([1,3]) = systems(sys2join).viewport(,min)([1,3]);
  sys(0).viewport([2,4]) = systems(sys2join).viewport(,max)([2,4]);

  grow,_pltheight,pltitle_height_vp(nonw(1));
  grow,_pltmargin,pltitle_margin(nonw(1));
  grow,_xytmargin,xytitle_margin(,nonw(1));

  pltitle_height_vp = _pltheight;
  pltitle_margin = _pltmargin;
  xytitle_margin = _xytmargin;

  xs = sys(0).viewport(dif:1:2);
  ys = sys(0).viewport(dif:3:4);
  
  plticks = [systems(0).ticks.horiz.flags(1),
             systems(0).ticks.vert.flags(1)];
  
  // adjust xytitle_margins
  xytitle_margin(1,0)= 0.015/xs^0.25-
    max(systems(0).ticks.vert.tickLen)*((plticks(2) & 0x010) > 0);
  xytitle_margin(2,0)= 0.021/ys^0.20-
    max(systems(0).ticks.horiz.tickLen)*((plticks(1) & 0x010) > 0);
      
  // adjust text height:
  sys(0).ticks.horiz.textStyle.height= \
    sys(0).ticks.vert.textStyle.height = \
    max([0.008,0.055*min([xs,ys])^1]);

  // adjust xytitle/pltitle text height:
  pltitle_height_vp(0)= sys(0).ticks.horiz.textStyle.height*900;

  //  adjust pltitle_margin
  if ((plticks(1) & 0x010) > 0) {
    pltitle_margin(0)= max(sys(0).ticks.horiz.tickLen);
  } else {
    pltitle_margin(0) = 0.;
  }

  set_style,landscape, sys, legends, clegends;
}

func testViewports(void)
{
  fma;
  x = span(0.,50.,200); y=1-cos(x)*exp(-x/5.);
  get_style,landscape, systems, legends, clegends;
  nviewports = numberof(systems);
  im = indgen(32)(-:1:32,)-16.;
  im = sqrt(im^2+transpose(im)^2.);
  for (i=1;i<=nviewports;i++) {
    plsys,i;
    if (nallof([systems(i).ticks.horiz.flags,systems(i).ticks.vert.flags])) {
      pli,im;
      limits;
    } else {
      plg,y*random()*1000.,x;
      xytitles_vp,"Time after shock [s]","Damping [N]";
      plmargin,0.02;
    }
    pltitle_vp,swrite(format="Viewport %d",i),pos=-1;
  }
  redraw;
}

func plmargin(margin,xy)
/* DOCUMENT plmargin(margin,xy)
   redefines the limits to leave a margin around the inner plot.
   Margin are in fraction of the plot width. Default 0.05.
   xy = 1 only sets margin for the x axis.
   xy = 2 only sets margin for the y axis.
   SEE ALSO:
 */
{
  if (is_void(margin)) margin=0.05;
  limits;
  l = limits();
  x = l(2)-l(1);
  y = l(4)-l(3);
  x*=margin; y*=margin;
  if (xy==1)       limits,l(1)-x,l(2)+x; \
  else if (xy==2)  range,l(3)-y,l(4)+y; \
  else             limits,l(1)-x,l(2)+x,l(3)-y,l(4)+y;
}

