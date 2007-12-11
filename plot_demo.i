/*
 * plot_demo.i --
 *
 *	Demonstration of routines in "plot.i".
 *	Provides routines: plot_demo, plot_demo1, plot_demo2.
 *
 * Copyright (c) 1996, Eric THIEBAUT (thiebaut@obs.univ-lyon1.fr, Centre de
 *	Recherche Astrophysique  de Lyon, 9 avenue Charles  Andre,
 *	F-69561 Saint Genis Laval Cedex).
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
 * History:
 *	$Id: plot_demo.i,v 1.1 2007-12-11 23:55:14 frigaut Exp $
 *	$Log: plot_demo.i,v $
 *	Revision 1.1  2007-12-11 23:55:14  frigaut
 *	Initial revision
 *
 *	Revision 1.2  2002/11/14 11:20:52  eric
 *	 - changed paths in require calls
 *
 */

require, "plot.i";
require, Y_SITE+"include/random.i";

func plot_demo {plot_demo1; plot_demo2;}

func plot_demo1
{
  /*
   * Select an eventually new window and choose "work" style.
   */
  win = max(0, current_window());
  window, win, wait=1, style="work.gs";
  limits;
  animate, 0;
  pldefault, font="helveticaBI", width=5, height=10, marks=0;

  /*
   * Some data.
   */
  x= span(0., 2., 500);
  y= sin(-pi*exp(x))*exp(-x);
  xp= x(15::30);
  yp= y(15::30);
  dx= 0.05;
  dy= 0.05 + yp/5.;	// RSB ~ 5
  xn= dx*random_n(dimsof(xp));
  yn= dy*random_n(dimsof(yp));

  write, " - true model and data points (in green)";
  plg, y, x;
  plp, yp+yn, xp+xn, color="green";
  fma;
  if (strtok(rdline(prompt="hit Enter to continue, Q to quit"))(1)=="q")
    return;

  write, " - data points (in green) with error bars (in red)";
  plg, y, x;
  plp, yp+yn, xp+xn, dx=dx, dy=dy, ticks=0, color="red", symbol=0;
  plp, yp+yn, xp+xn, color="green";
  fma;
  if (strtok(rdline(prompt="hit Enter to continue, Q to quit"))(1)=="q")
    return;

  write, " - data points with error bars and ticks (in blue)";
  plg, y, x;
  plp, yp+yn, xp+xn, dx=dx, dy=dy, ticks=3, color="blue";
  fma;
  if (strtok(rdline(prompt="hit Enter to continue, Q to quit"))(1)=="q")
    return;

  write, " - data points with error bars, ticks and symbol (in green)";
  plg, y, x;
  plp, yp+yn, xp+xn, dx=dx, dy=dy, ticks=3, symbol=5, size=1.5, width=1,
    color="green";
  fma;
  if (strtok(rdline(prompt="hit Enter to continue, Q to quit"))(1)=="q")
    return;

  write, " - data points with various symbols and colors";
  plg, y, x;
  colors= ["red", "green", "blue", "magenta", "cyan", "yellow"];
  for (i=1; i<=numberof(colors);i++) {
    xn= dx*random_n(dimsof(xp));
    yn= dy*random_n(dimsof(yp));
    plp, yp+yn, xp+xn, symbol=i-1, size=1.5, color=colors(i);
  }
  fma;
  if (strtok(rdline(prompt="hit Enter to continue, Q to quit"))(1)=="q")
    return;

  write, " - histograms with different justification (plh)";
  pldefault, font="helveticaBI", width=1, height=10, marks=0;
  plh, y(::10)-.1, x(::10), just=1, marks=0, color="blue";
  plh, y(::10), x(::10), just=2, marks=0;
  plh, y(::10)+.1, x(::10), just=3, marks=0, color="cyan";
  fma;
  if (strtok(rdline(prompt="hit Enter to continue, Q to quit"))(1)=="q")
    return;

}

func plot_demo2
{
  
  write, "WARNING - demo under construction ...";
  return;

  
  local alt, az;
  x= span(-1, 1, 30)(,-:1:30);
  y= transpose(x);
  a= abs(x, y);
  a= exp(-32.*a^2);
  a= a + .3 * roll(a, [5,-5]) - .7 * roll(a, [-5,-10]);

  /*
   * Select an eventually new window and choose "nobox" style.
   */
  win = max(0, current_window());
  window, win, wait=1, style="nobox.gs";
  palette, "rainbow.gp";
  limits;
  animate, 1;
  pldefault, font="helveticaBI", height=10;

  write, " - deformation of a surface mesh";
  pl3s, a, y, x, edges=1, fill=1, axis=1;
  fma;
  q= [100, 60, 45, 35, 25, 20, 15, 12, 9, 7, 6, 5];
  for (k = 1; k <= numberof(q); k++) {
    z= x+1i*y;
    z= q(k)*z/(q(k)+z*z);
    xx= z.re;
    yy= z.im;
    pl3s, a, yy, xx, edges=1, fill=1, axis=1;
    fma;
  }
  if (strtok(rdline(prompt="hit Enter to continue, Q to quit"))(1)=="q")
    return;

  write, " - surface mesh as wireframe (fill=0 or nil)";
  pl3s, a, yy, xx, edges=1, fill=0, axis=1, box=1;
  fma;
  if (strtok(rdline(prompt="hit Enter to continue, Q to quit"))(1)=="q")
    return;

  write, " - surface mesh filled with brightness (fill=1)";
  pl3s, a, yy, xx, edges=1, fill=1, axis=1, box=1;
  fma;
  if (strtok(rdline(prompt="hit Enter to continue, Q to quit"))(1)=="q")
    return;

  write, " - surface mesh shaded (fill=2)";
  pl3s, a, yy, xx, edges=1, fill=2, axis=1, box=1;
  fma;
  if (strtok(rdline(prompt="hit Enter to continue, Q to quit"))(1)=="q")
    return;

  write, " - rotation of the point of view";
  step= 20;
  az= 35.;
  alt= 35.;
  while (az <= 360.) {
    for (x=0; x<=360; x+=step) {
      pl3s, a, yy, xx, az=az, alt=alt+x, box=1, edges=1, fill=1, axis=0;
      fma;
    }
    for (x=step; x<=90; x+=step) {
      pl3s, a, yy, xx, az=az+x, alt=alt, box=1, edges=1, fill=1, axis=0;
      fma;
    }
    az += 90.;
  }
  if (strtok(rdline(prompt="hit Enter to continue, Q to quit"))(1)=="q")
    return;

  /*
   * Restore plot defaults.
   */
  animate, 0;

  /*
   * 2-D surface plot.
   */
  window, win, wait=1, style="work.gs";
  pldefault, width=1;
  write, " - filled mesh with contour (pls routine)";
  pls, a, cbar=1,  nlevs=10, xtitle="abscissa", ytitle="ordinate",
    title="Example of PLS routine", font="timesBI", height=14, marks=0;
  fma;
  if (strtok(rdline(prompt="hit Enter to continue, Q to quit"))(1)=="q")
    return;
  write, " - filled mesh with contour (pls routine)";
  pls, a, yy, xx, cbar=1,  nlevs=10, xtitle="abscissa", ytitle="ordinate",
    title="Example of PLS routine", font="timesBI", height=14, marks=0;
  fma;
  if (strtok(rdline(prompt="hit Enter to continue, Q to quit"))(1)=="q")
    return;

}

func plot_demo3(step)
{
  local alt, az;
  x=span(-1, 1, 40)(,-:1:30);
  y=span(-2, 4, 30)(-:1:40,);
  z=sin(x*x+y*x);

  /*
   * Select an eventually new window and choose "nobox" style.
   */
  win = max(0, current_window());
  window, win, wait=1, style="nobox.gs";
  palette, "rainbow.gp";
  limits;
  animate, 1;
  pldefault, font="helveticaBI", height=10;

  write, " - rotation of the point of view";
  if (!step) {
    step = 20;
  }
  az= 35.;
  alt= 35.;
  while (az <= 360.) {
    for (a=0; a<=360; a+=step) {
      pl3s, z, y, x, az=az, alt=alt+a, box=1, edges=1, fill=1, axis=1;
      fma;
      //rdline, prompt="hit Enter to continue";
     }
    for (a=step; a<=90; a+=step) {
      pl3s, z, y, x, az=az+a, alt=alt, box=1, edges=1, fill=1, axis=1;
      fma;
      //rdline, prompt="hit Enter to continue";
    }
    az += 90;
  }
  if (strtok(rdline(prompt="hit Enter to continue, Q to quit"))(1)=="q")
    return;

  /*
   * Restore plot defaults.
   */
  animate, 0;

}
