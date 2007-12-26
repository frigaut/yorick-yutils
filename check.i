/* Check routines for the yorick yutils package
 *
 *
 * Author: Francois Rigaut
 * Written 2003
 * last revision/addition: 2007
 *
 * Copyright (c) 2003-3007, Francois Rigaut
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
require,"linalg.i";
require,"util_fr.i";
require,"utils.i";
require,"random_et.i";
require,"random.i";

write,"\nCheck file in construction!!\n";

func clmfit(y,x,&a,function,&yfit)
/* DOCUMENT clmfit(y,x,&a,function,&yfit)
 * Useful wrapper for the lmfit procedure.
 * y = the data to fit vs x
 * a = the output coefficients (may have initial value on input)
 * function = a string containing the function definition where
 * x and a must be used as variable and coefficients name
 * e.g. "a(1)+a(2)*cos(x)"
 * yfit = optional output. Best fit.
 * SEE ALSO: lmfit
 */
{
  if (open("./.foo.i","r",1)) system,"rm ./.foo.i";

  f     = open("./.foo.i","w");
  write,f,"func foo(x,a) {return "+function+";}";
  close,f;
  include,"./.foo.i",10;
  r= lmfit(foo,x,a,y);

  yfit = foo(x,a);
  return a;
}

if (batch()) error,"Run as yorick -i check-yutils.i";

pwd;
ls;
pm,makegaussian(5,2);

write,"Various + random_et + plot tests";
n=64;
g=makegaussian(n,12)*100;
tic;
g=random_poisson(g)+random_n([2,n,n]);
write,format="Ellapsed time=%f\n",tac();

write,"pls,smooth(g)";
fma;
pls,smooth(g);
pause,1000;

write,"pl3s,smooth(g)";
fma;
pl3s,smooth(g),fill=1;
pause,1000;

write,"test of lmfit";
xy = indices(n);
a = [0,1,1,n/2,n/2];
foo="a(1)+a(2)*exp(-(abs(x(,,1)-a(4),x(,,2)-a(5))/a(3))^2.)";
clmfit,g,xy,a,foo,yfit;
tv,_(g,yfit),square=1;
pltitle,"Noisy gaussian and fit";
write,format="Fit parameters: %.2f+%.1f*exp(-(sqrt((x-%.2f)^2"+\
  "+(y-%.2f)^2)/%.2f)^2)\n",a(1),a(2),a(4),a(5),a(3);
s=swrite(format="%.1f+%.1f*exp(-(sqrt((x-%.2f)^2^"+\
  "+(y-%.2f)^2^)/%.2f)^2^)\n",a(1),a(2),a(4),a(5),a(3));
plt,s,-10,n,tosys=1,orient=1,height=10,justify="CC";
pause,1000;

write,"Test of additional color tables";
for (i=1;i<=41;i++) {loadct,i; pause,100;}

xy = indices([2,64,40]);
g = exp(-((xy(,,1)-28)/10)^2 - ((xy(,,2)-19)/4)^2.);
tv,g,square=1;
tv,undersample(g,2);
limits,square=0;
pause,1000;

write,"Ramdom_et and histo tests";
x = random_poisson(array(10,10000));
fma;
histo_plot,x;
pause,500;


write,"Input/output to disk tests"
n=100;
x = random_n(n);
iv = indgen(n);
//create a random filename:
fname=swrite(format="/tmp/junk-%d.dat",long(random()*10000));
f = open(fname,"w");
for (i=1;i<=n;i++) write,f,iv(i),x(i);
close,f;

d = rdcols(fname);
plot,*d(2),*d(1);

d = read_ascii(fname);
d(2,) = clip(d(2,),-0.5,0.5);
plg,d(2,),d(1,),color="red";

remove,fname;

stat,d;

write,"All tests successful for package yutils";
