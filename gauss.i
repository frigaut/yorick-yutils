/*
 * gauss.i
 *
 * $Id: gauss.i,v 1.3 2008-10-29 15:54:21 paumard Exp $
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
 * $Log: gauss.i,v $
 * Revision 1.3  2008-10-29 15:54:21  paumard
 * gauss.i: add gauss2d()
 *
 * Revision 1.2  2008/01/04 14:35:40  frigaut
 * - changed path for require statement
 *
 * Revision 1.1  2008/01/04 13:47:48  frigaut
 * initial import of thibaut's functions
 *
 *
 */


func gauss(x,a,&grad,deriv=)
/* DOCUMENT gauss(x,a)
   
    Returns a gaussian:
     I0*exp(-0.5*((x-x0)/dx)^2) [+a(4) [+a(5)*x]]
    Where:
    I0=a(1)
    x0=a(2)
    dx=a(3) (gaussian sigma)

   Works with lmfit, and can return derivates.

   Notes: FHWM=sigma*2*sqrt(2*alog(2)); sum(gauss)=I0*sigma*sqrt(2*pi)
   
   SEE ALSO: gauss_fit, asgauss, asgauss_fit
*/
{
    nterms=numberof(a);
    eps=1e-100;
    if (abs(a(3))<eps) a(3)=sign(a(3))*eps;
    if (a(3)==0) u1=0; else u1=exp(-0.5*((x-a(2))/a(3))^2);
    res=a(1)*u1;
    if (deriv) {
        grad=array(double,numberof(x),nterms);
        grad(,1)=u1;
        grad(,2)=res*(x-a(2))/a(3)^2;
        grad(,3)=res*(x-a(2))^2/a(3)^3;
        if (nterms>3) grad(,4)=1.;
        if (nterms==5) grad(,5)=x;
    }
    if (nterms>3) res=res+a(4);
    if (nterms==5) res=res+a(5)*x;
    return res;
}

func asgauss(x,a,&grad,deriv=)
/* DOCUMENT asgauss(x,a)
   
    Returns an assymetrical gaussian:
     I0*exp(-((x-x0)/dx)^2) [+a(5) [+a(6)*x]]
    Where:
    I0=a(1)
    x0=a(2)
    dx=a(3) for x<x0 and dx=a(4) for x>=x0

   Works with lmfit, and can return derivates.

   SEE ALSO: gauss, gauss_fit, asgauss_fit
*/
{
    nterms=numberof(a);
    ta=(x<a(2));
    tb=(x>=a(2));
    u1=exp(-0.5*((x-a(2))*(ta/a(3)+tb/a(4)))^2);
    res=a(1)*u1;
    if (deriv) {
        grad=array(double,numberof(x),nterms);
        grad(,1)=u1;
        grad(,2)=res*(x-a(2))*(ta/a(3)+tb/a(4))^2;
        grad(,3)=res*(x-a(2))^2*(ta/a(3))^3;
        grad(,4)=res*(x-a(2))^2*(tb/a(4))^3;
        if (nterms>4) grad(,5)=1.; // useless
        if (nterms==6) grad(,6)=x;
    }
    if (nterms>4) res=res+a(5);
    if (nterms==6) res=res+a(6)*x;
    return res;
}

func gauss_fit(y,x,w,guess=,nterms=,fit=,correl=,stdev=,gain=,tol=,deriv=,itmax=,lambda=,eps=,monte_carlo=) {
/* DOCUMENT gauss_fit(y,x,w,guess=,nterms=)

    Fits a gaussian (see gauss) profile on a data set using lmfit (see lmfit).
    The set  of data points  Y is the  only mandatory argument, X  defaults to
    indgen(numberof(y)), weights  W are optional (see  lmfit). GAUSS_FIT tries
    to guess  a set of  initial parameters, but  you can (and should  in every
    non-trivial case) provide one using  the GUESS keyword.  In case you don't
    provide a guess,  you should set NTERMS to 3  (simple gaussian), 4 (adjust
    constant  baseline) or 5  (adjust linear  baseline).  The  returned fitted
    parameters have the same format as GUESS, see gauss.

   SEE ALSO: gauss, asgauss, asgauss_fit
*/
    require,"lmfit.i";
    if (is_void(x)) x=indgen(numberof(y));
    if (is_void(guess)) {
        if (is_void(nterms)) nterms=3;
        if (nterms<3) nterms=3;
        if (nterms>5) nterms=5;
        guess=array(double,nterms);
        if (nterms==4) {
            base=median(y);
            guess(4)=base;
        } else if (nterms==5) {
            n=numberof(y);
            y1=median(y(1:long(n/2)));
            x1=median(x(1:long(n/2)));
            y2=median(y(-long(n/2):0));
            x2=median(x(-long(n/2):0));
            guess(5)=(y2-y1)/(x2-x1);
            if (guess(5)!=0) guess(4)=y1-guess(5)*x1;
            base=guess(4)+guess(5)*x;
        } else base=0.;
        y2=y-base;
        ind0=abs(y2)(mxx);
        guess(2)=x(ind0);
        guess(1)=y2(ind0);
        if (y2(ind0)==guess(1)) yy=y2;
        else yy=-y2;
        ind1=ind0;
        ind2=ind0;
        while (ind1>1 && yy(ind1)>0.5*guess(1)) ind1--;
        if (yy(ind1)<0.5*guess(1)) ind1++;
        while (ind2<numberof(y)-1 && yy(ind2)>0.5*guess(1)) ind2++;
        if (yy(ind2)<0.5*guess(1)) ind2--;
        guess(3)=abs(x(ind2)-x(ind1))/sqrt(2.);
    } else nterms=numberof(guess);
    a=guess;
    if (is_void(deriv)) deriv=1;
    result=lmfit(gauss,x,a,y,w,deriv=deriv,fit=fit,correl=correl,stdev=stdev,
                 gain=gain,tol=tol,itmax=itmax,lambda=lambda,
                 eps=eps,monte_carlo=monte_carlo);
    return a;
}

func asgauss_fit(y,x,w,guess=,nterms=){
/* DOCUMENT asgauss_fit(y,x,w,guess=,nterms=)

    Fits an  assymetrical gaussian  (see asgauss) profile  on a data  set using
    lmfit  (see  lmfit). The  set  of  data points  Y  is  the only  mandatory
    argument, X  defaults to indgen(numberof(y)), weights W  are optional (see
    lmfit). ASGAUSS_FIT tries  to guess a set of  initial parameters, but you
    can (and  should in  every non-trivial case)  provide one using  the GUESS
    keyword. In  case you don't  provide a guess,  you should set NTERMS  to 6
    (simple assymmetrical  gaussian), 7 (adjust constant baseline)  or 8 (adjust
    linear baseline). The  returned fitted parameters have the  same format as
    GUESS, see asgauss.

   SEE ALSO: asgauss, gauss, gauss_fit
*/
    require,"lmfit.i";
    if (is_void(x)) x=indgen(numberof(y));
    if (is_void(guess)) {
        if (is_void(nterms)) nterms=4;
        if (nterms<4) nterms=4;
        if (nterms>6) nterms=6;
        guess=array(double,nterms);
        if (nterms==5) {
            base=median(y);
            guess(5)=base;
        } else if (nterms==6) {
            n=numberof(y);
            y1=median(y(1:long(n/2)));
            x1=median(x(1:long(n/2)));
            y2=median(y(-long(n/2):0));
            x2=median(x(-long(n/2):0));
            guess(6)=(y2-y1)/(x2-x1);
            if (guess(6)!=0) guess(5)=y1-guess(6)*x1;
            base=guess(5)+guess(6)*x;
        } else base=0.;
        y2=y-base;
        ind0=abs(y2)(mxx);
        guess(2)=x(ind0);
        guess(1)=y2(ind0);
        if (y2(ind0)==guess(1)) yy=y2;
        else yy=-y2;
        ind1=ind0;
        ind2=ind0;
        while (ind1>1 && yy(ind1)>0.5*guess(1)) ind1--;
        if (yy(ind1)<0.5*guess(1)) ind1++;
        while (ind2<numberof(y)-1 && yy(ind2)>0.5*guess(1)) ind2++;
        if (yy(ind2)<0.5*guess(1)) ind2--;
        guess(3)=2*abs(x(ind0)-x(ind1));
        guess(4)=2*abs(x(ind2)-x(ind0));
    } else nterms=numberof(guess);
    a=guess;
    result=lmfit(asgauss,x,a,y,w,deriv=1);
    return a;
}

func gauss2d(xy, a, &grad, deriv=) {
/* DOCUMENT gauss(xy,a)
    
     Returns a 2D gaussian:
      I0*exp(-0.5*(X^2+Y^2)) [+a(7) [+a(8)*x [+a(9)*y]]]
     Where:
     x=xy(..,1)
     y=xy(..,2)
     X=((x-x0)*cos(alpha)+(y-y0)*sin(alpha))/dx
     Y=((y-y0)*cos(alpha)-(x-x0)*sin(alpha))/dy
     
     I0=a(1)
     x0=a(2)
     y0=a(3)
     dx=a(4) (gaussian sigma)
     dy=a(5)
     alpha=a(6)
     
    Works with lmfit, and can return derivates.
 
    Notes: FHWM=sigma*2*sqrt(2*alog(2)); sum(gauss2d)=2*pi*I0*dx*dy

    astro_util1.i contains two variants of this function: gaussian and
    gaussianRound. Those two functions do not provide derivatives and
    take a slightly different A vector (e.g. alpha in degrees instead
    of radians).
    
    SEE ALSO: gauss, gauss_fit, gaussian, gaussianRound
 */
  npars=numberof(a);
  eps=1e-100;
  if (abs(a(4))<eps) dx1=sign(a(4))/eps; else dx1=1./a(4);
  if (npars>=5) {
    if (abs(a(5))<eps) dy1=sign(a(5))/eps; else dy1=1./a(5);
  } else dy1=dx1;
  
  alpha=npars>=6?a(6):0.;
  X=((deltax=(xy(..,1)-(x0=a(2))))*(cosa=cos(alpha))+
     (deltay=(xy(..,2)-(y0=a(3))))*(sina=sin(alpha)))*dx1;
  Y=(deltay*cosa-deltax*sina)*dy1;
  
  u1=exp(-0.5*(r2=(X^2+Y^2)));
  res=a(1)*u1;

  if (numberof(a)>=7) res+=a(7);
  if (numberof(a)>=8) res+=a(8)*xy(..,1);
  if (numberof(a)>=9) res+=a(7)*xy(..,2);
  
  if (deriv) {
    grad=array(1.,dimsof(X),numberof(a));
    grad(..,1)=u1;
    grad(..,2)=((cosa*dx1)*X-(sina*dy1)*Y)*res;
    grad(..,3)=((sina*dx1)*X+(cosa*dy1)*Y)*res;
    grad(..,4)=dx1*X^2*res;
    
    if (numberof(a)>=5) grad(..,5)=dy1*Y^2*res; else grad(..,4)+=dy1*Y^2*res;
    if (numberof(a)>=6) grad(..,6)=X*Y*(dy1/dx1-dx1/dy1)*res;//<==
    //if (numberof(a)>=7) grad(..,7)=1.;
    if (numberof(a)>=8) grad(..,8)=xy(..,1);
    if (numberof(a)>=9) grad(..,9)=xy(..,2);
  }
  
  return res;
}
