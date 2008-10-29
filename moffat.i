/*
 * moffat.i
 *
 * $Id: moffat.i,v 1.1 2008-10-29 15:53:38 paumard Exp $
 *
 * This file is part of Yutils
 * Copyright (C) 2008  Thibaut Paumard <paumard@users.sourceforge.net>
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
 * $Log: moffat.i,v $
 * Revision 1.1  2008-10-29 15:53:38  paumard
 * moffat.i, multiprofile.i: initial import
 *
 *
 */

func moffat1d(x,a2,&grad,deriv=){
/* DOCUMENT moffat1d(x,a)

    Returns a (1D) Moffat profile:
    I=I0*(1+((x-x0)/dx)^2)^-b [+ k0 [+ k1*x]]
    Where:
    I0=a(1)
    x0=a(2) ; deriv: 2*I0*b*(x-x0)/(dx^2)*(1+((x-x0)/dx)^2)^(-b-1)
    dx=a(3) ; deriv: 2*I0*b*(x-x0)^2/(dx^3)*(1+((x-x0)/dx)^2)^(-b-1)
    b=a(4)

    and if a is of length 5 or 6:
    k0=a(5)
    k1=a(6)
    
    This function can be used directly with lmfit.

    Limitation: "b"  should  always  be  positive.  In  order  to  force  it,
    especially in fitting routines, its  abolute value is taken (except at some
    point in the computation of derivates).

   SEE ALSO: moffat1d_fit, asmoffat1d, asmoffat1d_fit
*/
    a=double(a2);
    extern __moffat_betamax,__moffat_vmax,__moffat_gradmax;
    if (is_void(__moffat_gradmax)) __moffat_gradmax=1e150;
    if (__moffat_betamax) big=__moffat_betamax ;
    else big=1e18; // higher value yield math errors, but a more meaningfull max can be set using __moffat_betamax
    nterms=numberof(a);
    if (__moffat_vmax) {
        if (abs(a(2)>__moffat_vmax)) {
            grad=array(double,numberof(x),nterms);
            return x*0;
        }
    }
    small=1e-80;
    if (a(3)<small) a(3)=small;
    u2=(x-a(2))/a(3);
    ind=where(abs(u2)>__moffat_gradmax);
    if (numberof(ind)) {
        u2(ind)=sign(u2(ind))*__moffat_gradmax;
        write,"*** Warning: MOFFAT caught overflows.";
    }
    u4=u2^2;
    u3=1+u4;
    if (abs(a(4))>big) {
        u1=u3*0;
        u1b=u1;
        ind=where(u4==0);
        if (numberof(ind)) {
            u1(ind)=1;
            u1b(ind)=1;
        }
    } else {
        u1=u3^-abs(a(4));
        u1b=u3^(-abs(a(4))-1);
    }
    res=a(1)*u1;
    if (nterms>4) res=res+a(5);
    if (nterms==6) res=res+a(6)*x;
    if (deriv) {
        grad=array(double,numberof(x),nterms);
        grad(,1)=u1;
        if (max(u1b)) grad(,2)=2*a(1)*(a(4))*u2/a(3)*u1b;
        if (max(u1b)) grad(,3)=2*a(1)*(a(4))*u4/a(3)*u1b;
        grad(,4)=-a(1)*log(u3)*u1;
        // Useless line due to initialisation:
        //if (nterms>4) grad(,5)=0;
        if (nterms==6) grad(,6)=x;
        // try to avoid overflows in lmfit.
        ind1=where(grad>__moffat_gradmax);
        if (numberof(ind1)) {
            grad(ind1)=__moffat_gradmax;
            write,"*** Warning: MOFFAT caught overflows.";
        }
        ind2=where(grad<-__moffat_gradmax);
        if (numberof(ind2)) {
            grad(ind2)=-__moffat_gradmax;
            write,"*** Warning: MOFFAT caught overflows.";
        }
        // try to avoid underflows in lmfit.
        ind3=where(abs(grad)<1/__moffat_gradmax);
        if (numberof(ind3)) {
            grad(ind3)=0;
            //write,"MOFFAT warning: grad underflows caught, grad is inaccurate.";
        }
    }
    return res;

}

func asmoffat1d(x,a2,&grad,deriv=){
/* DOCUMENT asmoffat1d(x,a)

    Returns a (1D) asymmetrical Moffat profile:
    I=I0*(1+((x-x0)/dx)^2)^-b [+ k0 [+ k1*x]]
    
    Where:
    I0=a(1)
    x0=a(2) ; deriv: 2*I0*b*(x-x0)/(dx^2)*(1+((x-x0)/dx)^2)^(-b-1)
    dx for x<x0 =a(3) ; deriv: 2*I0*b*(x-x0)^2/(dx^3)*(1+((x-x0)/dx)^2)^(-b-1)
    b for x<x0 =a(4)
    dx for x>=x0 =a(5) ; deriv: 2*I0*b*(x-x0)^2/(dx^3)*(1+((x-x0)/dx)^2)^(-b-1)
    b for x>=x0 =a(6)

    and if a is of length 7 or 8:
    k0=a(7)
    k1=a(8)
    
    This function can be used directly with lmfit.

   SEE ALSO: moffat1d, moffat1d_fit, asmoffat1d_fit
*/
    a=double(a2);
    extern __moffat_betamax,__moffat_vmax,__moffat_gradmax;
    if (is_void(__moffat_gradmax)) __moffat_gradmax=1e150;
    if (__moffat_betamax) big=__moffat_betamax ;
    else big=1e18; // higher value yield math errors, but a more meaningfull max can be set using __moffat_betamax
    nterms=numberof(a);
    if (__moffat_vmax) {
        if (abs(a(2)>__moffat_vmax)) {
            grad=array(double,numberof(x),nterms);
            return x*0;
        }
    }
    small=1e-80;
    if (a(3)<small) a(3)=small;
    if (a(5)<small) a(5)=small;
    ta=(x<a(2));
    tb=(x>=a(2));
    u2a=(x-a(2))/a(3);
    u2b=(x-a(2))/a(5);
    u4a=u2a^2;
    u4b=u2b^2;
    u3a=1+u4a;
    u3b=1+u4b;
    u3=u3a*ta+u3b*tb;
    if (abs(a(4))>big) {
        u1a=u3a*0;
        u1ab=u1a;
        ind=where(u4a==0);
        if (numberof(ind)) {
            u1a(ind)=1;
            u1ab(ind)=1;
        }
    } else {
        u1a=u3a^-abs(a(4));
        u1ab=u3a^(-abs(a(4))-1);
    }
    if (abs(a(6))>big) {
        u1b=u3b*0;
        u1bb=u1b;
        ind=where(u4b==0);
        if (numberof(ind)) {
            u1b(ind)=1;
            u1bb(ind)=1;
        }
    } else {
        u1b=u3b^-abs(a(6));
        u1bb=u3b^(-abs(a(6))-1);
    }
    u1=u1a*ta+u1b*tb;
    u1B=u1ab*ta+u1bb*tb;
    res=a(1)*u1;
    if (nterms>6) res=res+a(7);
    if (nterms==8) res=res+a(8)*x;
    if (deriv) {
        grad=array(double,numberof(x),nterms);
        grad(,1)=u1;
        if (max(u1B)) grad(,2)=2*a(1)*(a(4)*u2a/a(3)*u1ab*ta+a(6)*u2b/a(5)*u1bb*tb);
        if (max(u1ab)) grad(,3)=2*a(1)*(a(4)*u4a/a(3)*u1ab*ta);
        grad(,4)=-a(1)*log(u3a)*u1a*ta;
        if (max(u1bb)) grad(,5)=2*a(1)*(a(6)*u4b/a(5)*u1bb*tb);
        grad(,6)=-a(1)*log(u3b)*u1b*tb;
        // Useless line due to initialisation:
        //if (nterms>6) grad(,7)=0;
        if (nterms==8) grad(,8)=x;
        // try to avoid overflows in lmfit.
        ind1=where(grad>__moffat_gradmax);
        if (numberof(ind1)) {
            grad(ind1)=__moffat_gradmax;
            write,"MOFFAT warning: grad overflows caught, grad is inaccurate.";
        }
        ind2=where(grad<-__moffat_gradmax);
        if (numberof(ind2)) {
            grad(ind2)=-__moffat_gradmax;
            write,"MOFFAT warning: grad overflows caught, grad is inaccurate.";
        }
        // try to avoid underflows in lmfit.
        ind3=where(abs(grad)<1/__moffat_gradmax);
        if (numberof(ind3)) {
            grad(ind3)=0;
            //write,"MOFFAT warning: grad underflows caught, grad is inaccurate.";
        }
    }
    return res;

}

/*func asmoffat(x,a,&grad,deriv=){
/* DOCUMENT asmoffat(x,a)

    Returns a (1D) asymmetrical Moffat profile:
    I=I0*(1+((x-x0)/dx)^2)^-b [+ k0 [+ k1*x]]
    
    Where:
    I0=a(1)
    x0=a(2) ; deriv: 2*I0*b*(x-x0)/(dx^2)*(1+((x-x0)/dx)^2)^(-b-1)
    dx for x<x0 =a(3) ; deriv: 2*I0*b*(x-x0)^2/(dx^3)*(1+((x-x0)/dx)^2)^(-b-1)
    b for x<x0 =a(4)
    dx for x>=x0 =a(5) ; deriv: 2*I0*b*(x-x0)^2/(dx^3)*(1+((x-x0)/dx)^2)^(-b-1)
    b for x>=x0 =a(6)

    and if a is of length 7 or 8:
    k0=a(7)
    k1=a(8)
    
    This function can be used directly with lmfit.

   SEE ALSO: moffat, moffat_fit, asmoffat_fit

    nterms=numberof(a);
    ta=(x<a(2));
    tb=(x>=a(2));
    //xa=x*ta;
    //xb=x*tb;
    u2a=(x-a(2))/a(3);
    u2b=(x-a(2))/a(5);
    u4a=u2a^2;
    u4b=u2b^2;
    u3a=1+u4a;
    u3b=1+u4b;
    u3=u3a*ta+u3b*tb;
    u1a=u3a^-a(4);
    u1b=u3b^-a(6);
    u1=u1a*ta+u1b*tb;
    res=a(1)*u1;
    if (nterms>6) res=res+a(7);
    if (nterms==8) res=res+a(8)*x;
    if (deriv) {
        grad=array(double,numberof(x),nterms);
        grad(,1)=u1;
        grad(,2)=2*a(1)*(a(4)*u2a/a(3)*u3a^(-a(4)-1)*ta+a(6)*u2b/a(5)*u3b^(-a(6)-1)*tb);
        grad(,3)=2*a(1)*(a(4)*u4a/a(3)*u3a^(-a(4)-1)*ta);
        grad(,4)=-a(1)*log(u3)*u1*ta;
        grad(,5)=2*a(1)*(a(6)*u4b/a(5)*u3b^(-a(6)-1)*tb);
        grad(,6)=-a(1)*log(u3)*u1*tb;
        //if (nterms>6) grad(,7)=0;
        if (nterms==8) grad(,8)=x;
    }
    return res;

    }*/

func moffat1d_fit(y,x,w,guess=,nterms=,itmax=){
/* DOCUMENT asmoffat1d_fit(y,x,w,guess=,nterms=)

    Fits  a  moffat (see  moffat1d)  profile  on a  data  set  using lmfit  (see
    lmfit).  The set  of  data points  Y  is the  only  mandatory argument,  X
    defaults   to   indgen(numberof(y)),   weights   W   are   optional   (see
    lmfit). MOFFAT1D_FIT tries to guess a set of initial parameters, but you can
    (and  should  in every  non-trivial  case)  provide  one using  the  GUESS
    keyword. In  case you don't  provide a guess,  you should set NTERMS  to 4
    (simple  moffat),  5  (adjust  constant  baseline)  or  6  (adjust  linear
    baseline). The returned  fitted parameters have the same  format as GUESS,
    see moffat1d.

   SEE ALSO: moffat1d, asmoffat1d, asmoffat1d_fit
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
        guess(3)=abs(x(ind2)-x(ind1));
        guess(4)=1.;
    } else nterms=numberof(guess);
    a=guess;
    result=lmfit(moffat1d,x,a,y,w,deriv=1,itmax=itmax);
    return a;
}

func asmoffat1d_fit(y,x,w,guess=,nterms=){
/* DOCUMENT asmoffat1d_fit(y,x,w,guess=,nterms=)

    Fits an  assymetrical moffat  (see asmoffat1d) profile  on a data  set using
    lmfit  (see  lmfit). The  set  of  data points  Y  is  the only  mandatory
    argument, X  defaults to indgen(numberof(y)), weights W  are optional (see
    lmfit). ASMOFFAT1D_FIT tries  to guess a set of  initial parameters, but you
    can (and  should in  every non-trivial case)  provide one using  the GUESS
    keyword. In  case you don't  provide a guess,  you should set NTERMS  to 6
    (simple assymmetrical  moffat), 7 (adjust constant baseline)  or 8 (adjust
    linear baseline). The  returned fitted parameters have the  same format as
    GUESS, see asmoffat1d.

   SEE ALSO: asmoffat1d, moffat1d, moffat1d_fit
*/
    require,"Eric/lmfit.i";
    if (is_void(x)) x=indgen(numberof(y));
    if (is_void(guess)) {
        if (is_void(nterms)) nterms=6;
        if (nterms<6) nterms=6;
        if (nterms>8) nterms=8;
        guess=array(double,nterms);
        if (nterms==7) {
            base=median(y);
            guess(7)=base;
        } else if (nterms==8) {
            n=numberof(y);
            y1=median(y(1:long(n/2)));
            x1=median(x(1:long(n/2)));
            y2=median(y(-long(n/2):0));
            x2=median(x(-long(n/2):0));
            guess(8)=(y2-y1)/(x2-x1);
            if (guess(8)!=0) guess(7)=y1-guess(8)*x1;
            base=guess(7)+guess(8)*x;
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
        guess(3)=abs(x(ind2)-x(ind1));
//        guess(3)=2*abs(x(ind0)-x(ind1));
//        if (guess(3)==0) guess(3)=1;
        guess(4)=1.;
        guess(5)=abs(x(ind2)-x(ind1));
//        guess(5)=2*abs(x(ind2)-x(ind0));
//        if (guess(5)==0) guess(5)=1;
        guess(6)=1.;
        guess;
    } else nterms=numberof(guess);
    a=guess;
    result=lmfit(asmoffat1d,x,a,y,w,deriv=1);
    return a;
}

func moffat2d(xy, a, &grad, deriv=) {
/* DOCUMENT moffat2d(xy,a)

    Returns a (2D) Moffat profile:
     I=I0*(1+(X/dx)^2+(Y/dy)^2)^-b

    Where:
     X=(x-x0)*cos(alpha)+(y-y0)*sin(alpha);
     Y=(y-y0)*cos(alpha)-(x-x0)*sin(alpha);
     x=xy(..,1); y=xy(..,2);
     
    Paramater "a" is a vector with 5 or 7 elements:
        a = [I0, x0, y0, dx=dy, b] (then alpha=0)
     or a = [I0, x0, y0, dx, dy, b, alpha].

    This function can be used directly with lmfit and provides
    derivatives. Contrary to the similar functions gauss(), moffat1d()
    and gauss2d(), moffat2d() does not offer the possibility to add a
    linear background. See multiprofile.i for compositing several
    lmfit functions.

    Limitation:  "b"  should  always  be  positive.  In  order  to  force  it,
    especially in fitting routines, its  abolute value is taken (except at some
    point in the computation of derivates).

    astro_util1.i contains two variants of this function: moffat and
    moffatRound. Those two functions do not provide derivatives, take
    alpha in degrees instead of radians, and allow fitting a allow
    fitting a cnstant background, and take a slightly different A
    vector.

   SEE ALSO: moffat1d, gauss2d, moffat, moffatRound
*/
  a=double(a);
  npars=numberof(a);

  I0=a(1);
  x0=a(2);
  y0=a(3);
  dx=a(4);
  dy=(npars>=6?a(5):dx);
  b=(npars>=6?a(6):a(5));
  alpha=(npars>=7?a(7):0.);

  small=1e-80;
  if (abs(dx)<small) dx1=sign(sign)/eps; else dx1=1./dx;
  if (abs(dy)<small) dy1=sign(sign)/eps; else dy1=1./dy;

  X=((deltax=(xy(..,1)-x0))*(cosa=cos(alpha))+
     (deltay=(xy(..,2)-y0))*(sina=sin(alpha)))*dx1;
  Y=(deltay*cosa-deltax*sina)*dy1;
  R2=X^2+Y^2;

  u3=1+R2;
  u1=u3^-abs(b);
  u1b=u3^-abs(b+1);
  mof=I0*u1;
  if (deriv) {
    grad=array(double,dimsof(X),npars);
    grad(..,1)=u1;
    grad(..,2)=(2.*I0*b*dx1)*(X*u1b);
    grad(..,3)=(2.*I0*b*dy1)*(Y*u1b);
    grad(..,4)=grad(..,2)*X;
    if (npars>=6) grad(..,5)=grad(..,3)*Y;
    else grad(..,4)+=grad(..,3)*Y;
    grad(..,-1)=-log(u3)*mof;
    grad(..,0)=(2.*b*I0*(dx*dy1-dy*dx1))*X*Y*u1b;
  }
  return mof;
}
