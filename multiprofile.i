/*
 * multiprofile.i
 *
 * $Id: multiprofile.i,v 1.1 2008-10-29 15:53:38 paumard Exp $
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
 * $Log: multiprofile.i,v $
 * Revision 1.1  2008-10-29 15:53:38  paumard
 * moffat.i, multiprofile.i: initial import
 *
 *
 */

require,"lmfit.i";

local multiprofile;
/* DOCUMENT multiprofile.i

   PURPOSE
    multiprofile.i is a helper library for use with lmfit. It allows
    creating complex user functions by adding up already existing, simpler
    ones.

   RATIONALE
    Remember the lmfit calling sequence:
      result=lmfit(f, x, a, y, w, ...).
    where F is the model function to fit, X the "independent
    variables", which can indeed be anything required by F, A is the
    vector of parameters, and Y the observed values. lmfit finds the
    optimal values for A, in order to minimize the distance between
    F(X,A) and Y.

    In order for lmfit to converge efficiently, it is better if F is
    able to compute its own gradient when called as f(x, a, grad,
    deriv=1). Writing model functions that provide derivatives can be
    tiresome and error-prone. The goal of multiprofile.i is to make
    this process faster and more reliable, by allowing one to build
    complex model functions from pre-existing, well-optimized and
    well-tested primitives with derivatives.

    Of course, multiprofile.i is limited to a particular sub-set of
    all the types of functions one might want to use lmfit on: it
    concentrates on the case where the model function is the sum of
    simpler profiles, either of the same type (produced by the same
    primitive function, like 4 Gaussian), or different from each other
    (e.g. a Gaussian plus a Moffat).

    To do that, the multiprofile model function mp_func(x, a, grad,
    deriv=) accepts as first positional argument X a complex object
    which has to be set-up using the helper function mp_setx. X
    contains in itself a description of the profile: the primitive
    functions to use, the number of instances of each type of
    primitive function, and, naturally, whatever X parameter each
    primitive function requires to function properly. You then call
    lmfit with mp_func as its first argument and this complex X as its
    second argument.

   EXAMPLE
    For instance, assume you want to fit an observed Y, which seems to
    be well described as a sum of 3 Gaussian profiles. The usual
    process would require you to write a new function (e.g. gaussx3)
    specifically for this purpose. gaussx3() would have to compute
    both the sum of 3 Gaussian profiles, and the corresponding
    gradient.

    This is how you can do it with multiprofile.i (assuming x and y
    are already known, and you have found a reasonable first guess a1,
    a2 and a3 for each of the 3 components).

     #include "gauss.i"
     #include "multiprofile.i"
     MultiX=mp_setx(profile=gauss, realX=x, npar=3, ncomp=3);
     a=_(a1, a2, a3);
     result=mpfit(mp_func, MultiX, a, y, deriv=1);

    Now, assume you want to add a linear baseline to the three
    Gaussian profiles (note that linear() is provided by
    multiprofile.i). You have "guessed" as l0 and l1 the two
    corresponding parameters:
   
     linX=mp_setx(profile=linear, realX=x, npar=2);
     MultiX=mp_setx(profile=gauss, realX=x, npar=3, ncomp=3, more=linX);
     a=_(a1, a2, a3, [l0, l1]);
     result=mpfit(mp_func, MultiX, a, y, deriv=1);
   
   FUNCTIONS PROVIDED BY MULTIPROFILE.I
    mp_func: the F parameter to lmfit when using multiprofile.i;
    mp_setx: helper function to set-up the complex model function;
    mp_getx: reverse from mp_setx
    mp_seta: helper function to combine individual first guesses for
             each component into a first guess for the complex model
             function (use GET keyword fro the reverse);
    linear : a*x+b, with lmfit-friendly calling sequence and
             derivatives;
    linear2d: a*x+b*y+c, with lmfit-friendly calling sequence and
             derivatives;
    poly_lmfit  : same as poly() (compute 1D polynomials), with
             lmfit-friendly calling sequence and derivatives.
            
    offsetlines: an lmfit-friendly function for fitting lines on a
             spectrum. It exemplifies advanced usage of multiprofile.
    ol_setx: helper function akin to mp_setx, for use with
             offsetlines.

   SEE ALSO: lmfit, mp_func, mp_setx, mp_getx, mp_seta, linear,
             linear2d, poly_lmfit, gauss, gauss2d, moffat1d, moffat2d,
             offsetlines, ol_setx
    
 */

func mp_func(x,a,&grad,&comps,deriv=,returncomps=){
/* DOCUMENT mp_func(x,a,&grad,deriv=)

    A general  purpose routine to easily create  multicomponents profiles. The
    parameter scheme may  seem odd, but it is intended to  be easily used with
    lmfit. See "multiprofile" for an introduction.

    X: this parameter should be set using MP_SETX (which see). It
       contains both the "real" independent variables and a
       description of the model, split into several components.
    A: vector of parameters. MP_SETA can be used to set it up. If base
       profile needs NPAR parameters, MP_FUNC will transmit
       A(NPAR*I+1:NPAR*(I+1)) to Ith instance of PROFILE. In case some
       parameters must be equal for every components, give their index
       in X (using mp_setx(equal=...)), and simply suppress this
       parameter from the list of parameters for all components except
       the first. For instance, if three components of parameters
       [a1,b1,c1], [a2,b2,c2], and [a3,b3,c3] are to be adjusted with
       the restriction that b1=b2=b3, A is of the form
       A=[a1,b1,c1,a2,c2,a3,c3] and equal=[2] in the call to mp_setx.
    GRAD: upon return, derivatives of output Y relative to each
       parameter in A, if DERIV is set to non void an non null. Can be
       used only if base profile function is able to compute
       derivatives.
    DERIV: whether or not to compute derivatives.
    COMPS: multiprofile can return the individual profiles of each
       component in a 4th positional argument. Set RETURNCOMPS for
       this to happen. COMPS(,C) is the C-th component.
    
   EXAMPLE:

    require,"gauss.i"
    require,"multiprofile.i"
    axis=span(-10,10,101);
    more=mp_setx(profile=linear,npar=2);
    x=mp_setx(profile=gauss,npar=3,ncomp=2,realX=axis,more=more);
    a=[10,-5,2.,7,4,1.5,100,0.5];
    y=mp_func(x,a);
    plg,y,axis;
    
   SEE ALSO: lmfit, multiprofile
*/
  mp_getx, x, profile, realX, npar, ncomp, equal, more;
  y=profile(realX,a(1:npar),gradc,deriv=deriv);
    if (returncomps) {
        comps=array(double,numberof(y),ncomp);
        comps(,1)=y;
    }
    if (deriv) {
        grad=array(y,numberof(a));
        grad(..,1:npar)=gradc;
    }
    if (is_void(equal)){
        for (i=1;i<ncomp;i++) {
            y2=profile(realX,a(i*npar+1:(i+1)*npar),gradc,deriv=deriv);
            y=y+y2;
            if (returncomps) comps(,i+1)=y2;
            if (deriv) grad(,i*npar+1:(i+1)*npar)=gradc;
        }
        next=ncomp*npar+1;
    } else {
        template=array(double,npar);
        template(equal)=1;
        ind=where(!template);
        template(equal)=a(equal);
        np2=npar-numberof(equal);
        for (i=1;i<ncomp;i++) {
            template(ind)=a(npar+(i-1)*np2+1:npar+i*np2);
            y2=profile(realX,template,gradc,deriv=deriv);
            y=y+y2;
            if (returncomps) comps(,i+1)=y2;
            if (deriv) {
                grad(,npar+(i-1)*np2+1:npar+i*np2)=gradc(,ind);
                grad(,equal)+=gradc(,equal);
            }
        }
        next=npar+np2*(ncomp-1)+1;
    }
    if (!is_void(more)) {
      if (is_void(_car(more, 2))) _car, more, 2, realX;
        y+=mp_func(more,a(next:0),gradc,deriv=deriv);
        if (deriv) grad(,next:numberof(a))=gradc;
    }
    return y;
}

func mp_setx (profile=, npar=, ncomp=, realX=, more=, equal=) { 
/* DOCUMENT x=mp_setx(profile=myfunc, npar=npar, ncomp=ncomp,
                      realX=realX, more=more, equal=equal)

    Set x parameter for use with mp_func
    
    PROFILE: function to be used as base profile, same restrictions as
             for LMFIT;
    NPAR:    number of parameters needed by base profile;
    NCOMP:   number of components to use (default: 1);
    realX:   real X parameter to pass to base function PROFILE;
    MORE: optionally, result from a previous call to MP_SETX. Use when
             all the components of the complex profile you are
             building are of the same type (i.e. not the same PROFILE
             function);
    EQUAL: vector containing the indices of the parameters of the base
             profile which should be the same for every components.
             
   SEE ALSO: mp_func, mp_seta, mp_getx, multiprofile
*/
  ncomp=ncomp?ncomp:1;
  return  _lst(profile, realX, npar, ncomp, equal, more);
}

func mp_getx(x, &profile, &realX, &npar, &ncomp, &equal, &more) {
/* DOCUMENT mp_getx, multiX, profile, realX, npar, ncomp, equal, more

    Reverse from mp_setx: get information out of the complex lmfit "X"
    parameter used with mp_func.

   SEE ALSO: mp_func, mp_seta, mp_setx, multiprofile
*/
  profile=_car(x, 1);
  realX  =_car(x, 2);
  npar   =_car(x, 3);
  ncomp  =_car(x, 4);
  equal  =_car(x, 5);
  more   =_car(x, 6);
}

func mp_seta(params,equal=,more=,get=){
/* DOCUMENT multiprofileparams(params,equal=,more=,get=)

   Helps setting parameter A for MP_FUNC. (Note: for simple cases,
   using this function is overkill).

   PARAMS is a 2D array where PARAMS(i,) is the set of parameters for
   the i-th component, and conversely PARAMS(,n) is the vector of
   values of the n-th parameter. EQUAL can be set to a vector of
   indices which should be fitted as equal, and MORE to a set of
   parameters for the supplementary function. When a parameter is set
   in equal, its value is taken from PARAMS(1,).

   If keyword GET is set to a vector, does the contrary, setting
   PARAMS accordingly to GET. However, PARAMS and MORE must have the
   right shape before calling MP_SETA.

   Example: you want to form parameter A of multiprofile for fitting
   Gaussian lines, having initial guesses for three parameters
   AMPLITUDE, VELOCITY, WIDTH, every component having the same width.
   A=mp_seta([AMPLITUDE,VELOCITY,WIDTH],equal=[3])

   On the other hand, if you have an initial guess for each component
   in a1, a2 and a3:
   A=mp_seta(transpose([a1, a2, a3]), equal=[3]);
   
   SEE ALSO: multiprofile, mp_func, lmfit, mp_setx

*/
    sz=dimsof(params);
    ncomp=sz(2);
    npars=sz(3);
    npeq=numberof(equal);
    if (!is_void(get)) a=get;
    else a=array(double,ncomp*npars-(ncomp-1)*npeq+numberof(more));
    peigne=(indgen(ncomp)-1)*(npars-npeq);
    if (ncomp >1 && npeq >0) peigne(2:)+=npeq;
    for (p=1;p<=npars;p++){
        if (noneof(equal==p)) {
            if (!is_void(get)) params(,p)=a(p+peigne);
            else a(p+peigne)=params(,p);
        } else {
            if (!is_void(get)) params(,p)=a(p);
            else a(p)=params(1,p);
            if (ncomp >1) peigne(2:)--;
        }
    }
    if (!is_void(more)) {
        if (!is_void(get)) more=a(1-numberof(more):);
        a(1-numberof(more):)=more;
    }
    return a;
}

// primitives

func linear(x,a,&grad,deriv=) {
/* DOCUMENT linear(x,a)
         or linear(x,a,grad,deriv=1)

    a(1)+x*a(2)

    Returns derivatives if DERIV set to a "true" value. Very
    simplistic, but might come in handy, as it is compatible with
    lmfit (and multiprofile).

   SEE ALSO: linear2d, poly_lmfit, lmfit, multiprofile
*/
    if (deriv) grad=[array(1.,dimsof(x)), x];
    return a(1)+a(2)*x;
}

func linear2d(xy,a,&grad,deriv=) {
/* DOCUMENT linear2d(xy,a)
         or linear2d(xy,a,grad,deriv=1)

    a(1)+x*a(2)+y*a(3) where x=xy(..,1); y=xy(..,2).
 
    Returns derivatives if DERIV set to a "true" value. Very
    simplistic, but might come in handy, as it is compatible with
    lmfit (and multiprofile).

   SEE ALSO: linear, lmfit, multiprofile
*/
  if (deriv) grad=_(array(1.,dimsof(xy(..,1)),1), xy);
  return a(1)+a(2)*xy(..,1)+a(3)*xy(..,2);
}

func poly_lmfit(x,a,&grad,deriv=) {
/* DOCUMENT poly_lmfit(x,a)
         or poly_lmfit(x,a,grad,deriv=1)

    Returns the polynomial sum(a(n)*x^(n-1)), with derivatives in GRAD
    if DERIV set to a "true" value.  Very simplistic, but might come
    in handy, as it is compatible with lmfit (and multiprofile).

   SEE ALSO: poly, linear, lmfit, multiprofile
*/
    degp1=numberof(a);
    if (deriv){
        grad=array(1.,dimsof(x),degp1);
        //grad(,1)=0; //useless
        grad(..,1)=1;
        if (degp1>=2) grad(..,2)=x;
        for (n=2;n<degp1;n++) grad(..,n+1)=x^n;
    }
    if (degp1==1) return array(a(1), dimsof(x));
    y=a(1)+a(2)*x;
    for (n=3;n<=degp1;n++) y+=a(n)*x^(n-1);
    return y;
}

// Fit doppler-shifted lines over a spectrum

func ol_setx(profile=, realX=, lines=, positivity=, intensities=, fixedratio=) {
/* DOCUMENT X=ol_setx(profile=profile, realX=, lines=, positivity=
     
    Set up X parameter for offsetlines().

    profile: model function for individual line (default: moffat1d);
    realX  : independent variables used by PROFILE;
    lines  : vector containing the list of lines;
    positivity : for each line, 1 if the line should be forced a
             positive amplitude (emission line), -1 for a negative
             amplitude (absorption line).
    intensities : relative intensities of the lines.

    The intensities of the lines can be unconstrained (default),
    constrained to be emission lines or absorption lines (POSITIVITY
    set and either INTENSITIES not set or FIXEDRATIO set to 0), or
    constrained to have predefined relative intensities (INTENSITIES
    set and FIXEDRATIO not set or set to 1). If FIXEDRATIO is set to 1
    and INTENSITIES is not set, it defaults to array(1.,
    numberof(LINES)).

   SEE ALSO: offsetlines
*/
  if (is_void(profile)) {
    require, "moffat.i";
    profile=moffat1d;
  }
  if (is_void(fixedratio)) fixedratio=!is_void(intensities);
  if (fixedratio & is_void(intensities)) intensities=array(1., dimsof(lines));
  return _lst(profile, realX, lines, positivity, intensities, fixedratio);
}

func offsetlines(x,a,&grad,&comps,deriv=,returncomps=){
/* DOCUMENT offsetlines(x,a)

    PURPOSE
     Fit several lines of identical shape over a spectrum, sharing a
     common displacement (for instance Doppler shift, if the
     wavelength range is short enough).

    DESCRIPTION
     This function is suitable for call by lmfit. It returns a complex
     profile made of the sum of several lines (Moffat profiles, by
     default), which are moved only together (their relative distances
     remain unchanged). As long as the primitive profile is able to
     return derivatives, offsetlines does, too.

    PARAMETERS
     See ol_setx to better understand the parameters below.
     X: the result of a call to ol_setx, which see. X contains
        information on the lines to fit and the type of profile to use
        (Moffat by default). X also contains the wavelengths or
        frequencies.
     A: the vector of parameters to fit. If ol_setx() has been called
        with INTENSITIES set and FIXEDRATIO either not set or set to
        1, A(1) is the multiplicative coefficient by which to multiply
        each of these individual relative intensities. In all other
        cases, the first numberof(lines) elements of A are the
        individual intensities of the various lines. The remaining
        parameters are always common to all the lines: the offset
        relative to the rest position set with the LINES keyword of
        ol_setx, and then the other parameters for the PROFILE set
        using ol_setx. By default, the PROFILE==moffat1d, and requires
        two parameters for the line shape (line width and beta; see
        moffat1d()).

     EXAMPLE

      // Basic set-up
      x = span(2.0, 2.4, 200);        // set up wavelength (or
                                      // frequency) vector
                               
      lines=[2.058, 2.15, 2.16, 2.3]; // give rest wavelength or
                                      // frequency of each line

      // Prepare spectrum
      olx=ol_setx(realX=x, lines=lines);
      A=[   1, 0.5,   0.6, 1.2,       // individual intensities
         0.02, 0.005, 1.1];           // displacement, width, beta
      y=offsetlines(olx, A);
      plg, y, x;

      // Fit with free intensities
      y_obs= y+0.2*random_n(dimsof(y));
      res=lmfit(offsetlines, olx, A, y_obs,deriv=1);
      fma; plg, y_obs, x;
      plg, offsetlines(olx, A), x, color="red";
      
      // Prepare spectrum, setting INTENSITIES in ol_setx
      olx=ol_setx(realX=x, lines=lines, intensities=[1., 0.5, 0.6, 1.2]);
      A=[1., 0.02, 0.005, 1.1];
      y=offsetlines(olx, A);
      fma; plg, y, x;

      // Fit with tied intensities
      y_obs= y+0.2*random_n(dimsof(y));
      res=lmfit(offsetlines, olx, A, y_obs, deriv=1);
      fma; plg, y_obs, x;
      plg, offsetlines(olx, A), x, color="red";
      
      
     SEE ALSO: ol_setx, lmfit, multiprofile, moffat1d.
*/
  profile     = _car(x, 1); 
  realX       = _car(x, 2);
  lines       = _car(x, 3);
  positivity  = _car(x, 4);
  intensities = _car(x, 5);
  fixedratio  = _car(x, 6);
  
  nlines=numberof(lines);
  npars=numberof(a);
  if (!fixedratio) npars-=nlines-1;
  pars=array(double,nlines,npars);

  if (fixedratio) {
    pars(,1)=intensities;
    pars(,2)=lines+a(2);
    pars(,3:)=a(-,3:);
  } else {
    pars(,1)=a(1:nlines);
    if (!is_void(positivity)) {
      ind=where(positivity==-1);
      if (numberof(ind)) pars(ind,1)=-abs(pars(ind,1));
      ind=where(positivity==1);
      if (numberof(ind)) pars(ind,1)=abs(pars(ind,1));
    }
    pars(,2)=lines+a(nlines+1);
    pars(,3:)=a(-,nlines+2:);
  }

  a2=mp_seta(pars);
  X=mp_setx(npar=npars,ncomp=nlines,realX=realX,profile=profile);
  sp=mp_func(X,a2, grad2, deriv=deriv);
  
  if (deriv) {
    peigne=(indgen(nlines)-1)*npars;
    grad=array(double, dimsof(sp), numberof(a));
    if (fixedratio) grad(..,1)=sp;
    else grad(..,1:nlines)=grad2(..,1+peigne);
    offset=fixedratio?0:nlines-1;
    for (i=2; i<=npars;i++) grad(..,i+offset)=grad2(..,peigne+i)(..,sum);
  }
  
  if (fixedratio) sp*=a(1);
  return sp;
}
