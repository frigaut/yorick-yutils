/* A collection of routines for processing of astronomical data
 *
 *
 * Author: Francois Rigaut
 * Written 2003
 * last revision/addition: 2007
 *
 *  Main functions:
 *  ---------------
 *  autocuts(image,&sigma,p=)
 *  sky(image,&sigma,n=)
 *  ct2lst(lng,tz,jd)
 *  jdcnv(yr,mn,day,hr)
 *  altaz(ha, dec, lat, &alt, &az)
 *  airmass(sza)
 *  sigmaFilter(image,nsigma,iter=,silent=)
 *  deadpix(image,bpm,silent=)
 *  makeflat(biasfile,flatfiles)
 *  makebias(biasfiles)
 *  check_fwhmfit(nil)
 *  gaussianRound(x,a)
 *  gaussian(x,a)
 *  moffatRound(x,a)
 *  moffat(x,a)
 *  starsep(im,p,pixsize=,disp=,boxsize=,nwindow=)
 *  fwhmfit(bim,boxsize=,saturation=,pixsize=,funtype=,...
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


//---------------------------------------------------------

func autocuts(image,&sigma,p=)
/* DOCUMENT autocuts(image,&sigma,p=)
 * Often, the interesting information in astronomical images is burried
 * in intensity levels that will not appear if you do a simple "pli,image".
 * "autocuts" does a quick estimation of the cut levels needed to
 * display a fraction "p" of the pixel intensity distribution.
 * It then filters the image within these limits (using clip) and
 * returns it.
 * SEE ALSO: clip, sky
 */
{
  if (is_void(p)) {p = 0.99;}
  if ( (p < 0.) | (p > 1.) ) {
    error,"This value of p does not make sense. 0<p<1 !";
  }
  npixmax = 8192;

  if (numberof(image) <= npixmax) {
    subset = image(*);
  } else {
    subset = image(long(1+random(npixmax)*(numberof(image)-1.)));
  }

  subset= subset(sort(subset));
  npt   = numberof(subset);
  mini  = subset(round(clip(npt*(1.-p),1,npt)));
  maxi  = subset(round(npt*p));
  return clip(image,mini,maxi);
}

//---------------------------------------------------------

func sky(image,&sigma,n=)
{
  /* DOCUMENT sky(image,&sigma)
     returns the mode and the standard deviation of the sky in an
     image
     SEE ALSO: autocuts
  */
  ns  = 3.0;
  nit = 5;
  ns  = 2.;
  nit = 4;
  npixmax = 8192;
  if (!is_void(n)) {npixmax=n;}
  npix = numberof(image);

  if (npix <= npixmax) {
    subset = image(*);
  } else {
    subset = image(long(clip(random(npixmax)*npix,1,npix)));
  }

  bg     = median(subset);
  gind   = indgen(numberof(subset));
  
  for (i=1;i<=nit;i++) {
    sigma  = subset(gind)(rms);
    if (sigma==0.) return bg;
    //    print,bg,sigma;
    gind   = where( abs(subset-bg) < (ns*sigma) );
    bg     = avg(subset(gind));
  }
  return bg;
}

//++++++++++++++++++++++++++++++++++++++++++

func ct2lst(lng,tz,jd)
{
  /* DOCUMENT func ct2lst(lng,tz,julian day)
     PURPOSE:
     To convert from Local Civil Time to Local Mean Sidereal Time.

     CALLING SEQUENCE:
     CT2LST(Lng, dummy, JD)

     INPUTS:
     Lng  - The longitude in degrees (east of Greenwich) of the place for 
     which the local sidereal time is desired, scalar.   The Greenwich 
     mean sidereal time (GMST) can be found by setting Lng = 0.
     Tz  - The time zone of the site in hours.  Use this to easily account 
     for Daylight Savings time (e.g. 4=EDT, 5 = EST/CDT), scalar
     This parameter is not needed (and ignored) if Julian date is 
     supplied.
     JD  -  Julian date of time in question, scalar or vector
     use jdcnv to get the Julian date from the year, month and day

     OUTPUTS:
     Lst   The Local Sidereal Time for the date/time specified in hours.

     PROCEDURE:
     The Julian date of the day and time is question is used to determine
     the number of days to have passed since 0 Jan 2000.  This is used
     in conjunction with the GST of that date to extrapolate to the current
     GST; this is then used to get the LST.    See Astronomical Algorithms
     by Jean Meeus, p. 84 (Eq. 11-4) for the constants used.

     MODIFICATION HISTORY:
     Adapted from the FORTRAN program GETSD by Michael R. Greason, STX, 
     27 October 1988.
     Use IAU 1984 constants Wayne Landsman, HSTX, April 1995, results 
     differ by about 0.1 seconds  
     Converted to IDL V5.0   W. Landsman   September 1997
     Longitudes measured *east* of Greenwich   W. Landsman    December 1998
     Converted to Yorick, 2003jan31, F.Rigaut. Restricted to Julian date input
    
     SEE ALSO:
  */

  //                            Useful constants, see Meeus, p.84

  c = [280.46061837, 360.98564736629, 0.000387933, 38710000.0 ];
  jd2000 = 2451545.0;
  t0 = jd - jd2000;
  t = t0/36525;

  //                            Compute GST in seconds.

  theta = c(1) + (c(2) * t0) + t^2*(c(3) - t/ c(4) );

  //                            Compute LST in hours.

  lst = ( theta + double(lng))/15.0;

  lst = lst % 24.;
  lst = lst + 24*(lst < 0.);

  return lst;
}

//++++++++++++++++++++++++++++++++++++++++++
func jdcnv(yr,mn,day,hr)
{
  /* DOCUMENT 
     NAME:
     JDCNV
     PURPOSE:
     Converts Gregorian dates to Julian days   
   
     CALLING SEQUENCE:
     JDCNV, YR, MN, DAY, HR
   
     INPUTS:
     YR = Year (integer)  
     MN = Month (integer 1-12)
     DAY = Day  (integer 1-31) 
     HR  = Hours and fractions of hours of universal time (U.T.)
      	
     OUTPUTS:
     JULIAN = Julian date (double precision) 
   
     EXAMPLE:
     To find the Julian Date at 1978 January 1, 0h (U.T.)
   
     JDCNV, 1978, 1, 1, 0., JULIAN

     will give JULIAN = 2443509.5
     NOTES:
     (1) JDCNV will accept vector arguments 
     (2) JULDATE is an alternate procedure to perform the same function

     REVISON HISTORY:
     Converted to IDL from Don Yeomans Comet Ephemeris Generator,
     B. Pfarr, STX, 6/15/88
     Converted to IDL V5.0   W. Landsman   September 1997
     Converted to Yorick F.Rigaut, 2003jan31
   
     SEE ALSO:
  */

  // if N_params() LT 5 then begin
  //	print,'Syntax -  JDCNV, yr, mn, day, hr, julian   
  //	print,'   yr - Input Year (e.g. 1978), scalar or vector
  //	print,'   mn - Input Month (1-12), scalar or vector
  //	print,'   day - Input Day (1-31), scalar or vector
  //	print,'   hr - Input Hour (0-24), scalar or vector
  //	print,'   julian - output Julian date'
  //        return
  // endif

  yr= long(yr) ; mn= long(mn) ; day= long(day);	// Make sure integral
  l= (mn-14)/12; // In leap years, -1 for Jan, Feb, else 0
  julian= day-32075+1461*(yr+4800+l)/4 + 367*(mn-2-l*12)/12 - 3*((yr+4900+l)/100)/4;
  julian= double(julian) + (hr/24.0) - 0.5;

  return julian;
}

//++++++++++++++++++++++++++++++++++++++++++
func altaz(ha, dec, lat, &alt, &az)
{
  // Convert from Deg to radians
  rpd = pi/180.;
  h = ha*rpd  ;  d = dec*rpd  ;  l=lat*rpd;
  sh = sin(h)  ;  ch = cos(h);
  sd = sin(d)  ;  cd = cos(d);
  sl = sin(l)  ;  cl = cos(l);

  // Calculate Alt and Az
  arg1 = -cd*sh;
  arg2 =  sd*cl - cd*ch*sl;
  arg3 =  sd*sl + cd*ch*cl;

  alt = asin(arg3);
  az  = atan(arg1,arg2); 

  // Convert back to degrees
  alt = alt/rpd;
  az  = az/rpd;
  az = az+360*(az < 0.);
}

//++++++++++++++++++++++++++++++++++++++++++
func airmass(sza)
{
  // ripped off from a routine written by Paul Ricchiazzi
  // sza = zenith angle (0. at zenith) in degrees

  dtor= pi/180;
  return 1./(cos((clip(sza,,90.))*dtor)+0.15*exp(-1.253*log(93.885-(sza<90.))));
}
//++++++++++++++++++++++++++++++++++++++++++

func sigmaFilter(image,nsigma,iter=,silent=)
/* DOCUMENT func sigma_filter(image,nsigma,iter=,silent=)
   Filter out the pixels that deviate from the local statistics.
   The mean and rms of the 8 (the minimum and maximum of these
   8 neighbors are excluded in the mean and rms computation) is
   computed. All pixels that deviates more than "nsigma" rms
   from the mean are flagged as bad pixels. The image and newly
   created bad pixel map are passed to the routine "deadpix"
   for correction. The processus can be iterated.
   image	: input image
   nsigma: number of rms about the local mean out of which is
   pixel is considered aberrant. nsigma >= 5 recommended.
   iter  : Keyword, number of iterations. Recommended value : 3-5
   silent: No verbose
   F.Rigaut 2001/10
   SEE ALSO: deadpix
*/

{
  local im;
  radbox= 1;
  if (iter == []) {iter = 0;}
  s	= dimsof(image); sx = s(2) ; sy = s(3) ;
  im	= reform(image,sx*sy);
  dx	= span(-radbox,radbox,2*radbox+1)(,-:1:2*radbox+1);
  dy	= transpose(dx)*sx;
  d	= dx+dy;
  d	= reform(d,(2*radbox+1)^2);
  d	= d(where(d != 0));
  nd	= numberof(d);
 restart:
  arim	= array(float,sx*sy,nd);
  ind	= indgen(sx*sy);
  for (i=1;i<=nd;i++) 
    {
      vind = long(clip(ind+d(i),1,sx*sy));
      arim(,i) = im(vind);
    }
  arim	= arim(sort(arim,2));
  arim	= arim(,2:-1);
  av	= arim(,avg);
  st	= arim(,rms);
  ind	= where( abs(im-av) > nsigma*st );
  bpm 	= long(im)*0 ; 
  im	= reform(im,sx,sy);
  if (is_array(ind)) 
    { 
      bpm(ind) = 1;
      bpm	= reform(bpm,sx,sy);
      im	= deadpix(im,bpm,silent=silent);
      if (iter > 0) {iter = iter-1; im = reform(im,sx*sy); goto restart;}
      return im;
    }
  return im;
}

//++++++++++++++++++++++++++++++++++

func deadpix(image,bpm,silent=)
/* DOCUMENT func deadpix(image,bad pixel map,silent=)
   Correction of bad pixels in an image by averaging the (good) neighbors.
   image	= 2D array
   bpm   = 2D array
   bad pixel map has the same dimension as image, and is 1 at the 
   location of a bad pixel
   F.Rigaut 2001/10
   SEE ALSO: sigma_filter
*/

{
  local image,im;
  im	= image*(1-bpm);
  s	= dimsof(image);
  sx	= s(2); sy = s(3);
  while (sum(bpm) != 0) 
    {
      bind	= where(bpm);
      if (!is_set(silent)) {write,format="%i bad pixels to process\n",numberof(bind);}
      ind	= array(long,numberof(bind),8);
      ind(,1) = bind-1;
      ind(,2) = bind+1;
      ind(,3) = bind+sx;
      ind(,4) = bind-sx;
      ind(,5) = bind+sx-1;
      ind(,6) = bind+sx+1;
      ind(,7) = bind-sx-1;
      ind(,8) = bind-sx+1;
      // "ind" contains the indices of the neighbor pixels from bad pixels
      ind	= long(clip(ind,1,sx*sy));
      // ind is clipped to avoid outbound errors. The edge should
      // be mostly ok. might be a few special cases where it is not perfect.
      gpm	= 1-bpm;
      // gpm = good pixel map
      hmg	= (gpm(ind))(,sum);
      // hmg = "how many good" = how many of the neighbors are good pixels ?
      // this is a vector with as many elements as bad pixels.
      avv	= (im(ind))(,sum)/clip(float(hmg),0.5,);
      // avv is a vector that contains the average of the good neighbors
      wok	= where(hmg >= 3);
      // wok = list of indices in bind for which there 
      // is at least 3 good neighbors
      im(bind(wok)) = avv(wok);  // replace bad pixels
      bpm(bind(wok)) = 0;  // update bad pixel map
    }
  return im;
}

//+++++++++++++++++++++++++++

func makeflat(biasfile,flatfiles)
/* DOCUMENT function makeflat(biasfile,flatfiles)
   Build flat field from a biasfile (single file name) and
   a serie of flat fields (string containing the file names).
   Does NOT save the resulting flat.
   F.Rigaut, 2001/11/10.
   SEE ALSO: makebias.
*/
  
{
  print,"Reading arrays, assuming Unsigned Integers";
  bias = float(fits_read(biasfile));
  dx = (dimsof(bias))(2);
  dy = (dimsof(bias))(3);
  cube = array(float,dx,dy,numberof(flatfiles));
  for (i=1 ; i<=numberof(flatfiles) ; i++) {
    cube(,,i) = float(uint(fits_read(flatfiles(i))))-bias;
    mx = avg(median(cube(,,i)));
    cube(,,i) = cube(,,i)/mx;
  }
  print,"Computing Median of cube";
  flat = median(cube,3);
  flat = float(flat)/median(median(flat));  // to reduce cpu time req
  return flat;  
}

//+++++++++++++++++++++++++++

func makebias(biasfiles)
/* DOCUMENT function makebias(biasfiles)
   Build bias image from a serie of biasfiles (string containing 
   the file names). Does NOT save the resulting bias.
   F.Rigaut, 2001/11/10.
   SEE ALSO: makeflat.
*/

{
  print,"Reading arrays, assuming Unsigned Integers";
  im = uint(fits_read(biasfiles(0)));
  dx = (dimsof(im))(2);
  dy = (dimsof(im))(3);
  cube = array(int,dx,dy,numberof(biasfiles));
  cube(,,1) = im;
  for (i=2 ; i<=numberof(biasfiles) ; i++) {
    cube(,,i) = uint(fits_read(biasfiles(i)));}
  print,"Computing median from cube"
    bias = median(cube,3);
  return bias;
}

//-----------------------------------------
struct fwhmfitres { double xpos, xposerr, ypos, yposerr, xfwhm, xfwhmerr, yfwhm, yfwhmerr, flux, fluxerr, sky, skyerr, ell, ellerr, angle, peak;};

fwhmfit_version = "1.4";
fwhmfit_modifDate = "September 12, 2005";

require, "random.i";
require, "string.i";
require, "random_et.i";
require, "lmfit.i";

func check_fwhmfit(nil)
{
  im = poidev(makegaussian(256,5)*1000);
  im += gaussdev(dimsof(im))*10.;
  ima = im;
  for (i=1;i<=20;i++) {
    ima += roll(im,long((random(2)-0.5)*256))*random();
  }
  fwhmfit,ima,funtype="gaussian";
  return ima;
}

func gaussianRound(x,a)
{
  // a = [sky,Total flux,Xcent,Ycent,~fwhm]
  xp		= x(,,1)-a(3);
  yp		= x(,,2)-a(4);
  z			= exp(-((xp/a(5))^2.+(yp/a(5))^2.));
  s     = sum(z);
  if (s==0) return a(1); 
  z			= a(1)+a(2)*z/s;
  return z;
}

func gaussian(x,a)
{
  // a = [sky,Total flux,Xcent,Ycent,~Xfwhm,~Yfwhm,angle]
  a(7) = a(7)%360.;
  alpha = a(7)/180.*pi;
  xp		= (x(,,1)-a(3))*cos(alpha)-(x(,,2)-a(4))*sin(alpha);
  yp		= (x(,,1)-a(3))*sin(alpha)+(x(,,2)-a(4))*cos(alpha);
  r			= sqrt(xp^2.+yp^2.);
  z			= exp(-((xp/a(5))^2.+(yp/a(6))^2.));
  s     = sum(z);
  if (s==0) return a(1); 
  z			= a(1)+a(2)*z/s;
  return z;
}


func moffatRound(x,a)
{
  // a=[sky,total,xc,yc,a,coefpow]
  a1 = a(5);
  xp = x(,,1)-a(3);
  yp = x(,,2)-a(4);
  z = (1. + ((xp/a1)^2.+(yp/a1)^2.))^(-a(6));
  s     = sum(z);
  if (s==0) return a(1); 
  z = a(1)+a(2)*z/s;
  return z;
}

func moffat(x,a)
{
  // a=[sky,total,xc,yc,a,b,angle,coefpow]
  a(7) = a(7)%360.;
  alpha = a(7)/180.*pi;
  a1	= a(5);
  a2	= a(6);
  xp = (x(,,1)-a(3))*cos(alpha)-(x(,,2)-a(4))*sin(alpha);
  yp = (x(,,1)-a(3))*sin(alpha)+(x(,,2)-a(4))*cos(alpha);
  z = (1. + ((xp/a1)^2.+(yp/a2)^2.))^(-a(8));
  s     = sum(z);
  if (s==0) return a(1); 
  z = a(1)+a(2)*z/s;
  return z;
}

func starsep(im,p,pixsize=,disp=,boxsize=,nwindow=)
/* DOCUMENT starsep(image,type,pixsize=,disp=,boxsize=,nwindow=)
   Use this function to interactively determine the separation of 2
   objects in a stellar image.

   Type:
   > starsep,image,0
   and click on a star.
   This star will be the (x,y) zero point for further measurements

   Then type
   > starsep,image,1
   and click on another star.
   This will print the (X,Y) separation between this new object and
   the reference.

   Calling this function as a function does not print anything but
   returns the triplet (xsep,ysep,separation)

   Use pixsize=some_value to get the separation in arcsec.
   Use disp=1 to get the default behavior of fwhmfit (set up the
     windows and display the image)
   Use disp=2 to just set up the small fit/residual window)

   boxsize,nwindow: see fwhmfit manpage
   
   SEE ALSO: fwhmfit
 */
{
  extern _starsepxref,_starsepyref;

  if (is_void(disp)) disp=0;
  
  if (p==0) {
    r = fwhmfit(im,oneshot=1,disp=disp,silent=1,boxsize=boxsize,nwindow=nwindow);
    _starsepxref=r.xpos(0);
    _starsepyref=r.ypos(0);
  } else {
    if (is_void(_starsepxref)) error,"The zero point was not defined";
    r = fwhmfit(im,oneshot=1,disp=disp,silent=1,boxsize=boxsize,nwindow=nwindow);
    v = _(r.xpos(0)-_starsepxref,r.ypos(0)-_starsepyref);
    v = _(v,abs(v(1),v(2)));
    if (pixsize) v*=pixsize;
    if (am_subroutine()) {
      if (pixsize) {
        write,format="Separation (arcsec): x=%.2f; y=%.2f; Distance=%.2f\n",
          v(1),v(2),v(3);
      } else {
        write,format="Separation (pixels): x=%.2f; y=%.2f; Distance=%.2f\n",
          v(1),v(2),v(3);
      }
    } else return v;
  }
}

func fwhmfit(bim,boxsize=,saturation=,pixsize=,funtype=,\
             magswitch=,nwindow=,silent=,airmass=,disp=,oneshot=,dpi=)
/* DOCUMENT func fwhmfit(image,boxsize=,saturation=,pixsize=,funtype=,magswitch=,
                         nwindow=,silent=,airmass=,disp=,oneshot=,dpi=)
   image      = 2D image
   boxsize    = Specify the size of the box of sub-images
                (usually 4-10 times the fwhm)
   saturation = Saturation value (prevents picking saturated stars)
   pixsize    = Specify the image pixel size
   funtype    = function to use for fit (gaussian,special,moffat)
   magswitch  =  Output flux in magnitude (zp=25 is used)
   nwindow    = Number of window for UI (default 2)
   silent     = don't display the numbers on screen
   airmass    = airmass. Outputs airmass corrected FWHM values
   disp       = 0: no display at all
                1: normal behavior (set up window + displays)
                2: display only the fit & residual
   oneshot    = if set, exits after processing the first object
   dpi        = dpi of the created window (disp has to be = 1)
*/
{
  if (!is_set(boxsize)) boxsize = 40;
  if (!is_set(saturation)) saturation = 0.;
  if (!is_set(pixsize)) {pixsize = 1.;} else {pixset=1;};
  if (!is_set(funtype)) {funtype = "moffat";} else {funcset=1;};
  if (!is_set(magswitch)) magswitch=0;
  if (!is_set(airmass)) airmass=1.;
  if (!is_set(nwindow)) nwindow=2;
  if (is_void(disp)) disp=1;
  if (is_void(dpi)) dpi=75;

  if ((funtype=="gaussian")&&(!silent)) {write,"Using Gaussian fit";}
  //	if (funtype == "special") {write,"Using Special fit";}
  if ((funtype=="moffat")&&(!silent)) {write,"Using Moffat fit";}

  b	= boxsize/2;
  pow	= 0.85;
  zp	= 25.;
  f	= array(float,2,1);
  ferr	= array(float,2,1);
  el	= 0.;
  eler	= 0.;
  an	= 0.;
  airmass = double(airmass);
  allres = [];

  dims = (dimsof(bim))(2:3);

  sky1	= sky(bim);
  bim	= bim-sky1;
  if (saturation != 0.) {saturation -= sky1;}

  if (disp) {
    if (nwindow==1) {
      if (disp==2) {
        get_style, landscape, systems, legends, clegends;
        if (numberof(systems)!=2) {
          write,"WARNING: disp=2 and nwindow=1 but set up not done, doing it.";
          disp=1;
        }
      }
      if (disp!=2) {
        winkill,0;
        window,0,width=long(500.*dpi/75),height=long(620.*dpi/75),
          style="yfwhm.gs",wait=1,dpi=dpi;
      } else {
        plsys,2;
        pli,array('\xff',[3,3,3*boxsize,boxsize]);
        limits,-1,3*boxsize,-1,boxsize,square=1;
        plsys,1;
      }
    } else {	// then 2 windows
      if (disp!=2) window,0,style="boxed.gs",wait=1;
      window,1,style="nobox.gs",width=450,height=150,wait=1;
      window,0;
    }

    if (disp!=2) {
      fma;
      pli,cpc(bim);
      myxytitles,"pixels","pixels",[0.02,0.02];
      limits,square=1;
      plt,swrite(format="fwhmfit.i, yorick FWHM fitting routine version %s, F.Rigaut, %s.",
                 fwhmfit_version,fwhmfit_modifDate),0.1,0.25,tosys=0,orient=1,height=12;
    }
  }

  if (!silent) {
    write,"Left click on star for FWHM. Right click to exit.";
    write,"Middle click to remove last entry.";
    if (pixset) {
      if (!magswitch) {
        write,"X[pix]  Y[pix]    X FWHM[\"]    Y FWHM[\"]  FLUX[ADU] ELLIP  ANGLE    MAX";
      } else          {
        write,"X[pix]  Y[pix]    X FWHM[\"]    Y FWHM[\"]  MAGNITUDE ELLIP  ANGLE    MAX";
      }
    } else {
      if (!magswitch) {
        write,"X[pix]  Y[pix]  X FWHM[pix]  Y FWHM[pix]  FLUX[ADU] ELLIP  ANGLE    MAX";
      } else          {
        write,"X[pix]  Y[pix]  X FWHM[pix]  Y FWHM[pix]  MAGNITUDE ELLIP  ANGLE    MAX";
      }
    }
  }

  do {
    res	= mouse(1,0,"");
    c = long(res(1:2));
    but	 = res(10);
    if (but == 3) break;
    if (but == 2) {
      if (numberof(el) == 1) {
        write,"You can only unbuffer after having buffered at least one star!";
        continue;
      }
      f = f(,:-1);
      ferr = ferr(,:-1);
      el = el(:-1);
      eler = eler(:-1);
      an = an(:-1);
      write,"Last measurement taken out of star list";
      continue;
    }

    i1 = clip(c(1)-b,1,);
    i2 = clip(c(1)+b,,dims(1));
    j1 = clip(c(2)-b,1,);
    j2 = clip(c(2)+b,,dims(1));

    im	 = smooth(bim(i1:i2,j1:j2),2);
    wm	 = where2(im == max(im))(*)(1:2)-b-1;
    c	 = c + wm;
    im	 = bim(i1:i2,j1:j2);
    pos	 = c(1:2)-b;
    pos	 = [i1,j1]-1;
    im	 = sigmaFilter(im,5,iter=2,silent=1);
    if ((saturation > 0) && (max(im) > saturation)) {
      write,"Some pixels > specified saturation level. Aborting !";
      continue;
    }
    sky2 = sky(im,dev2);
    im	 = im - sky2;
    d	 = dimsof(im);

    w	 = 1.+0.*clip(im,dev2,)^2;

    x	 = indices(d);

    if (funtype == "gaussian") {

      // a = [sky,Total flux,Xcent,Ycent,fwhm_parameter]
      ai		= [0,sum(im-median(im(*))),d(2)/2.,d(3)/2.,5.];
      r			= lmfit(gaussianRound,x,ai,im,w,tol=1e-5,itmax=50,eps=0.01);
      // a = [sky,Total flux,Xcent,Ycent,a,b,angle]
      a			= [ai(1),ai(2),ai(3),ai(4),ai(5),ai(5),10.];
      r			= lmfit(gaussian,x,a,im,w,stdev=1,tol=1e-8,itmax=50,eps=0.01);
      tmp		= gaussian(x,a);
      err		= *r.stdev;
      pos			= pos + a(3:4);
      a(5:6)	= abs(a(5:6));
      if (a(5)<a(6)) { //fwhmY > fwhmX, swap
        a(5:6) = a(5:6)(::-1);
        err(5:6) = err(5:6)(::-1);
        a(7) +=90;
      }
      angle = ((a(7)+90) % 180.) ; //a(7) relative to Y
      if (angle < 0) { angle = angle+180.; }
      fwhm	=	 a(5:6)*2*(-log(0.5))^(1./2.)*pixsize; //gaussian
      fwhmerr = err(5:6)*2*(-log(0.5))^(1./2.)*pixsize;
      fwhm	= fwhm/airmass^0.6; fwhmerr = fwhmerr/airmass^0.6;
      ellip = abs(fwhm(2)-fwhm(1))/avg(fwhm);
      ellerr= 2*(fwhmerr(1)+fwhmerr(2))*(2*fwhm(2))/(fwhm(1)+fwhm(2))^2.;

    } else if (funtype == "moffat") {

      // a		= [sky,total,xc,yc,fwhm_parameter,beta]
      ai			= [0,sum(im-median(im(*))),d(2)/2.,d(3)/2.,5.,1.];
      r				= lmfit(moffatRound,x,ai,im,w,tol=1e-5,itmax=50,eps=0.01);
      // a		= [sky,total,xc,yc,a,b,angle,beta]
      a				= [ai(1),ai(2),ai(3),ai(4),ai(5),ai(5),0.,ai(6)];
      r				= lmfit(moffat,x,a,im,w,stdev=1,tol=2e-8,itmax=50,eps=0.01);
      tmp			= moffat(x,a);
      err			= *r.stdev;
      pos			= pos + a(3:4);
      a(5:6)	= abs(a(5:6));
      if (a(5)<a(6)) { //fwhmY > fwhmX, swap
        a(5:6) = a(5:6)(::-1);
        err(5:6) = err(5:6)(::-1);
        a(7) +=90;
      }
      angle		= ((a(7)+90) % 180.) ; //a(7) relative to Y
      if (angle < 0) { angle = angle+180.; }

      if (a(8)==0) {
        fhwm=fwhmerr=ellip=ellerr=0.;
        continue;
      }
      fwhm		=	 2*a(5:6)*sqrt(0.5^(-1./a(8))-1.)*pixsize; // moffat
      fwhmerr = fwhm*(err(5:6)/a(5:6)+
                      0.5*abs(log(0.5))*err(8)/a(8)^2.*0.5^(1./a(8))/(0.5^(1./a(8))-1.));
      fwhm = fwhm/airmass^0.6; fwhmerr = fwhmerr/airmass^0.6;
      ellip		= (fwhm(1)-fwhm(2))/avg(fwhm);
      ellerr	= 2*(fwhmerr(1)+fwhmerr(2))*(2*fwhm(2))/(fwhm(1)+fwhm(2))^2.;

    }

    maxim = max(tmp);
    if (disp) {
      if (nwindow==2) {
        window,1;
        tv,transpose(grow(transpose(im),transpose(tmp),
                          transpose(im-tmp+a(1)))),square=1;
        window,0;
      } else {
        plsys,2;
        pli,transpose(grow(transpose(im),transpose(tmp),
                           transpose(im-tmp+a(1))));
        limits,0,3*boxsize,0,boxsize,square=1;
        plsys,1;
        plt,"Data",0.248,1.0,tosys=0,height=12,justify="CN";
        plt,"Fit",0.407,1.0,tosys=0,height=12,justify="CN";
        plt,"Residual",0.572,1.0,tosys=0,height=12,justify="CN";
      }
    }
    grow,f,fwhm;
    grow,ferr,fwhmerr;
    grow,el,ellip;
    grow,eler,ellerr;
    grow,an,angle;

    if (magswitch) {flux = zp-2.5*log10(clip(a(2),1e-10,));} else {flux = a(2);}
    if (!silent) {
      write,format="%7.2f %7.2f %5.2f+/-%4.2f %5.2f+/-%4.2f  %9.1f  %4.2f %6.2f %6.1f\n",
        pos(1),pos(2),fwhm(1),fwhmerr(1),fwhm(2),fwhmerr(2),flux,ellip,angle,maxim;
    }
		
    res = fwhmfitres(xpos=pos(1),xposerr=err(3),ypos=pos(2),yposerr=err(4),
                     xfwhm=fwhm(1),xfwhmerr=fwhmerr(1),yfwhm=fwhm(2),yfwhmerr=fwhmerr(2),
                     flux=flux,fluxerr=err(2),sky=a(1),skyerr=err(1),
                     ell=ellip,ellerr=ellerr,angle=angle,peak=maxim);
    grow,allres,res;

    if (oneshot) break;
  } while (but != 3);

  if (numberof(f) == 2) { if (!silent) write,"Bye bye"; return;}

  f	= f(,2:);
  ferr	= ferr(,2:);
  el	= el(2:);
  eler	= eler(2:);
  if (anyof(ferr==0)) {
    avgfwhm=0.;
  } else {
    avgfwhm = sum((f*1./ferr)(*))/sum(1./ferr(*));
  }
  //	stdfwhm = f(*)(rms);
  stdfwhm = avg([f(1,)(rms),f(2,)(rms)]); // avg X and Y rms
  avgel		= avg(el);
  stdel		= el(rms)+sqrt(sum(eler^2.))/numberof(eler);

  if (!silent) {
    if (pixset) {
      write,format="\nMedian FWHM : X = %5.3f / Y = %5.3f / <XY> = %6.3f [arcsec]\n",
        median(f(1,)),median(f(2,)),avg([median(f(1,)),median(f(2,))]);
    } else {
      write,format="\nMedian FWHM : X = %6.3f / Y = %6.3f / <XY> = %6.3f [pixel]\n",
        median(f(1,)),median(f(2,)),avg([median(f(1,)),median(f(2,))]);
    }
  }
  return allres;
}

