/*
 * doppler.i
 *
 * $Id: doppler.i,v 1.1 2008-01-04 13:47:48 frigaut Exp $
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
 * $Log: doppler.i,v $
 * Revision 1.1  2008-01-04 13:47:48  frigaut
 * initial import of thibaut's functions
 *
 *
 */

extern LightSpeed;
/* DOCUMENT  LightSpeed
     The  velocity of  light in  vacuum and in m/s.
*/
LightSpeed=299792458;

func voflambda(lambda,lambda0,air=,microns=,coef=,vlsr=) {
/* DOCUMENT voflambda(lambda,lambda0)
   
    Attention: ALL velocities, including VLSR, are in m/s (NOT in km/s).

    Returns   radial   velocity   necessary   to   Doppler-shift   lambda0   to
    lambda. Wavelengths  are in vacuum  unless AIR is  set to non void  and non
    null. If AIR is specified, wavelengths  must be in angstroms, or MICRONS or
    COEF must be specified too (see  airvac and vacair). Velocities are in m/s.
    VLSR keyword can be  used to shift the returned value: VLSR  must be set to
    the real velocity  in the local standard of rest of  an object which radial
    velocity relative  to the earth  is zero at  the time of  observations.  In
    that case, VOFLAMBDA  returns velocities in the local  standard of rest. If
    not, it returns velocities relative to the observer.

   SEE ALSO: lambdaofv
*/
    if (is_void(vlsr)) vlsr=0.;
    if (air) {
        lambda=airvac(lambda,microns=microns,coef=coef);
        lambda0=airvac(lambda0,microns=microns,coef=coef);
    }
    lolc2=(lambda/lambda0)^2;
    return LightSpeed*(lolc2-1)/(lolc2+1)+vlsr;
}
func lambdaofv(v,lambda0,air=,microns=,coef=,vlsr=) {
/* DOCUMENT lambdaofv(v,lambda0)
   
    Returns observed  wavelength corresponding to  lambda0 Doppler-shifted with
    velocity v.  Wavelength are in vacuum unless AIR is set to non void and non
    null. If AIR is specified, wavelengths  must be in angstroms, or MICRONS or
    COEF must be specified too (see airvac and vacair). Velocities are in m/s.
    If VLSR keyword is  set to the real velocity in the  local standard of rest
    of an  object which radial  velocity relative to  the earth is zero  at the
    time of observations, V is considered a velocities in the local standard of
    rest and apparent wavelengths at  the time of observations are computed. If
    not, V  should not  be in the  local standard  of rest but  at the  time of
    observations for the return wavelengths to be accurate.

    Example:

    say you have a spectrum of  an object, which velocity in the local standard
    of rest is 0 but at the time of observations, an observed velocity of 0m/s
    would indeed  correspond to  a velocity  in the local  standard of  rest of
    VLSR(m/s).   AXIS contains  the  observed wavelength  in  micron for  this
    spectrum.   Then, lambdaofv(voflambda(AXIS,vlsr=VLSR),vlsr=0)  would return
    the axis, corrected for the earth's  motion. Now, if radial velocity of the
    object in the LSR is not 0,  but Vobj, then the axis corrected for both the
    earth's       and       object's       motions      is       given       by
    lambdaofv(voflambda(AXIS,vlsr=VLSR),vlsr=-Vobj). (Note:  don't trust me, at
    the time I'm writing these lines, I'm quite tired...)

   SEE ALSO: voflambda
*/
    if (is_void(vlsr)) vlsr=0.;
    if (air) lambda0=airvac(lambda0,microns=microns,coef=coef);
    b=(v-vlsr)/LightSpeed;
    lambda=lambda0*sqrt((1+b)/(1-b));
    if (air) lambda=vacair(lambda,microns=microns,coef=coef);
    return lambda;
}

func vacair(VAC,microns=,coef=){
/* DOCUMENT airwl=vacair(vacwl)
    Compute air wavelength from vacuum wavelength in Angstroms.
    Information found in;
    http://www-obs.univ-lyon1.fr/hypercat/pleinpot/imdwaxisc1.html

    "The IAU standard for conversion from air to vacuum wavelengths is given in
    Morton (1991,  ApJS, 77, 119).  For vacuum wavelengths (VAC)  in Angstroms,
    convert  to air wavelength  (AIR) via:
    AIR =  VAC /  (1.0 +  2.735182E-4 + 131.4182 / VAC^2 + 2.76249E8 / VAC^4)"
*/
    if (is_void(coef)) coef=1.;
    if (microns) coef=10000.;
    VAC=VAC*coef;
    AIR = VAC / (1.0 + 2.735182E-4 + 131.4182 / VAC^2 + 2.76249E8 / VAC^4);
    return AIR/coef;
}
func airvac(AIR,microns=,coef=){
/* DOCUMENT vacwl=airvac(airwl)
    Compute vacuum wavelength from air wavelength in Angstroms.
    Information found in;
    http://www-obs.univ-lyon1.fr/hypercat/pleinpot/imdwaxisc1.html

    "The IAU standard for conversion from air to vacuum wavelengths is given in
    Morton (1991,  ApJS, 77, 119).  For vacuum wavelengths (VAC)  in Angstroms,
    convert  to air wavelength  (AIR) via:
    AIR =  VAC /  (1.0 +  2.735182E-4 + 131.4182 / VAC^2 + 2.76249E8 / VAC^4)"
*/
    if (is_void(coef)) coef=1.;
    if (microns) coef=10000.;
    AIR=AIR*coef;
    VAC = AIR * (1.0 + 2.735182E-4 + 131.4182 / AIR^2 + 2.76249E8 / AIR^4);
    return VAC/coef;
}
