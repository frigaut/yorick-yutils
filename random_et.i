/* random_et.i: random number for Yorick
 * 
 * Copyright (c) 1996, Eric THIEBAUT (thiebaut@obs.univ-lyon1.fr, Centre de
 * Recherche Astrophysique  de Lyon, 9 avenue Charles  Andre, F-69561 Saint
 * Genis Laval Cedex).
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
 * HISTORY
 *   Nov. 22, 1995 by Eric THIEBAUT:
 *	- random_normal
 *	- random_poisson
 *	- kolmogorov
 *   Nov. 27, 1995 by Eric THIEBAUT:
 *	- fixed a bug for the borders in kolmogorov
 * 	$Id: random_et.i,v 1.1 2007-12-11 23:55:12 frigaut Exp $
 */

require, "gamma.i";

local random_normal_prev;
/* DOCUMENT random_normal_prev= []
     if not nil, is the previous value computed by random_normal.
 */
random_normal_prev= [];

func random_uniform(dims, seed=)
/* DOCUMENT random_uniform(dimemsion_list, seed=)
     returns an array of uniformly distributed random double values with
     the given DIMENSION_LIST (nil for a scalar result).
     Keyword SEED is a scalar between 0.0 and 1.0 non-inclusive and is
     used to reinitialized the random sequence. If SEED is out of range,
     the sequence is reinitialized as when Yorick starts.

   SEE ALSO: random, random_poisson, random_normal_prev.
*/
{
    if (!is_void(seed))
	random_seed, seed;
    return random(dims);
}
func random_normal(dims, seed=)
/* DOCUMENT random_normal(dimemsion_list)
     returns an array of normally distributed random double values with
     the given DIMENSION_LIST (nil for a scalar result).
     Keyword SEED is a scalar between 0.0 and 1.0 non-inclusive and is
     used to reinitialized the random sequence. If SEED is out of range,
     the sequence is reinitialized as when Yorick starts.
     The algorithm follows the Box-Muller method (see Numerical Recipes
     by Press et al.).

   SEE ALSO: random, random_poisson, random_normal_prev.
*/
{
    if (!is_void(seed))
	random_seed, seed;
    if (is_void(dims)) {
	if (is_void(random_normal_prev)) {
	    while (!(v1= random()));
	    v1= sqrt(-2.*log(v1));
	    v2= (2.*pi)*random();
	    random_normal_prev= v1*sin(v2);
	    return v1*cos(v2);
	} else {
	    a= random_normal_prev;
	    random_normal_prev= [];
	}
    } else {
	a= array(0., dims);
	if ((n= numberof(a)) % 2)
	    a(0)= random_normal();
	if ((n /= 2)) {
	    i= where(!(v1= random(n)));
	    while ((ni= numberof(i))) {
		j= where(!(v1(i)= random(ni)));
		i= numberof(j) ? i(j) : [];
	    }
	    v1= sqrt(-2.*log(v1));
	    v2= (2.*pi)*random(n);
	    a(1:n)= v1*cos(v2);
	    a(n+1:2*n)= v1*sin(v2);
	}
    }
    return a;
}

func random_poisson(xm, seed=, threshold=)
/* DOCUMENT random_poisson(mean)
     returns an array of random double values which follow a Poisson law
     of parameter MEAN (the output has the same geometry of the input).
     Keyword SEED is a scalar between 0.0 and 1.0 non-inclusive and is
     used to reinitialized the random sequence. If SEED is out of range,
     the sequence is reinitialized as when Yorick starts.
     The code is adapted from `POIDEV' an IDL routine by Wayne Landsman
     and the algorithm is from Numerical Recipes by Press et al.
     
   SEE ALSO: random, random_normal.
*/
{
    if (is_void(threshold)) threshold= 20;
    if (!is_void(seed))
	random_seed, seed;
    
    if (!(N= numberof(xm)))
	error, "ERROR - Poisson mean vector is undefined";
    if (N == 1 && dimsof(xm)(1) == 0) {
	output_is_scalar= 1n;
	xm= [xm];
    } else {
	output_is_scalar= 0n;
    }
    Ni= numberof((i= where(xm <= threshold)));

    if (Ni > 0) {
	g= exp(-xm(i));		// To compare with exponential distribution
	em= array(-1, Ni);	// Counts number of events
	t= array(1., Ni);	// Counts (log) of total time
	Nk= Nj= Ni;		// J indexes the original array,
	k= j= indgen(Ni);	// K indexes the J vector
	
	for (;;) {
	    em(j)++;		// Increment event counter
	    t= t(k)*random(Nk);	// Add exponential deviate, equivalent
				// to multiplying random deviate
	    k= where(t > g(j));	// Has sum of exponential deviates 
				//exceeded specified mean?
	    if (!(Nk= numberof(k)))
		break;
	    j= j(k);
	    Nj= Nk;
	}
    }
    output= array(double, dimsof(xm));
    if (Ni > 0) output(i)= em;
    if (Ni == N) return (output_is_scalar ? output(1) : output);

    Ni= numberof((i = where(xm > threshold)));
    xi= xm(i);
    sq = sqrt(2.*xi);
    alxm= log(xi);
    g= xi * alxm - ln_gamma(xi+1.);

    k= j= indgen((Nk= Nj= Ni));
    em= y= array(double, Nj);

    for (;;) {
	for (;;) {
	    y(j)= tan(pi * random(Nj));
	    l= where((em(j)= floor(sq(j)*y(j) + xi(j))) < 0.);
	    if (!(Nj= numberof(l)))
		break;
	    j= j(l);
	}
	
	t = 0.9*(1.+y(k)^2)*exp(em(k)*alxm(k)-ln_gamma(em(k)+1.)-g(k));
	l = where(random(Nk) > t);
	if (!(Nk= numberof(l)))
	    break;
	j= k= k(l);
	Nj= Nk;
    }
    
    output(i)= em;
    return (output_is_scalar ? output(1) : output);
}

func kolmogorov(diam, r0, all=, orig=, seed=)
/* DOCUMENT kolmogorov(diam, r0, all=, orig=, seed=)
            kolmogorov(diam, all=, orig=, seed=)
     returns an array of random phases which follow Kolmogorov  law on
     a square pupil of DIAM pixels per side with  a  Fried's parameter
     equal to R0 (in pixels, default is to set R0=DIAM).   If  a Point
     Spread Function is to  be  calculated  from  the  generated phase
     screen, it should be conveniently sampled  (i.e.,  R0  greater or
     equal 2 or 3 pixels).
         The  algorithm  is  the  mid-point  method  of  R.G.    Lane,
     A. Glindemann and J.C. Dainty (``Simulation of a Kolmogorov phase
     screen'' in Waves in Random Media, 2, 209-224, 1992).
         Keyword ORIG is a flag which  indicates  whether  or  not the
     original method by Lane et al. should be used (default is  to use
     the original algorithm).
         Keyword ALL is a flag which indicates whether or not  all the
     computed phase screen should be returned.  The  default behaviour
     is to return the smallest array into which a pupil  with diameter
     DIAM can fit.  The computed  phase  screen  is  a (2^N+1)*(2^N+1)
     array.
         Keyword SEED is a scalar between  0.0  and  1.0 non-inclusive
     and is used to reinitialized the random sequence.  If SEED is out
     of range, the sequence is reinitialized as when Yorick starts.

   SEE ALSO: random_normal.
*/
{
    diam= double(diam);
    if (is_void(r0)) r0= diam;	// default is to have R0=DIAM
    
    if (is_void(orig)) orig=1n;	// default is to use original algorithm
    if (is_void(all)) all=0n;	// default is to return a DIAM*DIAM array
    if (!is_void(seed)) random_seed, seed;

    n=2^long(ceil(log(diam-1.)/log(2.)));
    delta = sqrt(6.88*(diam/double(r0))^(5./3.));
    diam= long(ceil(diam));	// size of the minimum array which holds the
    				// phase screen over the pupil
    beta  = 1.7817974;
    c1 = 3.3030483e-1*delta;
    c2 = 6.2521894e-1*delta;
    c3 = 5.3008502e-1*delta;
    c4 = 3.9711507e-1*delta;
    if (orig) {
        c5 = 4.5420202e-1*delta;
    } else {
        c5 = 4.4355177e-1*delta;
        l5 = 4.5081546e-1;
        m5 = 9.8369088e-2;
    }
    a = random_normal([2,n+1,n+1]);
    b = c2*random_normal(2);
    
    // 4 first corners
    a(1,1) = c1*a(1,1)+b(1);
    a(0,0) = c1*a(0,0)-b(1);
    a(0,1) = c1*a(0,1)+b(2);
    a(1,0) = c1*a(1,0)-b(2);
    
    // all other points
    h = n;
    while (h >= 2) {
        s = h;		// step size
        h  /= 2;	// half the step size
        c3 /= beta;
        c4 /= beta;
        c5 /= beta;
        
	i= indgen(h+1:n+1-h:s);	// mid-point coordinates
	ip= i-h;		// coordinates of previous point
	in= i+h;		// coordinates of next point

	// centre of squares
        a(i,i) = c3*a(i,i) + .25*(a(ip,ip)+a(ip,in)+a(in,ip)+a(in,in));

	if (2*s <= n) {
	    // centre of losanges
	    j= indgen(s+1:n+1-s:s);	// vertice coordinates
	    jp= j-h;			// coordinates of previous point
	    jn= j+h;			// coordinates of next point
	    a(i,j) = c4*a(i,j) + .25*(a(i,jp)+a(i,jn)+a(ip,j)+a(in,j));
	    a(j,i) = c4*a(j,i) + .25*(a(j,ip)+a(j,in)+a(jp,i)+a(jn,i));
        }

	// borders
        if (orig) {
            a(1,i) = c5*a(1,i) + .5*(a(1,ip)+a(1,in));
            a(0,i) = c5*a(0,i) + .5*(a(0,ip)+a(0,in));
            a(i,1) = c5*a(i,1) + .5*(a(ip,1)+a(in,1));
	    a(i,0) = c5*a(i,0) + .5*(a(ip,0)+a(in,0));
        } else {
            a(1,i) = c5*a(1,i) + l5*(a(1,ip)+a(1,in))+m5*a(h,i);
	    a(0,i) = c5*a(0,i) + l5*(a(0,ip)+a(0,in))+m5*a(-h,i);
            a(i,1) = c5*a(i,1) + l5*(a(ip,1)+a(in,1))+m5*a(i,h);
	    a(i,0) = c5*a(i,0) + l5*(a(ip,0)+a(in,0))+m5*a(i,-h);
        }
    }
    
    return (all ? a : a(1:diam, 1:diam));
}
