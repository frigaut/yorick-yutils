/*
 * histo.i --
 *
 *	Yorick routines for histogram computation.
 *
 * Public routines:
 *	histo2      driver for the `histogram' function.
 *	histo_stat  compute statistics of data grouped in bins.
 *	histo_plot  compute (with histo2) and plot histogram of data.
 *
 * Private/low-level routines:
 *	_histo_sum  sum statistics of data grouped in bins.
 *	
 * Copyright (c) 1998-1999 Eric THIEBAUT.
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
 *	$Id: histo.i,v 1.1 2007-12-11 23:55:12 frigaut Exp $
 *	$Log: histo.i,v $
 *	Revision 1.1  2007-12-11 23:55:12  frigaut
 *	Initial revision
 *
 *	Revision 1.3  2002/11/14 11:25:08  eric
 *	 - changed paths in require/include calls
 *
 *	Revision 1.2  1999/09/21 10:04:50  eric
 *	 - Version saved before new public release.
 *
 *	Revision 1.1  1998/10/30 13:55:29  eric
 *	Initial revision
 */

func histo2(data, &hx, weight=, binsize=, binmin=, binmax=, interp=, average=)
/* DOCUMENT hy= histo2(data, hx);
     Returns histogram of DATA, the histogram abscissae get stored into
     pure output HX (don't forget to declare it as `local', unless you don't
     care of side effects).

     The size of the bins can be given by keyword BINSIZE (default 1.0).
     The minimum and maximum abscissae to consider can specified with
     keywords BINMIN and BINMAX.

     Keyword WEIGHT gives data point weights (default all 1).  If keyword
     INTERP is set with a non-zero value, linear interpolation is used to
     get the weights of the data points to each bin.

     If keyword AVERAGE is set with a non-zero value, returns an average
     rather than a sum of the weithed data points.  Note that with no weights
     or uniform weights, the average value will be an array of 1's (now
     you know!).  Averaging is useful for instance to compute the radial
     profile (average along azimuth direction):
       x= indgen(xdim)-x0;     // abscissa
       y= indgen(ydim)-y0;     // ordinate
       r= abs(x, y(-,));       // radius
       a= exp(-r*r/50.0)+(random(xdim, ydim) - 0.5)/10.0;
       local px;
       py= histo2(r, px, weight=a, average=1, interp=1);
       plg, py, px, color="red";
       plg, exp(-px*px/50.0), px, color="green";
     You can call histo2 one more time to compute the sample noise:
       py_sigma= sqrt(histo2(r, weight=a*a, average=1, interp=1) - py*py);
 
  SEE ALSO: histogram. */
{
  if (is_void(binsize)) binsize = 1.0;
  else data *= (1.0/binsize);
  zero = floor(min(data));
  if (interp) {
    data -= zero;
    i = 1 + long(data);
    w = i - data;
    n = max(i) + 1;
    if (average) hx = histogram(i, w, top=n) + histogram(i+1, 1.0 - w, top=n);
    if (is_void(weight)) weight = 1.0;
    else w *= weight;
    hy = histogram(i, w, top=n) + histogram(i+1, weight - w, top=n);
    if (average) hy = hy / (hx + !hx);
  } else {
    i = long((1.5 - zero) + data);
    hy = is_void(weight) ? double(histogram(i)) : histogram(i, weight);
    if (average) {
      hx = histogram(i);
      hy = hy / (hx + !hx);
    }
  }
  hx = binsize*(zero + indgen(0:numberof(hy)-1));
  if (! is_void(binmin) && binmin > zero) {
    if (! is_array((i = where(hx >= binmin)))) {
      hx = [];
      return;
    }
    i = i(1);
    hx = hx(i:);
    hy = hy(i:);
  }
  if (! is_void(binmax) && binmax < hx(0)) {
    if (! is_array((i = where(hx <= binmax)))) {
      hx = [];
      return;
    }
    i = i(0);
    hx = hx(:i);
    hy = hy(:i);
  }
  return hy;
}

func histo_stat(data, x, avg_std=, std=, binsize=, xmin=, xmax=,
		weight=, interp=)
/* DOCUMENT hs= histo_stat(data, x);
     Compute statistics of response DATA with respect to explanatory
     variable X.  DATA measurements are grouped in bins by rounding X
     to the nearest integer multiple of BINSIZE and the statistical moments
     are computed whithin each bin.  If you need unevenly spaced bins, you
     may either operate a change of explicit variable or use Yorick's
     `digitize' and `histogram' routines.

   ARGUMENTS:
     HS   <-  Result:
                HS(,1)= value of explanatory variable X in each bin
                HS(,2)= sum of weights for each bin
                HS(,3)= average of DATA in each bin
              only if keyword STD/AVG_STD is non-nil and non-zero:
                HS(,4)= standard deviation of DATA in each bin
              only if keyword AVG_STD is non-nil and non-zero:
                HS(,5)= standard deviation of HS(,3)
     DATA ->  Response data to average.
     X    ->  Value of explanatory variable for each data measurement (same
              geometry as DATA).

   KEYWORDS:
     STD=     Compute standard deviation per data sample in HS(,4).
     AVG_STD= Compute standard deviation of average in HS(,5) (Note:
              this also implies STD=1).
     WEIGHT=  Statistical weight (e.g., 0.0 where data is not significant).
     XMAX=    Maximum X value to account for (default max(X)).
     XMIN=    Minimum X value to account for (default min(X)).
     BINSIZE= Resolution for data bins, i.e. value of HS(,1)(dif) (default 1.0)
     INTERP=  Use linear interpolation, instead of rounding to the nearest
              integer multiple of BINSIZE?

   SEE ALSO: digitize, histogram, _histo_sum. */
{
  local hx, h0, h1, h2, h3;
  order= avg_std ? 3 : (std ? 2 : 1);
  n= _histo_sum(data, x, order, hx, h0, h1, h2, h3, weight=weight,
		binsize=binsize, xmax=xmax, xmin=xmin, interp=interp);
  if (n<=0) return;
  hs= array(double, n, order+2);
  hs(,1)= hx;
  hs(,2)= h0;
  hs(,3)= (h1*= (h0= 1.0/(h0 + !h0)));
  if (order>=2) hs(,4)= sqrt(h2*h0 - h1*h1);
  if (order>=3) hs(,5)= sqrt((h2*h0 - h1*h1)*h3)*h0;
  return hs;
}

/* ------------------------------------------------------------------------- */

func histo_plot(data, weight=, binsize=, binmin=, binmax=, interp=, average=,
		just=, legend=, hide=, type=, width=, color=, marks=, marker=,
		mspace=, mphase=, prenormalize=, postnormalize=, scale=)
/* DOCUMENT histo_plot, data;
     Compute (with  histo2) an  histogram of DATA  and plot it  (with plh).
     All  keywords  of  plh  and  histo2 can  be  used.   Further  keywords
     PRENORMALIZE and POSTNORMALIZE can  be used to normalize the histogram
     so  that  its sum  is  equal  to  PRE/POSTNORMALIZE: normalization  to
     PRENORMALIZE  (POSTNORMALIZE)   is  performed  *BEFORE*  (respectively
     *AFTER*) data values less than BINMIN or greater than BINMAX have been
     rejected.   Keyword  SCALE  can  be  used  to  multiply  ordinates  of
     histogram  by  SCALE (default  SCALE=1);  this  is  useful to  compare
     histograms of the same data set with different BINSIZE.

   KEYWORDS: weight, binsize, binmin, binmax, interp, average,
             just, legend, hide, type, width, color, marks, marker,
             mspace, mphase, prenormalize, postnormalize, scale.

   SEE ALSO: histo2, plh. */
{
  require, "plot.i";
  local hx;
  if (prenormalize) {
    if (is_void(weight)) {
      weight= array(prenormalize/double(numberof(data)), dimsof(data));
    } else {
      weight*= prenormalize / double(sum(weight));
    }
  }
  hy= histo2(data, hx, weight=weight, binsize=binsize, binmin=binmin,
	     binmax=binmax, interp=interp, average=average);
  if (postnormalize) hy*= postnormalize / sum(hy);
  plh, (is_void(scale) ? hy : scale*hy), hx,
    legend=legend, hide=hide, type=type, width=width, color=color,
    marks=marks, marker=marker, mspace=mspace, mphase=mphase;
}

/* ------------------------------------------------------------------------- */

func _histo_sum(data, x, order, &hx, &h0, &h1, &h2, &h3,
		binsize=, xmin=, xmax=, weight=, interp=)
/* DOCUMENT n= _histo_sum(data, x, order, hx, h0, h1, h2);
     Worker for histo_stat routine: sum moments of response DATA with respect
     to explanatory variable X.  See documentation of histo_stat for
     further explanations and description of keywords.  Return number of
     elements in output arrays (if N=0, outputs can be discarded...).

   ARGUMENTS:
     DATA ->  Response data to average.
     X    ->  Value of explanatory variable for each data measurement (same
              geometry as DATA).
     HX   <-  Value of X in each bin.
     H0   <-  Sum of WEIGHT in each bin.
     H1   <-  Sum of WEIGHT*DATA in each bin.
     H2   <-  Sum of WEIGHT*DATA^2 in each bin.
     H3   <-  Sum of WEIGHT*WEIGHT in each bin.
     ORDER->  0 to compute only HX and H0,
              >=1 to also compute H1
	      >=2 to also compute H2
	      >=3 to also compute H3.

   KEYWORDS: binsize, xmin, xmax, weight, interp.

   SEE ALSO: histo_stat, histogram, digitize. */
{
  /* Instanciate WEIGHT array. */
  if (is_void(weight)) weight= array(1.0, dimsof(data));
  else weight+= 0.0*data; /* Make sure WEIGHT is double precision and same
			     geometry as DATA. */
  if (! is_void(xmax) && is_array((i= where(x>xmax)))) weight(i)= 0.0;
  if (! is_void(xmin) && is_array((i= where(x<xmin)))) weight(i)= 0.0;
    
    /* Compute (interpolated) moments. */
  if (is_void(binsize)) binsize= 1.0;
  else if (binsize>0.0) x/= binsize;
  else error, "bad BINSIZE";
  zero= floor(min(x));
  if (interp) {
    w2= x-zero;
    i2= 1 + (i1= 1+long(w2));
    w2= 1.0 - (w1= i1-w2);
    n= max(i1)+1;
    h0= histogram(i1, (w1*= weight), top=n) +
      histogram(i2, (w2*= weight), top=n);
    if (order>=3) h3= histogram(i1, w1*w1, top=n) +
		    histogram(i2, w2*w2, top=n);
    if (order>=1) h1= histogram(i1, (w1*= data), top=n) +
		    histogram(i2, (w2*= data), top=n);
    if (order>=2) h2= histogram(i1, (w1*= data), top=n) +
		    histogram(i2, (w2*= data), top=n);
  } else {
    i= long(1.5-zero+x);
    h0= histogram(i, weight);
    if (order>=3) h3= histogram(i, weight*weight);
    if (order>=1) h1= histogram(i, (weight*= data));
    if (order>=2) h2= histogram(i, (weight*= data));
  }
  hx= binsize*(zero+indgen(0:numberof(h0)-1));
  if (! is_void(xmax) && xmax < hx(0)) {
    if (! is_array((i= where(hx <= xmax)))) return 0;
    i= i(0);
    hx= hx(:i);
    h0= h0(:i);
    if (order>=1) h1= h1(:i);
    if (order>=2) h2= h2(:i);
    if (order>=3) h3= h3(:i);
  }
  if (! is_void(xmin) && xmin > hx(1)) {
    if (! is_array((i= where(hx >= xmin)))) return 0;
    i= i(1);
    hx= hx(i:);
    h0= h0(i:);
    if (order>=1) h1= h1(i:);
    if (order>=2) h2= h2(i:);
    if (order>=3) h3= h3(i:);
  }
  return numberof(hx);
}
