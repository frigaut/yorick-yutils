/*
 * detect.i --
 *	Detection of local minima/maxima for Yorick.
 *
 * Copyright (c) 2003, Eric THIEBAUT.
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
 * Routines:
 *      find_1d_minmax - find local minima/maxima in 1-D array.
 *      plot_1d_minmax - plot local minima/maxima in 1-D array.
 *	find_2d_max - find isolated local maxima in 2-D array.
 *
 * History:
 *	$Id: detect.i,v 1.1.1.1 2007-12-11 23:55:13 frigaut Exp $
 *	$Log: detect.i,v $
 *	Revision 1.1.1.1  2007-12-11 23:55:13  frigaut
 *	Initial Import - yorick-yutils
 *
 *
 *-----------------------------------------------------------------------------
 */

func find_1d_minmax(a, what, inf=, sup=, alev=, rlev=, hysteresis=)
/* DOCUMENT find_1d_minmax(a)
 *     -or- find_1d_minmax(a, what)
 *   Find local minima/maxima in 1-D array  A.  If WHAT is nil or zero, the
 *   function returns an array of integers  with same shape as A and set to
 *   +1 where A is a local maximum, to -1 where A is a local minimum and to
 *   0  elsewhere.  Otherwise, the  function returns  the indices  of local
 *   maxima or local minima depending  whether WHAT is positive or negative
 *   (the result may be empty).  WHAT may also be a string: "any", "min" or
 *   "max".
 *
 *   Contiguous extrema,  say a  local maximum LOCMAX  and a  local minimum
 *   LOCMIN, are separated by a strict hysteresis (or a gap) such that:
 *
 *      LOCMAX - LOCMIN > HYSTERESIS*(max(A) - min(A)) >= 0
 *
 *   The default  is HYSTERESIS=0, i.e. all strict  local minima/maxima are
 *   detected.  However,  in order  to avoid being  too sensitive  to local
 *   extrema  (for  instance  because  of  noise), the  hysteresis  of  the
 *   algorithm  can  be  adjusted  by  keywords INF,  SUP,  ALEV,  RLEV  or
 *   HYSTERESIS.   Note  that  the   easiest  keywords  to  play  with  are
 *   HYSTERESIS or ATOL and RTOL (SUP and INF are more tricky to use).
 *
 *   HYSTERESIS = level of hysteresis  relative to peak-to-peak value of A.
 *         HYSTERESIS   must   be  conformable   with   A  and   everywhere
 *         non-negative (this  is not checked).  HYSTERESIS  is another way
 *         to  specify the absolute  tolerance and  is only  significant if
 *         ALEV is not specified.   Specifying HYSTERESIS gives an absolute
 *         tolerance:
 *            ALEV = (max(A) - min(A))*HYSTERESIS.
 *         Hence the lower  is the hysteresis, the more  local extrema will
 *         be  detected.   As  a  rule  of  thumb, a  good  value  for  the
 *         hysteresis is the 3-4 divided by the signal-to-noise-ratio.
 *
 *   ALEV = absolute level of  hysteresis.  ALEV must be conformable with A
 *         and such that  ALEV >= 0 everywhere (this  is not checked).  The
 *         default is  the same as with ALEV=0  (unless  keyword HYSTERESIS
 *         is specified).
 *
 *   RLEV = relative level of hysteresis.  RLEV must be conformable with A
 *         and such  that 0 <= RLEV  < 1 everywhere (this  is not checked).
 *         The default is the same as with RLEV=0.
 *
 *   INF = inferior  bound with respect to  a maximum: A(i) may  be a local
 *         maximum with respect to A(j) if  and only if A(j) < INF(i).  INF
 *         must  have the  same number  of elements  as A.   If INF  is not
 *         specified,  it is  computed from  the  value of  ALEV and  RLEV.
 *         Given ALEV and RLEV, the inferior bound is:
 *             INF = A - (ALEV + RLEV*abs(A))
 *
 *   SUP = superior  bound with respect to  a minimum: A(i) may  be a local
 *         minimum with respect to A(j) if  and only if A(j) > SUP(i).  SUP
 *         must  have the  same number  of elements  as A.   If SUP  is not
 *         specified,  it is  computed from  the  value of  ALEV and  RLEV.
 *         Given ALEV and RLEV, the superior bound is such that:
 *             A = SUP - (ALEV + RLEV*abs(SUP))
 *         or:
 *             SUP = (A + ALEV)/(1 - sign(SUP)*RLEV)
 *         since 0 <= RLEV < 1  then SUP has the same  sign as A + ALEV and
 *         finally:
 *             SUP = (A + ALEV)/(1 - sign(A + ALEV)*RLEV)
 *
 * SEE ALSO: plot_1d_minmax.
 */
{
  if (structof(what) == string) {
    if (what == "any") what = 0;
    else if (what == "max") what = 1;
    else if (what == "min") what = -1;
    else error, "bad value for WHAT (must be \"any\", \"min\" or \"max\")";
  }
  if (! is_array(a) || (dimsof(a)(1)) != 1) error, "expecting 1-D array";
  if ((s = structof(a)) != double) {
    if (s == complex) error, "illegal complex array";
    a = double(a);
  }
  n = numberof(a);

  /* compute the inferior/superior bounds */
  if (is_void(inf) || is_void(sup)) {
    if (is_void(alev)) {
      if (is_void(hysteresis)) alev = 0.0;
      else alev = (max(a) - min(a))*hysteresis;
    }
    if (noneof(rlev)) {
      if (noneof(alev)) {
        if (is_void(inf)) eq_nocopy, inf, a;
        if (is_void(sup)) eq_nocopy, sup, a;
      } else {
        if (is_void(inf)) inf = a - alev;
        if (is_void(sup)) sup = a + alev;
      }
    } else {
      if (is_void(inf)) inf = a - rlev*abs(a) - alev;
      if (is_void(sup)) {
        sup = a + alev; /* temporary */
        sup = sup/(1.0 - sign(sup)*rlev);
      }
    }
  }
  type = array(long, n);

  /* start with leftmost element */
  imin = imax = i = 1;
  vmin = vmax = a(1);
  vinf = inf(1);
  vsup = sup(1);
  state = 0;
  for (;;) {
    val = a(++i);
    if (i == n) {
      /* end-point */
      if (state > 0) {
        type((val > vmax ? i : imax)) = 1;
      } else if (state < 0) {
        type((val < vmin ? i : imin)) = -1;
      }
      break;
    }
    if (state >= 0) {
      /* seeking for a local maximum */
      if (val < vinf) {
        /* accept maximum value found so far as a local maximum and setup
           to start seeking for next local minimum */
        type(imax) = 1;
        imin = i;
        vmin = val;
        vsup = sup(i);
        state = -1;
      } else if (val > vmax) {
        /* higher maximum found */
        imax = i;
        vmax = val;
        vinf = inf(i);
      }
    }
    if (state <= 0) {
      /* seeking for a local minimum */
      if (val > vsup) {
        /* accept maximum value found so far as a local maximum and setup
           to start seeking for a local minimum */
        type(imin) = -1;
        imax = i;
        vmax = val;
        vinf = inf(i);
        state = 1;
      } else if (val < vmin) {
        /* lower minimum found */
        imin = i;
        vmin = val;
        vsup = sup(i);
      }
    }
  }
  return (what ? (what > 0 ? where(type > 0) : where(type > 0)) : type);
}

func plot_1d_minmax(y, x, list, nocurve=, type=, width=, color=,
                    symbol=, size=, fill=,
                    what=, inf=, sup=, alev=, rlev=, hysteresis=)
/* DOCUMENT plot_1d_minmax, y;
       -or- plot_1d_minmax, y, x;
       -or- plot_1d_minmax, y, x, list;

     Plot (X,Y) curve with local minima/maxima.  LIST is the list of
     extrema as returned by find_1d_minmax; if LIST is nil, find_1d_minmax
     is used to find the extrema (with argument Y, and values of keywords
     WHAT, INF, SUP, ALEV, RLEV and/or HYSTERESIS).

     Unless keyword NOCURVE is true, the curve (X,Y) is plotted as well
     (with values of keywords TYPE, WIDTH and/or COLOR).

     Keywords SYMBOL, SIZE, FILL, and COLOR can be used to customize the
     plotting of local minima/maxima.  If SYMBOL is unspecified and both
     minima and maxima are to be plotted, triangles pointing to the top (to
     the bottom) will be used to display maxima (minima).

     When called as a function, actual LIST is returned.

   SEE ALSO: find_1d_minmax, plp, plg. */
{
  if (is_void(list)) list = find_1d_minmax(y, what, inf=inf, sup=sup,
                                           alev=alev, rlev=rlev,
                                           hysteresis=hysteresis);
  if (is_void(x)) x = double(indgen(numberof(y)));
  if (! nocurve) plg, y, x, color=color, type=type, width=width;
  if (numberof(list) == numberof(y)) {
    /* plot both minima and maxima */
    if (is_array((i = where(list < 0)))) {
      plp, y(i), x(i), symbol=(is_void(symbol) ? 7 : symbol),
        size=size, color=color, fill=fill;
    }
    if (is_array((i = where(list > 0)))) {
      plp, y(i), x(i), symbol=(is_void(symbol) ? 3 : symbol),
        size=size, color=color, fill=fill;
    }
  } else if (! is_void(list)) {
    plp, y(list), x(list), symbol=symbol, size=size, color=color, fill=fill;
  }
  return list;
}

/*---------------------------------------------------------------------------*/

func find_2d_max(img, alev=, rlev=, cmin=, cmax=, bad=, debug=)
/* DOCUMENT map = find_2d_max(img)
     Find disjoint  local maxima in  2-D array IMG  and return an  array of
     integers MAP with same shape as IMG and set as follow:
         MAP(x,y) = -1       for bad pixels (or strictly above CMAX)
         MAP(x,y) =  0       for pixels not assigned to any local maximum
         MAP(x,y) =  N (N>0) for pixels assigned to N-th local maximum
     The  maxima  are  labelled  from   the  higher  to  the  lower.  Hence
     where(MAP==1) gives the indices of pixels around the stronger maximum.

     The selection  works as follows.   The algorithm starts with  the next
     (unassigned) higher maximum and  then marks all connected pixels which
     are greater or equal to a given  threshold.  If a bad pixel or a pixel
     already assigned to another  maximum is encountered during this stage,
     the algorithm  gives up this maximum  and proceeds with  the next one.
     Otherwise,  the marked  region  is  assigned to  the  maximum and  the
     algorithm proceeds with the  next one.  This algorithm guarantees that
     the marked regions are all separated (by at least one pixel) from each
     other and from any bad pixel.  The threshold reads:
         THRESHOLD = PEAK - RLEV*abs(PEAK) - ALEV
     where PEAK is the current  maximum, ALEV (ALEV>=0 everywhere) and RLEV
     (0<=RLEV<1 everywhere) are the absolute and relative threshold levels.
     ALEV and RLEV are given by keyword. By default, ALEV=0 and RLEV=0.
     
     Keyword CMIN can be used to  set the minimum value of a local maximum.
     Since searching all  maxima may be prohibitively long,  it is strongly
     recommended to limit the depth of the search by the keyword CMIN.

     Keywords BAD  and/or CMAX can be used  to mark as bad  pixels the ones
     for which BAD is non-zero and/or which are (strictly) above CMAX.

     If  specified, keywords ALEV,  RLEV, CMIN,  CMAX and  BAD must  all be
     conformable with  IMG; you can  therefore setup things on  a per-pixel
     basis, or columnwise, or rowwise...

     
  EXAMPLE:  
     For instance, if SIGMA is the standard deviation of noise in the image
     and  BACKGROUND is  its background  level (both  could  be pixelwise),
     then:
         find_2d_max(IMG, cmin=BACKGROUND+3*SIGMA, alev=4*SIGMA)
     will find all the maxima in  IMG which are above the background with a
     3 SIGMA  confidence level  and mark the  regions around  every maximum
     (with value PEAK) where connected pixels are such that:
         IMG >= PEAK - 4*SIGMA

   HINTS:
     1. Use  keyword CMIN  to limit  the  search (possibly  on a  per-pixel
        basis).

     2. The search  necessitates to sort  the pixels elligible to  be local
        maxima (all which are above CMIN,  below CMAX and not in BAD), this
        sorting can be very long for large images (again use CMIN) but also
        for integer valued images  (a drawback of the quicksort algorithm?)
        to overcome this  it is sufficient to add a  small amount of random
        noise in the image, for instance:
            find_2d_max(IMG + 1e-9*(random(dimsof(IMG)) - 0.5), ...)
        but beware that this can make the result (slightly) inpredictible.
      
   SEE ALSO: sort, find_1d_minmax. */
{
  if (! is_array(img) ||
      ((dims = dimsof(img))(1)) != 2) error, "expecting 2-D array";
#if 0
  if ((s = structof(img)) != double) {
    if (s == complex) error, "illegal complex array";
    img = double(img);
  }
#endif

  /* mark bad points */
  region = array(long, dims); /* needed to make BAD conformable with IMG */
  if (is_void(bad)) {
    if (! is_void(cmax)) bad = (img > cmax);
  } else if (is_void(cmax)) {
    bad |= region; /* make BAD conformable with IMG */
  } else {
    bad |= (img > cmax);
  }
  if (anyof(bad)) region(where(bad)) = -1;

  /* sort pixels eligible for being local maxima */
  if (debug) write, format="%s...", "sorting";
  if (is_void(cmin) && is_void(bad)) {
    index = sort(img(*));
  } else {
    if (is_void(cmin))     index = where(! bad);
    else if (is_void(bad)) index = where(img >= cmin);
    else                   index = where((! bad) & (img >= cmin));
    if (! is_array(index)) {
      write, "warning no pixel are eligible for being local maxima";
      return region;
    }
    index = index(sort(img(index)));
  }
  if (debug) write, format="%s\n", "done";
  bad = cmin = cmax = []; /* free some memory */

  /* compute threshold */
  if (is_void(rlev)) {
    if (is_void(alev)) threshold = img(index);
    else               threshold = (img - alev)(index);
  } else {
    if (is_void(alev)) threshold = (img - rlev*abs(img))(index);
    else               threshold = (img - alev - rlev*abs(img))(index);
  }
  alev = rlev = []; /* free some memory */

  /* serach local maxima */
  number = numberof(img);
  width = dims(2);
  height = dims(3);
  list = array(long, number); /* indices of pixels in current region */
  state = array(long, dims);
  mark = 1;
  for (i=numberof(index) ; i>=1 ; --i) {
    j = index(i);
    if (region(j)) continue;
    level = threshold(i);
#if 0
    if (debug) {
      write, format="search max around (%d,%d) %g >= %g\n",
        1 + (j - 1)%width, 1 + (j - 1)/width,double(img(j)),
        double(level);
    }
#endif
    
    /* Use a kind of non-recursive flood-fill algorithm.
     *
     * The 3 following bits are used to indicate the directions to
     * investigate (so that we limit the number of checks undergone by a
     * pixel):
     *
     *     +---+
     *     | 4 |
     * +---+---+---+
     * | 2 | x | 1 |
     * +---+---+---+
     *     | 8 |
     *     +---+
     *
     * region(x,y) =  0  if unused
     * region(x,y) = -1  if invalid
     * region(x,y) =  n  if inside n-th blob
     *
     * TO DO: use same array for REGION and STATE
     *
     */
    region(j) = mark; /* mark current maximum */
    state(j) = 15;    /* will check all neighbors of current maximum */
    count = 1;        /* number of pixels in the current region */
    list(1) = j;      /* current maximum belongs to current region */
    discard = 0n;     /* no error yet */
    for (j=1 ; j<=count ; ++j) {
      k = list(j);
      x = 1 + (k - 1)%width;
      y = 1 + (k - 1)/width;
      s = state(k);
      //if (debug) write,format="state(%d,%d)=\n";
      if (s & 1) {
        if (x < width) {
          l = k + 1;
          if (! (r = region(l))) {
            if (img(l) >= level) {
              region(l) = mark;
              state(l)  = 13; /* (1|2|4|8) & ~2 */
              list(++count) = l;
            }
          } else if (r == mark) {
            state(l) = (s & 13);
          } else {
            discard = 1n;
            break;
          }
        }
      }
      if (s & 2) {
        if (x > 1) {
          l = k - 1;
          if (! (r = region(l))) {
            if (img(l) >= level) {
              region(l) = mark;
              state(l)  = 14; /* (1|2|4|8) & ~1 */
              list(++count) = l;
            }
          } else if (r == mark) {
            state(l) = (s & 14);
          } else {
            discard = 1n;
            break;
          }
        }
      }
      if (s & 4) {
        if (y < height) {
          l = k + width;
          if (! (r = region(l))) {
            if (img(l) >= level) {
              region(l) = mark;
              state(l)  = 7; /* (1|2|4|8) & ~8 */
              list(++count) = l;
            }
          } else if (r == mark) {
            state(l) = (s & 7);
          } else {
            discard = 1n;
            break;
          }
        }
      }
      if (s & 8) {
        if (y > 1) {
          l = k - width;
          if (! (r = region(l))) {
            if (img(l) >= level) {
              region(l) = mark;
              state(l)  = 11; /* (1|2|4|8) & ~4 */
              list(++count) = l;
            }
          } else if (r == mark) {
            state(l) = (s & 11);
          } else {
            discard = 1n;
            break;
          }
        }
      }
    }
    l = list(1:count);
    if (discard) {
      region(l) = 0;
    } else {
      ++mark;
      if (debug) {
        fma;
        //pli, region > 0;
        pli, region;
        pause, 1;
      }
    }
  }
  return region;
}

/*---------------------------------------------------------------------------*/
