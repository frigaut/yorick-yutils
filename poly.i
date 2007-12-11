/*
 * poly.i --
 *
 *	1D and 2D polynomial routines (evaluation, fit, ...).
 *
 * Copyright and warranty:
 *	Copyright (c) 1999, Eric THIEBAUT  (thiebaut@obs.univ-lyon1.fr, Centre
 *	de Recherche Astrophysique de Lyon, 9  avenue  Charles  Andre, F-69561
 *	Saint Genis Laval Cedex).
 *
 *	This program is free software; you can redistribute  it  and/or modify
 *	it under the terms of the GNU General Public License  as  published by
 *	the Free Software Foundation; either version 2 of the License,  or (at
 *	your option) any later version.
 *
 *	This program is distributed in the hope that it  will  be  useful, but
 *	WITHOUT  ANY  WARRANTY;  without   even   the   implied   warranty  of
 *	MERCHANTABILITY or FITNESS FOR  A  PARTICULAR  PURPOSE.   See  the GNU
 *	General Public License for more details (to receive a copy of  the GNU
 *	General Public License, write to the Free  Software  Foundation, Inc.,
 *	675 Mass Ave, Cambridge, MA 02139, USA).
 *
 * History:
 *	$Id: poly.i,v 1.1.1.1 2007-12-11 23:55:13 frigaut Exp $
 *	$Log: poly.i,v $
 *	Revision 1.1.1.1  2007-12-11 23:55:13  frigaut
 *	Initial Import - yorick-yutils
 *
 */

func poly1(x, c)
/* DOCUMENT: poly1(x, c);
     Returns value of the 1D polynomial:
       C(1) + C(2)*X + C(3)*X^2 + ... 

   SEE ALSO: poly, poly1_deriv, poly1_fit, poly2. */
{
  y= c((i= numberof(c)));
  while (--i>0) y= x*y + c(i);
  return y;
}

func poly1_deriv(x, c)
/* DOCUMENT: y= poly1_deriv(x, c);
     Returns value of the derivative of the 1D polynomial:
       C(1) + C(2)*X + C(3)*X^2 + ... 
     in other words:
       Y= C(2) + 2*C(3)*X + 3*C(4)*X² + ...

   SEE ALSO: poly1. */
{
  if ((n= numberof(c))==1) return array(0.0, dimsof(x));
  return poly1(x, c(2:)*double(indgen(n-1)));
}
/*---------------------------------------------------------------------------*/
func poly2(x1, x2, c)
/* DOCUMENT: poly2(x1, x2, c);
     Returns value of the 2D polynomial:
       C(1) + C(2)*X1 + C(3)*X2 + C(4)*X1^2 + C(5)*X1*X2 + C(6)*X2^2 + ...
     X1 and X2 must be conformable.  For a 2D polynomial of degree 1, 2 or 3
     (3, 6 and 10 coefficients respectively) poly2 uses hard-coded
     factorized expressions to minimize the number of operations.

   SEE ALSO: poly, poly1. */
{
  n= numberof(c);

  /* Optimized factorizations (just cut out this block if you don't want to
     use or don't trust the following expressions). */
#if 1
  if (n <= 10) {
    if (n==3) return c(1)+c(2)*x1+c(3)*x2;
    if ((d1= dimsof(x1))(1)==(d2= dimsof(x2))(1) && allof(d1==d2)) {
      if (n==6) return c(1)+(c(2)+c(5)*x2+c(4)*x1)*x1+(c(3)+c(6)*x2)*x2;
      if (n==10) return c(1)+(c(2)+c(5)*x2+(c(4)+c(7)*x1+c(8)*x2)*x1)*x1+
		   (c(3)+(c(6)+c(9)*x1+c(10)*x2)*x2)*x2;
    } else {
      /* Use a different factorization to optimize number of operations. */
      if (n==6) return c(1)+(c(2)+c(4)*x1)*x1+c(5)*x1*x2+(c(3)+c(6)*x2)*x2;
      if (n==10) return c(1)+(c(2)+(c(4)+c(7)*x1)*x1)*x1+
		   (c(3)+(c(6)+c(10)*x2)*x2)*x2+
		   (c(5)+c(8)*x1)*x1*x2+c(9)*x2*x2*x1;
    }
  }
#endif

  /* For a 2D polynomial of degree M, there are N=(M+1)*(M+2)/2 coefficients.
     => M = (sqrt(8*N+1)-3)/2 */
  m= long((sqrt(8*n+1)-2)/2);  /* -2 (instead of -3) to get nearest integer */

  /* Pre-compute powers of X1 and X2. */
  xp1= array(pointer, m); xp1(1)= &(tmp1= x1);
  xp2= array(pointer, m); xp2(1)= &(tmp2= x2);
  for (k=2 ; k<=m ; k++) {
    xp1(k)= &(tmp1*= x1);
    xp2(k)= &(tmp2*= x2);
  }
  tmp1= tmp2= x1= x2= [];

  /* Current degree is: K=K1+K2=0..M; K1, K2 are the powers of X1 and X2. */
  y= c((i= 1));                             /* K=0, K1=0, K2=0 */
  for (k=1 ; k<=m ; k++) { 
    y+= c(++i) * (*xp1(k));                 /* K1=K, K2=0 */
    for (k1=k-1, k2=1 ; k1>0 ; k1--, k2++)
      y+= c(++i) * (*xp1(k1)) * (*xp2(k2)); /* K1=K-1..1, K2=1..K-1 */
    y+= c(++i) * (*xp2(k));                 /* K1=0, K2=K */
  }
  if (i!=n)
    error, "bad number of coefficients";
  return y;
}

func poly2_deriv(x1, x2, c)
/* DOCUMENT drv= poly2_deriv(x1, x2, c);
     Return derivatives of 2D polynomial poly2(X1,X2,C) with respect
     to X1 and X2:
       DRV(..,1)= d poly2(X1,X2,C) / d X1
       DRV(..,2)= d poly2(X1,X2,C) / d X2

   BUGS: Only works for polynomials of degree 1 to 4 (needs factorization
         for degree 4). */
{
  dims= dimsof(x,y);
  dims(1)++;
  grow, dims, 2;
  drv= array(double, dims);
  n= numberof(c);
  if (n==14) {
    drv(..,1)= c(2) + 2*c(4)*x + c(5)*y + 3*c(7)*x*x + 2*c(8)*x*y + c(9)*y*y +
      4*c(11)*x*x*x + 3*c(12)*x*x*y + 2*c(13)*x*y*y + c(14)*y*y*y;
    drv(..,2)= c(3) + c(5)*x + 2*c(6)*y + c(8)*x*x + 2*c(9)*x*y + 3*c(10)*y*y +
      c(12)*x*x*x + 2*c(13)*x*x*y + 3*c(14)*x*y*y + 4*c(15)*y*y*y;
  } else if (n==10) {
    drv(..,1)= c(2)+(2*c(4)+3*c(7)*x+2*c(8)*y)*x+(c(5)+c(9)*y)*y;
    drv(..,2)= c(3)+(c(5)+c(8)*x+2*c(9)*y)*x+(2*c(6)+3*c(10)*y)*y;
  } else if (n==6) {
    drv(..,1)= c(2)+2*c(4)*x+c(5)*y;
    drv(..,2)= c(3)+c(5)*x+2*c(6)*y;
  } else if (n==2) {
    drv(..,1)= c(2);
    drv(..,2)= c(3);
  } else {
      error, "bad number of coefficients";
  }
  return drv;
}

/*---------------------------------------------------------------------------*/
/* NOTES ABOUT LEAST SQUARES FIT:
 *    Chi2 = Sum_k( weight_k * (model_k - data_k)^2 )
 *
 *    d Chi2                                             d model_k
 *    ------- = 2 Sum_k( weight_k * (model_k - data_k) * --------- )
 *    d alpha                                            d alpha
 *
 */
func poly1_fit(y, x, m, w)
/* DOCUMENT: c= poly1_fit(y, x, m);
        -or- c= poly1_fit(y, x, m, w);
     Returns coefficients of the 1D polynomial of degree M:
       C(1) + C(2)*X + C(3)*X^2 + ... + C(M+1)*X^M
     which best fits Y in the least squares sense:
       C = arg min sum(W*(poly1(X,C) - Y)^2)
     The weights W are optional (by default, W=1.0).  The array X and Y must
     have the same shape.

   SEE ALSO: poly1. */
{
  if (structof(x)!=double) x= double(x);
  n= m+1; /* number of coefficients */

  /* Compute powers of X. */
  xp= array(pointer, n);
  if (is_void(w)) {
    xp(1)= &array(1.0);
    if (n>1) {
      xp(2)= &(tmp= x);
      for (i=3 ; i<=n ; i++) {
	xp(i)= &(tmp*= x);
      }
    }
  } else {
    /* Apply weights (if any, also do apply scalar weight to let the user
       rescale the problem in case of overflows). */
    xp(1)= &(w= sqrt(w)); /* raise a SIGFPE if any of W < 0 */
    if (n>1) {
      xp(2)= &(w * (tmp= x));
      for (i=3 ; i<=n ; i++) {
	xp(i)= &(w * (tmp*= x));
      }
    }
    y*= w;
    w= [];
  }
  tmp= x= [];
  
  /* Solve. */
  return solve_lfit(y, xp);
}
/*---------------------------------------------------------------------------*/
func poly2_fit(y, x1, x2, m, w)
/* DOCUMENT: c= poly2_fit(y, x1, x2, m);
        -or- c= poly2_fit(y, x1, x2, m, w);
     Returns the (M+1)*(M+2)/2 coefficients of the 2D polynomial of degree M:
       C(1) + C(2)*X1 + C(3)*X2 + C(4)*X1^2 + C(5)*X1*X2 + C(6)*X2^2 + ... 
     which best fits Y in the least squares sense:
       C = arg min sum(W*(poly2(X1,X2,C) - Y)^2)
     The weights W are optional (by default, W=1.0).

   SEE ALSO: poly2. */
{
  if (structof(x1)!=double) x1= double(x1);
  if (structof(x2)!=double) x2= double(x2);

  /* Pre-compute powers of X1 and X2. */
  xp1= array(pointer, m); xp1(1)= &(tmp1= x1);
  xp2= array(pointer, m); xp2(1)= &(tmp2= x2);
  for (k=2 ; k<=m ; k++) {
    xp1(k)= &(tmp1*= x1);
    xp2(k)= &(tmp2*= x2);
  }
  tmp1= tmp2= x1= x2= [];

  /* Compute powers: X1^K1 * X2^K2 (with current degree K = K1+K2 = 0..M).  */
  n= (m+1)*(m+2)/2; /* number of coefficients */
  xp= array(pointer, n);
  if (is_void(w)) {
    xp((i= 1))= &array(1.0);                 /* K=0, K1=0, K2=0 */
    for (k=1 ; k<=m ; k++) {
      xp(++i)= xp1(k);                       /* K1=K, K2=0 */
      for (k1=k-1, k2=1 ; k1>0 ; k1--, k2++)
	xp(++i)= &(*xp1(k1) * *xp2(k2));     /* K1=K-1..1, K2=1..K-1 */
      xp(++i)= xp2(k);                       /* K1=0, K2=K */
    }
  } else {
    /* Apply weights (if any, also do apply scalar weight to let the user
       rescale the problem in case of overflows). */
    w= sqrt(w);                              /* SIGFPE if any of W < 0 */
    xp((i= 1))= &w;                          /* K=0, K1=0, K2=0 */
    for (k=1 ; k<=m ; k++) {
      xp(++i)= &(w * *xp1(k));               /* K1=K, K2=0 */
      for (k1=k-1, k2=1 ; k1>0 ; k1--, k2++)
	xp(++i)= &(w * *xp1(k1) * *xp2(k2)); /* K1=K-1..1, K2=1..K-1 */
      xp(++i)= &(w * *xp2(k));               /* K1=0, K2=K */
    }
    y*= w;
    w= [];
  }
  xp1= xp2= [];

  /* Solve. */
  return solve_lfit(y, xp);
}
/*---------------------------------------------------------------------------*/
func solve_lfit(y, yp, w)
/* DOCUMENT x= solve_lfit(data, ptr);
       -or- x= solve_lfit(data, ptr, wght);

     Return solution of a weighted least square linear fit:

       X= arg min sum( WGHT * (MODEL(X) - DATA)^2 )

     where the model is obtained by linear combination of the "basic
     model components" stored in the array of pointers PTR:

       MODEL(X)= X(1) * *PTR(1) + ... + X(N) * *PTR(N)

     where N=numberof(PTR).  Each component *PTR(i) must be conformable with
     the DATA array.

     If the weights WGHT array is missing, the result is the same as with
     WGHT=1.0 (actually WGHT set to any strictly positive scalar yields the
     same result).

   SEE ALSO: poly1_fit, poly2_fit. */
{
  n= numberof(yp);

  /* Apply weights (if any, also do apply scalar weight to let the user
     rescale the problem in case of overflows). */
  if (! is_void(w)) {
    yp= tmp= yp; /* Make a local copy, so that further assignations like
		    YP(i)= ... do not affect the contents of PTR for the
		    caller. */
    w= sqrt(w);  /* SIGFPE if any of W negative */
    for (i=1 ; i<=n ; i++)
      yp(i)= &(w * *yp(i));
    y*= w;
    w= tmp= [];
  }

  /* Compute left hand-side matrix A and right-hand side vector B. */
  a= array(double, n, n);
  b= array(double, n);
  one= array(1.0, dimsof(y)); /* Multiplying by this array ensure that all
				 the *YP(i) and Y have the same shape, being
				 conformable is not sufficient, because the
				 sum(...)'s below would be incorrect. */
  for (i=1 ; i<=n ; i++) {
    b(i)= sum(y * (ypi= one * *yp(i)));
    for (j=1 ; j<i ; j++) {
      a(i,j)= sum(ypi * *yp(j));
    }
    a(i,i)= 0.5 * sum(ypi * ypi); /* 0.5 because of the transpose below */
  }
  y= yp= ypi= [];
  a+= transpose(a);

  /* Solve. */
#if 0
  if (noneof(a)) {
    /* This can only appends if all weights are zero but the above check
       should be faster than checking directly W. */
    error, "bad weight W=0.0 everywhere";
  }
#endif
  return LUsolve(a, b);
}
/*---------------------------------------------------------------------------*/
require, "plot.i";

func plpwf(y, x, color=, degree=, symbol=)
/* DOCUMENT plpwf, y, x;
       -or- c= plpwf(y, x);
     plot data as points with polynomial fit.  When called as a subroutine,
     prints out the coefficients of the polynomial.  When called as a
     function, returns the polynomial coefficients.

   KEYWORDS
     DEGREE  polynomial degree, default 1.
     COLOR   color for the plot (data points and curve fit).
     SYMBOL  symbol shape (see plp).

   SEE ALSO: plp, poly1_fit. */
{
  if (is_void(degree)) degree= 1;
  c= poly1_fit(y, x, degree);
  plp, y, x, color=color, symbol=symbol;
  plg, poly1(x, c), x, color=color;
  if (am_subroutine()) print, c;
  else return c;
}
/*---------------------------------------------------------------------------*/
func poly1_gcv_fit(y, x, w, degree=, verbose=, all=, get_info=)
/* DOCUMENT c= poly1_gcv_fit(y, x);
       -or- c= poly1_gcv_fit(y, x, w);
     Apply Generalized Cross Validation criteria to select the best polynomial
     that fits Y(X).  For each increasing polynomial degree, the ability of
     the model to predict measured values is estimated by:

        GCV_CHI2 = sum_i [ W(i) * (PREDICTED(i) - Y(i))^2 ]

     where PREDICTED(i) is the value at X(i) of a model that fits all data
     points (X,Y) but the Ith one (i.e. the point (X(i),Y(i)) is missing).
     One can expect that, for too simple models (too low polynomial degrees)
     GCV_CHI2 will be large due to the inability of the model to fit the
     real behaviour of the data.  Besides, as models get more complicated,
     they tend to also fit the noise contribution in the data in, thus yield
     large value for GCV_CHI2.

     Where W is the optional third argument: weight for Y (default 1.0).

     The coefficients C returned by poly1_gcv_fit correspond to the `best'
     fit: the less complicated model for which GCV_CHI2 is minimal.

   KEYWORDS
     DEGREE   Polynomial degrees to check as: [DEGREE_MIN, DEGREE_MAX] or just
              DEGREE_MAX (DEGREE_MIN will be 0), default [0, numberof(y)-2].
     VERBOSE  Flag: print out polynomial degree and GCV value.
     ALL      Force checking of all the degrees between DEGREE_MIN and DEGREE_MAX.
              The default behaviour is to stop when the 1st minimum is found.
              Should only be used with VERBOSE=1 or GET_INFO=1.
     GET_INFO Flag: return misfit information?  The default is to return
              the coefficients of the best polynomial fit.  With this flag,
	      the result is: C(1,)= polynomial degree
	                     C(2,)= sqrt(GCV_CHI2/sum(W))

   SEE ALSO: poly1_fit. */
{
  if (numberof(x) < numberof(y)) x*= array(1.0, dimsof(y));
  if (numberof(x) > numberof(y)) y*= array(1.0, dimsof(x));
  n= numberof(x);
  if (is_void(w)) {
    w= array(1.0, dimsof(y));
  } else {
    if (numberof(w) < numberof(y)) w*= array(1.0, dimsof(y));
    if (numberof((i= where(w>0))) != n) {
      if (!is_array(i) || anyof(w<0)) error, "bad weight W";
      w= w(i);
      x= x(i);
      y= y(i);
      n= numberof(x);
    }
  }
  if (numberof(degree)==2) {
    degree_min= min(degree);
    degree_max= max(degree);
  } else if (numberof(degree)==1) {
    degree_min= 0;
    degree_max= degree;
  } else {
    degree_min= 0;
    degree_max= n-2;
  }
  if (degree_min<0) error, "DEGREE_MIN too small";
  if (degree_max>n-2) error, "DEGREE_MAX too large";
  if (get_info) {
    out= array(double, 2, degree_max-degree_min+1);
    out(1,)= indgen(degree_min:degree_max);
  }

  if (verbose) {
    s= "    DEGREE:";
    for (degree=degree_min ; degree<=degree_max ; degree++) {
      s+= swrite(format="      %-3d", degree);
    }
    write, format="%s\n%s", s, " GCV CHI2:";
  }
  
  best_chi2= -1;
  best_degree= -1;
  sw= sum(w);
  for (degree=degree_min ; degree<=degree_max ; degree++) {
    for (chi2=0.0, i=1 ; i<=n ; i++) {
      wp= w;
      wp(i)= 0;
      chi2+= w(i) * (y(i) - poly1(x(i), poly1_fit(y, x, degree, wp)))^2;
    }
    gcv_rms= sqrt(chi2 / sw);
    if (verbose) write, format=" %8.3g", gcv_rms;
    if (get_info) out(2, degree-degree_min+1)= gcv_rms;
 
    /* With same CHI2, we favor the least polynomial degree: to be retained,
       the new CHI2 must be STRICTLY LESS than the previous one. */
    if (best_chi2<0 || chi2<best_chi2) {
      best_degree= degree;
      best_chi2= chi2;
    } else if (! all) break;
  }
  if (verbose) write, "";
  if (get_info) return out;
  return poly1_fit(y, x, best_degree, w);
}
/*---------------------------------------------------------------------------*/
