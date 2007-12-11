/*
 * linalg.i -
 *
 *	Linear Algebra functions for Yorick.
 *
 *-----------------------------------------------------------------------------
 *
 *	Copyright (C) 2003-2004 Eric THIEBAUT.
 *
 *	This file is part of OptimPack.
 *
 *	OptimPack is  free software; you can redistribute  it and/or modify
 *	it under the  terms of the GNU General  Public License as published
 *	by the Free  Software Foundation; either version 2  of the License,
 *	or (at your option) any later version.
 *
 *	OptimPack is  distributed in the hope  that it will  be useful, but
 *	WITHOUT  ANY  WARRANTY;  without   even  the  implied  warranty  of
 *	MERCHANTABILITY or  FITNESS FOR A PARTICULAR PURPOSE.   See the GNU
 *	General Public License for more details.
 *
 *	You should have  received a copy of the  GNU General Public License
 *	along with  Yeti (file "COPYING"  in the top source  directory); if
 *	not, write to the Free  Software Foundation, Inc., 59 Temple Place,
 *	Suite 330,  Boston, MA  02111-1307 USA.
 *
 *-----------------------------------------------------------------------------
 *
 * History:
 *	$Id: linalg.i,v 1.1 2007-12-11 23:55:10 frigaut Exp $
 *	$Log: linalg.i,v $
 *	Revision 1.1  2007-12-11 23:55:10  frigaut
 *	Initial revision
 *
 */

func gram_schmidt_orthonormalization(b)
/* DOCUMENT gram_schmidt_orthonormalization, b;
       -or- gram_schmidt_orthonormalization(b)
     Performs Gram-Schmidt orthonormalization of basis functions B.  If
     B is an array of pointers, then the input basis vectors are *B(i)
     for i=1,..,numberof(B); otherwise, the input basis vectors are B(..,i)
     for i=1,..,dimsof(B)(0).  When called as a subroutine, the operation
     is done "in-place".
     
   SEE ALSO: SVdec. */
{
  local b_i, b_j;

  /* NOTE: type conversion of array X, e.g. double(X), is a no-operation
     if X already of given type. */
  if (! is_array(b)) error, "expecting array parameter";
  type = double;
  if ((s = structof(b)) == pointer) {
    ptr = 1;
    n = numberof(b);
    if (! am_subroutine()) b = b; /* make a private copy */
    for (i=1 ; i<=n ; ++i) {
      if ((s = structof(*b(i))) == complex) type = complex;
      else if (s != double && s != float && s != long && s != int &&
               s != short && s != char) error, "bad data type";
    }
  } else {
    ptr = 0;
    n = dimsof(b)(0);
    if ((s = structof(b)) == complex) type = complex;
    else if (s != double && s != float && s != long && s != int &&
             s != short && s != char) error, "bad data type";
    if (s != type) {
      if (am_subroutine()) error, "bad data type for in-place conversion";
      b = type(b);
    }
  }
  if (type == complex) error, "complex data type not yet implemented";
  
  /* Gram-Schmidt Orthonormalization. */
  for (i=1 ; i<=n ; ++i) {
    /* get i-th basis vector */
    if (prt) eq_nocopy, b_i, type(*b(i));
    else     b_i = b(.., i);
    
    /* make the i-th vector othogonal to previous ones */
    if (i > 1) {
      if (prt) eq_nocopy, b_j, type(*b(1));
      else     b_j = b(.., 1);
      s = sum(b_i*b_j)*b_j;
      for (j=2 ; j<i ; ++j) {
        if (prt) eq_nocopy, b_j, type(*b(j));
        else     b_j = b(.., j);
        s += sum(b_i*b_j)*b_j;
      }
      b_i -= s;
    }

    /* normalize i-th vector */
    s = sum(b_i*b_i);
    if (ptr) {
      b(i) = &((s > 0.0 ? (1.0/sqrt(s))*b_i : array(double, dimsof(b_i))));
    } else {
      b(..,i) = (s > 0.0 ? (1.0/sqrt(s))*b_i : array(double, dimsof(b_i)));
    }
  }
  return b;
}

func trace(a)
/* DOCUMENT trace(a)
     Returns the trace of matrix A.
     
   SEE ALSO: diag. */
{
  if (! is_array(a) || (dims = dimsof(a))(1) != 2)
    error, "expecting a 2D array";
  m = dims(2);
  n = dims(3);
  return a(sum:1:m*min(m,n):m+1);
}

func diag(a)
/* DOCUMENT diag(a)
     Returns the diagonal of matrix A (if A is a 2D array) or a square
     diagonal matrix with diagonal elements equal to those of vector A (if
     A is a 1D array).
     
   SEE ALSO: trace, unit. */
{
  if (is_array(a)) {
    if ((dims = dimsof(a))(1) == 1) {
      m = dims(2);
      (mx = array(structof(a), m, m))(1:m*m:m+1) = a;
      return mx;
    } else if (dims(1) == 2) {
      m = dims(2);
      n = dims(3);
      return a(1:m*min(m,n):m+1);
    }
  }
  error, "expecting a 1D or 2D array";
}

func euclidean_norm(x)
/* DOCUMENT euclidean_norm(x)
    Returns the Euclidian norm of X: sqrt(sum(X*X)), taking care of overflows.
*/
{
  if (! (s = max(-min(x), max(x)))) return 0.0;
  x *= 1.0/s;
  return s*sqrt(sum(x*x));
}

func pm(a, ndigits=, file=, format=)
/* DOCUMENT pm, a;
     Prints matrix A.  Keyword FILE can be set to the name/stream of output
     file; the default is to use standard output.  If keyword NDIGITS is
     set, floating-point values get printed with that number of significant
     digits; alternatively, keyword FORMAT may be set with the format for
     each element of A -- for complex numbers, the real and imaginary parts
     use the same format.
     
   SEE ALSO: write. */
{
  if (! is_array(a) || (dims = dimsof(a))(1) != 2) {
    error, "expecting a 2-dimensional array";
  }
  m = dims(2);
  n = dims(3);
  s = structof(a);
  if (s == complex) {
    if (is_void(format)) {
      if (is_void(ndigits)) ndigits = 5;
      format = swrite(format="%%.%dg+%%.%dgi", ndigits, ndigits);
    } else {
      format += "+" + format + "i";
    }
    a = swrite(format=format, double(a), a.im);
  } else if (s == pointer) {
    a = swrite(a);
  } else {
    if (is_void(format)) {
      if (s == double) {
        if (is_void(ndigits)) ndigits = 5;
        format = swrite(format="%%.%dg", ndigits);
      } else if (s == float) {
        if (is_void(ndigits)) ndigits = 3;
        format = swrite(format="%%.%dg", ndigits);
      } else if (s == long || s == int || s == short) {
        format = "%d";    
      } else if (s == char) {
        format = "0x%02x";
      } else if (s == string) {
        /* should escape quotes in input strings */
        format = "\"%s\"";
      } else {
        error, "bad data type";
      }
    }
    a = swrite(format=format, a);
  }
  if (structof(file) == string) file = open(file, "w");
  cols = swrite(format="(,%d)",indgen(n));
  rows = swrite(format="(%d,)",indgen(m));
  fmt0 = swrite(format="%%%ds", max(strlen(rows)));
  fmt1 = swrite(format=" %%-%ds", max(max(strlen(cols)), max(strlen(a))));
  fmt2 = " %s\n";
  write, file, format=fmt0, "";
  for (j=1 ; j<n ; ++j) write, file, format=fmt1, cols(j);
  write, file, format=fmt2, cols(n);
  for (i=1 ; i<=m ; ++i) {
    write, file, format=fmt0, rows(i);
    for (j=1 ; j<n ; ++j) write, file, format=fmt1, a(i,j);
    write, file, format=fmt2, a(i,n);
  }
}


/*---------------------------------------------------------------------------*/
/* Cholesky Decomposition */

func cholesky(a, raw)
/* DOCUMENT cholesky(a)
       -or- cholesky(a, 0/1)
       
     Given  a  symmetric  positive  definite  matrix  A,  returns  a  lower
     triangular  matrix C  which is  the Cholesky  decomposition of  A such
     that:

       A = C(+,)*C(+,);

     If  optional  second argument  is  true  (non-nil  and non-zero),  the
     scratch values in  the upper triangular part of  C are left unchanged;
     otherwise (the  default behavior), the  upper triangular part of  C is
     filled with zeros.
*/
{
  if (! is_array(a) || structof(a) == complex ||
      (dims = dimsof(a))(1) != 2 || (n = dims(2)) != dims(3))
    error, "expecting a N × N non-complex array";
  a = double(a);

  if ((s = a(1,1)) <= 0.0) error, "the matrix is not positive definite";
  a(1,1) = sqrt(s);
  for (j=2 ; j<=n ; ++j) {
    a(1,j) = (t = a(1,j)/a(1,1));
    s = t*t;
    for (k=2 ; k<j ; ++k) {
      rng = 1:k-1;
      a(k,j) = (t = (a(k,j) - sum(a(rng,k)*a(rng,j)))/a(k,k));
      s += t*t;
    }
    s = a(j,j) - s;
    if (s <= 0.0) error, "the matrix is not positive definite";
    a(j,j) = sqrt(s);
  }
#if 0 /* slower code (in Yorick) but less obscure */ 
  for (j=1 ; j<=n ; ++j) {
    s = 0.0;
    for (k=1 ; k<j ; ++k) {
      if (k == 1) {
        t = a(1,j) / a(1,1);
      } else {
        rng = 1:k-1;
        t = (a(k,j) - sum(a(rng,k)*a(rng,j))) / a(k,k);
      }
      a(k,j) = t;
      s += t*t;
    }
    s = a(j,j) - s;
    if (s <= 0.0) error, "the matrix is not positive definite";
    a(j,j) = sqrt(s);
  }
#endif
  if (! raw) {
    for (k=1 ; k<n ; ++k) a(k+1, 1:k)=0;
  }
  return a;
}


/*---------------------------------------------------------------------------*/
/* Singular Vlaue Decomposition */

local sv_intro;
/* DOCUMENT sv_intro - introduction to SVD Yorick package
 *
 * Notes about matrix multiplication in Yorick:
 *
 *   A.B  = A(,+)*B(+,)
 *   A'.B = A(+,)*B(+,)  // transpose of A times B
 *   A.B' = A(,+)*B(,+)  // A times transpose of B
 *
 *   diag(s).A  = diag(s)(,+)*A(+,)
 *              = s*A = A*s
 *
 *   diag(s).A' = diag(s)(,+)*A(,+)
 *              = transpose(s(-,)*A) = transpose(A*s(-,))
 *
 *   A.diag(s)  = A(,+)*diag(s)(+,)
 *              = A*s(-,) = s(-,)*A
 *
 *   A'.diag(s) = A(+,)*diag(s)(+,) = A(+,)*diag(s)(,+)
 *              = transpose(A*s) = transpose(s*A)
 *
 * Singular Value Decomposition:
 *
 *   A = U.diag(SIGMA).V' = U.diag(SIGMA).VT
 *     = (U*SIGMA(-,))(,+)*VT(+,)
 *     = U(,+)*(SIGMA*VT)(+,)
 *
 * where:
 *
 *   SIGMA = SVdec(A, U, VT)
 *
 * Columns  of  U  and  V form  an  orthonormal basis  (i.e.  U  and V  are
 * column-orthonormal):
 *
 *   U'.U = V'.V = I
 *
 * in Yorick notation:
 *
 *   U(+,)*U(+,) = V(+,)*V(+,) = VT(,+)*VT(,+) = unit(N)
 *
 * Note (to be verified): if U and/or V are square, they are also
 * row-orthonormal:
 *
 *   U'.U = U.U' = I   (if U is square)
 *   V'.V = V.V' = I   (if V is square)
 *
 * Generalized-inverse of A:
 *
 *   AP = V.diag(1/SIGMA).U' = VT'.diag(1/SIGMA).U'
 *     = VT(+,)*((1.0/SIGMA)(-,)*U)(,+)
 *     = ((1.0/SIGMA)*VT)(+,)*U(,+)
 *
 * Solving a linear problem: A.x ~ b with SVD (taking care of index
 * ordering for faster matrix multiplication):
 *
 *   X = V.diag(W).U'.B
 *     = (W*VT)(+,)*U(,+))(,+)*B(+,..)
 *     = (W*VT)(+,)*(U(+,)*B(+,..))(+,..)  // sum over 1st indices: faster
 *
 * where W is an approximation of 1/SIGMA.
 *
 * SEE ALSO: sv_dcmp, sv_solve_trunc, sv_solve_wiener, SVdec, SVsolve.
 */

func sv_dcmp(a, full)
/* DOCUMENT sv_dcmp(a)
       -or- sv_dcmp(a, full)
     Computes the singular value decomposition of matrix A.  The result
     is an array of pointers:

         [&SIGMA, &U, &VT]

     where SIGMA is the vector of  singular values of A, and the columns of
     U  and  the  rows of  VT  are  the  left  and right  singular  vectors
     (respectively).  Using matrix notation:

         A = U.diag(SIGMA).V' = U.diag(SIGMA).VT

     and using Yorick notation:

         A = (U*SIGMA(-,))(,+)*VT(+,)
           = U(,+)*(SIGMA*VT)(+,)

     FULL has the same meaning as keyword FULL in SVdec (to see).
     
   SEE ALSO: sv_dcmp, sv_solve_trunc, sv_solve_wiener, SVdec. */
{
  local u, vt;
  sigma = SVdec(a, u, vt, full=full);
  return [&sigma, &u, &vt];
}

func _sv_get_dcmp(a, full)
/* DOCUMENT local sigma, u, vt; _sv_get_dcmp(a);
       -or- local sigma, u, vt; _sv_get_dcmp(a, full)
     Private routine used to extract Singular Value Decomposition of A into
     external symbols: SIGMA, U and VT.
     
   SEE ALSO: sv_dcmp, sv_solve_trunc, sv_solve_wiener. */
{
  extern sigma, u, vt;
  if (structof(a) == pointer) {
    eq_nocopy, sigma, *a(1);
    eq_nocopy, u,     *a(2);
    eq_nocopy, vt,    *a(3);
  } else {
    sigma = SVdec(a, u, vt, full=full);
  }
}

func sv_solve_trunc(a, b, eps, full=)
/* DOCUMENT sv_solve_trunc(a, b, eps)
     Solve linear problem A.x = b by truncated singular value method.  A is
     either a matrix or the  singular value value decomposition as returned
     by sv_dcmp  (to see).  B  is the right  hand side vector (or  array to
     solve for  several right  hand sides  at a time).   EPS (in  the range
     [0,1])  is the  minimum  relative  singular value  to  keep, i.e.  all
     singular values  less than EPS*max(SIGMA) get discarded  (SIGMA is the
     vector of singular values of A).  The result is:
     
         (W*VT)(+,)*(U(+,)*B(+,..))(+,..)
       
     where  SIGMA,  U and  VT  are the  components  of  the singular  value
     decomposition of A and W is:

         W(i) = 1/SIGMA(i)   if SIGMA(i) > EPS*max(SIGMA);
                0            otherwise.
     
   SEE ALSO: sv_dcmp, sv_intro, sv_solve_wiener. */
{
  local sigma, u, vt;
  _sv_get_dcmp, a, full;
  if (is_void(eps)) eps = 1e-1;
  w = double(sigma > eps*sigma(1))/(sigma + !sigma);
  return (w*vt)(+,)*(u(+,)*b(+,..))(+,..);
}

func sv_solve_wiener(a, b, eps, full=)
/* DOCUMENT sv_solve_wiener(a, b, eps)
     Solve  linear problem  A.x =  b by  Wiener filtering  of  the singular
     values.   A   is  either  a   matrix  or  the  singular   value  value
     decomposition as  returned by sv_dcmp (to  see).  B is  the right hand
     side  vector (or  array to  solve for  several right  hand sides  at a
     time).  EPS (in the range [0,1]) is the relative singular value filter
     level.  The result is:
     
         (W*VT)(+,)*(U(+,)*B(+,..))(+,..)
       
     where  SIGMA,  U and  VT  are the  components  of  the singular  value
     decomposition of A and W is:

         W = SIGMA/(SIGMA^2 + (EPS*max(SIGMA))^2)
     
   SEE ALSO: sv_dcmp, sv_intro, sv_solve_trunc. */
{
  local sigma, u, vt;
  _sv_get_dcmp, a, full;
  if (is_void(eps)) eps = 1e-1;
  w = sigma/(sigma*sigma + (eps*sigma(1))^2);
  return (w*vt)(+,)*(u(+,)*b(+,..))(+,..);
}

/*---------------------------------------------------------------------------*/
