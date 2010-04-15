/*
 * emulate_yeti.i --
 *
 *	Interpreted functions which emulate some builtin functions of Yeti.
 *
 * Copyright (c) 2005-2007, Eric Thiebaut, Observatoire de Lyon (France).
 *
 * Routines:
 *	swap         - exchanges contents of two variables.
 *	unref        - returns X, destroying X in the process.
 *	is_vector    - check if argument is a vector.
 *	is_matrix    - check if argument is a matrix.
 *	is_real      - check if argument is of float or double type.
 *	is_complex   - check if argument is of complex type.
 *	is_integer   - check if argument is of integer type.
 *	is_numerical - check if argument is of numerical type.
 *	make_dimlist - build-up dimension list
 *
 * History:
 *	$Id: emulate_yeti.i,v 1.3 2010-04-15 17:17:37 frigaut Exp $
 *	$Log: emulate_yeti.i,v $
 *	Revision 1.3  2010-04-15 17:17:37  frigaut
 *
 *	- protected re-definition of new yorick builtins in emulate_yeti.i
 *
 *	Revision 1.2  2010/04/07 06:15:23  paumard
 *	- remove strchr, strrchr, is_scalar and is_vector, they are in string.i (beware, no autoloads) ;
 *	- move is_integer_scalar from utils.i to emulate_yeti.i
 *	
 *	Revision 1.1  2010/04/06 15:36:09  paumard
 *	- move emulate-yeti*.i to emulate_yeti*.i
 *	
 *	Revision 1.2  2010/04/06 14:21:51  paumard
 *	- move strlower & strupper from utils.i to emulate-yeti.i;
 *	- move round from util_fr.i to emulate-yeti.i;
 *	- round returns a double, like the Yeti implementation;
 *	- review autoloads (adding emulate-yeti_start.i);
 *	- add missing files to Makefile.
 *	
 *	Revision 1.1  2010/02/10 13:27:12  paumard
 *	- Synchronize files with Eric Thiebaut's: fft_utils.i, img.i, plot.i, utils.i.
 *	- Import emulate_yeti.i
 *	- Remove basename() and dirname(), standard in pathfun.i.
 *	- Remove the accents in Erics name to prevent a crash on amd64.
 *	
 *	Revision 1.1  2007/04/24 06:58:44  eric
 *	Initial revision
 *
 *
 *-----------------------------------------------------------------------------
 */

if (is_func(yeti_init)) {
  error, "This file define functions redundant with Yeti package.";
}

func is_integer_scalar(x)
/* DOCUMENT is_integer_scalar(x)
     Check whether or not X is an integer scalar.

   SEE ALSO is_scalar, is_integer. */
{ return (((s=structof(x))==long || s==int || s==short || s==char) &&
          ! dimsof(x)(1)); }

// The following requires Yorick >= 1.6.02
func strlower(s) { return strcase(0, s); }
func strupper(s) { return strcase(1, s); }
/* DOCUMENT strlower(s)
       -or- strupper(s)
     Convert (array of) string(s) S to lower/upper case letters.

   SEE ALSO strcase */


local make_dimlist;
func grow_dimlist(&dimlist, arg)
/* DOCUMENT make_dimlist, dimlist, next_argument;
 * 
 *   Builds a dimension list DIMLIST, as used in the array function
 *   (which see).  Use like this (all extra arguments in your function
 *   are considered as dimensions or dimension lists):
 *   
 *     func your_function(arg1, arg2, etc, ..)
 *     {
 *       dimlist = [0];
 *       while (more_args()) make_dimlist, dimlist, next_arg();
 *       ...
 *     }
 *   
 *   After this, DIMLIST will be an array of the form [ndims, dim1,
 *   dim2,...], compounded from the multiple arguments in the same way
 *   as the array function.  Another possibility is to define your
 *   function as:
 *
 *     func your_function(arg1, arg2, etc, dimlist, ..)
 *     {
 *       while (more_args()) make_dimlist, dimlist, next_arg();
 *       ...
 *     }
 *
 *   But in this  latter case, if no DIMLIST  arguments given, DIMLIST will
 *   be [] instead of [0], which  will act the same in most situations.  If
 *   that possibility is unacceptable, you may add
 *
 *     if (is_void(dimlist)) dimlist= [0];
 *
 *   before/after the while loop.
 *   
 *   
 * SEE ALSO:
 *   array, build_dimlist.
 */
{
  if (is_array(arg)) {
    if (structof((n = arg(1)+0)) == long) {
      if (! (d1 = dimsof(arg)(1))) {
        if (is_void(dimlist)) {
          dimlist = [1, n];
        } else {
          grow, dimlist, n;
          ++dimlist(1);
        }
        return;
      } else if (d1 == 1) {
        if (is_void(dimlist)) {
          dimlist = long(arg);
          return;
        } else {
          if (n == numberof(arg)-1) {
            grow, dimlist, long(arg(2:0));
            dimlist(1) += n;
            return;
          }
        }
      }
      error, "bad dimension list";
    }
  } else if (is_void(arg)) {
    if (is_void(dimlist)) dimlist = [0];
    return;
  }
  error, "bad data type in dimension list";
}
if (is_func(make_dimlist) != 2) {
  make_dimlist = grow_dimlist; /* for old code */
}

//========================================================================
// the following are builtin function in yorick 2.1.05x, > april 2010.
// we will only redefine these functions if they don't already exist


func __unref(&x) /* interpreted version */
/* DOCUMENT unref(x)
     returns X, destroying X in the process (useful to deal with temporary
     big arrays).  Written after Yorick's FAQ.
   SEE ALSO: eq_nocopy, swap. */
{
  local y;
  eq_nocopy, y, x;
  x = [];
  return y;
}
if (typeof(unref)!="builtin") unref=__unref;


func __swap(&a, &b) /* interpreted version */
/* DOCUMENT swap, a, b;
     Exchanges  the contents  of variables  A and  B without  requiring any
     temporary copy.
   SEE ALSO: eq_nocopy, unref. */
{
  local tmp;
  eq_nocopy, tmp, a;
  eq_nocopy, a, b;
  eq_nocopy, b, tmp;
}
if (typeof(swap)!="builtin") swap=__swap;


func __is_matrix(x) { return (is_array(x) && dimsof(x)(1) == 2); }
/* DOCUMENT is_matrix(x)
 *   Returns true if X is a matrix (i.e. a 2-D array).
 *
 *  SEE ALSO: dimsof, is_array, is_integer, is_scalar, is_vector
 */
if (typeof(is_matrix)!="builtin") is_matrix=__is_matrix;


func __is_integer(x)   {return ((s=structof(x))==long || s==int || s==char ||
                              s==short);}
func __is_real(x)      {return ((s=structof(x))==double || s==float);}
func __is_complex(x)   {return structof(x)==complex;}
func __is_numerical(x) {return ((s=structof(x))==long || s==double || s==int ||
                              s==char || s==complex || s==short || s==float);}
func __is_string(x)    { return structof(x)==string;}
/* DOCUMENT is_integer(x)
 *     -or- is_real(x)
 *     -or- is_complex(x)
 *     -or- is_numerical(x)
 *     -or- is_string(x)
 *   These functions  return true if  X is an  array of type:  integer, real
 *   (i.e.  double or  float), complex,  numerical (i.e.  integer,  real or
 *   complex) or string.
 *
 * SEE ALSO: structof, dimsof, is_array, is_scalar.
 */
if (typeof(is_integer)!="builtin")   is_integer   =__is_integer;
if (typeof(is_real)!="builtin")      is_real      =__is_real;
if (typeof(is_complex)!="builtin")   is_complex   =__is_complex;
if (typeof(is_numerical)!="builtin") is_numerical =__is_numerical;
if (typeof(is_string)!="builtin")    is_string    =__is_string;


func __round(arg)
/* DOCUMENT round(arg)
 * Returns the rounded version of a floating point argument
 * modified 2007dec06 to fix problem with negative numbers
 * F.Rigaut 2001/10, Return double TP 2010/04
 * SEE ALSO: ceil, floor
 */
{return double(long(arg+0.5)-(arg<0));}
if (typeof(round)!="builtin") round =__round;


 
/*---------------------------------------------------------------------------*/

#if 0 /* obsolete since Yorick 1.6 */

local _strlower, _strupper;
/* DOCUMENT local _strlower, _strupper;
     Private arrays to convert char to upper/lowercase letters.
   SEE ALSO strlower, strupper */
(_strlower = char(indgen(0:255)))(1+'A':1+'Z') = _strlower(1+'a':1+'z');
(_strupper = char(indgen(0:255)))(1+'a':1+'z') = _strupper(1+'A':1+'Z');

local strlower, strtolower; /* needed for documentation */
func __strlower(s) /* interpreted version */
/* DOCUMENT strlower(s)
       -or- strtolower(s)
     Convert a string or an array of strings S to lower case letters.
   SEE ALSO strupper */
{
  /* fool codger */ extern _strlower;
  n = numberof((r = array(string, dimsof(s))));
  for (i=1; i<=n; ++i) r(i)= string(&_strlower(1+*pointer(s(i))));
  return r;
}

local strupper, strtoupper; /* needed for documentation */
func __strupper(s) /* interpreted version */
/* DOCUMENT strupper(s)
       -or- strtoupper(s)
     Convert a string or an array of strings S to upper case letters.
   SEE ALSO strlower */
{
  /* fool codger */ extern _strupper;
  n = numberof((r = array(string, dimsof(s))));
  for (i=1; i<=n; ++i) r(i)= string(&_strupper(1+*pointer(s(i))));
  return r;
}

/* replace non-builtin functions by interpreted ones */
if (is_func(strupper) != 2) strupper = __strupper;
if (is_func(strlower) != 2) strlower = __strlower;
if (is_func(strtoupper) != 2) strtoupper = strupper;
if (is_func(strtoupper) != 2) strtolower = strtolower;

#endif

/*---------------------------------------------------------------------------*
 * Local Variables:                                                          *
 * mode: Yorick                                                              *
 * tab-width: 8                                                              *
 * fill-column: 75                                                           *
 * coding: latin-1                                                           *
 * End:                                                                      *
 *---------------------------------------------------------------------------*/
