/*
 * utils.i --
 *
 *	General purpose utility routines for Yorick.
 *
 * Copyright (c) 1996-2004, Eric Thiébaut, Observatoire de Lyon (France).
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
 *      eval         - evaluate textual code.
 *	pwd          - print/get working directory.
 *	unref        - returns X, destroying X in the process.
 *	swap         - exchanges contents of two variables.
 *	grow_dimlist - build dimension list.
 *	reform       - change dimension list of an array.
 *	undersample  - shrink array dimension(s).
 *	spline_zoom  - resize an array by cubic spline interpolation
 *	map          - apply function to every elements of array or list.
 *	str(to)upper - convert string(s) to upper case letters.
 *	str(to)lower - convert string(s) to lower case letters.
 *	strcut       - cut text to fit into lines of given length.
 *	strjoin      - make array of strings into a single string.
 *	is_scalar    - check if argument is a scalar.
 *	is_vector    - check if argument is a vector.
 *	is_matrix    - check if argument is a matrix.
 *	is_real      - check if argument is of float or double type.
 *	is_complex   - check if argument is of complex type.
 *	is_integer   - check if argument is of integer type.
 *	is_numerical - check if argument is of numerical type.
 *	is_integer_scalar - check if argument is a scalar of integer type.
 *      expand_file_name - expand leading tilde in file name(s).
 *	get_file_name - get path name of file associated with a stream.
 *	protect_file_name - protect special characters in file name.
 *	read_ascii   - read ascii numeric data
 *	load_text    - load all lines of a text file.
 *	dump_text    - dump string array in a text file.
 *	guess_compression - guess compression method of existing file
 *	xopen        - extended open with (de)compression, primitives, ...
 *	stat         - display statistics/info about symbols/expressions.
 *	smooth       - smooth array along its dimensions.
 *      pdb_list     - lists contents of PDB binary file.
 *      pdb_restore_all - restore all non-record variables of a PDB file.
 *      timer_start  - (re)start the profiling timer.
 *      timer_elapsed - get/print the elapsed time since timer_start.
 *
 * History:
 *	$Id: utils.i,v 1.1 2007-12-11 23:55:13 frigaut Exp $
 *	$Log: utils.i,v $
 *	Revision 1.1  2007-12-11 23:55:13  frigaut
 *	Initial revision
 *
 *	Revision 1.12  2004/10/14 09:51:37  eric
 *	 - Unused (to my knowledge) function "filenameof" removed,
 *	   it is superseded by "get_file_name".
 *	 - New functions: "expand_file_name", "get_file_name",
 *	   and "protect_file_name".
 *
 *	Revision 1.11  2004/08/31 07:26:17  eric
 *	 - New routines for PDB files: pdb_list, pdb_restore_all.
 *	 - New routines for profiling: timer_start, timer_elapsed.
 *
 *	Revision 1.10  2003/06/03 20:37:36  eric
 *	 - Function map() now accept a list argument (as _map) and use
 *	   weird names for local variables to avoid name clash.
 *	 - New functions: xopen, guess_compression, read_ascii,
 *	   load_text, and dump_text.
 *
 *	Revision 1.9  2002/11/22 08:24:27  eric
 *	 - slight changes in eval() code
 *	 - fix list of routines in leading comments of this file
 *
 *	Revision 1.8  2002/11/20 09:22:15  eric
 *	 - take care of not overwriting builtin functions (unref, strlower,
 *	   strupper, ...)
 *	 - new function: grow_dimlist
 *
 *	Revision 1.7  2002/07/25 10:52:32  eric
 *	- New function: eval.
 *
 *	Revision 1.6  2002/07/01 09:41:45  eric
 *	 - New function: pwd.
 *
 *	Revision 1.5  2002/06/06 14:19:42  eric
 *	 - New function: undersample.
 *
 *	Revision 1.4  2002/02/22 16:19:24  eric
 *	 - Change (one more time) names of str(to)upper/lower functions to
 *         avoid clash with builtin Yeti routines.
 *	 - Add "unref" routines (after Yorick's FAQ, so the true author is
 *         Dave, not me).
 *
 *	Revision 1.3  2001/12/08 22:33:37  eric
 *	 - stat(): computation of standard deviation improved.
 *
 *	Revision 1.2  2001/11/26 08:17:11  eric
 *	 - Functions strto{lower,upper} renamed as str{lower,upper}.
 *
 *	Revision 1.1  2001/03/23 16:20:52  eric
 *	Initial revision
 *
 *-----------------------------------------------------------------------------
 */

func eval(code, tmp=, debug=)
/* DOCUMENT eval, code;
       -or- eval(code);       
     Evaluates CODE given as a string or as an array of strings (considered
     as  different lines  in the  script).  Since  CODE can  be dynamically
     build,   this  routine   allows  the   execution  of   virtually  (see
     hints/restrictions below)  any Yorick's code  (e.g. dynamic definition
     of  structures,  of functions,  etc.).   For  instance, the  following
     statement defines a new structure:
       eval, "struct NewStruct {string a; long b; float c, d;}";

     Since  the script  gets evaluated  at the  scope level  of  the "eval"
     routine some local variables of the  "eval" routine may be used in the
     script:
       "eval_tmp"    contains  the  name of  the temporary script  file and
                     must not be changed by the script;
       "eval_debug"  contains the value of  the keyword DEBUG and  must not
                     be changed by the script;
       "eval_code"   contains the value of the argument CODE;
       "eval_result" is  returned by  "eval", its  contents may  be defined
                     into the script to provide a returned value.
     Note: impredictible  results may  occur if CODE  changes the  value of
     symbols "eval_tmp" and "eval_debug".

     Keyword TMP  can be  used to  specify the file  name of  the temporary
     script.  The default file name is:
       "$YORICK_EVAL_TMP"      if environment variable "YORICK_EVAL_TMP" is
                               set;
       "/tmp/$USER-eval_tmp.i" if environment variable "USER" set;
       "~/.eval_tmp.i"         otherwise.
     If  keyword DEBUG  is true  (non-zero and  non-nil), the  name  of the
     temporary file is printed out and the file is not removed.


   SEE ALSO: include. */
{
  /* Dump script into a temporary file. */
  if (is_void(tmp)) {
    /* Create default name for yorick temporary code. */
    tmp = get_env("YORICK_EVAL_TMP");
    if (! tmp) {
      tmp = get_env("USER");
      tmp = (tmp ? "/tmp/"+tmp+"-" : "~/.") + "eval_tmp.i";
    }
  }
  write, format="%s\n", open(tmp, "w"), code;
  
  /* Use "eval_" prefix in order to somewhat protect local variables
     from caller's code. */
  local eval_result, eval_tmp, eval_code, eval_debug;
  eq_nocopy, eval_tmp,   tmp;
  eq_nocopy, eval_debug, debug;
  eq_nocopy, eval_code,  code;

  /* Source script and return result. */
  include, eval_tmp, 1;
  if (eval_debug) write, format="Yorick code written in \"%s\"\n", eval_tmp;
  else remove, eval_tmp;
  return eval_result;
}

func pwd(nil)
/* DOCUMENT pwd
       -or- pwd()
     Prints out (subroutine form) or returns (function form) full path
     of current working directory.

   SEE ALSO: cd, lsdir. */
{
  if (! is_void(nil)) error, "unexpected non-nil argument";
  dir = cd(".");
  if (am_subroutine()) write, format="%s\n", dir;
  else return dir;
}

local unref; /* needed for documentation */
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
/* replace non-builtin function by interpreted one */
if (is_func(unref) != 2) unref =  __unref;

local swap; /* needed for documentation */
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
/* replace non-builtin function by interpreted one */
if (is_func(swap) != 2) swap =  __swap;

func grow_dimlist(&dimlist, arg)
/* DOCUMENT grow_dimlist, dimlist, next_argument
     builds a dimension  list DIMLIST, as used in  the array function.  Use
     like  this (all  extra arguments  in your  function are  considered as
     dimensions or dimension lists):
       func your_function(arg1, arg2, etc, ..)
       {
         dimlist = [0];
         while (more_args()) grow_dimlist, dimlist, next_arg();
         ...
       }
     After    this,   DIMLIST   will    be   an    array   of    the   form
     [ndims, dim1, dim2,...], compounded from the multiple arguments in the
     same way as the array function.  Another possibility is to define your
     function as:
       func your_function(arg1, arg2, etc, dimlist, ..)
       {
         while (more_args()) grow_dimlist, dimlist, next_arg();
         ...
       }
     But in this  latter case, if no DIMLIST  arguments given, DIMLIST will
     be [] instead of [0], which  will act the same in most situations.  If
     that possibility is unacceptible, you may add
       if (is_void(dimlist)) dimlist= [0];
     before/after the while loop.


   SEE ALSO: array, build_dimlist */
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

func reform(x, ..)
/* DOCUMENT reform(x, dimlist, ...);
       -or- reform(x);
     returns array X reshaped according to dimension list DIMLIST.  Without
     DIMLIST, discards all dimensions equal to 1.  In most cases, prefer
     this to reshape.

   SEE ALSO: array, dimsof, grow_dimlist. */
{
  if (more_args()) {
    /* Build new dimension list. */
    dims = [0];
    while (more_args()) grow_dimlist, dims, next_arg();
  } else {
    /* Discard all dimensions equal to 1 */
    if (numberof(x) == 1) return x(1);
    dims = dimsof(x)(2:);
    dims = dims(where(dims>1));
    dims = [numberof(dims), dims];
  }
  y = array(structof(x), dims);
  y(*) = x(*);   /* will blow up if lengths differ */
  return y;
}

func undersample(a, nsub, which=, op=)
/* DOCUMENT undersample(a, nsub)
     Returns  array A  with all  (some)  dimensions divided  by NSUB.   The
     dimensions of interest must be a multiple of NSUB.

     Keyword  WHICH can  be used  to  specify the  dimension(s) to  shrink.
     Values  in  WHICH  less  or  equal  zero are  counted  from  the  last
     dimension.  By default, all dimensions of A get undersampled.

     Keyword OP can  be used to specify the range operator  to apply to the
     sets of NSUB adjacent values along the considered dimensions:
       OP=sum   to sum the values
       OP=avg   to average values
       OP=min   to keep the smallest value
       OP=max   to keep the largest value
     By default,  the median is  taken (WARNING: with the  median operator,
     the result depends in which order the dimensions of A are considered).

   SEE ALSO: median. */
{
  if (nsub < 1) error, "NSUB must be >= 1";
  if (nsub == 1) return a;
  if (! is_array(a)) error, "expecting an array";
  rank = dimsof(a)(1);
  if (is_void(which)) {
    which = indgen(rank);
  } else {
    which += (which <= 0)*rank;
    if (structof(which) != long)
      error, "bad data type for keyword WHICH";
    if (min(which) < 1 || max(which) > rank)
      error, "out of range dimension in keyword WHICH";
  }
  nw = numberof(which);
  noop = is_void(op); /* take median value */
  if (! noop && typeof(op) != "range") error, "OP must be nil or a range operator";
  dims = array(rank+1, rank+2);
  for (k=1 ; k<=nw ; ++k) {
    this = which(k);
    if (this != 1) a = transpose(a, [1,this]);
    dims(2:) = dimsof(a);
    dims(2) = nsub;
    dims(3) = dims(3)/nsub;
    (tmp = array(structof(a), dims))(*) = a(*);
    if (noop) {
      a = median(tmp, 1);
    } else {
      a = tmp(op,..);
    }
    tmp = []; /* free some memory */
    if (this != 1) a = transpose(a, [this,1]);
  }
  return a;
}

func spline_zoom(a, factor, rgb=)
/* DOCUMENT spline_zoom(a, fact)
     Return an array obtained by cubic spline interpolation of A with all
     its dimension multiplied by a factor FACT.  If keyword RGB is true the
     first dimsion of A is left unchanged.  If keyword RGB is not
     specified, then it is considered as true if A is a 3 dimensional array
     of 'char' with its first dimension equal to 3.
     
   SEE ALSO: spline, transpose. */
{
  if (! is_func(spline)) require, "spline.i";
  dims = dimsof(a);
  ndims = dims(1);

  if (is_void(rgb)) {
    /* RGB image? */
    rgb = (structof(a) == char && ndims == 3 && dims(2) == 3);
  }
  if (rgb) {
    a = transpose(a, 0);
    k = 1;
  } else {
    k = 0;
  }
  while (++k <= ndims) {
    dims = dimsof(a);
    n0 = dims(2);
    n1 = max(long(factor*n0 + 0.5), 1);
    if (n1 == 1) {
      a = a(avg,..)(..,-);
    } else {
      if (n1 != n0) {
        dims(2) = n1;
        b = array(double, dims);
        x0 = indgen(0:n0-1);
        x1 = indgen(0:n1-1)*((n0 - 1.0)/(n1 - 1.0));
        n = numberof(a)/n0;
        for (i=1 ; i<=n ; ++i) b(,i) = spline(a(,i), x0, x1);
        eq_nocopy, a, b;
      }
      if (ndims > 1) a = transpose(a, 0);
    }
  }
  if (rgb) return char(max(min(floor(a + 0.5), 255.0), 0.0));
  return a;
}

func map(__map__f, __map__x)
/* DOCUMENT map(f, input)
     Map scalar function F onto array/list argument INPUT to mimics
     element-wise unary operation.
   SEE ALSO: _map. */
{
  /* all locals here must have weird names, since the user's function may
     rely on external variables for arguments not varying in the source,
     or for accumulated outputs */
  if (is_array(__map__x)) {
    __map__y = array(structof((__map__i = __map__f(__map__x(1)))),
                     dimsof(__map__x));
    __map__y(1) = __map__i;
    __map__n = numberof(__map__x);
    for (__map__i=2 ; __map__i<=__map__n ; ++__map__i) {
      __map__y(__map__i) = __map__f(__map__x(__map__i));
    }
  } else if ((__map__n = typeof(__map__x)) == "list") {
    __map__y = __map__i = _lst(__map__f(_car(__map__x)));
    for (__map__x = _cdr(__map__x) ; ! is_void(__map__x) ;
         __map__x = _cdr(__map__x)) {
      _cat, __map__i, _lst(__map__f(_car(__map__x)));
      __map__i= _cdr(__map__i);
    }
  } else if (! is_void(__map__x)) {
    error, "unsupported data type \""+__map__n+"\"";
  }
  return __map__y;
}

/*---------------------------------------------------------------------------*/
/* STRING ROUTINES */

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

func strcut(str, len)
/* DOCUMENT strcut(str, len)
     Cut input scalar string STR in pieces of length less or equal LEN and
     return an array of such pieces.

   SEE ALSO strjoin */
{
  if ((str_len= strlen(str))<=len) return str;
  n= (str_len+len-1)/len;
  result= array(string, n);
  for (i=1, i1=1, i2=len ; i<=n ; ++i, i1+=len, i2+=len)
    result(i)= strpart(str, i1:i2);
  return result;
}

func strjoin(str, glue)
/* DOCUMENT strjoin(str)
       -or- strjoin(str, glue)
     Join strings from array STR into a single long string.  The string GLUE
     (default "") is used between each pair of element from STR.

   SEE ALSO strcut */
{
  if ((n= numberof(str)) >= 1) {
    s= str(1);
    if (glue) for (i=2 ; i<=n ; ++i) s+= glue+str(i);
    else      for (i=2 ; i<=n ; ++i) s+= str(i);
    return s;
  }
}

/*---------------------------------------------------------------------------*/
/* LOGICAL ROUTINES */
func _is_scalar(x) { return (is_array(x) && ! dimsof(x)(1)); }
/* DOCUMENT is_scalar(x)
     returns true if X is a scalar.

   SEE ALSO is_scalar, is_vector, is_matrix, is_array,
            is_integer, is_real, is_complex, is_numerical. */
if (!is_scalar) is_scalar = _is_scalar;

func _is_vector(x) { return (is_array(x) && dimsof(x)(1) == 1); }
/* DOCUMENT is_vector(x)
     returns true if array X is a vector (i.e. a 1D array).

   SEE ALSO is_scalar, is_vector, is_matrix, is_array,
            is_integer, is_real, is_complex, is_numerical. */
if (!is_vector) is_vector = _is_vector;

func _is_matrix(x) { return (is_array(x) && dimsof(x)(1) == 2); }
/* DOCUMENT is_matrix(x)
     returns true if array X is a matrix (i.e. a 2D array).

   SEE ALSO is_scalar, is_vector, is_matrix, is_array,
            is_integer, is_real, is_complex, is_numerical. */
if (!is_matrix) is_matrix = _is_matrix;


func _is_real(x)    { return ((s=structof(x))==double || s==float); }
/* DOCUMENT is_real(x)
     returns true if array X if of real type (i.e. double or float).

   SEE ALSO is_scalar, is_vector, is_matrix, is_array,
            is_integer, is_real, is_complex, is_numerical. */
if (!is_real) is_real = _is_real;


func _is_complex(x) { return structof(x)==complex; }
/* DOCUMENT is_integer(x)
     returns true if array X if of integer type.

   SEE ALSO is_scalar, is_vector, is_matrix, is_array,
            is_integer, is_real, is_complex, is_numerical. */
if (!is_complex) is_complex = _is_complex;


func _is_integer(x) 
/* DOCUMENT is_integer(x)
     returns true if array X if of integer type.

   SEE ALSO is_scalar, is_vector, is_matrix, is_array,
            is_integer, is_real, is_complex, is_numerical. */
{ return ((s=structof(x))==long || s==int || s==char || s==short);}
if (!is_integer) is_integer = _is_integer;


func _is_numerical(x) 
/* DOCUMENT is_numerical(x)
     returns true if array X if of numerical type.

   SEE ALSO is_scalar, is_vector, is_matrix, is_array,
            is_integer, is_real, is_complex, is_numerical. */
{ return ((s=structof(x))==long || s==double || s==int || s==char ||
          s==complex || s==short || s==float); }
if (!is_numerical) is_numerical = _is_numerical;


func _is_integer_scalar(x) 
/* DOCUMENT is_integer_scalar(x)
     Check whether or not X is an integer scalar.

   SEE ALSO is_scalar, is_integer. */
{ return (((s=structof(x))==long || s==int || s==short ||
           s==char) &&! dimsof(x)(1)); }
if (!is_integer_scalar) is_integer_scalar = _is_integer_scalar;

/*---------------------------------------------------------------------------*/
/* FILE ROUTINES */

func expand_file_name(_path)
/* DOCUMENT expand_file_name(path)
     Expand leading "~" in file name PATH which must be an array of
     strings (or a scalar string).
   SEE ALSO: strpart, cd, get_cwd, get_file_name, protect_file_name. */
{
  result = array(string, dimsof(_path));
  n = numberof(_path);
  cwd = get_cwd(); /* memorize current working directory */
  head = string(0);
  local path;
  for (i=1 ; i<=n ; ++i) {
    /* Expand leading "~" in file name(s). */
    eq_nocopy, path, _path(i);
    if (strpart(path, 1:1) == "~") {
      sread, path, format="%[^/]", head;
      home = cd(head);
      if (home) {
        cd, cwd; /* restore working directory */
        tail = strpart(path, strlen(head)+1:0);
        if (strlen(tail)) {
          /* remove trailing '/' in HOME to avoid two '/' in result */
          result(i) = strpart(home, 1:-1) + tail;
        } else if (strlen(home) > 1) {
          path = strpart(home, 1:-1);
        } else {
          eq_nocopy, path, home;
        }
      }
    }
    result(i) = path;
  }
  return result;
#if 0
  c = *pointer(path);
  i = numberof(c);
  expand = (i>1 && c(1) == '~');
  slash = (i>1 && c(-1) == '/');
  while (--i>0 && c(i) != '/')
    ;
#endif
}

func get_file_name(obj)
/* DOCUMENT get_file_name(obj)
     If OBJ is a stream, returns the path name of the file associated with
     the stream.  Otherwise if OBJ is an array of strings, expand leading
     "~" in the elements of OBJ.
     
   SEE ALSO: open, print, expand_file_name, protect_file_name. */
{
  /* Expand leading "~" if string object. */
  if (structof(obj) == string) {
    return expand_file_name(obj);
  }

  /* Check input and get description of stream by the print() command. */
  if ((id = typeof(obj)) == "stream") {
    id = 1;
    s = print(obj);
  } else if (id == "text_stream") {
    id = 2;
    s = print(obj)(2:);
  } else {
    error, "unexpected non-string, non-stream argument";
  }

  /* Join backslash terminated lines from print() result (another
     possibility would be to change the line length with `print_format' but
     there is no way to restore the previous line_lenght unles we building
     a wrapper around original `print_format' routine and make a
     substitution). */
  join = (strpart(s, 0:0) == "\\");
  if (anyof(join)) {
    r = array(string, (ns = numberof(s)) - sum(join) + join(0));
    i = j = 0;
    while (i < ns) {
      w = s(++i);
      while (join(i)) {
        w = strpart(w, :-1);
        if (++i > ns) break;
        w += s(i);
      }
      r(++j) = w;
    }
    s = r;
    w = r = [];
  }

  /* Recover the full path of the stream file from the joined lines. */
  if (id == 1) {
    /* Binary stream. */
    if (numberof(s) == 2) {
      w1 = w2 = string(0);
      if (sread(s(1), format="%[^:]", w1) == 1 &&
          sread(s(2), format="%[^/]", w2) == 1) {
        return strpart(s(2), strlen(w2)+1:0) + strpart(s(1), strlen(w1)+3:0);
      }
    }
    error, "unexpected binary stream descriptor";
  } else {
    /* Text stream. */
    if (numberof(s) == 1) {
      w = string(0);
      if (sread(s(1), format="%[^/]", w) == 1) {
        return strpart(s(1), strlen(w)+1:0);
      }
    }
    error, "unexpected text stream descriptor";
  }
}

local _protect_file_name_table, _protect_file_name_list;
func protect_file_name(path)
/* DOCUMENT protect_file_name(path)
     Protect special characters in PATH (apostrophes, quotes, $, *, ?, [,
     etc) by backslashes to avoid them being interpreted by the shell, for
     instance when using the system() builtin function.
   SEE ALSO: system, get_file_name, expand_file_name. */
{
  c = *pointer(path);
  n = numberof(c) - 1; /* same as strlen(path) */
  p = _protect_file_name_table(1 + c);
  r = array(char, 2*n + 1);
  i = 0;
  j = 0;
  while (++i <= n) {
    if (p(i)) r(++j) = '\\';
    r(++j) = c(i);
  }
  return string(&r);
}
_protect_file_name_list = ['$','&','!','#','?','*',
                           '[',']','{','}','<','>',
                           '\\','"','\'',
                           ' ','\t','\r','\n','\v','\f'];
(_protect_file_name_table = array(char, 256))(1 + _protect_file_name_list) = 1;

func read_ascii(file, compress=)
/* DOCUMENT read_ascii(file_or_name)
     Reads ascii numeric  data in columns from text  file.  FILE_OR_NAME is
     the name of the  file or an already open file stream.  The result is a
     NCOLUMNS-by-NLINES array of doubles.

     Data are  read as double values  arranged in columns  separated by any
     number of  spaces or tabs.  Comments  starting with a "#"  or an FILEy
     other character  which is not part of  a number are ignored  up to the
     end-of-line.  Blank lines  are ignored.  The first non-blank/commented
     line  gives the  number of  values per  column, for  subsequent lines.
     Subsequent lines  must have  the same number  of columns --  blanks in
     columns are  not permitted, use  0.0 instead.  However,  minimal error
     checking  is performed,  and if  the data  is not  really  in columns,
     read_ascii  can silently  fail to  interpret  your file  as you  would
     scanning it by eye.
     
     The  read operation will  be much  faster if  the number  of commented
     lines is  relatively small.   Blank lines cost  nothing, while  a line
     containing just a "#" is expensive.

     If the  file is specified by its  name, it may be  compressed in which
     case it get automatically decompressed while reading (see xopen).

   SEE ALSO: xopen, read. */
{
  /* open the file if it's not already open */
  if (structof(file) == string)
    file = xopen(file, compress=(is_void(compress) ? "auto" : compress));

  /* read lines one at a time until the "model" line which
   * determines the number of columns is discovered
   * assume the number of columns is less than 128 */
  x = array(0.0, 128);
  ncols = 0;
  while ((line = rdline(file))) {
    ncols = sread(line, x);
    if (ncols) break;          /* got a line with numbers */
  }
  if (! ncols) return [];

  nrows = 1;
  list = _lst([x(1:ncols)]);
  x = array(0.0, ncols, 10000/ncols + 1);
  for(;;) {
    /* try to grab at least 10000 numbers from the file
     * blank lines will be skipped, but any comments will
     * interrupt the read */
    if (! (n = read(file, x))) {
      /* if didn't get any, drop back to reading comments one
       * line at a time until we get some more numbers */
      while ((line = rdline(file))) {
	if ((n = sread(line, x))) break;
      }
      if (! line) break;    /* rdline detected end-of-file, n==0 too */
    }
    if (n%ncols) error, "data is not in columns";
    n /= ncols;

    /* grow the list the fast way, adding new values to its head
     * (adding to the tail would make growth an n^2 proposition,
     *  as would using the grow function) */
    list = _cat(x(,1:n), list);
    nrows += n;
  }

  /* pop chunks off list and reassemble result */
  x = array(0.0, ncols, nrows);
  for (i=nrows ; list ; list=_cdr(list)) {
    n = numberof(_car(list))/ncols;
    x(,i-n+1:i) = _car(list);
    i -= n;
  }

  return x;
}

func load_text(file, compress=)
/* DOCUMENT load_text(file)
     Returns all lines of text file as a vector of strings.  Returns nil if
     there are no lines to read.  FILE can be a file name or a text stream.
     In the latter case, the lines not yet read get returned.

     By  default,  if  the file  is  specified  by  its  name, it  will  be
     automatically decompressed if its  compressed; it is possible to force
     another  behaviour by  specifying a  value different  from  "auto" for
     keyword COMPRESS (see xopen).
     
   SEE ALSO: xopen, rdline, dump_text. */
{
  if (structof(file) == string)
    file = xopen(file, compress=(is_void(compress) ? "auto" : compress));
  text = rdline(file, 1000);
  while (text(0)) grow, text, rdline(file, numberof(text));
  if (is_array((j = where(text)))) return text(j);
}

func dump_text(file, text, compress=, level=, preserve=)
/* DOCUMENT dump_text, file, text;
     Dump every  elements of string array  TEXT as individual  lines into a
     text file.  FILE can be a file name or a text stream.

     If the file is specified by  its name, keywords COMPRESS, LEVEL can be
     used to specify  a compression method (see xopen).   The default value
     of COMPRESS is  "auto" (i.e., method guessed on the  basis of the file
     extension).

     If the file  is specified by its name, keyword PRESERVE  can be set to
     true to avoid overwriting an existing file.
     
   SEE ALSO: xopen, rdline, load_text. */
  
{
  if ((anything = ! is_void(text)) && structof(text) != string)
    error, "expecting array of strings or nil";
  if (structof(file) == string) {
    file = xopen(file, "w", compress=(is_void(compress) ? "auto" : compress),
                 level=level, preserve=preserve);
  }
  if (anything) write, file, format="%s\n", text;
}

func guess_compression(filename)
/* DOCUMENT guess_compression, filename;
       -or- guess_compression(filename)  
     Guess  which compression  program was  used to  produce  file FILENAME
     according to first  bytes of this file.  When  called as a subroutine,
     the file name is printed out  with the name of the compression program
     if any.  If called as a function, the result is an integer:
       1 - if file compressed with "gzip";
       2 - if file compressed with "bzip2";
       3 - if file compressed with "compress";
       4 - if file compressed with "pack";
       0 - otherwise.

  SEE ALSO: xopen. */
{
  /* according to information in /etc/magic:
   *
   * method    min.            bytes
   *           size      0    1    2    3
   * --------- ----    ---- ---- ---- ----
   * pack:       3     \037 \036
   * compress:   3     \037 \235
   * gzip:      20     \037 \213   c         (1)
   * bzip2:     14      'B'  'Z'  'h'   c    (2)
   *
   *   (1)  if c<8, compression level, else if c==8 deflated
   *   (2)  with '0' <= c <= '9', block size = c*100kB
   *   minimum size has been computed for an empty file:
   *     pack     ->  3 bytes
   *     compress ->  3 bytes
   *     bzip2    -> 14 bytes
   *     gzip     -> 20 bytes, if compression level is specified
   *                 24 bytes, otherwise
   */
  magic = array(char, 4);
  n = _read(open(filename, "rb"), 0, magic);
  if ((c = magic(1)) == '\037') {
    if ((c = magic(2)) == '\213') {
      if (! am_subroutine()) return 1; /* gzip */
      write, format="%s: compressed with \"gzip\"\n", filename;
      return;
    } else if (c == '\235') {
      if (! am_subroutine()) return 3; /* compress */
      write, format="%s: compressed with \"compress\"\n", filename;
      return;
    } else if (c == '\036') {
      if (! am_subroutine()) return 4; /* pack */
      write, format="%s: compressed with \"pack\"\n", filename;
      return;
    }   
  } else if (c == 'B' && magic(2) == 'Z' && magic(3) == 'h' &&
             '0' <= (c = magic(4)) && c <= '9') {
    if (! am_subroutine()) return 2; /* bzip2 */
    write, format="%s: compressed with \"bzip2\" (block size = %d kB)\n",
      filename, 100*(c - '0');
    return;
  }
  if (! am_subroutine()) return 0;
  write, format="%s: uncompressed?\n", filename;
}

func xopen(filename, filemode, preserve=, nolog=, compress=, level=, prims=)
/* DOCUMENT xopen(filename)
       -or- xopen(filename, filemode)
     Opens the file FILENAME according to FILEMODE (both are strings).  The
     return value is an IOStream (or just stream for short).  When the last
     reference to this return value is discarded, the file will be closed.
     The file can also be  explicitly closed with the close function (which
     see).  The  FILEMODE (default  "r" -- open  an existing text  file for
     reading) determines whether  the file is to be  opened in read, write,
     or update mode,  and whether writes are restricted  to the end-of-file
     (append mode).  FILEMODE also determines whether the file is opened as
     a text  file or  as a  binary file.  FILEMODE  can have  the following
     values, which are the same as for the ANSI standard fopen function:
         "r"   - read only
         "w"   - write only, random access, existing file overwritten
         "a"   - write only, forced to end-of-file, existing file preserved
         "r+"  - read/write, random access, existing file preserved
         "w+"  - read/write, random access, existing file overwritten
         "a+"  - read/write, reads random access, writes forced to
                 end-of-file, existing file preserved
         "rb"  "wb"  "ab"  "r+b"  "rb+"  "w+b"  "wb+"  "a+b"  "ab+"
                 without b means text file, with b means binary file
     
     Keyword COMPRESS can be used  to specify compression method for a text
     file open for reading (FILEMODE="r") or writing (FILEMODE="w") only --
     (de)compression  is unsupported in  append mode  or for  binary files.
     The value of keyword COMPRESS can be a scalar string or an integer:
          "auto"     - guess compression according to first bytes of file
                       in read  mode, or according to file extension in 
                       write mode: ".gz" for gzip, ".bz2" for bzip2 and
                       ".Z" for compress.
       0  "none"     - no (de)compression
       1  "gzip"     - use gzip to (de)compress
       2  "bzip2"    - use bzip2 to (de)compress
       3  "compress" - use compress to (de)compress
       4  "pack"     - use pack to (de)compress
     The default  value for COMPRESS is  "auto" in read mode  and "none" in
     write mode.  Note that "gzip", "bzip2", "pack" and "compress" commands
     must  exists in  your  $PATH for  compressing  with the  corresponding
     methods.  Decompression of files compressed with "pack" and "compress"
     is  done  by  "gzip".   If  keyword  COMPRESS  is  explicitely  0,  no
     decompression  is ever  applied;  if keyword  COMPRESS is  explicitely
     non-zero, the  file must have been compressed.   The compression level
     for gzip  and bzip2  can be  specified as an  integer value  thanks to
     keyword LEVEL.
       
     Keyword PRIMS  can be used  to specify primitives data  type different
     than  the native  ones for  binary files  (PRIMS is  ignored  for text
     files).  PRIMS can  be a scalar string (i.e.,  "alpha", "mac", "sun3",
     "cray", "macl", "sun", "dec",  "pc", "vax", "vaxg", "i86", "sgi64", or
     "xdr") or  a 32-element  vector of long's  as taken  by set_primitives
     (which see).

     When a binary file is created, it is possible to avoid the creation of
     the log-file FILENAME+"L" by setting keyword NOLOG to true.

     Keyword PRESERVE can  be set to true to  avoid overwriting an existing
     file when FILENAME is open for writing (i.e. with a "w" in FILEMODE).

     
   BUGS:
     If (de)compression is used, FILENAME must not contain any double quote
     character (").

     
   SEE ALSO: close, guess_compression, open, popen, set_primitives. */
{
  if (is_void(filemode) || filemode == "r") {
    /* Open file for reading in text mode. */
    compress = __xopen_get_compress(compress, filename, 1);
    if (! compress) return open(filename, "r");
    if (compress == 2) return popen("bzip2 -dc \"" + filename + "\"", 0);
    if (compress == -1) error, "bad value for keyword COMPRESS";
    return popen("gzip -dc \"" + filename + "\"", 0);
  }

  if (preserve && strmatch(filemode, "w") && open(filename, "r", 1))
    error, "file \""+filename+"\" already exists";

  filemode
  
  if (filemode == "w") {
    /* Open file for writing in text mode. */
    compress = __xopen_get_compress(compress, filename, 0);
    if (! compress) return open(filename, filemode);
    if (compress == 1) {
      if (is_void(level)) command = swrite(format="gzip > \"%s\"", filename);
      else command = swrite(format="gzip -%d > \"%s\"", level, filename);
    } else if (compress == 2) {
      if (is_void(level)) command = swrite(format="bzip2 > \"%s\"", filename);
      else command = swrite(format="bzip2 -%d > \"%s\"", level, filename);
    } else if (compress == 3) {
      command = swrite(format="compress > \"%s\"", filename);
    } else if (compress == 2) {
      command = swrite(format="pack > \"%s\"", filename);
    } else {
      error, "bad value for keyword COMPRESS";
    }
    command
    return popen(command, 1);
  }

  /* Open file for other modes. */
  if (! (is_void(compress) || compress == 0 || compress == "none"))
    error, "(de)compression unsupported in mode \""+filemode+"\"";
  if (binary && nolog) {
    /* will remove log-file unless it already exists */
    logfile = filename + "L";
    if (open(logfile, "r", 1)) logfile = [];
  } else {
    logfile = [];
  }
  file = open(filename, filemode);
  if (logfile) remove, logfile;

  /* Return open file after optionally installing primitive data types. */
  if (is_void(prims) || ! strmatch(filemode, "b")) return file;
  if ((s = structof(prims)) == string) {
    if (! dimsof(prims)(1)) {
      if (prims != "set" && prims != "get" &&
          is_func((sym = symbol_def(prims+"_primitives"))) == 1) {
        sym, file;
        return file;
      } else if (is_array((sym = symbol_def("__"+prims))) &&
                 structof(sym) == long && numberof(sym) == 32 &&
                 dimsof(sym)(1) == 1) {
        set_primitives, file, sym;
        return file;
      }
    } else if (s == long && numberof(prims) == 32 && dimsof(prims)(1) == 1) {
      set_primitives, file, prims;
      return file;
    }
  }
  error, "bad value for keyword PRIMS";
}

func __xopen_get_compress(compress, filename, for_reading)
/* DOCUMENT __xopen_get_compress(compress, filename, for_reading)
     Private function called by xopen.

   SEE ALSO xopen. */
{
  if (is_void(compress)) {
    if (for_reading) return guess_compression(filename);
    return 0;
  } else if (is_array(compress) && ! dimsof(compress)(1)) {
    if ((s = structof(compress)) == string) {
      if (compress == "auto") {
        if (for_reading) return guess_compression(filename);
        if (strpart(filename, -2:0) == ".gz" ) return 1; /* gzip */
        if (strpart(filename, -3:0) == ".bz2") return 2; /* bzip2 */
        if (strpart(filename, -1:0) == ".Z"  ) return 3; /* compress */
        return 0;
      }
      if (compress == "none"    ) return 0;
      if (compress == "gzip"    ) return 1;
      if (compress == "bzip2"   ) return 2;
      if (compress == "compress") return 3;
      if (compress == "pack"    ) return 4;
    } else if ((s == long || s == int || s == short || s == char) &&
               compress >= 0 && compress <= 4) {
      return compress;
    }
  }
  return -1;
}


/*---------------------------------------------------------------------------*/
/* PDB FILES */

func pdb_list(file)
/* DOCUMENT pdb_list, file;
       -or- pdb_list(file)
     Lists contents of PDB binary file.  FILE can be either a file name or
     a binary stream.
     
   SEE ALSO: createb, openb, restore, pdb_restore_all. */
{
  if (structof(file) == string) file = openb(file);
  vars = get_vars(file);
  if (! am_subroutine()) return vars;
  title = ["Non-record variables", "    Record variables"];
  for (i=1 ; i<=2 ; ++i) {
    write, format="%s:", title(i);
    if (numberof(*vars(i))) {
      write, format=" %s", *vars(i);
      write, format="%s\n", "";
    } else {
      write, format="%s\n", " <none>";
    }
  }
}

func pdb_restore_all(_f_i_l_e_)
/* DOCUMENT pdb_restore_all, file;
     Restore all non-record variables of a PDB file.  FILE can be either a
     file name or a binary stream. 
     
   SEE ALSO: createb, openb, restore, pdb_list. */
{
  if (structof(_f_i_l_e_) == string) _f_i_l_e_ = openb(_f_i_l_e_);
  restore, _f_i_l_e_;
}

/*---------------------------------------------------------------------------*/
/* PROFILING ROUTINES */

local _timer_stamp;
func timer_start {
  extern _timer_stamp;
  _timer_stamp = array(double, 3);
  timer, _timer_stamp;
}
func timer_elapsed(count)
/* DOCUMENT timer_start;
       -or- timer_elapsed;
       -or- timer_elapsed, count;
       -or- timer_elapsed()
       -or- timer_elapsed(count)
     The  subroutine  timer_start (re)starts  the  timer  and the  function
     timer_elapsed   computes  the   elapsed  time   since  last   call  to
     timer_start.  If COUNT is given, the elapsed time is divided by COUNT.
     When  called as  a subroutine,  timer_elapsed prints  out  the elapsed
     time; when  called as a  function, it returns  [CPU,SYSTEM,WALL], with
     all three  times measured in seconds.   The two functions  make use of
     external variable _timer_stamp to memorize the initiale times.

     For instance:
       timer_start;
       ...             // some code to be profiled
       timer_elapsed;
       ...             // some more code to be profiled
       timer_elapsed;  // prints out _total_ elapsed time

  SEE ALSO: timer. */
{
  extern _timer_stamp;
  elapsed = _timer_stamp;
  timer, elapsed;
  elapsed -= _timer_stamp;
  if (! is_void(count)) elapsed /= count;
  if (am_subroutine()) {
    write, format="cpu=%g, system=%g, wall=%g\n",
      elapsed(1), elapsed(2), elapsed(3);
  } else {
    return elapsed;
  }
}

/*---------------------------------------------------------------------------*/

func _stat_worker(x)
/* DOCUMENT _stat_worker(x)
     Private routine used by stat, returns vector of double's:
        [min(X), max(X), avg(X), std(X)]
     where std(X) is the standard deviation of X.

   SEE ALSO stat. */
{
  if (structof(x)!=double) x= double(x);
  avg_x= avg(x);
  dx = x - avg_x;
  return [min(x), max(x), avg_x, sqrt(avg(dx*dx))];
}

func stat(..)
/* DOCUMENT stat, x, ...
     Print out statistics and information for all the arguments. */
{
  ith= 0;
  while (more_args()) {
    ++ith;
    x= next_arg();
    write, format="%2d: ", ith;
    if (is_array(x)) {
      write, format="array(%s", typeof(x);
      dims= dimsof(x);
      n= numberof(dims);
      for (k=2 ; k<=n ; ++k) write, format=",%d", dims(k);
      type= structof(x);
      is_numerical= (type==double || type==long || type==int || type==char ||
                     type==complex || type==float || type==short);
      write, format=")%s", (is_numerical ? " " : "\n");
      if (is_numerical) {
        fmt= "min=%g max=%g avg=%g std=%g\n";
        if (type==complex) {
          s= _stat_worker(double(x));
          write, format="\n         real part: "+fmt, s(1), s(2), s(3), s(4);
          s= _stat_worker(x.im);
          write, format="    imaginary part: "+fmt, s(1), s(2), s(3), s(4);
          s= _stat_worker(abs(x));
          write, format="           modulus: "+fmt, s(1), s(2), s(3), s(4);
        } else {
          s= _stat_worker(x);
          write, format=fmt, s(1), s(2), s(3), s(4);
        }
      }
    } else {
      write, format="%s, %s\n", typeof(x), strjoin(print(x));
    }
  }
}

/*---------------------------------------------------------------------------*/

func smooth(a, level)
/* DOCUMENT smooth(a)
       -or- smooth(a, level)
     Returns array A smoothed along its dimensions.  I.e. for a 1D array:
       smooth(A) = A(pcen)(zcen)
     for a 2D array:
       smooth(A) = A(pcen,pcen)(zcen,zcen)
     ... (up to 6 dimensions).

     For a greater number of dimensions,  each  direction  is  smoothed and
     transposed in turn: apart from rounding errors, the result is the same
     but the computation time is approximately  multiplied  by  3.   If you
     oftenly smooth arrays with more than 6 dimensions you may  think about
     modifying the source...

     Optional argument  LEVEL  (default  1)  set  the  number  of  time the
     smoothing operation is performed.

   PROPERTIES OF THE SMOOTHING OPERATOR:
     (i)   The smoothing operator is linear and symmetric.  For instance,
           for a vector, A, smooth(A)=S(,+)*A(+) where the matrix S is
           tridiagonal:
                    [3 1         ]
                    [1 2 1       ]
                    [  1 2 1     ]
             0.25 * [   \ \ \    ]    where, to improve readability,
                    [    \ \ \   ]    missing values are all zero.
                    [     1 2 1  ]
                    [       1 2 1]
                    [         1 3]
           You can, in principle, reverse the smoothing operation with
           TDsolve along each dimensions of smooth(A).  Note:For a vector
           A, the operator S-I applied to A (where I is the identity
           matrix) is the finite difference 2nd derivatives of A (but for
           the edges).

     (ii)  The smoothing operator does not change the sum of the element
           values of its argument, i.e.: sum(smooth(A)) = sum(A).

     (iii) Only an array with all elements having the same value is
           invariant by the smoothing operator.  In fact "slopes" along
           dimensions of A are almost invariant, only the values along the
           edges are changed.

     The symmetry of the smoothing operator is important for the
     computation of gradients.  For instance, let Y = smooth(X) and DQ_DY
     be the gradient of a scalar function Q with respect to Y, then the
     gradient of Q with respect to X is simply: DQ_DX = smooth(DQ_DY)

   TO DO:
     By default A is smoothed along all its dimensions, but the list
     of dimensions to smooth can be specified with keyword WHICH.  As
     usual, negative dimensions are taken as offset from the last one.

     If keyword WRAP is true (non-nil and non-zero) a wrapped version
     of the operator (with same properties but no longer tridiagonal)
     is applied instead.  This is suitable for periodic arrays (e.g.
     FFT transformed arrays).

   SEE ALSO: TDsolve. */
{
  n= dimsof(a)(1);
  if (is_void(level) || level == 1) {
    if (n == 1)
      return a(pcen)(zcen);
    if (n == 2)
      return a(pcen,pcen)(zcen,zcen);
    if (n == 3)
      return a(pcen,pcen,pcen)(zcen,zcen,zcen);
    if (n == 4)
      return a(pcen,pcen,pcen,pcen)(zcen,zcen,zcen,zcen);
    if (n == 5)
      return a(pcen,pcen,pcen,pcen,pcen)(zcen,zcen,zcen,zcen,zcen);
    if (n == 6)
      return a(pcen,pcen,pcen,pcen,pcen,pcen)(zcen,zcen,zcen,zcen,zcen,zcen);
    while (n--)
      a= transpose(a(pcen,..)(zcen,..));
    return a;
  }
  if (n == 1) {
    for (i=1; i<=level; i++)
      a= a(pcen)(zcen);
  } else if (n == 2) {
    for (i=1; i<=level; i++)
      a= a(pcen,pcen)(zcen,zcen);
  } else if (n == 3) {
    for (i=1; i<=level; i++)
      a= a(pcen,pcen,pcen)(zcen,zcen,zcen);
  } else if (n == 4) {
    for (i=1; i<=level; i++)
      a= a(pcen,pcen,pcen,pcen)(zcen,zcen,zcen,zcen);
  } else if (n == 5) {
    for (i=1; i<=level; i++)
      a= a(pcen,pcen,pcen,pcen,pcen)(zcen,zcen,zcen,zcen,zcen);
  } else if (n == 6) {
    for (i=1; i<=level; i++)
      a= a(pcen,pcen,pcen,pcen,pcen,pcen)(zcen,zcen,zcen,zcen,zcen,zcen);
  } else {
    while (n--) {
      for (i=1; i<=level; i++)
	a= a(pcen,..)(zcen,..);
      a= transpose(a);
    }
  }
  return a;
}

/*---------------------------------------------------------------------------*/
