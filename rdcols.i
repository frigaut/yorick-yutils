/*
 * rdcols.i -- $Id: rdcols.i,v 1.1 2007-12-11 23:55:12 frigaut Exp $
 * routines to assist in reading columns from ascii files
 * 
 * Author: David Munro
 * Written 2002
 *
 * Copyright (c) 2002, David Munro
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
 */

func rdcols(f, ncols, width=, delim=, type=, missing=, marker=,
            comment=, nskip=, nlines=)
/* DOCUMENT cols = rdcols(f)
 *       or cols = rdcols(f, ncols)
 *          eq_nocopy, col1, *cols(1)
 *          eq_nocopy, col2, *cols(2)
 *          ...
 *          eq_nocopy, colN, *cols(ncols)
 *
 *  cracks ascii file F into NCOLS columns, returning an array of
 *  pointers to the columns.  A "column" is defined by either its
 *  width in characters, or by the appearance of a delimiting
 *  character (such as a blank, a comma, or a tab).  Multiple
 *  delimiting characters may either be skipped (as usual if the
 *  character is a blank), or may represent a number of empty
 *  columns (as for reading a typical text file exported from a
 *  spreadsheet or database program).
 *
 *  rdcols returns an array of pointers COLS, with *COLS(i) the
 *  contents of the i-th column.  The NCOLS parameter may be omitted
 *  in which case rdcols will guess the number of columns.
 *
 *  F may be either a file handle or a filename.
 *
 *  nskip=m       skip the first M lines of the file (M=0 by default)
 *  nlines=n      read only the first M+N lines of the file
 *  comment=text  interpret lines beginning with TEXT (possibly preceded
 *                by blanks) as comments (blank lines are always comments)
 *  width=w       column width, or list of widths (w=0 means any width)
 *  delim=d       column delimiters (as for strtok), or list of delimiters
 *  type=t        column type, or list of types
 *    t=0 means guess, t=1 means string, t=2 means integer, t=3 means real,
 *    t=4 means either integer or real
 *  marker=k      marker column delimiters (as for strtok)
 *  missing=m     missing value, or list of values
 *                for empty numeric columns
 *
 *  The width=, delim=, type=, and missing= keywords may either be scalar
 *  values to apply to every column, or lists to apply to successive
 *  columns.  The marker= keyword is similar to the delim= keyword,
 *  except that multiple consecutive occurrences indicate empty
 *  columns.  If any list is shorter than NCOLS, its final value
 *  applies to all remaining columns.  If any list is longer than
 *  NCOLS, its trailing values are silently ignored.
 *
 *  By default, width=0.  Non-zero width is very tricky, since a
 *  column begins wherever the previous column ended; if the previous
 *  column had width=0, its delimiter is treated as a part of it, so
 *  it is not included in the width count.  Consequently, the width=
 *  keyword is best suited to situations in which only the final
 *  width value is 0.
 *
 *  By default, marker=[].  If present, then every column is delimited
 *  by a character in marker, and every occurrence of such a character
 *  indicates a new column.  Use marker="\t" to read tab-delimited text
 *  files exported from a spreadsheet or database program.  Non-zero
 *  width= values supersede marker=, but delim= is ignored if marker
 *  is present.
 *
 *  By default, delim=" \t",  and type=0, so that blanks and tabs delimit
 *  columns, and rdcols guesses whether a column contains string or
 *  numeric data.  A numeric column ends at the first character that
 *  cannot be interpreted as part of the number, whether or not that is
 *  a character in delim.  If you use type= to force a numeric column,
 *  empty rows or rows which are not numbers get the value 0.0 by
 *  default, or the value specified by the missing= keyword.  Missing
 *  values in string columns always get the value "".
 *
 *  If you need finer control of the returned types than provided by
 *  the type= keyword, see rdconvert.
 *
 * SEE ALSO: read, rdline, rdconvert
 */
{
  if (structof(f) == string) f = open(f);

  if (is_void(ncols)) nc = 1;
  else if (ncols < 1) return [];
  else nc = ncols;
  result = array(pointer, nc);

  if (!is_void(nskip) && nskip>0) rdline, f, nskip;
  arb = is_void(nlines);
  if (arb) nlines = 10000;
  if (nlines < 1) return result;

  /* ingest entire file as string array
   * note each read grabs twice as many lins as previous,
   * so grow operation scales as nlines*log(nlines), which is ok */
  lines = _rc_rdline(f, nlines);
  if (arb) {
    for (l=lines ; ; nlines*=2) {
      l = _rc_rdline(f, nlines);
      grow, lines, l;
      if (numberof(l) < nlines) break;
    }
  }
  if (!numberof(lines)) return result;

  /* remove blank and comment lines */
  tok = strtok(lines)(1,);
  mask = !tok;
  if (is_void(comment) && is_void(marker)) comment = "#";
  if (comment && strlen(comment)) {
    comment = strtok(comment)(1);
    mask |= strpart(tok, 1:strlen(comment))==comment;
  }
  if (anyof(mask)) {
    list = where(!mask);
    if (!numberof(list)) return result;
    lines = lines(list);
  }

  if (is_void(width)) width = 0;
  else width = long(width);
  nw = numberof(width);

  if (is_void(type)) type = 0;
  else type = long(type);
  nt = numberof(type);

  if (is_void(missing)) missing = 0.0;
  else missing = double(missing);
  nm = numberof(missing);

  if (is_void(delim)) delim = is_void(marker)? " \t" : string(0);
  nk = 0;
  if (!is_void(marker)) {
    if (strlen(marker) > 0) {
      nk = 1;
      /* valid is a character not in marker */
      valid = strtok(string(&char(indgen(255))), marker)(1);
      valid = strpart(valid,1:1);
    } else {
      marker = [];
    }
  }
  nd = numberof(delim);

  for (i=1 ; ; i++) {
    w = width(min(i,nw));
    d = delim(min(i,nd));
    if (!strlen(d)) d = string(0);
    t = type(min(i,nt));
    m = missing(min(i,nm));
    if (w > 0) {         /* column delimited by its width */
      lines = strpart(lines, w+1:0);
      col = _rc_convert(strpart(lines, 1:w), t, m);
    } else if (nk) {     /* column delimited by marker= */
      col = strtok(valid+lines, marker);
      lines = col(2,);
      col = _rc_convert(strpart(col(1,),2:0), t, m);
    } else {             /* column delimited by delim= */
      col = _rc_convert(lines, t, m, d, lines);
    }
    result(i) = &col;
    if (!is_void(ncols) && i==ncols) break;
    if (noneof(strlen(lines))) break;
    if (i == nc) {
      grow, result, array(pointer, nc);
      nc *= 2;
    }
  }
  if (is_void(ncols)) result = result(1:i);

  return result;
}

func rdconvert(cols, ..)
/* DOCUMENT rdconvert, cols, type1, type2, type3, ...
 *       or cols = rdconvert(cols, type1, type2, type3, ...)
 *
 *  converts the data types of the COLS, so that COLS(1) becomes
 *  TYPE1, COLS(2) TYPE2, COLS(3) TYPE3, and so on.  The COLS
 *  array is an array of pointers as returned by rdcols.  If any
 *  of the TYPEi is nil, that column is not altered.  This function
 *  is only useful if you are not satisfied with either the long
 *  or double types returned by rdcols, e.g.-
 *     cols = rdcols("datafile", 5);
 *     rdconvert, cols, , short, float, , int;
 *
 * SEE ALSO: rdcols
 */
{
  for (i=1 ; more_args() ; i++) {
    type = next_arg();
    if (!is_void(type)) cols(i) = &type(*cols(i));
  }
  return cols;
}

func _rc_rdline(f, nlines)
{  /* read at most nlines lines from f */
  lines = rdline(f, nlines);
  n = sum(!lines);
  if (n) {
    nlines -= n;
    lines = nlines? lines(1:nlines) : [];
  }
  return lines;
}

func _rc_guess(lines)
{
  tok = strtok(lines, " \t")(1,);
  t = strpart(tok, 1:1);
  list = where((t=="-") | (t=="+"));
  if (numberof(list)) {
    tok(list) = x = strpart(tok(list), 2:0);
    t(list) = strpart(x, 1:1);
  }
  list = where(t==".");
  if (numberof(list))
    t(list) = strpart(tok(list), 2:2);
  guess = ((t>="0") & (t<="9"));
  list = where(guess);
  if (numberof(list)) {
    tok = tok(list);
    t = strpart(strtok(tok, "0123456789")(1,), 1:1)
    r = (t==".");
    maybe = where((t=="e") | (t=="E") | (t=="d") | (t=="D"));
    if (numberof(maybe)) {
      u = strtok(tok(maybe), "eEdD")(2,);
      t = strpart(u, 1:1);
      l = where((t=="-") | (t=="+"));
      if (numberof(l)) t(l) = strpart(u(l), 2:2);
      r(maybe) = ((t>="0") & (t<="9"));
    }
    guess(list) += r;
  }
  /* 1 where string, 2 where integer, 3 where real */
  return guess+1;
}

func _rc_convert(lines, type, missing, delim, &remains)
{
  if (type != 1) {
    guess = _rc_guess(lines);
    if (type == 0) {
      /* if more than 80% of a column is numbers, guess it is numbers */
      if (sum(guess==1)>numberof(lines)/5) type = 1;
      else if (anyof(guess==3)) type = 3;
      else type = 2;
    } else if (type!=2 && type!=3) {
      if (anyof(guess==3)) type = 3;
      else type = 2;
    }
  }

  if (type == 1) {
    if (delim) {
      lines = strtok(lines, delim);
      remains = lines(2,);
      lines = lines(1,);
    } else {
      remains = array(string, numberof(lines));
    }
    return lines;
  }

  /* change non-numeric lines into numeric ones */
  list = where(guess==1);
  if (numberof(list)) {
    vals = lines(list);
    lines(list) = "0"+vals;
    if (delim) rems = strtok(vals, delim)(2,);
  }
  /* make sure all lines have at least one non-numeric char at end */
  lines += "?";

  vals = array(0.0, numberof(lines));
  remains = array(string, numberof(lines));
  if (sread(lines,format="%le%[^\n]",vals,remains) != 2*numberof(lines))
    error, "impossible error during rdcols()";
  remains = strpart(remains, 1:-1);
  if (numberof(list)) {
    vals(list) = missing;
    if (delim) remains(list) = rems;
  }

  if (type == 2) vals = long(vals);
  return vals;
}
