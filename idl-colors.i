/* 
 * idl-colors.i --
 *
 *	Routines to manipulate IDL color files.
 *	Provides functions:
 *	  - loadct: load IDL color table / get names of IDL color tables.
 *
 * $Id: idl-colors.i,v 1.1.1.1 2007-12-11 23:55:10 frigaut Exp $
 *
 * Copyright (c) 1996, Eric THIEBAUT (thiebaut@obs.univ-lyon1.fr, Centre de
 * Recherche Astrophysique de Lyon, 9 avenue Charles  Andre,  F-69561 Saint
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
 */

require,"string.i";
require,"pathfun.i";

func loadct(which, file=)
/* DOCUMENT loadct, which;
            color_names= loadct();
     In the first form, load IDL color table identified by WHICH (either
     a number or a name).  When called as a function, e.g., second form,
     returns an array of names of color tables.

     Keyword FILE can be used to indicate an alternate file name (default
     is Y_SITE + "contrib/colors1.tbl").

   SEE ALSO palette.
*/
{
  /* Open color tables file. */
  if (is_void(file)) {
    file = find_in_path("colors1.tbl",takefirst=1);
  }
  file= open(file, "rb");

  /* Get number of color tables. */
  ntables= char();
  if (_read(file, 0, ntables) != sizeof(ntables)) {
    error, "cannot read number of color tables";
  }
  ntables= long(ntables);

  /* Eventually, seek names of color tables. */
  if (structof(which) == string || !am_subroutine()) {
    buf= array(char, 32, ntables);
    if (_read(file, ntables * 768 + 1, buf) != sizeof(buf)) {
      error, "cannot read names of color tables";
    }
    names= array(string, ntables);
    for (i=1; i<=ntables; i++) {
      names(i)= strtrim(string(&buf(,i)));
    }
    if (structof(which)==string) {
      i= where(strtolower(names) == strtolower(which));
      if (numberof(i) == 0) {
	error, "bad color table name";
      }
      which= i(1);
    }    
  } else if (which < 1 || which > ntables) {
    error, "bad color table number";
  }

  /* Read color table data. */
  if (!is_void(which)) {
    if (structof(which) != long || dimsof(which)(1) != 0) {
      error, "color table number must be a LONG scalar";
    }
    local r, g, b;
    lut= array(char, 256, 3);
    if (_read(file, 1 + (which - 1) * 768, lut) != sizeof(lut)) {
      error, "cannot read color table";
    }
    //lut= long(lut);
    palette, r, g, b, query=1;
    x= span(0., 1., 256);
    xp= span(0., 1., numberof(r));
    palette, int(.5+interp(lut(,1), x, xp)),
      int(.5+interp(lut(,2), x, xp)),
      int(.5+interp(lut(,3), x, xp));
  }

  /* Close file and return. */
  close, file;
  if (!am_subroutine()) {
    return names;
  }
}
