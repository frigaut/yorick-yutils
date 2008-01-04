/*
 * graphk.i
 *
 * $Id: graphk.i,v 1.1 2008-01-04 13:47:48 frigaut Exp $
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
 * $Log: graphk.i,v $
 * Revision 1.1  2008-01-04 13:47:48  frigaut
 * initial import of thibaut's functions
 *
 *
 */


extern graphk;
/* DOCUMENT graphk.i
    Alternate syntax for graphic routines:  every keywords are set through only
    one, of  type GraphK. Members of  GraphK are all pointer,  for some graphic
    keywords support several datatypes. This syntax is useful when one wants to
    carry  graphic keywords  trough programs  in a  handy way,  not  really for
    command line. Currently supported: plhk and plgk.
   SEE ALSO: GraphK, plhk, plgk, plmkk, drawmaskk in drawmask.i
*/

func plhk(y,x,keywords=) {
/* DOCUMENT plhk,y
         or plhk,y,x

    Wrapper to plh, which accepts a single keyword: KEYWORDS (of type GraphK).

    Example: plhk,y,x,keywords=GraphK(color=&string("white"));

    Sounds nuts, but it  can be quite useful to have all  graphic keywords in a
    single variable, that can be made external or belong to a sructure...

   SEE ALSO: plh, plgk, GraphK, graphk, pltk
*/
    if (is_void(keywords)) plh,y,x;
    else plh,y,x,just=*keywords.just,legend=*keywords.legend,
             hide=*keywords.hide, type=*keywords.type, width=*keywords.width,
             color=*keywords.color, marks=*keywords.marks,
             marker=*keywords.marker, mspace=*keywords.mspace,
             mphase=*keywords.mphase;
}

func plgk(y,x,keywords=) {
/* DOCUMENT plhk,y
         or plhk,y,x

    Wrapper to plg, which accepts a single keyword: KEYWORDS (of type GraphK).

    Example: plgk,y,x,keywords=GraphK(color=&string("white"));

    Sounds nuts, but it  can be quite useful to have all  graphic keywords in a
    single variable, that can be made external or belong to a sructure...

   SEE ALSO: plg, plhk, GraphK, graphk, pltk
*/
    if (is_void(keywords)) plg,y,x;
    else plg,y,x,legend=*keywords.legend, hide=*keywords.hide,
             type=*keywords.type, width=*keywords.width,
             color=*keywords.color, closed=*keywords.closed,
             smooth=*keywords.smooth, marks=*keywords.marks,
             marker=*keywords.marker, mspace=*keywords.mspace,
             mphase=*keywords.mphase, rays=*keywords.rays,
             arrowl=*keywords.arrowl, arroww=*keywords.arroww,
             rspace=*keywords.rspace, rphase=*keywords.rphase;
}

func plmkk(y,x,keywords=){
/* DOCUMENT plmkk,y
         or plmkk,y,x

    Wrapper to plmk, which accepts a single keyword: KEYWORDS (of type GraphK).

    Example: plmkk,y,x,keywords=GraphK(msize=&double(0.2));

    Sounds nuts, but it  can be quite useful to have all  graphic keywords in a
    single variable, that can be made external or belong to a sructure...

   SEE ALSO: plg, plhk, GraphK, graphk, plgk, pltk
*/
    if (is_void(keywords)) plmk,y,x;
    else plmk,y,x,width=*keywords.width,color=*keywords.color,
             marker=*keywords.marker,msize=*keywords.msize;
}
    

func pltk(t,x,y,keywords=){
/* DOCUMENT pltk,text,x,y

    Wrapper to plt, which accepts a single keyword: KEYWORDS (of type GraphK).

    Example: pltk,"text",x,y,keywords=GraphK(font=&string("helveticaB"));

    Sounds nuts, but it  can be quite useful to have all  graphic keywords in a
    single variable, that can be made external or belong to a sructure...

   SEE ALSO: plg, plhk, GraphK, graphk, plgk, plmkk
*/
    if (is_void(keywords)) plt,t,x,y;
    else plt,t,x,y,legend=*keywords.legend,hide=*keywords.hide,
             color=*keywords.color,font=*keywords.font,height=*keywords.height,
             opaque=*keywords.opaque,orient=*keywords.orient,
             justify=*keywords.justify,tosys=*keywords.tosys;
}
    

struct GraphK {
/* DOCUMENT GraphK
       A   structure   for   storing   all   possible   keywords   to   graphic
       routines.  Members are  _pointers_, as  a few  of these  keywords accept
       several data types.

       Example:
       mykeywords=GraphK(color=&string("white"),msize=&double(0.5)[...]);
       plgk,y,x,keywords=mykeywords;

       Note: You cannot make a reference to a known value directly like "&0.5",
       but  you can  always  do  "&double(0.5)".  Makes  the  syntax even  more
       awkward,   but  this   still  simplifies   somewhat   implementing  some
       programs...
       
   SEE ALSO: plgk, plhk, plg, plh, plmkk, graphk, pltk
*/ 
    pointer legend, hide,type, width, color, closed, smooth, marks, marker,
        mspace, mphase, rays, arrowl, arroww, rspace, rphase, just, msize,
        font, height, opaque, orient, justify, tosys;
}

