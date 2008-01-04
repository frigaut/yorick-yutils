/*
 * coords.i
 * 
 * $Id: coords.i,v 1.1 2008-01-04 13:47:48 frigaut Exp $
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
 * $Log: coords.i,v $
 * Revision 1.1  2008-01-04 13:47:48  frigaut
 * initial import of thibaut's functions
 *
 */


extern coords,ndc2pt,pt2inch,inch2cm,ndc2inch,ndc2cm,pt2cm,cm2inch,cm2pt,cm2ndc,inch2pt,inch2ndc,pt2ndc;
/* DOCUMENT ndc2pt,pt2inch,inch2cm,ndc2inch,ndc2cm,pt2cm,cm2inch,cm2pt,cm2ndc,inch2pt,inch2ndc,pt2ndc

     convert between these.
*/
  

func ndc2pt(value)
{
  return value/0.0013;
}

func pt2inch(value)
{
  return value/72.27;
}

func inch2cm(value)
{
  return value*2.54;
}

func pt2ndc(value)
{
  return value*0.0013;
}

func inch2pt(value)
{
  return value*72.27;
}

func cm2inch(value)
{
  return value/2.54;
}

func ndc2inch(value)
{
  return value/0.093951;
}

func inch2ndc(value)
{
  return value*0.093951;
}

func ndc2cm(value)
{
  return value/0.0369886;
}

func cm2ndc(value)
{
  return value*0.0369886;
}

func pt2cm(value)
{
  return value/28.4528;
}

func cm2pt(value)
{
  return value*28.4528;
}
