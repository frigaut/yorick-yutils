/* DOCUMENT  yorick -batch makenix.i distrib
         or  yorick -batch makenix.i install

         distrib create a tarfile of the distribution in ..
         install copy the files where they belong
         
         This file is NOT PART of pkg_mngr.i
         It is mostly for a manual install (admin) of yutils.
   SEE ALSO:
 */

func yutils_install(void)
{
  system,"mv makenix.i makenix"; //bad practice, but low risks

  cmd="cp -p *.i "+Y_SITE+"i0/";
  write,cmd; system,cmd;
  cmd="cp -p *.tbl "+Y_SITE+"i0/";
  write,cmd; system,cmd;
  //  cmd="cp -p README "+Y_SITE+"i0/yutils.README";
  //  write,cmd; system,cmd;

  system,"mv makenix makenix.i";

  majorv = 0;
  minorv = 0;
  sread,strtok(Y_VERSION,".")(1),majorv;
  sread,strtok(strtok(Y_VERSION,".")(2),".")(1),minorv;
  if ((minorv>=6)||(majorv>=2)) {
    cmd="cp -p yutils_start.i "+Y_SITE+"i-start/";  
    write,cmd; system,cmd;
  }
  write,"\n Include files copied in "+Y_SITE+"i0/";
  write,"Color table copied in "+Y_SITE+"i0/";
  if ((minorv>=6)||(majorv>=2)) {
    write,"Autoload file copied in "+Y_SITE+"i-start/";
  }
}

func yutils_distrib(void)
/* DOCUMENT
   AS FAR AS I KNOW, THIS ROUTINE IS OBSOLETE.
   Use the yutils Makefile instead.
   SEE ALSO:
 */
{
  cmd="cd ..;rm yutils.tar yutils.tar.gz; tar cvf yutils.tar yutils; gzip yutils.tar";
  write,cmd; system,cmd;
  write,"yutils.tar.gz created in ..";
}

if (!batch()) error,"makenix has to be run in batch mode";

arg = get_argv();

if (numberof(arg)<2) {
  write,"Syntax: ";
  write,"    yorick -batch makenix.i distrib";
  write,"    yorick -batch makenix.i install";
  quit;
}

if (arg(2)=="install") yutils_install;
if (arg(2)=="distrib") yutils_distrib;
