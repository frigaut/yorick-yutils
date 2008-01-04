%define name yorick-yutils
%define version 1.3.0
%define release gemini2008jan04

Summary: Set of utility interpreted functions for yorick
Name: %{name}
Version: %{version}
Release: %{release}
Source0: %{name}-%{version}.tar.bz2
License: GPLv2
Group: Development/Languages
Packager: Francois Rigaut <frigaut@gemini.edu>
Url: http://www.maumae.net/yorick/doc/plugins.php
BuildRoot: %{_tmppath}/%{name}-%{version}-%{release}-buildroot
Requires: yorick >= 2.1
BuildArch: noarch


%description
yutils contains a collection of utility functions, ranging from
plotting, to system access, to math and coordinates transforms.
More details:

1. Content:
README
LICENSE
astro_util1.i
check.i
constants.i
copy_plot.i
detect.i
fft_utils.i
histo.i
idl-colors.i
img.i
linalg.i
lmfit.i
plot_demo2.i
plot_demo.i
plot.i
plvp.i
poly.i
pyk.i
random_et.i
rdcols.i
rgb.i
util_fr.i
utils.i
coords.i
doppler.i
graphk.i
gauss.i
tws*.i

%prep
%setup -q

%build
if [ -f check.i ] ; then
   mv check.i %{name}_check.i
fi;

%install
rm -rf $RPM_BUILD_ROOT
mkdir -p $RPM_BUILD_ROOT/usr/lib/yorick/i
mkdir -p $RPM_BUILD_ROOT/usr/lib/yorick/data
mkdir -p $RPM_BUILD_ROOT/usr/lib/yorick/python
mkdir -p $RPM_BUILD_ROOT/usr/lib/yorick/i-start
mkdir -p $RPM_BUILD_ROOT/usr/share/doc/yorick-yutils

install -m 644 *.i $RPM_BUILD_ROOT/usr/lib/yorick/i
install -m 644 colors1.tbl $RPM_BUILD_ROOT/usr/lib/yorick/data
install -m 644 *.py $RPM_BUILD_ROOT/usr/lib/yorick/python
install -m 644 *_start.i $RPM_BUILD_ROOT/usr/lib/yorick/i-start
install -m 644 LICENSE $RPM_BUILD_ROOT/usr/share/doc/yorick-yutils
install -m 644 README $RPM_BUILD_ROOT/usr/share/doc/yorick-yutils

rm $RPM_BUILD_ROOT/usr/lib/yorick/i/*_start.i


%clean
rm -rf $RPM_BUILD_ROOT

%files
%defattr(-,root,root)
/usr/lib/yorick/i/*.i
/usr/lib/yorick/data/colors1.tbl
/usr/lib/yorick/python/*.py
/usr/lib/yorick/i-start/*_start.i
/usr/share/doc/yorick-yutils/*

%changelog
* Fri Jan 04 2008 <frigaut@users.sourceforge.net>
- added files from Thibaut Paumard (coords, graphk, doppler, gauss, tws*)
- updated Makefile, README, info file
- bumped to v1.3.0

* Mon Dec 31 2007 <frigaut@users.sourceforge.net>
- new distro directory structure
- updated cvs

* Tue Dec 11 2007 <frigaut@users.sourceforge.net>
- 1.2.0: various fixes (sky in astro_utils, round in util_fr)
- Homogeneized/changed licences to GPLv2
- gotten rid of pdb_utils for license issues
- added pyk.py
- modified idl-colors.i and pyk.i to search the whole path for include files
- fixed paths in rgb.i and added error checking.

* Thu Dec  6 2007 <frigaut@users.sourceforge.net>
- 1.1.03gemini Fixes bug in round() for negative numbers

