%define name yorick-yutils
%define version 1.2.0
%define release gemini2007dec07

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
plotting, to system access, to math. More details:

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


%prep
%setup -q

%build

%install
rm -rf $RPM_BUILD_ROOT
mkdir -p $RPM_BUILD_ROOT/usr/lib/yorick/i0
mkdir -p $RPM_BUILD_ROOT/usr/lib/yorick/i-start

install -m 644 *.i $RPM_BUILD_ROOT/usr/lib/yorick/i0
install -m 644 colors1.tbl $RPM_BUILD_ROOT/usr/lib/yorick/i0
install -m 644 *_start.i $RPM_BUILD_ROOT/usr/lib/yorick/i-start

rm $RPM_BUILD_ROOT/usr/lib/yorick/i0/*_start.i


%clean
rm -rf $RPM_BUILD_ROOT

%files
%defattr(-,root,root)
/usr/lib/yorick/i0/*.i
/usr/lib/yorick/i0/colors1.tbl
/usr/lib/yorick/i-start/*_start.i

%changelog

* Tue Dec 11 2007 <frigaut@users.sourceforge.net>
- 1.2.0: various fixes (sky in astro_utils, round in util_fr)
- Homogeneized/changed licences to GPLv2
- gotten rid of pdb_utils for license issues
- added pyk.py
- modified idl-colors.i and pyk.i to search the whole path for include files
- fixed paths in rgb.i and added error checking.

* Thu Dec  6 2007 <frigaut@users.sourceforge.net>
- 1.1.03gemini Fixes bug in round() for negative numbers

