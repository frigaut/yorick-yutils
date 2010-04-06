# these values filled in by    yorick -batch make.i
Y_MAKEDIR=/usr/lib/yorick
Y_EXE=/usr/lib/yorick/bin/yorick
Y_EXE_PKGS=
Y_EXE_HOME=/usr/lib/yorick
Y_EXE_SITE=/usr/lib/yorick

# 
# !! THIS IS NOT A PLUGIN !!
# This is a package made of several interpreted 
# include file. This makefile is just used to install,
# uninstall it or build the distribution tar file.

# ------------------------------------------------ macros for this package

# used for distribution
PKG_NAME = yutils
# include files for this package
PKG_I=astro_util1.i constants.i coords.i copy_plot.i detect.i doppler.i \
	emulate_yeti.i fft_utils.i gauss.i graphk.i histo.i idl-colors.i img.i \
	linalg.i lmfit.i moffat.i multiprofile.i plot_demo2.i plot_demo.i \
	plot.i plvp.i poly.i pyk.i random_et.i rdcols.i rgb.i tws_button.i \
	tws_field.i tws_grid.i tws.i tws_label.i tws_popup.i tws_radio.i \
	tws_root.i util_fr.i utils.i
# autoload file for this package, if any
PKG_I_START = emulate_yeti_start.i yutils_start.i

# override macros Makepkg sets for rules and other macros
# Y_HOME and Y_SITE in Make.cfg may not be correct (e.g.- relocatable)
Y_HOME=$(Y_EXE_HOME)
Y_SITE=$(Y_EXE_SITE)

# include $(Y_MAKEDIR)/Make.cfg
DEST_Y_SITE=$(DESTDIR)$(Y_SITE)
DEST_Y_HOME=$(DESTDIR)$(Y_HOME)

# ------------------------------------- targets and rules for this package

build:
	@echo "Nothing to build. This is not a plugin"
	@echo "other targets: install, uninstall, clean"
	@echo "for maintainers: package, distpkg"

clean:
	-rm -rf pkg *~

install:
	mkdir -p $(DEST_Y_SITE)/i
	mkdir -p $(DEST_Y_SITE)/data
	mkdir -p $(DEST_Y_SITE)/python
	mkdir -p $(DEST_Y_SITE)/i-start
	cp -p $(PKG_I) $(DEST_Y_SITE)/i/
	cp -p colors1.tbl $(DEST_Y_SITE)/data/
	cp -p pyk.py $(DEST_Y_SITE)/python/
	cp -p $(PKG_I_START) $(DEST_Y_SITE)/i-start/

uninstall:
	-cd $(DEST_Y_SITE)/i; rm $(PKG_I)
	-rm $(DEST_Y_SITE)/i-start/yutils_start.i
	-rm $(DEST_Y_SITE)/data/colors1.tbl
	-rm $(DEST_Y_SITE)/python/pyk.py

# -------------------------------------------- package build rules

PKG_VERSION = $(shell (awk '{if ($$1=="Version:") print $$2}' $(PKG_NAME).info))
# .info might not exist, in which case he line above will exit in error.

package:
	mkdir -p pkg/$(PKG_NAME)/dist/y_site/i
	mkdir -p pkg/$(PKG_NAME)/dist/y_site/python
	mkdir -p pkg/$(PKG_NAME)/dist/y_site/data
	mkdir -p pkg/$(PKG_NAME)/dist/y_site/i-start
	cp -p $(PKG_I) pkg/$(PKG_NAME)/dist/y_site/i/
	cd pkg/$(PKG_NAME)/dist/y_site/i/; if test -f "check.i"; then rm check.i; fi
	if test -f "check.i"; then cp -p check.i pkg/$(PKG_NAME)/.; fi
	cp -p *.py pkg/$(PKG_NAME)/dist/y_site/python/
	cp -p *.tbl pkg/$(PKG_NAME)/dist/y_site/data/
	if test -n "$(PKG_I_START)"; then cp -p $(PKG_I_START) \
	  pkg/$(PKG_NAME)/dist/y_site/i-start/; fi
	cp -p $(PKG_NAME).info pkg/$(PKG_NAME)/$(PKG_NAME).info
	cd pkg; tar zcvf $(PKG_NAME)-$(PKG_VERSION)-pkg.tgz $(PKG_NAME)

distpkg:
#tarball there
	if test -f "pkg/$(PKG_NAME)-$(PKG_VERSION)-pkg.tgz" ; then \
	  ncftpput -f $(HOME)/.ncftp/maumae www/yorick/packages/tarballs/ \
	  pkg/$(PKG_NAME)-$(PKG_VERSION)-pkg.tgz; fi
#info files in each architecture directory
	if test -f "pkg/$(PKG_NAME)/$(PKG_NAME).info" ; then \
		ncftpput -f $(HOME)/.ncftp/maumae www/yorick/packages/darwin-ppc/info/ \
		pkg/$(PKG_NAME)/$(PKG_NAME).info; fi
	if test -f "pkg/$(PKG_NAME)/$(PKG_NAME).info" ; then \
		ncftpput -f $(HOME)/.ncftp/maumae www/yorick/packages/darwin-i386/info/ \
		pkg/$(PKG_NAME)/$(PKG_NAME).info; fi
	if test -f "pkg/$(PKG_NAME)/$(PKG_NAME).info" ; then \
		ncftpput -f $(HOME)/.ncftp/maumae www/yorick/packages/linux-ppc/info/ \
		pkg/$(PKG_NAME)/$(PKG_NAME).info; fi
	if test -f "pkg/$(PKG_NAME)/$(PKG_NAME).info" ; then \
		ncftpput -f $(HOME)/.ncftp/maumae www/yorick/packages/linux-x86/info/ \
		pkg/$(PKG_NAME)/$(PKG_NAME).info; fi

distsrc:
	make clean
	-rm -rf pkg
	cd ..; tar --exclude pkg --exclude .svn --exclude CVS --exclude *.spec -zcvf \
	   $(PKG_NAME)-$(PKG_VERSION)-src.tgz yorick-$(PKG_NAME)-$(PKG_VERSION);\
	ncftpput -f $(HOME)/.ncftp/maumae www/yorick/packages/src/ \
	   $(PKG_NAME)-$(PKG_VERSION)-src.tgz
	ncftpput -f $(HOME)/.ncftp/maumae www/yorick/contrib/ \
	   ../$(PKG_NAME)-$(PKG_VERSION)-src.tgz


# -------------------------------------------------------- end of Makefile
