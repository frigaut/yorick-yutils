# these values filled in by    yorick -batch make.i
Y_MAKEDIR=/home/frigaut/yorick-2.1/Linux-i686
Y_EXE=/home/frigaut/yorick-2.1/Linux-i686/bin/yorick
Y_EXE_PKGS=
Y_EXE_HOME=/home/frigaut/yorick-2.1/Linux-i686
Y_EXE_SITE=/home/frigaut/yorick-2.1

# 
# >>>>  THIS IS NOT A PLUGIN !!!! <<<<
# 
# This is a package made of several interpreted 
# include file. This makefile is just used to build the distribution tar file.

PKG_NAME = yutils
PKG_I_START = yutils_start.i
PKG_I_EXTRA=astro_util1.i check.i constants.i copy_plot.i detect.i fft_utils.i histo.i idl-colors.i img.i linalg.i lmfit.i plot_demo2.i plot_demo.i plot.i plvp.i poly.i pyk.i random_et.i rdcols.i rgb.i util_fr.i utils.i

PKG_OS =
# ^^^ this should be empty for this package (not a plugin!)
PKG_VERSION = $(shell (awk '{if ($$1=="Version:") print $$2}' $(PKG_NAME).info))
# .info might not exist, in which case he line above will exit in error.

include $(Y_MAKEDIR)/Make.cfg
DEST_Y_SITE=$(DESTDIR)$(Y_SITE)
DEST_Y_HOME=$(DESTDIR)$(Y_HOME)
DEST_Y_BINDIR=$(DESTDIR)$(Y_BINDIR)

build:
	@echo "Nothing to build. This is not a plugin"
	@echo "other targets: install, clean"
	@echo "for maintainers: package, distpkg"

clean:
	-rm -rf pkg *~

install:
	cp -p *.i $(DEST_Y_SITE)/i/
	-rm $(DEST_Y_SITE)/i/yutils_start.i
	mkdir -p $(DEST_Y_SITE)/data
	cp -p colors1.tbl $(DEST_Y_SITE)/data/
	mkdir -p $(DEST_Y_SITE)/python
	cp -p pyk.py $(DEST_Y_SITE)/python/
	mkdir -p $(DEST_Y_SITE)/i-start
	cp -p yutils_start.i $(DEST_Y_SITE)/i-start/

uninstall:
	@echo removing $(PKG_I_EXTRA)
	@-for i in $(PKG_I_EXTRA); do \
		rm $(DEST_Y_SITE)/i/$$i; \
        done
	-rm $(DEST_Y_SITE)/i-start/yutils_start.i
	-rm $(DEST_Y_SITE)/data/colors1.tbl
	-rm $(DEST_Y_SITE)/python/pyk.py


package:
	mkdir -p pkg/$(PKG_NAME)/dist/y_site/i-start
	mkdir -p pkg/$(PKG_NAME)/dist/y_site/i
	mkdir -p pkg/$(PKG_NAME)/dist/y_site/python
	cp -p *.i pkg/$(PKG_NAME)/dist/y_site/i/
	if test -f "check.i"; then rm pkg/$(PKG_NAME)/dist/y_site/i/check.i; fi
	cp -p *.tbl pkg/$(PKG_NAME)/dist/y_site/i/
	cp -p *.py pkg/$(PKG_NAME)/dist/y_site/python/
	if test -f "check.i"; then cp -p check.i pkg/$(PKG_NAME)/.; fi
	if test -n "$(PKG_I_START)"; then cp -p $(PKG_I_START) \
	  pkg/$(PKG_NAME)/dist/y_site/i-start/; fi
	cp -p $(PKG_NAME).info pkg/$(PKG_NAME)/$(PKG_NAME).info
	cd pkg; tar zcvf $(PKG_NAME)-$(PKG_VERSION)-pkg.tgz $(PKG_NAME)

distpkg:
	if test -f "pkg/$(PKG_NAME)-$(PKG_VERSION)-pkg.tgz" ; then \
	  ncftpput -f $(HOME)/.ncftp/maumae www/yorick/packages/tarballs/ \
	  pkg/$(PKG_NAME)-$(PKG_VERSION)-pkg.tgz; fi
		if test -f "pkg/$(PKG_NAME)/$(PKG_NAME).info" ; then \
		  ncftpput -f $(HOME)/.ncftp/maumae www/yorick/packages/macosx/info/ \
		  pkg/$(PKG_NAME)/$(PKG_NAME).info; fi
	if test -f "pkg/$(PKG_NAME)/$(PKG_NAME).info" ; then \
	  ncftpput -f $(HOME)/.ncftp/maumae www/yorick/packages/linux/info/ \
	  pkg/$(PKG_NAME)/$(PKG_NAME).info; fi
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
	make clean; rm -rf pkg
	cd ..; tar --exclude pkg --exclude .svn -zcvf \
	   $(PKG_NAME)-$(PKG_VERSION)-src.tgz $(PKG_NAME);\
	ncftpput -f $(HOME)/.ncftp/maumae www/yorick/packages/src/ \
	   $(PKG_NAME)-$(PKG_VERSION)-src.tgz


# -------------------------------------------------------- end of Makefile
