# 
# >>>>  THIS IS NOT A PLUGIN !!!! <<<<
# 
# This is a package made of several interpreted 
# include file. This makefile is just used to build the distribution tar file.

PKG_NAME = yutils
PKG_I_START = yutils_start.i

PKG_OS =
# ^^^ this should be empty for this package (not a plugin!)
PKG_VERSION = $(shell (awk '{if ($$1=="Version:") print $$2}' $(PKG_NAME).info))
# .info might not exist, in which case he line above will exit in error.

build:
	@echo "Nothing to build. This is not a plugin"
	@echo "other targets: install, clean"
	@echo "for maintainers: package, distpkg"

install:
	yorick -batch makenix.i install

clean:
	-rm -rf pkg *~

package:
	mkdir -p pkg/$(PKG_NAME)/dist/y_site/i-start
	mkdir -p pkg/$(PKG_NAME)/dist/y_site/i0
	mkdir -p pkg/$(PKG_NAME)/dist/y_site/python
	cp -p *.i pkg/$(PKG_NAME)/dist/y_site/i0/
	if test -f "check.i"; then rm pkg/$(PKG_NAME)/dist/y_site/i0/check.i; fi
	cp -p *.tbl pkg/$(PKG_NAME)/dist/y_site/i0/
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
