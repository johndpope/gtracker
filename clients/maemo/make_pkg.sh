#!/bin/sh

VERSION=`cat find_me/main.cpp  | grep Version | grep "const QString" | tr -s '"' '\t' | cut -f 2`
NAME=findme
DST=${NAME}-${VERSION}
SRC=${DST}/src
MAIL="inndie@gmail.com"

mkdir -p ${SRC}
cp -R find_me/* ${SRC}

cat >> ${DST}/findme.pro << EOF
QMAKEVERSION = $$[QMAKE_VERSION]
ISQT4 = $$find(QMAKEVERSION, ^[2-9])
isEmpty( ISQT4 ) {
   error("Use the qmake include with Qt4.4 or greater, on Debian that is qmake-qt4");
}

TEMPLATE = subdirs
SUBDIRS  = src
EOF

cat > ${SRC}/deb/findme.desktop <<EOF
[Desktop Entry]
Encoding=UTF-8
Categories=gps-location
Version=${VERSION}
Type=Application
Name=FindMe
Comment=GTracker Service Client
Exec=/usr/bin/${NAME}
Icon=${NAME}
X-Window-Icon=${NAME}
X-Osso-Type=application/x-executable
EOF

export DEBFULLNAME="Sergey Kovalevich"
cd ${DST}
dh_make --createorig --single -e ${MAIL} -c gpl

cat > debian/postinst << EOF
#!/bin/sh
gtk-update-icon-cache -f /usr/share/icons/hicolor/
maemo-select-menu-location ${NAME}.desktop
EOF

chmod +x debian/postinst

cat > debian/rules << EOF
#!/usr/bin/make -f
APPNAME := ${NAME}
builddir:
	mkdir -p builddir

builddir/Makefile: builddir
	cd builddir && qmake-qt4 PREFIX=/usr ../\$(APPNAME).pro

build: build-stamp

build-stamp: builddir/Makefile
	dh_testdir
	# Add here commands to compile the package.
	cd builddir && \$(MAKE)
	touch \$@

clean:
	dh_testdir
	dh_testroot
	rm -f build-stamp
	# Add here commands to clean up after the build process.
	rm -rf builddir
	dh_clean

install: build
	dh_testdir
	dh_testroot
	dh_clean -k
	dh_installdirs

   # Add here commands to install the package into debian/your_appname
	cd builddir && \$(MAKE) INSTALL_ROOT=\$(CURDIR)/debian/\$(APPNAME) install

# Build architecture-independent files here.
binary-indep: build install
# We have nothing to do by default.

# Build architecture-dependent files here.
binary-arch: build install
	dh_testdir
	dh_testroot
	dh_installdocs
	dh_installexamples
	dh_installman
	dh_link
	dh_strip --dbg-package=my-application-dbg
	dh_compress
	dh_fixperms
	dh_installdeb
	dh_shlibdeps
	dh_gencontrol
	dh_md5sums
	dh_builddeb

binary: binary-indep binary-arch
.PHONY: build clean binary-indep binary-arch binary install configure
EOF

cat > debian/control << EOF
Source: findme
Section: user/navigation 
Version: ${VERSION}
Priority: optional
Maintainer: Sergey Kovalevich <inndie@gmail.com>
Build-Depends: debhelper (>= 5)
Standards-Version: 3.7.2

Package: findme
Architecture: any
Depends: ${shlibs:Depends}, ${misc:Depends}, libqt4-gui, libqt4-network
Description: Maemo client for gtracker.ru service
EOF

dpkg-buildpackage -rfakeroot -b -k${MAIL}
cd -
#rm -rf ${DST}
