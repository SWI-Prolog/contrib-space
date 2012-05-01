################################################################
# Build the SWI-Prolog space package for MS-Windows
#
# Author: Willem Robert van Hage
#
# Use:
#	nmake /f Makefile.mak
#	nmake /f Makefile.mak install
################################################################

PLHOME=..\..
!include $(PLHOME)\src\rules.mk
CFLAGS=$(CFLAGS) /D__SWI_PROLOG__ -I $(PLHOME)\packages\cpp

LIBS=	spatialindex_i.lib geos.lib user32.lib
LIBPL=	dbpedia.pl demo_space.pl freebase.pl georss.pl gml.pl kml.pl \
	space.pl space_web_loader.pl wgs84.pl wkt.pl

LIBDIR=$(PLBASE)\library\space
{src}.cc{src}.obj:
		@$(CC) -I. -Irc -I $(PLHOME)\include $(CFLAGS) /Fo$@ $<

OBJ=		src/space.obj src/globals.obj src/Index.obj src/Search.obj src/Shapes.obj src/lock.obj src/debug.obj

all:		space.dll

space.dll:	$(OBJ)
		$(LD) /dll /out:$@ $(LDFLAGS) $(OBJ) $(PLLIB) $(LIBS)

!IF "$(CFG)" == "rt"
install:	idll
!ELSE
install:	idll ilib
!ENDIF

################################################################
# Testing
################################################################

check::

################################################################
# Installation
################################################################

idll::
		copy space.dll "$(BINDIR)"
		copy $(PLHOME)\lib\spatialindex1.dll "$(BINDIR)"
		copy $(PLHOME)\lib\geos.dll "$(BINDIR)"
!IF "$(PDB)" == "true"
		copy space.pdb "$(BINDIR)"
!ENDIF

ilib::
		if not exist "$(LIBDIR)\$(NULL)" $(MKDIR) "$(LIBDIR)"
		@for %f in ($(LIBPL)) do @copy %f "$(LIBDIR)"
		$(MAKEINDEX)

uninstall::
		deltree "$(LIBDIR)"
		del "$(BINDIR)\space.dll"
		del "$(BINDIR)\spatialindex1.dll"
		del "$(BINDIR)\geos.dll"
		$(MAKEINDEX)

html-install::
		copy space.html "$(PKGDOC)"

xpce-install::

installer::
		copy space.nsi "$(PLBASE)\..\space.nsi"
		"$(NSIS)" $(NSISDEFS) "$(PLBASE)\..\space.nsi"

clean::
		if exist src\*.obj del src\*.obj
		if exist *~ del *~

distclean:	clean
		-DEL *.dll *.lib *.exp *.ilk *.pdb 2>nul


