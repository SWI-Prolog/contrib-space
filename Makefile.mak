################################################################
# Build the SWI-Prolog nlp package for MS-Windows
#
# Author: Jan Wielemaker
# 
# Use:
#	nmake /f Makefile.mak
#	nmake /f Makefile.mak install
################################################################

PLHOME=..\..
!include $(PLHOME)\src\rules.mk
CFLAGS=$(CFLAGS) /D__SWI_PROLOG__ -I $(PLHOME)\packages\cpp

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
!IF "$(PDB)" == "true"
		copy space.pdb "$(BINDIR)"
!ENDIF

ilib::
		copy *.pl "$(PLBASE)\library"
		$(MAKEINDEX)

uninstall::
		del "$(BINDIR)\space.dll"
		del "$(PLBASE)\library\*.pl"
		$(MAKEINDEX)

html-install::
		copy space.html "$(PKGDOC)"

xpce-install::

clean::
		if exist *.obj del *.obj
		if exist *~ del *~

distclean:	clean
		-DEL *.dll *.lib *.exp *.ilk *.pdb 2>nul


