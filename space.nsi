# NSSI install-script for the SWI-Prolog space package

!define TEMP1 $R0 ; Temp variable
!define EXT    $3 ; Filename extension for Prolog sources
!define CWD    $4 ; Working directory for startmenu shortcut
!define GRP    $5 ; Startmenu group
!define SHCTX  $6 ; Shell context (current/all)
!define ARCH   $7 ; Architecture (x86, ia64 or amd64)

!ifdef WIN64
!define REGKEY SOFTWARE\SWI\Prolog64
!else
!define REGKEY SOFTWARE\SWI\Prolog
!endif

Name "SWI-Prolog space package 0.1.2."
OutFile "w32swispace012.exe"

RequestExecutionLevel admin
SetCompressor bzip2
MiscButtonText "<back" "next>" "abort" "finished"

InstallDir $PROGRAMFILES\pl
InstallDirRegKey HKLM ${REGKEY} "home"
ComponentText "This will install the SWI-Prolog spatial indexing package"
DirText "Please select the SWI-Prolog installation to install in"

LicenseData pl\COPYING.TXT
LicenseText "The SWI-Prolog spatial index package is governed by the LGPL"

!ifdef WIN64
Page custom Check64 "" ": Checking for AMD64 architecture"
!endif
Page license
Page directory
Page instfiles

Section "Spatial Indexing Library"
  SetOutPath $INSTDIR\bin
  File pl\bin\space.dll
  File pl\bin\geos.dll
  File pl\bin\spatialindex1.dll

  SetOutPath $INSTDIR\library
  File /r pl\library\space

  SetOutPath $INSTDIR\doc\packages
  File pl\doc\packages\space.html
SectionEnd

Section "Update library index"
  SectionIn RO			# do not allow to delete this
  ExecWait '"$INSTDIR\bin\swipl-win.exe" -f none -g "make_library_index(swi(library)),halt"'
SectionEnd

!macro Create_Internet_Shorcut URLName URLhost
  FileOpen $0 "$INSTDIR\doc\${URLName}.url" w
  FileWrite $0 "[InternetShortcut]$\r$\n"
  FileWrite $0 "URL=${URLhost}"
  FileClose $0
  CreateShortCut "$SMPROGRAMS\${GRP}\${URLName}.lnk" \
		 "$INSTDIR\doc\${URLName}.url" "" \
		 "$INSTDIR\doc\${URLName}.url" \
		 0 "SW_SHOWNORMAL" "" "Visit the Web site"
!macroend

Section "Update library index"
  SectionIn RO			# do not allow to delete this
  ExecWait '"$INSTDIR\bin\swipl-win.exe" -f none -g "make_library_index(swi(library)),halt"'
SectionEnd

Section "Create uninstaller"
  ; Write uninstaller
  WriteRegStr HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\SWI-Prolog-space" "DisplayName" "SWI-Prolog space package (remove only)"
  WriteRegStr HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\SWI-Prolog-space" "UninstallString" '"$INSTDIR\uninstall.exe"'
  WriteUninstaller "uninstall.exe"
SectionEnd

################################################################
# The uninstaller
################################################################

UninstallText "This will uninstall the SWI-Prolog space package. Hit Uninstall to continue."

Section "Uninstall"
  MessageBox MB_YESNO "Delete the following components?$\r$\n \
                       Install dir: $INSTDIR\library\space$\r$\n \
		       DLLs: space.dll geoss.dll spatialindex.dll" \
		      IDNO Done

  IfFileExists "$INSTDIR\library\space\space.pl" 0 NoDir
    RMDir /r "$INSTDIR\library\space"
    Delete "$INSTDIR\bin\space.dll"
    Delete "$INSTDIR\bin\geoss.dll"
    Delete "$INSTDIR\bin\spatialindex.dll"
    goto Done

  NoDir:
    MessageBox MB_OK "Folder $INSTDIR\library\space doesn't seem to contain the space package"

  Done:
    DeleteRegKey HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\SWI-Prolog-space"
SectionEnd

################################################################
# FUNCTIONS
################################################################

Function .onInit

!ifdef WIN64
# We are a 32-bit app, the real path for 64-bit apps is in ProgramW6432
  ReadEnvStr $INSTDIR ProgramW6432
  StrCpy $INSTDIR "$INSTDIR\pl"
!endif

FunctionEnd

################################################################
# Check 64-bit environment
# Note that NSIS is a 32-bit executable.  Such executables have
# set PROCESSOR_ARCHITEW6432 to IA64 or AMD64 on 64-bit platforms
################################################################

Function Check64
  ClearErrors
  ReadEnvStr ${ARCH} PROCESSOR_ARCHITEW6432
  IfErrors WrongArch
  StrCmpS ${ARCH} "AMD64" 0 WrongArch
    Return

WrongArch:
  MessageBox MB_OK \
	"Not an AMD64 version of Windows!$\r$\n\
	 This version of SWI-Prolog runs on 64-bits Windows$\r$\n\
	 using the AMD64/X64 architecture only"
  Quit
FunctionEnd

Function .onInstSuccess
FunctionEnd

Function .onInstFailed
  MessageBox MB_OK "Installation failed.$\r$\n\
		    If you cannot resolve the issue or it is a bug in the$\r$\n\
		    installer, please contact bugs@swi-prolog.org"
FunctionEnd
