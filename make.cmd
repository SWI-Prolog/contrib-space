
@echo off

SETLOCAL

call ..\..\src\call_vcvars.cmd

rem Build default multi-threaded version
nmake /f makefile.mak %*

ENDLOCAL
