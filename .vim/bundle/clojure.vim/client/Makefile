# This Makefile has only been tested on linux.  It uses
# MinGW32 to cross-compile for windows.  To install and
# configure MinGW32 on linux, see
# http://www.mingw.org/MinGWiki/index.php/BuildMingwCross
#
# Marty Lamb

CC=gcc
CFLAGS=-Wall -pedantic -s -O3

# Special library requirements
# Default:
LIBS=

# OpenSolaris 2009.06
#LIBS=-lsocket -lnsl

# Windows
#LIBS=-lwsock32

ng: ngclient/ng.c
	@echo "Building ng client.  To build a Windows binary, type 'make ng.exe'"
	${CC} ${CFLAGS} ${LIBS} -o ng ngclient/ng.c

clean:
	@echo "If you have a Windows binary, 'make clean' won't delete it."
	@echo "You must remove this manually.  Most users won't have MinGW"
	@echo "installed - so I'd rather not delete something they can't rebuild."
	rm ng
#	rm ng.exe
