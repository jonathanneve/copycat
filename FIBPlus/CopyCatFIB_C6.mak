# ---------------------------------------------------------------------------
!if !$d(BCC32)
BCC32 = bcc32pch $(SHOWPROCESSID) -pch="CopyCatFIB_C6_pch.h" -q
!endif
!if !$d(DCC32)
DCC32 = dcc32 -q
!endif

!if !$d(BCB)
BCB = $(MAKEDIR)\..
!endif

# ---------------------------------------------------------------------------
# IDE SECTION
# ---------------------------------------------------------------------------
# The following section of the project makefile is managed by the BCB IDE.
# It is recommended to use the IDE to change any of the values in this
# section.
# ---------------------------------------------------------------------------

VERSION = BCB.06.00
# ---------------------------------------------------------------------------
PROJECT = $(BCB)\Projects\Bpl\CopyCatFIB_C6.bpl
OBJFILES = CopyCatFIB_C6.obj CcProvFIBPlus.obj
RESFILES = 
MAINSOURCE = CopyCatFIB_C6.cpp
RESDEPEN = $(RESFILES)
LIBFILES = 
IDLFILES = 
IDLGENFILES = 
LIBRARIES = 
PACKAGES = rtl.bpi vcl.bpi dbrtl.bpi vcldb.bpi FIBPlus_CB6.bpi CopyCat_C6.bpi
SPARELIBS = rtl.lib vcl.lib dbrtl.lib vcldb.lib FIBPlus_CB6.lib CopyCat_C6.lib
DEFFILE = 
OTHERFILES = 
# ---------------------------------------------------------------------------
DEBUGLIBPATH = "$(BCB)\lib\debug"
RELEASELIBPATH = "$(BCB)\lib\release"
USERDEFINES = _DEBUG
SYSDEFINES = _RTLDLL;NO_STRICT;USEPACKAGES
INCLUDEPATH = "..\..\BCB6";"$(BCB)\include";"$(BCB)\include\vcl";"C:\Program Files\FIBPlus\BCB6"
LIBPATH = "..\..\BCB6";"$(BCB)\lib\release";"$(BCB)\projects\lib";"$(BCB)\lib\obj";"$(BCB)\lib";"c:\program files\fibplus\bcb6"
WARNINGS= -w-par
PATHCPP = "."
PATHASM = "."
PATHPAS = "."
PATHRC = "."
PATHOBJ = ".";"$(LIBPATH)"
# ---------------------------------------------------------------------------
CFLAG1 = -Od -H=c:\PROGRA~1\borland\CB3B65~1\lib\vcl60.csm -Hc -Vx -Ve -X- -r- -a8 \
    -b- -k -y -v -vi- -c -tWM
IDLCFLAGS = 
PFLAGS = -$YD -$W -$O- -$A8 -v -JPHNE -M
RFLAGS = 
AFLAGS = /mx /w2 /zd
LFLAGS = -D"CopyCat FIBPlus provider" -aa -Tpp -GD -s -Gn -Gl -Gi -v
# ---------------------------------------------------------------------------
ALLOBJ = c0pkg32.obj $(PACKAGES) Memmgr.Lib sysinit.obj $(OBJFILES)
ALLRES = $(RESFILES)
ALLLIB = $(LIBFILES) $(LIBRARIES) import32.lib cp32mti.lib
# ---------------------------------------------------------------------------
!ifdef IDEOPTIONS

[Version Info]
IncludeVerInfo=0
AutoIncBuild=0
MajorVer=1
MinorVer=0
Release=0
Build=0
Debug=0
PreRelease=0
Special=0
Private=0
DLL=0

[Version Info Keys]
CompanyName=
FileDescription=
FileVersion=1.0.0.0
InternalName=
LegalCopyright=
LegalTrademarks=
OriginalFilename=
ProductName=
ProductVersion=1.0.0.0
Comments=

[Debugging]
DebugSourceDirs=$(BCB)\source\vcl

!endif





# ---------------------------------------------------------------------------
# MAKE SECTION
# ---------------------------------------------------------------------------
# This section of the project file is not used by the BCB IDE.  It is for
# the benefit of building from the command-line using the MAKE utility.
# ---------------------------------------------------------------------------

.autodepend
# ---------------------------------------------------------------------------
!if "$(USERDEFINES)" != ""
AUSERDEFINES = -d$(USERDEFINES:;= -d)
!else
AUSERDEFINES =
!endif

!if !$d(BCC32)
BCC32 = bcc32
!endif

!if !$d(CPP32)
CPP32 = cpp32
!endif

!if !$d(DCC32)
DCC32 = dcc32
!endif

!if !$d(TASM32)
TASM32 = tasm32
!endif

!if !$d(LINKER)
LINKER = ilink32
!endif

!if !$d(BRCC32)
BRCC32 = brcc32
!endif


# ---------------------------------------------------------------------------
!if $d(PATHCPP)
.PATH.CPP = $(PATHCPP)
.PATH.C   = $(PATHCPP)
!endif

!if $d(PATHPAS)
.PATH.PAS = $(PATHPAS)
!endif

!if $d(PATHASM)
.PATH.ASM = $(PATHASM)
!endif

!if $d(PATHRC)
.PATH.RC  = $(PATHRC)
!endif

!if $d(PATHOBJ)
.PATH.OBJ  = $(PATHOBJ)
!endif
# ---------------------------------------------------------------------------
$(PROJECT): $(OTHERFILES) $(IDLGENFILES) $(OBJFILES) $(RESDEPEN) $(DEFFILE)
    $(BCB)\BIN\$(LINKER) @&&!
    $(LFLAGS) -L$(LIBPATH) +
    $(ALLOBJ), +
    $(PROJECT),, +
    $(ALLLIB), +
    $(DEFFILE), +
    $(ALLRES)
!
    @IF EXIST "$(BCB)\Projects\Bpl\CopyCatFIB_C6.lib" move /Y "$(BCB)\Projects\Bpl\CopyCatFIB_C6.lib" "$(BCB)\Projects\Lib\"
    @IF EXIST "$(BCB)\Projects\Bpl\CopyCatFIB_C6.bpi" move /Y "$(BCB)\Projects\Bpl\CopyCatFIB_C6.bpi" "$(BCB)\Projects\Lib\"
# ---------------------------------------------------------------------------
.pas.hpp:
    $(BCB)\BIN\$(DCC32) $(PFLAGS) -U$(INCLUDEPATH) -D$(USERDEFINES);$(SYSDEFINES) -O$(INCLUDEPATH) --BCB {$< }

.pas.obj:
    $(BCB)\BIN\$(DCC32) $(PFLAGS) -U$(INCLUDEPATH) -D$(USERDEFINES);$(SYSDEFINES) -O$(INCLUDEPATH) --BCB {$< }

.cpp.obj:
    $(BCB)\BIN\$(BCC32) $(CFLAG1) $(WARNINGS) -I$(INCLUDEPATH) -D$(USERDEFINES);$(SYSDEFINES) -n$(@D) {$< }

.c.obj:
    $(BCB)\BIN\$(BCC32) $(CFLAG1) $(WARNINGS) -I$(INCLUDEPATH) -D$(USERDEFINES);$(SYSDEFINES) -n$(@D) {$< }

.c.i:
    $(BCB)\BIN\$(CPP32) $(CFLAG1) $(WARNINGS) -I$(INCLUDEPATH) -D$(USERDEFINES);$(SYSDEFINES) -n. {$< }

.cpp.i:
    $(BCB)\BIN\$(CPP32) $(CFLAG1) $(WARNINGS) -I$(INCLUDEPATH) -D$(USERDEFINES);$(SYSDEFINES) -n. {$< }

.asm.obj:
    $(BCB)\BIN\$(TASM32) $(AFLAGS) -i$(INCLUDEPATH:;= -i) $(AUSERDEFINES) -d$(SYSDEFINES:;= -d) $<, $@

.rc.res:
    $(BCB)\BIN\$(BRCC32) $(RFLAGS) -I$(INCLUDEPATH) -D$(USERDEFINES);$(SYSDEFINES) -fo$@ $<



# ---------------------------------------------------------------------------





nolink: $(OTHERFILES) $(IDLGENFILES) $(OBJFILES) $(RESDEPEN) $(DEFFILE)
