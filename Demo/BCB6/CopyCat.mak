# ---------------------------------------------------------------------------
!if !$d(BCC32)
BCC32 = bcc32pch $(SHOWPROCESSID) -pch="CopyCat_pch.h" -q
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
PROJECT = CopyCat.exe
OBJFILES = CopyCat.obj Unit1.obj Unit2.obj "C:\Program Files\EurekaLog 5\CBuilder6\ExceptionLog.obj"
RESFILES = CopyCat.res
MAINSOURCE = CopyCat.cpp
RESDEPEN = $(RESFILES) Unit1.dfm Unit2.dfm
LIBFILES = 
IDLFILES = 
IDLGENFILES = 
LIBRARIES = FIBPlus_CB6.lib dbexpress.lib dsnap.lib cds.lib bdertl.lib \
    CopyCatIBX_C6.lib CopyCat_C6.lib ibxpress.lib vcldb.lib dbrtl.lib vcl.lib \
    rtl.lib
PACKAGES = rtl.bpi vcl.bpi dbrtl.bpi vcldb.bpi ibxpress.bpi CopyCat_C6.bpi \
    CopyCatIBX_C6.bpi
SPARELIBS = rtl.lib vcl.lib dbrtl.lib vcldb.lib ibxpress.lib CopyCat_C6.lib \
    CopyCatIBX_C6.lib bdertl.lib cds.lib dsnap.lib dbexpress.lib FIBPlus_CB6.lib
DEFFILE = 
OTHERFILES = 
# ---------------------------------------------------------------------------
DEBUGLIBPATH = "$(BCB)\lib\debug"
RELEASELIBPATH = "$(BCB)\lib\release"
USERDEFINES = EUREKALOG;EUREKALOG_VER5
SYSDEFINES = NO_STRICT
INCLUDEPATH = "C:\Program Files\EurekaLog 5\CBuilder6";"C:\Program Files\Borland\CBuilder6\Projects";"C:\Program Files\Borland\CBuilder5\Projects";"C:\Program Files\EurekaLog 5\CBuilder5";"$(BCB)\include";"$(BCB)\include\vcl";"C:\Program Files\Fibplus\bcb6";"C:\Program Files\CopyCatFull\BCB6"
LIBPATH = "C:\Program Files\EurekaLog 5\CBuilder6";"C:\Program Files\Borland\CBuilder6\Projects";"C:\Program Files\Borland\CBuilder5\Projects";"C:\Program Files\EurekaLog 5\CBuilder5";"$(BCB)\lib\obj";"$(BCB)\lib";"C:\Program Files\Fibplus\bcb6";"$(BCB)\Projects\lib";"C:\Program Files\CopyCatFull\BCB6"
WARNINGS= -w-par
PATHCPP = "."
PATHASM = "."
PATHPAS = ".";"C:\Program Files\EurekaLog 5\CBuilder6"
PATHRC = "."
PATHOBJ = ".";"$(LIBPATH)"
# ---------------------------------------------------------------------------
CFLAG1 = -O2 -Vx -Ve -X- -a8 -b- -k- -v -vi -c -tW -tWM
IDLCFLAGS = 
PFLAGS = -$Y- -$L- -$A8 -v -JPHNE -M
RFLAGS = 
AFLAGS = /mx /w2 /zn
LFLAGS = -D"" -aa -Tpe -GD -s -Gn -v
# ---------------------------------------------------------------------------
ALLOBJ = c0w32.obj sysinit.obj $(OBJFILES)
ALLRES = $(RESFILES)
ALLLIB = $(LIBFILES) $(LIBRARIES) import32.lib cp32mt.lib
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
