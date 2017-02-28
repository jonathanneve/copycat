//CopyCat replication suite<p/>
//Copyright (c) 1996 AO ROSNO<p/>
//Copyright (c) 1997, 1998 Master-Bank<p/>
//Copyright (c) 2005 Microtec Communications

// Conditional defines that affect which Indy version (9 or 10) is used for compilation

{$I ..\CC.INC}

{$DEFINE HAS_CONTNRS_UNIT}

// Comment out the line below to use Indy9 in D2005 or later compilers
//{$IFNDEF CC_D2K5} // Borland Delphi 2005 or higher
  // Comment out the line below to use Indy10 in D7 or earlier compilers
//  {$DEFINE INDY9} // Indy9 shipped with D7, earlier compilers should be upgraded to at least Indy9
//{$ENDIF}

