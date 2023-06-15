#
# RightQuasigroups: Computing with one-sided quasigroups in GAP.
#
# This file is a script which compiles the package manual.
#
if fail = LoadPackage("AutoDoc", "2018.02.14") then
    Error("AutoDoc version 2018.02.14 or newer is required.");
fi;

if not (IsBound( extr) and extr) then 
    extr := false;
fi;

AutoDoc( 
    rec( 
        scaffold := rec( bib := "bib.xml.bib" ), 
        autodoc := true, 
        extract_examples := extr 
    ) 
);

if not extr then 
    Print( "# Use >>gap -c \"extr:=true;\" makedoc.g<< to extract examples.\n" );
fi;

QUIT;
