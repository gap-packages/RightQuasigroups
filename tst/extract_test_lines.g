# 6/6/2023 GP Nagy
# This file extract the gap script out a *.tst file in the package /tst directory.
# It saves the result to *.g in the same directory. 
# Running gap with *.g produces a correct temp_*.tst file in the /tmp directory. (Or fails.)
############################################################################################
LoadPackage("RightQuasigroups");

extract_test_lines := function( fname )
    local path,filein,s,cont;
    path:=DirectoriesPackageLibrary("RightQuasigroups","tst");
    filein := InputTextFile(Filename(path, Concatenation(fname, ".tst")));
    s := ReadLine(filein);
    cont:="LogTo(\"/tmp/temp_XXX.tst\");\nSTART_TEST(\"RightQuasigroups package: XXX.tst\");\nSizeScreen([80,23]);\n\nLoadPackage(\"RightQuasigroups\", false);\n\n";
    cont:=ReplacedString(cont,"XXX",fname);
    while s<>fail do
        if s[1]='#' or s="\n" then 
            Append(cont,s); 
        elif Size(s)>=4 and s{[1..4]}="gap>" then
            Append(cont,s{[6..Length(s)]});
        fi;
        s:=ReadLine(filein);
    od;
    CloseStream(filein);
    Append(cont,ReplacedString("\n\nSTOP_TEST( \"XXX.tst\", 10000 );\nLogTo();","XXX",fname));
    PrintTo(Filename(path[1], Concatenation(fname, ".g")),cont);
end;

# extract_test_lines("Constructors");
# extract_test_lines("CoreMethods");
# extract_test_lines("Elements");

extract_test_lines_2 := function( fname )
    local path,filein,s,cont;
    path:=DirectoriesPackageLibrary("RightQuasigroups","gap");
    filein:=Filename(path, Concatenation(fname, ".gd"));
    filein := InputTextFile(filein);
    #cont:="SizeScreen([80,23]);\nLoadPackage(\"RightQuasigroups\", false);\nLogTo(\"/tmp/temp_XXX.tst\");\n";
    cont:="LoadPackage(\"RightQuasigroups\", false);\nLogTo(\"/tmp/temp_XXX.tst\");\n";
    cont:=ReplacedString(cont,"XXX",fname);
    s := ReadLine(filein);
    while s<>fail do
        if Size(s)>=8 and s{[1..8]}="#! gap> " then
            Append(cont,s{[9..Length(s)]});
        fi;
        if Size(s)>=5 and s{[1..5]}="#! > " then
            Append(cont,"  ");
            Append(cont,s{[6..Length(s)]});
        fi;
        if Size(s)>=23 and s{[1..23]}="#! @BeginExampleSession" then
            Append(cont,"############################################\n");
        fi;
        s:=ReadLine(filein);
    od;
    CloseStream(filein);
    Append(cont,"############################################\nLogTo();\nQUIT;\n");
    PrintTo("temp.g",cont);
    Exec("gap < temp.g");
    Exec(Concatenation("sed -i 's/^/#! /' temp_",fname,".tst"));
    Exec("rm temp.g");
end;

#extract_test_lines_2("BolLoops");

for fn in [
    "BolLoops","Examples","Mappings","MoufangModifications",
    "Parser","Racks","Topisms","Constructors","Extensions",
    "MltSearch","MoufangTriality","PermutationGroups","Random",
    "Convert","HSP","Morphisms","NilpotencySolvability","Properties",
    "Representation"
    ]  do extract_test_lines_2(fn); od;
