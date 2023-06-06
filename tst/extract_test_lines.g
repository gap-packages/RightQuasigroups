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
    cont:="LogTo(\"/tmp/temp_XXX.tst\");\nSTART_TEST(\"RightQuasigroups package: XXX.tst\");\nSizeScreen([72,23]);\n\nLoadPackage(\"RightQuasigroups\", false);\n\n";
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

extract_test_lines("Constructors");
extract_test_lines("CoreMethods");
extract_test_lines("Elements");
