#
# RightQuasigroups: Computing with one-sided quasigroups in GAP.
#
# This file runs package tests. It is also referenced in the package
# metadata in PackageInfo.g.
#
LoadPackage( "RightQuasigroups" );

TestDirectory(
    DirectoriesPackageLibrary( "RightQuasigroups", "tst" ), 
    rec(
        exitGAP := false,
        rewriteToFile := false,
        exclude := [ "Constructors.tst"],
        testOptions := rec(
            width := 80,
            compareFunction := "uptowhitespace"
        )
    )
);

FORCE_QUIT_GAP(1); # if we ever get here, there was an error
