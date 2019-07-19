#
# RightQuasigroups: Computing with one-sided quasigroups in GAP.
#
# This file runs package tests. It is also referenced in the package
# metadata in PackageInfo.g.
#
LoadPackage( "RightQuasigroups" );

TestDirectory(DirectoriesPackageLibrary( "RightQuasigroups", "tst" ),
  rec(exitGAP := true));

FORCE_QUIT_GAP(1); # if we ever get here, there was an error
