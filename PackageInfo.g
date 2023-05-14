#
# RightQuasigroups: Computing with one-sided quasigroups in GAP.
#
SetPackageInfo( rec(

PackageName := "RightQuasigroups",
Subtitle := "Computing with one-sided quasigroups in GAP.",
Version := "0.853",
Date := "31/01/2022", # dd/mm/yyyy format
License := "GPL-2.0-or-later",

Persons := [
  rec(
    IsAuthor := true,
    IsMaintainer := true,
    FirstNames := "Gábor P.",
    LastName := "Nagy",
    WWWHome := "https://algebra.math.bme.hu/nagy-gabor-peter",
    Email := "nagyg@math.bme.hu",
    PostalAddress := Concatenation(
               "Department of Algebra, Budapest University of Technology\n",
               "Egry József utca 1\n",
               "H-1111 Budapest (Hungary)" ),
    Place := "Budapest",
    Institution := "Budapest University of Technology and Economics",
    # WWWHome := "http://www.math.u-szeged.hu/~nagyg",
    # Email := "nagyg@math.u-szeged.hu",
    # PostalAddress := Concatenation(
    #            "Bolyai Institute of the University of Szeged\n",
    #            "Aradi vértanúk tere 1\n",
    #            "H-6720 Szeged (Hungary)" ),
    # Place := "Szeged",
    # Institution := "University of Szeged",
  ),
  rec(
    IsAuthor := true,
    IsMaintainer := true,
    FirstNames := "Petr",
    LastName := "Vojtěchovský",
    WWWHome := "http://www.math.du.edu/~petr/",
    Email := "petr@math.du.edu",
    PostalAddress := Concatenation(
               "Department of Mathematics, University of Denver\n",
               "2390 S York St\n",
               "Denver, CO 80208\n",
               "USA" ),
    Place := "Denver",
    Institution := "University of Denver",
  ),
],

#SourceRepository := rec( Type := "TODO", URL := "URL" ),
#IssueTrackerURL := "TODO",
PackageWWWHome := "http://xyz/",
PackageInfoURL := Concatenation( ~.PackageWWWHome, "PackageInfo.g" ),
README_URL     := Concatenation( ~.PackageWWWHome, "README.md" ),
ArchiveURL     := Concatenation( ~.PackageWWWHome,
                                 "/", ~.PackageName, "-", ~.Version ),

ArchiveFormats := ".tar.gz",

##  Status information. Currently the following cases are recognized:
##    "accepted"      for successfully refereed packages
##    "submitted"     for packages submitted for the refereeing
##    "deposited"     for packages for which the GAP developers agreed
##                    to distribute them with the core GAP system
##    "dev"           for development versions of packages
##    "other"         for all other packages
##
Status := "dev",

AbstractHTML   :=  Concatenation( 
  "This package implements basic functionalities for the class of one-sided ",
  "right quasigroups in such a way that they can be naturally extended to ",
  "other nonassociative structures like (two-sided) quasigroups, loops, ",
  "racks, quandles, etc.</p>",
  "While the methods of this package are intended to use for finite right ",
  "quasigroups, some of them may work in the infinite case as well.</p>"
),

PackageDoc := rec(
  BookName  := "RightQuasigroups",
  ArchiveURLSubset := ["doc"],
  HTMLStart := "doc/chap0.html",
  PDFFile   := "doc/manual.pdf",
  SixFile   := "doc/manual.six",
  LongTitle := "Computing with one-sided quasigroups in GAP.",
),

Dependencies := rec(
  GAP := ">= 4.9",
  NeededOtherPackages := [ ],
  SuggestedOtherPackages := [ ],
  ExternalConditions := [ ],
),

AvailabilityTest := ReturnTrue,

TestFile := "tst/testall.g",

Keywords := [ "right quasigroup", "nonassociative", "quasigroup", "loop", "rack", "quandle" ],

));
