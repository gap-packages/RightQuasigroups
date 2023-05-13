# RightQuasigroups, chapter 12
#
# DO NOT EDIT THIS FILE - EDIT EXAMPLES IN THE SOURCE INSTEAD!
#
# This file has been generated by AutoDoc. It contains examples extracted from
# the package documentation. Each example is preceded by a comment which gives
# the name of a GAPDoc XML file and a line range from which the example were
# taken. Note that the XML file in turn may have been generated by AutoDoc
# from some other input.
#
gap> START_TEST( "rightquasigroups12.tst");

# doc/_Chapter_Libraries_of_loops_racks_and_quandles.xml:61-71
gap> DisplayLibraryInfo("nilpotent loops");
The library contains all nonassociative nilpotent loops of order less than 12.
------
Extent of the library:
   2 algebras of order 6
   134 algebras of order 8
   8 algebras of order 9
   1043 algebras of order 10
true

# doc/_Chapter_Libraries_of_loops_racks_and_quandles.xml:85-90
gap> LibraryAlgebra( "Moufang loop", 64, 10 );
<Moufang loop 64/10>
gap> MoufangLoop( 64, 10 );
<Moufang loop 64/10>

#
gap> STOP_TEST("rightquasigroups12.tst", 1 );
# RightQuasigroups, chapter 12
#
# DO NOT EDIT THIS FILE - EDIT EXAMPLES IN THE SOURCE INSTEAD!
#
# This file has been generated by AutoDoc. It contains examples extracted from
# the package documentation. Each example is preceded by a comment which gives
# the name of a GAPDoc XML file and a line range from which the example were
# taken. Note that the XML file in turn may have been generated by AutoDoc
# from some other input.
#
gap> START_TEST( "rightquasigroups12.tst");

# doc/_Chapter_Libraries_of_loops_racks_and_quandles.xml:61-71
gap> DisplayLibraryInfo("nilpotent loops");
The library contains all nonassociative nilpotent loops of order less than 12.
------
Extent of the library:
   2 loops of order 6
   134 loops of order 8
   8 loops of order 9
   1043 loops of order 10
true

# doc/_Chapter_Libraries_of_loops_racks_and_quandles.xml:84-89
gap> LibraryAlgebra( "Moufang loops", 64, 10 );
<Moufang loop 64/10>
gap> MoufangLoop( 64, 10 );
<Moufang loop 64/10>

# doc/_Chapter_Libraries_of_loops_racks_and_quandles.xml:117-122
gap> LibraryAlgebras( "Moufang loops", Size, 81, IsCommutative, false );
[ <Moufang loop 81/3>, <Moufang loop 81/4>, <Moufang loop 81/5> ]
gap> MoufangLoops( [65..81], IsCommutative, Exponent, 3 ); 
[ <Moufang loop 81/1> ]

#
gap> STOP_TEST("rightquasigroups12.tst", 1 );
