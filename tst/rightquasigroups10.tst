# RightQuasigroups, chapter 10
#
# DO NOT EDIT THIS FILE - EDIT EXAMPLES IN THE SOURCE INSTEAD!
#
# This file has been generated by AutoDoc. It contains examples extracted from
# the package documentation. Each example is preceded by a comment which gives
# the name of a GAPDoc XML file and a line range from which the example were
# taken. Note that the XML file in turn may have been generated by AutoDoc
# from some other input.
#
gap> START_TEST("rightquasigroups10.tst");

# doc/_Chapter_Bol_loops_Bruck_loops_and_Moufang_loops.xml:30-34
gap> Q := RightBolLoop(21,1);;
gap> AssociatedRightBruckLoop( Q );
<right Bruck loop of size 21>

# doc/_Chapter_Bol_loops_Bruck_loops_and_Moufang_loops.xml:60-68
gap> G := SymmetricGroup( 5 );;
gap> H1 := Subgroup( G, [(1,2),(1,3),(1,4)] );;
gap> H2 := Subgroup( G, [(1,2,3,4,5)] );;
gap> IsExactGroupFactorization( G, H1, H2 );
true
gap> RightBolLoopByExactGroupFactorization( G, H1, H2 );
<right Bol loop of size 120>

#
gap> STOP_TEST("rightquasigroups10.tst", 1);
