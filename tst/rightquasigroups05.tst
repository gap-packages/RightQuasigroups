# RightQuasigroups, chapter 5
#
# DO NOT EDIT THIS FILE - EDIT EXAMPLES IN THE SOURCE INSTEAD!
#
# This file has been generated by AutoDoc. It contains examples extracted from
# the package documentation. Each example is preceded by a comment which gives
# the name of a GAPDoc XML file and a line range from which the example were
# taken. Note that the XML file in turn may have been generated by AutoDoc
# from some other input.
#
gap> START_TEST("rightquasigroups05.tst");

# doc/_Chapter_PermGroups.xml:116-136
gap> Q := AsQuasigroup( Group( (1,2,3,4,5,6 ) ) );;
gap> Size( RightMultiplicationGroup( Q ) );
6
gap> S := Subquasigroup( Q, [ Q[(1,3,5)(2,4,6)] ] );
<associative quasigroup of size 3>
gap> RightTranslation( S, (1,3,5)(2,4,6) );
(1,3,5)
gap> LeftTranslation( S, (1,3,5)(2,4,6) );
(1,3,5)
gap> Size( LeftMultiplicationGroup( S ) );
3
gap> RelativeLeftMultiplicationGroup( Q, S );
Group([ (1,3,5)(2,4,6) ])
gap> LeftMultiplicationGroup( S );
Group([ (1,3,5) ])
gap> RelativeRightMultiplicationGroup( Q, S );
Group([ (1,3,5)(2,4,6) ])
gap> Size( MultiplicationGroup( Q ) );
6

# doc/_Chapter_PermGroups.xml:196-218
gap> ct := [[1,2,3,4,5],[2,1,4,5,3],[3,5,1,2,4],[4,3,5,1,2],[5,4,2,3,1]];;
gap> Q := LoopByCayleyTable( ct );; # not commutative
gap> LeftInnerMapping( Q, Q.3, Q.3 );
(2,4,5)
gap> L := LeftMultiplicationGroup( Q );;
gap> LInn := LeftInnerMappingGroup( Q );;
gap> [ Size( L ), Size( LInn ) ];
[ 120, 24 ]
gap> (2,4,5) in L;
true
gap> LInn = Stabilizer( L, 1 );
true
gap> RInn := RightInnerMappingGroup( Q );;
gap> Size( RInn );
24
gap> ForAll( Q, x -> ForAll( Q, y -> RightInnerMapping( Q, x, y ) in RInn ) );
true
gap> LInn = RInn;
true
gap> Size( MiddleInnerMappingGroup( Q ) );
12

# doc/_Chapter_PermGroups.xml:285-296
gap> Q := QuasigroupByFunction( GF(5), \- );
<quasigroup of size 5>
gap> RightPosDisplacementGroup( Q );
Group([ (1,2,3,5,4) ])
gap> LeftDisplacementGroup( Q );
Group([ (1,2,3,5,4) ])
gap> IsIsotopicToGroup( Q );
true
gap> IsIsotopicToAbelianGroup( Q );
true

# doc/_Chapter_PermGroups.xml:387-404
gap> G := AlternatingGroup( 6 );;
gap> OneLoopWithMltGroup( G, 2, 0 );
[ [ (), (1,2)(3,4,5,6), (1,3)(2,4,6,5), (1,4)(2,5,3,6), 
      (1,5)(2,6,4,3), (1,6)(2,3,5,4) ] ]
gap> Q := LoopByRightSection( [1..6], last[ 1 ] );
<loop of size 6>
gap> MultiplicationGroup( Q ) = G;
true
gap> AllLoopsWithMltInGroup( SymmetricGroup( 4 ), 2, 0);
[ [ (), (1,2)(3,4), (1,3)(2,4), (1,4)(2,3) ], 
  [ (), (1,2)(3,4), (1,3,2,4), (1,4,2,3) ], 
  [ (), (1,2,3,4), (1,3)(2,4), (1,4,3,2) ] ]
gap> a := AllLoopsWithMltInGroup( PGL(3,3), 3, 0 );; Size(a);
56
gap> a := AllLoopsWithMltGroup(PGL(3,3), 3, 0);; Size(a);
52

#
gap> STOP_TEST("rightquasigroups05.tst", 1);