LogTo("/tmp/temp_CoreMethods.tst");
START_TEST("RightQuasigroups package: CoreMethods.tst");
SizeScreen([72,23]);

LoadPackage("RightQuasigroups", false);

# multiplication groups
Q := AsQuasigroup( Group( (1,2,3,4,5,6 ) ) );;
Size( RightMultiplicationGroup( Q ) );
S := Subquasigroup( Q, [ Q[(1,3,5)(2,4,6)] ] );
RightTranslation( S, (1,3,5)(2,4,6) );
LeftTranslation( S, (1,3,5)(2,4,6) );
Size( LeftMultiplicationGroup( S ) );
RelativeLeftMultiplicationGroup( Q, S );
LeftMultiplicationGroup( S );
RelativeRightMultiplicationGroup( Q, S );
Size( MultiplicationGroup( Q ) );
# inner mappings and inner mapping groups
LeftInnerMapping( Q, Q.1, Q.2 );
RightInnerMapping( Q, Q.1, Q.2 );
MiddleInnerMapping( Q, Q.1 );
ct := [[1,2,3,4,5],[2,1,4,5,3],[3,5,1,2,4],[4,3,5,1,2],[5,4,2,3,1]];;
Q := LoopByCayleyTable( ct );;
LeftInnerMapping( Q, Q.3, Q.3 );
L := LeftMultiplicationGroup( Q );;
LInn := LeftInnerMappingGroup( Q );;
Size( L );
(2,4,5) in L;
Size( LInn );
LInn = Stabilizer( L, 1 );
RInn := RightInnerMappingGroup( Q );;
Size( RInn );
ForAll( Q, x -> ForAll( Q, y -> RightInnerMapping( Q, x, y ) in RInn ) );
Size( RightMultiplicationGroup( Q ) ) = Size( Q )*Size( RInn );
LInn = RInn;
Size(MiddleInnerMappingGroup( Q ));
IsSubgroup( LeftInnerMappingGroup(Q), MiddleInnerMappingGroup(Q));
# nuclei
LeftNucleus(Q);
Q := AsLoop( CyclicGroup( 100 ) );;
RightNucleus(Q);
Nuc(Q);
Size(Commutant( Q ));
Center(Q);
# exponent
Exponent( Q );


STOP_TEST( "CoreMethods.tst", 10000 );
LogTo();