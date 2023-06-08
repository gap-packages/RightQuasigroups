gap> START_TEST("RightQuasigroups package: CoreMethods.tst");
gap> SizeScreen([80,23]);
[ 80, 23 ]
gap> 
gap> LoadPackage("RightQuasigroups", false);
true
gap> 
gap> # multiplication groups
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
gap> # inner mappings and inner mapping groups
gap> LeftInnerMapping( Q, Q.1, Q.2 );
()
gap> RightInnerMapping( Q, Q.1, Q.2 );
()
gap> MiddleInnerMapping( Q, Q.1 );
()
gap> ct := [[1,2,3,4,5],[2,1,4,5,3],[3,5,1,2,4],[4,3,5,1,2],[5,4,2,3,1]];;
gap> Q := LoopByCayleyTable( ct );;
gap> LeftInnerMapping( Q, Q.3, Q.3 );
(2,4,5)
gap> L := LeftMultiplicationGroup( Q );;
gap> LInn := LeftInnerMappingGroup( Q );;
gap> Size( L );
120
gap> (2,4,5) in L;
true
gap> Size( LInn );
24
gap> LInn = Stabilizer( L, 1 );
true
gap> RInn := RightInnerMappingGroup( Q );;
gap> Size( RInn );
24
gap> ForAll( Q, x -> ForAll( Q, y -> RightInnerMapping( Q, x, y ) in RInn ) );
true
gap> Size( RightMultiplicationGroup( Q ) ) = Size( Q )*Size( RInn );
true
gap> LInn = RInn;
true
gap> Size(MiddleInnerMappingGroup( Q ));
12
gap> IsSubgroup( LeftInnerMappingGroup(Q), MiddleInnerMappingGroup(Q));
true
gap> # nuclei
gap> LeftNucleus(Q);
<trivial group with 1 generator>
gap> Q := AsLoop( CyclicGroup( 100 ) );;
gap> RightNucleus(Q);
<associative loop of size 100>
gap> Nuc(Q);
<associative loop of size 100>
gap> Size(Commutant( Q ));
100
gap> Center(Q);
<associative loop of size 100>
gap> # exponent
gap> Exponent( Q );
100
gap> 
gap> 
gap> STOP_TEST( "CoreMethods.tst", 10000 );
