# multiplication groups
gap> Q := AsQuasigroup( Group( (1,2,3,4,5,6 ) ) );;
gap> Size( RightMultiplicationGroup( Q ) );
6
gap> S := Subquasigroup( Q, [ Q[(1,3,5)(2,4,6)] ] );
<quasigroup of size 3>
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

# inner mappings and inner mapping groups
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

# nuclei
gap> LeftNucleus(Q);
<loop of size 1>
gap> Q := AsLoop( CyclicGroup( 100 ) );;
gap> RightNucleus(Q);
<loop of size 100>
gap> Nuc(Q);
<loop of size 100>
gap> Size(Commutant( Q ));
100
gap> Center(Q);
<loop of size 100>

# exponent
gap> Exponent( Q );
100