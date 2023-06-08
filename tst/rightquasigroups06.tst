# RightQuasigroups, chapter 6
#
# DO NOT EDIT THIS FILE - EDIT EXAMPLES IN THE SOURCE INSTEAD!
#
# This file has been generated by AutoDoc. It contains examples extracted from
# the package documentation. Each example is preceded by a comment which gives
# the name of a GAPDoc XML file and a line range from which the example were
# taken. Note that the XML file in turn may have been generated by AutoDoc
# from some other input.
#
gap> START_TEST("rightquasigroups06.tst");

# doc/_Chapter_Nilpotency_and_solvability.xml:246-282
gap> uset := Union( List([0..3], i-> [[i,0],[i,1]] ) ); # the underlying set
[ [ 0, 0 ], [ 0, 1 ], [ 1, 0 ], [ 1, 1 ], [ 2, 0 ], [ 2, 1 ], 
  [ 3, 0 ], [ 3, 1 ] ]
gap> ct := [[0,1,2,3],[1,3,0,2],[2,0,3,1],[3,2,1,0]];;
gap> mult := function( x, y )
>         if x[2]=0 or y[2]=0 then
>                 return [ (x[1]+y[1]) mod 4, (x[2]+y[2]) mod 2 ];
>         else
>                 return [ ct[x[1]+1,y[1]+1], (x[2]+y[2]) mod 2 ];
>         fi;
>   end;
function( x, y ) ... end
gap> Q := LoopByFunction( uset, mult ); # Z_4 x Z_2 with one quandrant "replaced" with ct
<loop of size 8>
gap> DerivedSeries( Q );
[ <loop of size 8>, <loop of size 4>, 
  <trivial group with 1 generator> ]
gap> IsSolvable( Q );
true
gap> DerivedLength( Q );
2
gap> C := CommutatorOfNormalSubloops(Q,Q,Q); # congruence derived subloop = derived subloop
<loop of size 4>
gap> D := CommutatorOfNormalSubloops(Q,C,C); # 2nd congruence derived subloop differs from 2nd derived subloop
<loop of size 4>
gap> CongruenceDerivedSeriesOfLoop( Q );
[ <loop of size 8>, <loop of size 4> ]
gap> IsCongruenceSolvableLoop( Q );
false
gap> CongruenceDerivedLength( Q );
fail
gap> IsCommutative( C ) and IsAssociative( C ) and IsNormal( Q, C ); # commutative group, normal in Q
true
gap> IsAbelianNormalSubloop( Q, C ); # but not abelian in Q
false

# doc/_Chapter_Nilpotency_and_solvability.xml:349-381
gap> K := AsLoop( CyclicGroup( 4 ) );;
gap> F := LoopByCayleyTable( [ [ "a", "b" ], [ "b", "a" ] ] );;
gap> AutomorphismGroup( K );
Group([ (2,4) ])
gap> phi := [ (), (2,4) ];; # homomorphism from F to Aut( K )
gap> theta := [ [ 1, 1 ], [ 1, 4 ] ];; # loop cocycle from FxF to K
gap> IsLoopCocycle( K, F, theta );
true
gap> Q := LoopByNuclearExtension( K, F, phi, theta );
<loop of size 8>
gap> S := NucleusOfLoop( Q );
<associative loop of size 4>
gap> IsNormal( Q, S );
true
gap> AsCanonicalPerm( IsomorphismLoops( F, Q/S ) );
()
gap> Display( MultiplicationTable( Q ) );
[ [  1,  2,  3,  4,  5,  6,  7,  8 ],
  [  2,  7,  8,  5,  6,  3,  4,  1 ],
  [  3,  4,  5,  6,  7,  8,  1,  2 ],
  [  4,  1,  2,  7,  8,  5,  6,  3 ],
  [  5,  6,  7,  8,  1,  2,  3,  4 ],
  [  6,  3,  4,  1,  2,  7,  8,  5 ],
  [  7,  8,  1,  2,  3,  4,  5,  6 ],
  [  8,  5,  6,  3,  4,  1,  2,  7 ] ]
gap> Q.1; # the underlying set of Q is the carthesian product of underlying sets K x F
l[ <identity> of ..., "a" ]
gap> LoopByCentralExtension( K, F, theta );
<loop of size 8>
gap> Center( last );
<associative loop of size 8>

# doc/_Chapter_Nilpotency_and_solvability.xml:420-440
gap> Q := MoufangLoop(32,3);;
gap> Nuc( Q ); # here, the nucleus is commutative and properly contains the center
<associative loop of size 4>
gap> Center( Q );
<associative loop of size 2>
gap> ext := NuclearExtensionByNormalSubloop( Q, Nuc( Q ) );
[ <associative loop of size 4>, <Moufang loop of size 8>, 
  [ (), (), (), (2,4), (), (2,4), (2,4), (2,4) ], 
  [ [ 1, 1, 1, 1, 1, 1, 1, 1 ], [ 1, 1, 1, 1, 1, 1, 3, 3 ], 
      [ 1, 3, 1, 1, 3, 1, 1, 1 ], [ 1, 1, 1, 1, 3, 1, 1, 3 ], 
      [ 1, 3, 1, 1, 3, 1, 3, 3 ], [ 1, 1, 1, 1, 3, 1, 3, 1 ], 
      [ 1, 3, 1, 1, 1, 1, 1, 3 ], [ 1, 3, 1, 1, 1, 1, 3, 1 ] ] ]
gap> copyQ := LoopByNuclearExtension( ext[1],ext[2],ext[3],ext[4] );;
gap> AsCanonicalPerm( IsomorphismLoops( Q, copyQ ) );
(4,9,11,28,29,24,22,21,20,25,27,12,13,8,6,5)(7,10)(14,30)(15,31)(16,32)(23,26)
gap> ext := CentralExtensionByNormalSubloop( Q, Center( Q ) );;
gap> copyQ := LoopByCentralExtension( ext );; # extension can also be given as a list
gap> AsCanonicalPerm( IsomorphismLoops( Q, copyQ ) );
()

# doc/_Chapter_Nilpotency_and_solvability.xml:485-504
gap> F := AsLoop( Group( (1,2), (3,4) ) );; # the Klein group
gap> coc := LoopCocyclesInVariety( F, 2, [ "x*(y*(x*z)) = (x*(y*x))*z" ] );; # basis of left Bol cocycles
gap> Length( coc ); # dimension of the vector space of cocycles
6
gap> theta := coc[3];; Display( theta ); # one cocycle
[ 0*Z(2), 0*Z(2), 0*Z(2), 0*Z(2), 0*Z(2), 0*Z(2), 0*Z(2), 0*Z(2), 0*Z(2), 
Z(2)^0, Z(2)^0, 0*Z(2), 0*Z(2), 0*Z(2), 0*Z(2), 0*Z(2) ]
gap> theta := List( theta, x -> IntFFE( x ) + 1 );; Display( theta ); # converting cocycle entries to [1..Size(K)]
[ 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 1, 1, 1, 1, 1 ]
gap> theta := AsSquareTable( theta );; Display( theta ); # converting cocycle to square table
[ [  1,  1,  1,  1 ],
  [  1,  1,  1,  1 ],
  [  1,  2,  2,  1 ],
  [  1,  1,  1,  1 ] ]
gap> Q := LoopByCentralExtension( AsLoop( CyclicGroup(2) ), F, theta );
<loop of size 8>
gap> IsLeftBolLoop( Q );
true

# doc/_Chapter_Nilpotency_and_solvability.xml:557-581
gap> F := AsLoop( Group( (1,2), (3,4 ) ) );; # Klein group
gap> coc := LoopCocyclesInVariety( F, 2, [ "x*(y*(x*z)) = (x*(y*x))*z" ] );; Length( coc ); # basis of left Bol cocycles
6
gap> cob := LoopCoboundaries( F, 2 );; Length( cob ); # basis of coboundaries
1
gap> Length( LoopCocyclesModAction( F, 2, coc, cob ) ); # cocycles modulo coboundaries and action of Aut(K)x Aut(F)
10
gap> Length( AllLoopCocyclesInVariety( F, 2, [ "x*(y*(x*z)) = (x*(y*x))*z" ] ) ); # the same in one step
#I  RQ: Calculating coboundaries
#I  RQ: Coboundaries have dimension 1
#I  RQ: Calculating cocycles
#I  RQ: Cocycles have dimension 6
10
gap> lps := AllLoopCentralExtensionsInVariety( F, 2, [ "x*(y*(x*z)) = (x*(y*x))*z" ] );; Length( lps ); # all central extensions in one step
#I  RQ: Calculating coboundaries
#I  RQ: Coboundaries have dimension 1
#I  RQ: Calculating cocycles
#I  RQ: Cocycles have dimension 6
10
gap> ForAll( lps, IsLeftBolLoop );
true
gap> Length( LoopsUpToIsomorphism( lps ) ); # filtering up to isomorphism (no isomorphisms in this example)
10

# doc/_Chapter_Nilpotency_and_solvability.xml:631-640
gap> Q := MoufangLoop(64,5);
MoufangLoop( 64, 5 )
gap> F := FrattiniSubloop(Q);
<Moufang loop of size 8>
gap> Exponent(Q/F);
2
gap> FrattiniSubrightquasigroup( ProjectionRightQuasigroup([1,2]) );
[  ]

#
gap> STOP_TEST("rightquasigroups06.tst", 1);
