gap> START_TEST("RightQuasigroups package: Elements.tst");
gap> SizeScreen([80,23]);
[ 80, 23 ]
gap> 
gap> LoadPackage("RightQuasigroups", false);
true
gap> 
gap> # naming of elements
gap> Q := AsLoop( GF(8) );;
gap> SetLoopElementsName( Q, "L");; Q.1;
L0*Z(2)
gap> SetQuasigroupElementsName( Q, "Q");; Q.1;
Q0*Z(2)
gap> SetRightQuasigroupElementsName( Q, "R");; Q.1;
R0*Z(2)
gap> # comparison of elements
gap> Q := RightQuasigroupByFunction( [0..9], function(x,y) return (x+y) mod 10; end );;
gap> Q[0]<Q[1];
true
gap> Q[1]<Q[0];
false
gap> Q[0]=Q[0];
true
gap> # elements, underlying set elements, indices
gap> S := Subrightquasigroup( Q, [5] );;
gap> S[5];
r5
gap> S.2;
r1
gap> S[8]; # works because it makes sense in the parent
r8
gap> AsParentInd( S[5] );
Error, Variable: 'AsParentInd' must have a value
not in any function at *stdin*:22
gap> AsUnderlyingSetElm( Q.6 );
Error, Variable: 'AsUnderlyingSetElm' must have a value
not in any function at *stdin*:23
gap> # One, LeftInverse, RightInverse
gap> Q := AsLoop( Q );;
gap> One( Q );
l0
gap> RightInverse( Q[3] );
l7
gap> LeftInverse( Q[3] );
l7
gap> Inverse( Q[3] );
l7
gap> Q[3]^-1;
l7
gap> # commutator, associator
gap> Commutator( Q[1], Q[2] );
l0
gap> Associator( Q[1], Q[2], Q[3] );
l0
gap> Q := ProjectionRightQuasigroup( [0..9] );;
gap> Commutator( Q[1], Q[2] ); # x*y=x, so (1*2)/(2*1)=1
r1
gap> Associator( Q[1], Q[2], Q[3] );
r1
gap> 
gap> 
gap> STOP_TEST( "Elements.tst", 10000 );