# naming of elements
gap> Q := AsLoop( GF(8) );;
gap> SetLoopElementsName( Q, "L");; Q.1;
L:0*Z(2)
gap> SetQuasigroupElementsName( Q, "Q");; Q.1;
Q:0*Z(2)
gap> SetRightQuasigroupElementsName( Q, "R");; Q.1;
R:0*Z(2)

# comparison of elements
gap> Q := RightQuasigroupByFunction( [0..9], function(x,y) return (x+y) mod 10; end );;
gap> Q[0]<Q[1];
true
gap> Q[1]<Q[0];
false
gap> Q[0]=Q[0];
true

# elements, underlying set elements, indices
gap> S := Subrightquasigroup( Q, [5] );;
gap> S[5];
r:5
gap> S.2;
r:5
gap> S[8]; # works because it makes sense in the parent
r:8
gap> AsParentInd( S[5] );
6
gap> AsUnderlyingSetElm( Q.6 );
5

# One, LeftInverse, RightInverse
gap> Q := AsLoop( Q );;
gap> One( Q );
l:0
gap> RightInverse( Q[3] );
l:7
gap> LeftInverse( Q[3] );
l:7
gap> Inverse( Q[3] );
l:7
gap> Q[3]^-1;
l:7

# commutator, associator
gap> Commutator( Q[1], Q[2] );
l:0
gap> Associator( Q[1], Q[2], Q[3] );
l:0
gap> Q := ProjectionRightQuasigroup( [0..9] );;
gap> Commutator( Q[1], Q[2] ); # x*y=x, so (1*2)/(2*1)=1
r:1
gap> Associator( Q[1], Q[2], Q[3] );
r:1