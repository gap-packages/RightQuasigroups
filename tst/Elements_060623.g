LogTo("/tmp/temp_Elements.tst");
START_TEST("RightQuasigroups package: Elements.tst");
SizeScreen([72,23]);

LoadPackage("RightQuasigroups", false);

# naming of elements
Q := AsLoop( GF(8) );;
SetLoopElementsName( Q, "L");; Q.1;
SetQuasigroupElementsName( Q, "Q");; Q.1;
SetRightQuasigroupElementsName( Q, "R");; Q.1;
# comparison of elements
Q := RightQuasigroupByFunction( [0..9], function(x,y) return (x+y) mod 10; end );;
Q[0]<Q[1];
Q[1]<Q[0];
Q[0]=Q[0];
# elements, underlying set elements, indices
S := Subrightquasigroup( Q, [5] );;
S[5];
S.2;
S[8]; # works because it makes sense in the parent
ParentInd( S[5] );
UnderlyingSetElm( Q.6 );
# One, LeftInverse, RightInverse
Q := AsLoop( Q );;
One( Q );
RightInverse( Q[3] );
LeftInverse( Q[3] );
Inverse( Q[3] );
Q[3]^-1;
# commutator, associator
Commutator( Q[1], Q[2] );
Associator( Q[1], Q[2], Q[3] );
Q := ProjectionRightQuasigroup( [0..9] );;
Commutator( Q[1], Q[2] ); # x*y=x, so (1*2)/(2*1)=1
Associator( Q[1], Q[2], Q[3] );


STOP_TEST( "Elements.tst", 10000 );
LogTo();