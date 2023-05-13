# RightQuasigroups, chapter 9
#
# DO NOT EDIT THIS FILE - EDIT EXAMPLES IN THE SOURCE INSTEAD!
#
# This file has been generated by AutoDoc. It contains examples extracted from
# the package documentation. Each example is preceded by a comment which gives
# the name of a GAPDoc XML file and a line range from which the example were
# taken. Note that the XML file in turn may have been generated by AutoDoc
# from some other input.
#
gap> START_TEST( "rightquasigroups09.tst");

# doc/_Chapter_Topisms.xml:141-151
gap> Q := MoufangLoop( 12, 1 );;
gap> f := (1,2,3);; g := Transformation( [3,3,3] );; h := MappingByFunction( Q, Q, x->x^-1 );; # note various formats
gap> T := RightQuasigroupTwist( Q, f, g, h ); # f and h must be bijective 
<right quasigroup of size 12>
gap> QuasigroupTwist( Q, [ f, (1,12), h ] ); # g must be bijective for quasigroups
<quasigroup of size 12>
gap> f := RightTranslation( Q, Q.2 )^-1;; g := LeftTranslation( Q, Q.3 )^-1;;
gap> LoopTwist( Q, f, g, (), ConstructorStyle( true, true ) ); # principal loop isotope
<loop of size 12>

# doc/_Chapter_Topisms.xml:202-211
gap> style := ConstructorStyle( false, false );;
gap> P := QuasigroupByFunction( [0..99999], function(x,y) return (x-y) mod 10^5; end, style );
<quasigroup of size 100000>
gap> f := (1,10,100,1000,10000,100000);; g := (3,4);; h := ();;
gap> R := QuasigroupIsotope( P, f, g, h, style );
<quasigroup of size 100000>
gap> R.1000*R.4;
q97

# doc/_Chapter_Topisms.xml:225-231
gap> Q := QuasigroupByFunction( GF(11), \- );;
gap> P := PrincipalLoopIsotope( Q, Q.3, Q.4 );
<loop of size 11>
gap> UnderlyingSetElm( Q.4*Q.3 ) = UnderlyingSetElm( One( P ) );
true

# doc/_Chapter_Topisms.xml:316-325
gap> IsAffineRightQuasigroupArithmeticForm( 10, 3, 5, 1 ); # suitable for (3*x+5*y+1) mod 10
true
gap> IsAffineQuasigroupArithmeticForm( 10, 3, 5, 1 ); # gcd(10,5) <> 1
false
gap> Q := AffineRightQuasigroup( 10, 3, 5, 1 );
<right quasigroup of size 10>
gap> Q := AffineRightQuasigroup( 100000, 333, 777, 5, ConstructorStyle(false,false) ); # non-index based example
<right quasigroup of size 100000>

# doc/_Chapter_Topisms.xml:329-335
gap> F := GF(9);; f := Z(9);; g := Z(9)^3;; c := Z(9)^5;;
gap> IsAffineQuasigroupArithmeticForm( F, f, g, c ); # suitable for f*x+g*y+c
true
gap> AffineQuasigroup( F, f, g, c );
<quasigroup of size 9>

# doc/_Chapter_Topisms.xml:339-345
gap> G := CyclicGroup(10);; A := AutomorphismGroup( G );; f := A.1;; g := A.2;; c := G.1;;
gap> IsAffineQuasigroupArithmeticForm( G, f, g, c ); # suitable for x^f*y^g*c
true
gap> AffineQuasigroup( G, f, g, c );
<quasigroup of size 10>

# doc/_Chapter_Topisms.xml:349-356
gap> G := DihedralGroup(12);;
gap> f := MappingByFunction( G, G, x->x^(G.1) );; g := MappingByFunction( G, G, x->x^(G.2) );; u := G.1;; v := G.2;;
gap> IsAffineQuasigroupArithmeticForm( G, f, u, g, v ); # suitable for (x^f*u)*(y^g*v)
true
gap> AffineQuasigroup( G, f, u, g, v ); 
<quasigroup of size 12>

# doc/_Chapter_Topisms.xml:361-369
gap> G := AutomorphicLoop( 10, 1 );; # inner mappings are automorphisms here
gap> f := AsRightQuasigroupMapping( G, LeftInnerMapping( G, G.2, G.3 ) );;
gap> g := f*f;; u := G.1;; v := G.2;;
gap> IsAffineQuasigroupArithmeticForm( G, u, f, v, g ); # suitable for (u*x^f)*(v*y^g)
true 
gap> Q := AffineQuasigroup( G, u, f, v, g ); 
<quasigroup of size 10>

# doc/_Chapter_Topisms.xml:398-408
gap> Q1 := RandomQuasigroup( 32 );;
gap> G := SymmetricGroup( 32 );;
gap> Q2 := QuasigroupIsotope( Q1, Random( G ), Random( G ), Random( G ) );;
gap> IsotopismQuasigroups( Q1, Q2 );
[ MappingByFunction( <quasigroup of size 32>, <quasigroup of size 32>, function( x ) ... end ),
  MappingByFunction( <quasigroup of size 32>, <quasigroup of size 32>, function( x ) ... end ),
  MappingByFunction( <quasigroup of size 32>, <quasigroup of size 32>, function( x ) ... end ) ]
gap> IsQuasigroupIsotopism( last );
true

# doc/_Chapter_Topisms.xml:423-432
gap> t := [ [1,2,3,4,5,6], [2,1,6,5,3,4], [3,4,5,2,6,1], [4,6,1,3,2,5], [5,3,4,6,1,2], [6,5,2,1,4,3] ];;
gap> Q1 := LoopByCayleyTable( t );;
gap> Q2 := LoopIsomorph( Q1, (3,4) );;
gap> Q3 := PrincipalLoopIsotope( Q2, Q2.5, Q2.6 );; # a loop isotopic to Q1
gap> IsotopismLoops( Q1, Q3 ); # add optional argument `true` to use a method via principal loop isotopes
[ MappingByFunction( <loop of size 6>, <loop of size 6>, function( x ) ... end ),
  MappingByFunction( <loop of size 6>, <loop of size 6>, function( x ) ... end ),
  MappingByFunction( <loop of size 6>, <loop of size 6>, function( x ) ... end ) ]

# doc/_Chapter_Topisms.xml:494-507
gap> q:=LoopByCayleyTable([[1,2,3,4,5 ],[2,1,4,5,3],[3,4,5,1,2],[4,5,2,3,1],[5,3,1,2,4]]);
<loop of size 5>
gap> f:=[(1,5,4), (2,4,3), (1,5,4)];
[ (1,5,4), (2,4,3), (1,5,4) ]
gap> atp:=AutotopismObject@RightQuasigroups(q,f[1],f[2],f[3]);
IsRightQuasigroupAutotopismObject((1,5,4), (2,4,3), (1,5,4))
gap> AmbientRightQuasigroup(atp);
<loop of size 5>
gap> One(atp);
IsRightQuasigroupAutotopismObject((), (), ())
gap> atp^-4;
IsRightQuasigroupAutotopismObject((1,4,5), (2,3,4), (1,4,5))

# doc/_Chapter_Topisms.xml:558-567
gap> Q := RightBolLoop(8,1);
<right Bol loop 8/1>
gap> AutotopismFromPrincipalLoopIsotope( Q, Q.4, Q.3 );
IsRightQuasigroupAutotopismObject((1,3)(2,4)(5,7)(6,8), (1,4)(2,3)(5,8), (1,2)(3,4)(5,6)(7,8))
gap> AutotopismGroup( Q );
<autotopism group with 5 generators>
gap> Size( last );
128

#
gap> STOP_TEST("rightquasigroups09.tst", 1 );
