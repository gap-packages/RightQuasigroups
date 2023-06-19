# RightQuasigroups, chapter 11
#
# DO NOT EDIT THIS FILE - EDIT EXAMPLES IN THE SOURCE INSTEAD!
#
# This file has been generated by AutoDoc. It contains examples extracted from
# the package documentation. Each example is preceded by a comment which gives
# the name of a GAPDoc XML file and a line range from which the example were
# taken. Note that the XML file in turn may have been generated by AutoDoc
# from some other input.
#
gap> START_TEST("rightquasigroups11.tst");

# doc/_Chapter_Racks_and_quandles.xml:125-132
gap> PermutationalRack( 10, (3,4,5) );
<rack of size 10>
gap> Q := PermutationalRack( 100000, (1,100000), ConstructorStyle( false, true ) );
<rack of size 100000>
gap> Q[1]*Q[2];
r100000

# doc/_Chapter_Racks_and_quandles.xml:175-190
gap> # affine rack on [0..11]
gap> [ IsAffineRackArithmeticForm( 12, 5, 2, 6 ), AffineRack( 12, 5, 2, 6 ) ];
[ true, <rack of size 12> ]
gap> # affine rack on GF(9)
gap> F := GF(9);; f := 2*Z(9);; g := Z(9)+One(F);; c := Zero(F);;
gap> [ IsAffineRackArithmeticForm( F, f, g, c ), AffineRack( F, f, g, c ) ]; # latin racks are quandles
[ true, <latin quandle of size 9> ]
gap> # affine rack on cyclic group of order 4
gap> x := (1,2,3,4);; G := Group( x );;
gap> f := GroupHomomorphismByImages( G, G, [x], [x^-1] );;
gap> g := GroupHomomorphismByImages( G, G, [x], [x^2] );;
gap> c := x^2;;
gap> [ IsAffineRackArithmeticForm( G, f, g, c ), AffineRack( G, f, g, c ) ];
[ true, <rack of size 4> ]

# doc/_Chapter_Racks_and_quandles.xml:226-238
gap> # affine quandle on [0..9]
gap> [ IsAffineQuandleArithmeticForm( 10, 3 ), AffineQuandle( 10, 3 ) ];
[ true, <quandle of size 10> ]
gap> # affine quandle on GF(9)
gap> [ IsAffineQuandleArithmeticForm( GF(9), 2*Z(9) ), AffineQuandle( GF(9), 2*Z(9) ) ];
[ true, <latin quandle of size 9> ]
gap> # affine quandle on cyclic group of order 5
gap> G := CyclicGroup(5);; f := Elements( AutomorphismGroup( G ) )[2];
[ f1 ] -> [ f1^2 ]
gap> [ IsAffineQuandleArithmeticForm( G, f ), AffineQuandle( G, f ) ];
[ true, <latin quandle of size 5> ]

# doc/_Chapter_Racks_and_quandles.xml:273-281
gap> G := AlternatingGroup( 5 );;
gap> Q := CoreOfGroup( G );
<quandle of size 60>
gap> Q[(1,2,3)]*Q[(1,2,3,4,5)] = Q[(1,2,3)*(1,2,3,4,5)^(-1)*(1,2,3)];
true
gap> CoreOfRightBolLoop( RightBolLoop(8,1) );
<quandle of size 8>

# doc/_Chapter_Racks_and_quandles.xml:299-305
gap> G := SymmetricGroup( 4 );; H := Subgroup( G, [(1,2)] );;
gap> f := Filtered( AutomorphismGroup( G ), g -> (1,2)^g = (1,2) )[3];
^(3,4)
gap> Q := GalkinQuandle( G, H, f );
<quandle of size 12>

# doc/_Chapter_Racks_and_quandles.xml:320-328
gap> G := SymmetricGroup( 3 );; 
gap> ConjugationQuandle( G ); # y^-1*x*y on G
<quandle of size 6>
gap> ConjugationQuandle( ConjugacyClass( G, (1,2,3) ) ); # y^-1*x*y on [ (1,2,3), (1,3,2) ]
<quandle of size 2>
gap> ConjugationQuandle( G, 2 ); # y^-2*x*y^2 on G
<quandle of size 6>

# doc/_Chapter_Racks_and_quandles.xml:398-414
gap> Q := SmallQuandle( 10, 1000 );
SmallQuandle( 10, 1000 )
gap> env := QuandleEnvelope( Q );
[ Group([ (1,3,2)(7,8) ]), [ 1, 4, 5, 6, 7, 9, 10 ], 
  [ (), (), (1,2,3), (1,3,2)(7,8), (1,2,3), (1,3,2)(7,8), 
      (1,3,2)(7,8) ] ]
gap> Q2 := QuandleByQuandleEnvelope( env );
<quandle of size 10>
gap> IsomorphismQuandles( Q, Q2 );
MappingByFunction( SmallQuandle( 10, 1000 ), <quandle of size 10>, fun\
ction( x ) ... end )
gap> AsParentTransformation( last );
IdentityTransformation
gap> QuandleByQuandleEnvelope( env[1], env[2], env[3] ); # separate arguments also supported for envelopes
<quandle of size 10>

# doc/_Chapter_Racks_and_quandles.xml:450-456
gap> Q := AffineRack( 12, 5, 2, 6 );;
gap> S := Subrack( Q, [ Q[2] ] );
<rack of size 2>
gap> IsSubrack( Q, S );
true

# doc/_Chapter_Racks_and_quandles.xml:471-475
gap> Q := ConjugationQuandle( SymmetricGroup( 3 ) );;
gap> List( AllSubquandles( Q ), Size );
[ 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 3, 5, 2, 4, 6, 3 ]

# doc/_Chapter_Racks_and_quandles.xml:493-499
gap> Q := ConjugationQuandle( SymmetricGroup( 3 ) );;
gap> S := QuandleWithGenerators( [ Q[(1,2)], Q[(1,2,3)] ] );
<quandle of size 5>
gap> GeneratorsOfMagma( S );
[ r(1,2), r(1,2,3) ]

# doc/_Chapter_Racks_and_quandles.xml:551-560
gap> g := AdjointGroup( DihedralQuandle( 3 ) );
<fp group on the generators [ q0, q1 ]>
gap> GeneratorsOfGroup( g );
[ q0, q1 ]
gap> RelatorsOfFpGroup( g );
[ q0^-1*q1^2*q0^-1, (q1^-1*q0)^3 ]
gap> Size( g );
infinity

#
gap> STOP_TEST("rightquasigroups11.tst", 1);