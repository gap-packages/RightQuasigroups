# By default, all constructors produce index based right quasigroups and their arguments are not checked.

# Adding optional boolean arguments indexBased, checkArgs changes the default behavior.
# It is possible to list no optional arguments, only the optional argument indexBased, or both optional arguments.

# TESTING ALGEBRA SHELL (not for casual users)

# right quasigroup shell
gap> Q := RQ_AlgebraShell( IsRightQuasigroup, ["a","b","c"] );
<right quasigroup of size 3>
gap> Q.1; # Note: Q.1*Q.1 will fail since no operations are installed in the algebra shell
r:a
gap> Q["b"]; 
r:b
gap> IsIndexBased( Q );
true
gap> Q.1![1];
1
gap> AsUnderlyingSetElm( Q.1 );
"a"
gap> SetRightQuasigroupElementsName(Q,"my");; # Note: Variations exist for quasigroups and loops
gap> Q.1;
my:a

# quasigroup shell
gap> Q := RQ_AlgebraShell( IsQuasigroup, ["a","b","c"], false );
<quasigroup of size 3>
gap> Q.1;
q:a
gap> IsIndexBased( Q );
false
gap> Q.1![1];
"a"

# loop shell
gap> Q := RQ_AlgebraShell( IsLoop, ["a","b","c"], false );
<loop of size 3>
gap> Elements(Q);
[ l:a, l:b, l:c ]

# Note: One(Q) will fail because no identity element has been installed in the algebra shell
#
#
# adding operations to a non-index based shell manually
gap> Q := RQ_AlgebraShell( IsQuasigroup, [0..6], false );
<quasigroup of size 7>
gap> F := FamilyObj( Q.1 );;
gap> F!.mult := function(x,y) return (x+y) mod 7; end;;
gap> Q[5]*Q[4];
q:2
gap> RQ_AddDefaultDivisionsAndOne( IsQuasigroup, Q );;
gap> Q[5]/Q[4];
q:1
gap> LeftDivision(Q[5],Q[4]); 
q:6

# Note: One(Q) will fail, identity element (which mathematically exists) has not been added since IsQuasigroup should not have one
#
#
# CONSTRUCTORS BY CAYLEY TABLE
#
gap> ct := [["a","a"],["b","b"]];;
gap> IsRightQuasigroupCayleyTable(ct);
true
gap> IsQuasigroupCayleyTable(ct);
false
gap> Q := RightQuasigroupByCayleyTable( ct );
<right quasigroup of size 2>
gap> HasMultiplicationTable( Q );
true
gap> Q := RightQuasigroupByCayleyTable( ct, false );;
gap> HasMultiplicationTable( Q );
false
gap> Q["a"]*Q["b"];
r:a
gap> Q["a"]/Q["b"];
r:a
gap> Q := LoopByCayleyTable( ct ); # Note: Works because by default checking does not happen
<loop of size 2>

# Note: Q := LoopByCayleyTable( ct, false, true ); will fail since arguments are checked
#
gap> Q := LoopByCayleyTable([["c","a","b"],["a","b","c"],["b","c","a"]],true,true); 
<loop of size 3>
gap> One(Q); # Note: The neutral element need not be the first element of the underlying set
l:b
gap> HasLeftDivisionTable( Q );
true
gap> HasLeftDivisionCayleyTable( Q );
false
gap> LeftDivisionCayleyTable( Q );
[ [ "b", "c", "a" ], [ "a", "b", "c" ], [ "c", "a", "b" ] ]

# CONSTRUCTORS BY FUNCTIONS
#
# testing parameters
gap> IsRightQuasigroupFunction([1..10],function(x,y) return x; end);
true
gap> IsQuasigroupFunction([1..10],function(x,y) return x; end);
false
gap> IsQuasigroupFunction(GF(8),\+); # Note: we accept GF(8) or anything that can be made into a set
true

# IsQuasigroupFunction(GF(8)),\+,\-); fails since we need 1 or 3 functions
#
gap> IsQuasigroupFunction(GF(8),\+,\-,function(x,y) return -x+y; end);
true
gap> IsLoopFunction(GF(8),\+,\-,function(x,y) return -x+y; end); # neutral element is optional
true
gap> IsLoopFunction(GF(8),\+,\-,function(x,y) return -x+y; end, One(GF(8))); # false because One(GF(8)) is not the neutral element with respect to addition
false
gap> IsLoopFunction(GF(8),\+,\-,function(x,y) return -x+y; end, Zero(GF(8)));
true

# constructors
gap> mult := function(x,y) return (x+y) mod 1000; end;;
gap> rdiv := function(x,y) return (x-y) mod 1000; end;;
gap> Q := RightQuasigroupByFunctions([0..999],mult,rdiv,false,false); # instantaneous
<right quasigroup of size 1000>
gap> Q[500]*Q[501];
r:1
gap> Q := RightQuasigroupByFunctions([0..999],mult,rdiv,false,true); # 500ms, verification takes time
<right quasigroup of size 1000>
gap> Q := RightQuasigroupByFunctions([0..999],mult,rdiv,true,true); # 1000ms, building operation tables takes more time
<right quasigroup of size 1000>
gap> ldiv := function(x,y) return (-x+y) mod 1000; end;;
gap> Q := QuasigroupByFunctions([0..999],mult,rdiv,ldiv,false,false); 
<quasigroup of size 1000>
gap> Q := LoopByFunctions([0..999],mult,rdiv,ldiv,0,false,false); 
<loop of size 1000>

# CONSTRUCTORS BY FUNCTION
#
gap> Q := RightQuasigroupByFunction(GF(8),\+);
<right quasigroup of size 8>
gap> Q := QuasigroupByFunction(GF(8),\+);
<quasigroup of size 8>
gap> Q := LoopByFunction(GF(8),\+);
<loop of size 8>

# the multiplicative group of GF(8)
gap> Q := RightQuasigroupByFunction( List([1..7],i->Z(8)^i), \* );
<right quasigroup of size 7>
gap> Elements(Q){[1,2,3]};
[ r:Z(2)^0, r:Z(2^3), r:Z(2^3)^2 ]

# SPECIAL RIGHT QUASIGROUPS
#
# projection right quasigroup: multiplication x*y = x
gap> Q := ProjectionRightQuasigroup( ["a","b","c"] );
<right quasigroup of size 3>
gap> CayleyTable( Q );
[ [ "a", "a", "a" ], [ "b", "b", "b" ], [ "c", "c", "c" ] ]

# right core of group: multiplication given by y*x^(-1)*y
gap> Q := RightCoreOfGroup(SymmetricGroup(5));
<right quasigroup of size 120>

# TRANSLATIONS, SECTIONS, MULTIPLICATION GROUPS
#
gap> Q := QuasigroupByFunction(GF(8),\+);;
gap> RightTranslation( Q, Zero(GF(8)) );
()
gap> RightTranslation( Q, Q.1 );
()
gap> RightTranslation( Q, Q[Zero(GF(8))] );
()
gap> LeftTranslation( Q, One(GF(8)) );
(1,2)(3,5)(4,8)(6,7)
gap> RightSection( Q ){[1,2,3]};
[ (), (1,2)(3,5)(4,8)(6,7), (1,3)(2,5)(4,6)(7,8) ]
gap> LeftSection( Q ) = RightSection( Q );
true

# affine quasigroup
gap> Q := QuasigroupByFunction([0..6], function(x,y) return (x-2*y) mod 7; end, true, true );
<quasigroup of size 7>
gap> RightTranslation(Q,0);
()
gap> LeftTranslation(Q,0);
(2,6,5,7,3,4)
gap> R := RightMultiplicationGroup( Q ); # note the small generating set not consisting of all right translations
Group([ (1,6,4,2,7,5,3) ])
gap> Size(R);
7
gap> L := LeftMultiplicationGroup( Q );
Group([ (2,4,3,7,5,6), (1,2,3,4,5,6,7) ])
gap> Size( L );
42
gap> M := MultiplicationGroup( Q );;
gap> Size(M);
42

# CONSTRUCTORS BY RIGHT SECTION
#
# testing right sections
gap> section := [(),(1,2,3,4),(),(1,3)(2,4)];;
gap> IsRightQuasigroupRightSection([1..4],section);
true
gap> IsRightQuasigroupRightSection([1..3],section);
false
gap> IsRightQuasigroupRightSection("abcd",section); # implicit permutation action on the set ['a','b','c','d']
true
gap> IsQuasigroupRightSection([1..4],section);
false
gap> id := Z(2)*[[1,0],[0,1]];;
gap> ma := Z(2)*[[1,1],[0,1]];;
gap> section := [ma,ma,id,id];;
gap> IsRightQuasigroupRightSection( GF(2)^2, section );
true
gap> IsQuasigroupRightSection( GF(2)^2, section );
false

# constructors
gap> Q := RightQuasigroupByRightSection([1..4],[(),(1,2,3,4),(),(1,3)(2,4)]);
<right quasigroup of size 4>
gap> Q[3]*Q[2];
r:4
gap> Q := RightQuasigroupByRightSection("abcd",[(),(1,2,3,4),(),(1,3)(2,4)] );
<right quasigroup of size 4>
gap> id := Z(2)*[[1,0],[0,1]];;
gap> ma := Z(2)*[[1,1],[0,1]];;
gap> Q := RightQuasigroupByRightSection( GF(2)^2, [ma,ma,id,id] );
<right quasigroup of size 4>
gap> Q.1;
r:[ 0*Z(2), 0*Z(2) ]

# larger example 
gap> S := GF(2)^10;;
gap> sec := List(S,x->Random(GL(10,2)));;
gap> uu0 := RightQuasigroupByRightSection(S,sec); # 1200ms
<right quasigroup of size 1024>
gap> uu1 := RightQuasigroupByRightSection(S,sec,false);; # 15ms
gap> RightSection(uu0);; # 0ms, attribute is stored
gap> RightSection(uu1);; # 0ms, attribute is stored
gap> List( uu0, x -> RightTranslation( uu0, x ));; # 78ms, quick, because RightTranslation checks if RightSection exists
gap> List( uu1, x -> RightTranslation( uu1, x ));; # quick again

# CONSTRUCTORS BY RIGHT FOLDER
#
# There are no explicit checks of arguments yet. A right section is built and then checked.
#
gap> G := SymmetricGroup(6);;
gap> H := Subgroup(G,[(1,2,3)]);;
gap> T := RightTransversal(G,H);;
gap> Q := RightQuasigroupByRightFolder(G,H,T,false,true); #100ms
<right quasigroup of size 240>
gap> Q := RightQuasigroupByRightFolder(G,H,T,true,true); #140ms
<right quasigroup of size 240>

# Note: QuasigroupByRightFolder(G,H,T,false,true) fails since the folder does not give rise to left division
#
gap> G := SymmetricGroup(2);;
gap> H := Subgroup(G,[()]);;
gap> T := RightTransversal(G,H);;
gap> Q := LoopByRightFolder(G,H,T,false,true);
<loop of size 2>

# CONVERSIONS
#
gap> AsRightQuasigroup(GF(7)^2); # additive group
<right quasigroup of size 49>
gap> AsRightQuasigroup(AlternatingGroup(4)); # group
<right quasigroup of size 12>

# conversions among right quasigroup, quasigroup, loop
# Note: Will have problems if the original algebra is very large and NOT index based since it creates the Cayley table as part of the conversion.
gap> R := RightQuasigroupByFunction(GF(5),\+);; # in fact a loop
gap> Q := AsQuasigroup( R );
<quasigroup of size 5>
gap> L := AsLoop( R, false, true ); # optional arguments still apply here
<loop of size 5>
gap> R2 := AsRightQuasigroup( L ); # downgrade is possible
<right quasigroup of size 5>
gap> R = R2;
false

# another conversion example
gap> R := ProjectionRightQuasigroup([1..3]);;
gap> Q := AsQuasigroup( R, false ); # Note: not mathematically a quasigroup but we did not ask to check argument
<quasigroup of size 3>

# Note: Q := AsQuasigroup( R, true, true ) will fail
#
# SUBALGEBRAS
#
gap> Q := ProjectionRightQuasigroup([1,3,5,7,9]);;
gap> S := Subrightquasigroup(Q,[Q.2,Q.3]);; 
gap> Elements(S);
[ r:3, r:5 ]
gap> S := Subrightquasigroup(Q,[Q[3],Q[5]]);;
gap> Elements(S);
[ r:3, r:5 ]
gap> CayleyTable(S);
[ [ 3, 3 ], [ 5, 5 ] ]
gap> MultiplicationTable(S);
[ [ 1, 1 ], [ 2, 2 ] ]

#
gap> Q := QuasigroupByFunction([0..7],function(x,y) return (x+y) mod 8; end);;
gap> S := Subquasigroup( Q, [4] );;
gap> RightTranslation( Q, Q[4] );
(1,5)(2,6)(3,7)(4,8)
gap> RightTranslation( S, S[4] ); # note the indexing relative to parent
(1,5)
gap> Q := QuasigroupByFunction([0..7],function(x,y) return (x+y) mod 8; end, false);; # no multiplication table
gap> S := Subquasigroup( Q, [4] );;
gap> RightTranslation( Q, Q[4] );
(1,5)(2,6)(3,7)(4,8)
gap> RightTranslation( S, S[4] );
(1,5)

#
gap> Q := AsRightQuasigroup( GF(8) );;
gap> Elements( Q ){[1,2,3]};
[ r:0*Z(2), r:Z(2)^0, r:Z(2^3) ]
gap> x := Z(2^3)^3;;
gap> S := Subrightquasigroup( Q, [x]);;
gap> Elements(S);
[ r:0*Z(2), r:Z(2^3)^3 ]
gap> RightTranslation(Q,x); 
(1,5)(2,3)(4,7)(6,8)
gap> RightTranslation(S,x);
(1,5)
gap> RelativeRightMultiplicationGroup(Q,S);
Group([ (1,5)(2,3)(4,7)(6,8) ])
gap> RightMultiplicationGroup(S);
Group([ (1,5) ])
gap> RightMultiplicationGroup(Q);
Group([ (1,4)(2,8)(3,6)(5,7), (1,3)(2,5)(4,6)(7,8), (1,2)(3,5)(4,8)(6,7) ])

#
gap> Q := AsRightQuasigroup( GF(8) );; 
gap> x := Z(2^3)^3;;

# S := Subquasigroup(Q,[x]) will fail. S is a quasigroup mathematically, but we are trying to call Subquasigroup on a right quasigroup
gap> Q := AsQuasigroup( Q );;
gap> S := Subquasigroup(Q,[x]); # it works because underlying elements have not changed during AsQuasigroup upgrade
<quasigroup of size 2>

# CONGRUENCES AND FACTORS
#
gap> Q := ProjectionRightQuasigroup([1..6]);;
gap> SetRightQuasigroupElementsName(Q,"q");;
gap> C := EquivalenceRelationByPartition(Q,[[Q.1,Q.2],[Q.3,Q.4,Q.5],[Q.6]]);;
gap> IsRightQuasigroupCongruence(C);
true
gap> F := Q/C;;
gap> Elements(F);
[ r:{q:1}, r:{q:3}, r:{q:6} ]
gap> H := FactorRightQuasigroup( Q, C, false ); # non index based version is supported (but not for /)
<right quasigroup of size 3>
gap> HasCayleyTable( H );
false
gap> H.1*H.2;
r:{q:1}
gap> CayleyTable(H);
[ [ {q:1}, {q:1}, {q:1} ], [ {q:3}, {q:3}, {q:3} ], [ {q:6}, {q:6}, {q:6} ] ]

# analogous methods exist for quasigroups and loops
#
# PAIGE LOOP EXAMPLE
#
gap> DotProduct := function( x, y ) return Sum( [1..Length(x)], i -> x[i]*y[i] ); end;;
gap> CrossProduct := function( x, y ) return [ x[2]*y[3]-x[3]*y[2], x[3]*y[1]-x[1]*y[3], x[1]*y[2]-x[2]*y[1] ]; end;;
gap> PaigeNorm := function( x ) return x[1]*x[8] - DotProduct( x{[2,3,4]},x{[5,6,7]} ); end;;
gap> PaigeMult := function( x, y )
> local a, b, c, d;
> a := x[1]*y[1] + DotProduct(x{[2,3,4]},y{[5,6,7]});
> b := x[1]*y{[2,3,4]} + x{[2,3,4]}*y[8] - CrossProduct(x{[5,6,7]},y{[5,6,7]});
> c := x{[5,6,7]}*y[1] + x[8]*y{[5,6,7]} + CrossProduct(x{[2,3,4]},y{[2,3,4]});
> d := DotProduct(x{[5,6,7]},y{[2,3,4]})+x[8]*y[8];
> return Concatenation( [a], b, c, [d] );
> end;;

# Paige loop over GF(2) (index based approach in characteristic 2)
gap> F := GF(2);;
gap> S := Filtered( F^8, x -> PaigeNorm( x ) = One( F ) );;
gap> P := LoopByFunction( S, PaigeMult, true, true ); 
<loop of size 120>

# general approach (not index based, any characteristic, using congruences)
gap> n := 3;; # any prime power works but it will be slow
gap> F := GF(n);;
gap> S := Filtered( F^8, x -> PaigeNorm( x ) = One( F ) );;
gap> M := LoopByFunction( S, PaigeMult, false, false );;
gap> SetLoopElementsName(M,"m");; # for easier reading of factor elements, must factor out +/- one
gap> C := EquivalenceRelationByPartition( M, Set( S, x -> Set( [ M[x], M[-x] ] ) ) );; 
gap> P := FactorLoop(M,C,false,false); # 2000 ms
<loop of size 1080>

# another approach using normal subloop
gap> n := 3;; F := GF(n);; S := Filtered( F^8, x -> PaigeNorm( x ) = One( F ) );;
gap> M := LoopByFunction( S, PaigeMult, false, false );;
gap> one := [ Z(n)^0, 0*Z(n), 0*Z(n), 0*Z(n), 0*Z(n), 0*Z(n), 0*Z(n), Z(n)^0 ];;
gap> N := Subloop( M, [-one] );;
gap> P := FactorLoop( M, N, false, false ); # 2000 ms, mostly because the neutral element is listed in position 352 and it takes a while to find it
<loop of size 1080>

# the Paige construction can be hacked by providing left division and right division functions