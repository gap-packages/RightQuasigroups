LogTo("/tmp/temp_Constructors.tst");
START_TEST("RightQuasigroups package: Constructors.tst");
SizeScreen([72,23]);

LoadPackage("RightQuasigroups", false);

# By default, all constructors produce index based right quasigroups and their arguments are not checked.

# Adding optional boolean arguments indexBased, checkArgs changes the default behavior.
# It is possible to list no optional arguments, only the optional argument indexBased, or both optional arguments.

# TESTING ALGEBRA SHELL (not for casual users)

# right quasigroup shell
Q := RQ_AlgebraShell( IsRightQuasigroup, ["a","b","c"] );
Q.1; # Note: Q.1*Q.1 will fail since no operations are installed in the algebra shell
Q["b"]; 
IsIndexBased( Q );
Q.1![1];
UnderlyingSetElm( Q.1 );
SetRightQuasigroupElementsName(Q,"my");; # Note: Variations exist for quasigroups and loops
Q.1;

# quasigroup shell
Q := RQ_AlgebraShell( IsQuasigroup, ["a","b","c"] );
Q.1;
IsIndexBased( Q );
Q.1![1];

# loop shell
Q := RQ_AlgebraShell( IsLoop, ["a","b","c"] );
Elements(Q);


# Note: One(Q) will fail, identity element (which mathematically exists) has not been added since IsQuasigroup should not have one
#
#
# CONSTRUCTORS BY CAYLEY TABLE
#
ct := [["a","a"],["b","b"]];;
IsRightQuasigroupCayleyTable(ct);
IsQuasigroupCayleyTable(ct);
Q := RightQuasigroupByCayleyTable( ct );
HasMultiplicationTable( Q );
Q := RightQuasigroupByCayleyTable( ct );;
HasMultiplicationTable( Q );
Q["a"]*Q["b"];
Q["a"]/Q["b"];
Q := LoopByCayleyTable( ct ); # Note: Works because by default checking does not happen

# Note: Q := LoopByCayleyTable( ct, false, true ); will fail since arguments are checked
#
Q := LoopByCayleyTable([["c","a","b"],["a","b","c"],["b","c","a"]],true,true); 
One(Q); # Note: The neutral element need not be the first element of the underlying set
HasLeftDivisionTable( Q );
HasLeftDivisionCayleyTable( Q );
LeftDivisionCayleyTable( Q );

# CONSTRUCTORS BY FUNCTIONS
#
# testing parameters
IsRightQuasigroupFunction([1..10],function(x,y) return x; end);
IsQuasigroupFunction([1..10],function(x,y) return x; end);
IsQuasigroupFunction(GF(8),\+); # Note: we accept GF(8) or anything that can be made into a set

# IsQuasigroupFunction(GF(8)),\+,\-); fails since we need 1 or 3 functions
#
IsQuasigroupFunction(GF(8),\+,\-,function(x,y) return -x+y; end);
IsLoopFunction(GF(8),\+,\-,function(x,y) return -x+y; end); # neutral element is optional
IsLoopFunction(GF(8),\+,\-,function(x,y) return -x+y; end, One(GF(8))); # false because One(GF(8)) is not the neutral element with resp\
ect to addition
IsLoopFunction(GF(8),\+,\-,function(x,y) return -x+y; end, Zero(GF(8)));

# constructors
mult := function(x,y) return (x+y) mod 1000; end;;
rdiv := function(x,y) return (x-y) mod 1000; end;;
Q := RightQuasigroupByFunctions([0..999],mult,rdiv,false,false); # instantaneous
Q[500]*Q[501];
Q := RightQuasigroupByFunctions([0..999],mult,rdiv,false,true); # 500ms, verification takes time
Q := RightQuasigroupByFunctions([0..999],mult,rdiv,true,true); # 1000ms, building operation tables takes more time
ldiv := function(x,y) return (-x+y) mod 1000; end;;
Q := QuasigroupByFunctions([0..999],mult,rdiv,ldiv,false,false); 
Q := LoopByFunctions([0..999],mult,rdiv,ldiv,0,false,false); 

# CONSTRUCTORS BY FUNCTION
#
Q := RightQuasigroupByFunction(GF(8),\+);
Q := QuasigroupByFunction(GF(8),\+);
Q := LoopByFunction(GF(8),\+);

# the multiplicative group of GF(8)
Q := RightQuasigroupByFunction( List([1..7],i->Z(8)^i), \* );
Elements(Q){[1,2,3]};

# SPECIAL RIGHT QUASIGROUPS
#
# projection right quasigroup: multiplication x*y = x
Q := ProjectionRightQuasigroup( ["a","b","c"] );
CayleyTable( Q );

# right core of group: multiplication given by y*x^(-1)*y
Q := RightCoreOfGroup(SymmetricGroup(5));

# TRANSLATIONS, SECTIONS, MULTIPLICATION GROUPS
#
Q := QuasigroupByFunction(GF(8),\+);;
RightTranslation( Q, Zero(GF(8)) );
RightTranslation( Q, Q.1 );
RightTranslation( Q, Q[Zero(GF(8))] );
LeftTranslation( Q, One(GF(8)) );
RightSection( Q ){[1,2,3]};
LeftSection( Q ) = RightSection( Q );

# affine quasigroup
Q := QuasigroupByFunction([0..6], function(x,y) return (x-2*y) mod 7; end, true, true );
RightTranslation(Q,0);
LeftTranslation(Q,0);
R := RightMultiplicationGroup( Q ); # note the small generating set not consisting of all right translations
Size(R);
L := LeftMultiplicationGroup( Q );
Size( L );
M := MultiplicationGroup( Q );;
Size(M);

# CONSTRUCTORS BY RIGHT SECTION
#
# testing right sections
section := [(),(1,2,3,4),(),(1,3)(2,4)];;
IsRightQuasigroupRightSection([1..4],section);
IsRightQuasigroupRightSection([1..3],section);
IsRightQuasigroupRightSection("abcd",section); # implicit permutation action on the set ['a','b','c','d']
IsQuasigroupRightSection([1..4],section);
id := Z(2)*[[1,0],[0,1]];;
ma := Z(2)*[[1,1],[0,1]];;
section := [ma,ma,id,id];;
IsRightQuasigroupRightSection( GF(2)^2, section );
IsQuasigroupRightSection( GF(2)^2, section );

# constructors
Q := RightQuasigroupByRightSection([1..4],[(),(1,2,3,4),(),(1,3)(2,4)]);
Q[3]*Q[2];
Q := RightQuasigroupByRightSection("abcd",[(),(1,2,3,4),(),(1,3)(2,4)] );
id := Z(2)*[[1,0],[0,1]];;
ma := Z(2)*[[1,1],[0,1]];;
Q := RightQuasigroupByRightSection( GF(2)^2, [ma,ma,id,id] );
Q.1;

# larger example 
S := GF(2)^10;;
sec := List(S,x->Random(GL(10,2)));;
uu0 := RightQuasigroupByRightSection(S,sec); # 1200ms
uu1 := RightQuasigroupByRightSection(S,sec,false);; # 15ms
RightSection(uu0);; # 0ms, attribute is stored
RightSection(uu1);; # 0ms, attribute is stored
List( uu0, x -> RightTranslation( uu0, x ));; # 78ms, quick, because RightTranslation checks if RightSection exists
List( uu1, x -> RightTranslation( uu1, x ));; # quick again

# CONSTRUCTORS BY RIGHT FOLDER
#
# There are no explicit checks of arguments yet. A right section is built and then checked.
#
G := SymmetricGroup(6);;
H := Subgroup(G,[(1,2,3)]);;
T := RightTransversal(G,H);;
Q := RightQuasigroupByRightFolder(G,H,T,false,true); #100ms
Q := RightQuasigroupByRightFolder(G,H,T,true,true); #140ms

# Note: QuasigroupByRightFolder(G,H,T,false,true) fails since the folder does not give rise to left division
#
G := SymmetricGroup(2);;
H := Subgroup(G,[()]);;
T := RightTransversal(G,H);;
Q := LoopByRightFolder(G,H,T,false,true);

# CONVERSIONS
#
AsRightQuasigroup(GF(7)^2); # additive group
AsRightQuasigroup(AlternatingGroup(4)); # group

# conversions among right quasigroup, quasigroup, loop
# Note: Will have problems if the original algebra is very large and NOT index based since it creates the Cayley table as part of the c\
onversion.
R := RightQuasigroupByFunction(GF(5),\+);; # in fact a loop
Q := AsQuasigroup( R );
L := AsLoop( R, false, true ); # optional arguments still apply here
R2 := AsRightQuasigroup( L ); # downgrade is possible
R = R2;

# another conversion example
R := ProjectionRightQuasigroup([1..3]);;
Q := AsQuasigroup( R, false ); # Note: not mathematically a quasigroup but we did not ask to check argument

# Note: Q := AsQuasigroup( R, true, true ) will fail
#
# SUBALGEBRAS
#
Q := ProjectionRightQuasigroup([1,3,5,7,9]);;
S := Subrightquasigroup(Q,[Q.2,Q.3]);; 
Elements(S);
S := Subrightquasigroup(Q,[Q[3],Q[5]]);;
Elements(S);
CayleyTable(S);
MultiplicationTable(S);

#
Q := QuasigroupByFunction([0..7],function(x,y) return (x+y) mod 8; end);;
S := Subquasigroup( Q, [4] );;
RightTranslation( Q, Q[4] );
RightTranslation( S, S[4] ); # note the indexing relative to parent
Q := QuasigroupByFunction([0..7],function(x,y) return (x+y) mod 8; end, false);; # no multiplication table
S := Subquasigroup( Q, [4] );;
RightTranslation( Q, Q[4] );
RightTranslation( S, S[4] );

#
Q := AsRightQuasigroup( GF(8) );;
Elements( Q ){[1,2,3]};
x := Z(2^3)^3;;
S := Subrightquasigroup( Q, [x]);;
Elements(S);
RightTranslation(Q,x); 
RightTranslation(S,x);
RelativeRightMultiplicationGroup(Q,S);
RightMultiplicationGroup(S);
RightMultiplicationGroup(Q);

#
Q := AsRightQuasigroup( GF(8) );; 
x := Z(2^3)^3;;

# S := Subquasigroup(Q,[x]) will fail. S is a quasigroup mathematically, but we are trying to call Subquasigroup on a right quasigroup
Q := AsQuasigroup( Q );;
S := Subquasigroup(Q,[x]); # it works because underlying elements have not changed during AsQuasigroup upgrade

# CONGRUENCES AND FACTORS
#
Q := ProjectionRightQuasigroup([1..6]);;
SetRightQuasigroupElementsName(Q,"q");;
C := EquivalenceRelationByPartition(Q,[[Q.1,Q.2],[Q.3,Q.4,Q.5],[Q.6]]);;
IsRightQuasigroupCongruence(C);
F := Q/C;;
Elements(F);
H := FactorRightQuasigroup( Q, C, false ); # non index based version is supported (but not for /)
HasCayleyTable( H );
H.1*H.2;
CayleyTable(H);

# analogous methods exist for quasigroups and loops
#
# PAIGE LOOP EXAMPLE
#
DotProduct := function( x, y ) return Sum( [1..Length(x)], i -> x[i]*y[i] ); end;;
CrossProduct := function( x, y ) return [ x[2]*y[3]-x[3]*y[2], x[3]*y[1]-x[1]*y[3], x[1]*y[2]-x[2]*y[1] ]; end;;
PaigeNorm := function( x ) return x[1]*x[8] - DotProduct( x{[2,3,4]},x{[5,6,7]} ); end;;
PaigeMult := function( x, y )

# Paige loop over GF(2) (index based approach in characteristic 2)
F := GF(2);;
S := Filtered( F^8, x -> PaigeNorm( x ) = One( F ) );;
P := LoopByFunction( S, PaigeMult, true, true ); 

# general approach (not index based, any characteristic, using congruences)
n := 3;; # any prime power works but it will be slow
F := GF(n);;
S := Filtered( F^8, x -> PaigeNorm( x ) = One( F ) );;
M := LoopByFunction( S, PaigeMult, false, false );;
SetLoopElementsName(M,"m");; # for easier reading of factor elements, must factor out +/- one
C := EquivalenceRelationByPartition( M, Set( S, x -> Set( [ M[x], M[-x] ] ) ) );; 
P := FactorLoop(M,C,false,false); # 2000 ms

# another approach using normal subloop
n := 3;; F := GF(n);; S := Filtered( F^8, x -> PaigeNorm( x ) = One( F ) );;
M := LoopByFunction( S, PaigeMult, false, false );;
one := [ Z(n)^0, 0*Z(n), 0*Z(n), 0*Z(n), 0*Z(n), 0*Z(n), 0*Z(n), Z(n)^0 ];;
N := Subloop( M, [-one] );;
P := FactorLoop( M, N, false, false ); # 2000 ms, mostly because the neutral element is listed in position 352 and it takes a while to \
find it

# the Paige construction can be hacked by providing left division and right division functions

STOP_TEST( "Constructors.tst", 10000 );
LogTo();