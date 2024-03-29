# HSP.gd
# Direct products, subalgebras and factor algebras
# =============================================================================

#! @Chapter Direct products, subalgebras and factor algebras

# DIRECT PRODUCT OF RIGHT QUASIGROUPS
# _____________________________________________________________________________

#! @Section Direct product of right quasigroups

# RQ_DirectProduct( list )
# auxiliary function that returns the direct product of right quasigroups in list
DeclareGlobalFunction( "RQ_DirectProduct" );

# DirectProduct already declared 

#! <P/>`DirectProduct(Q1,...,Qn)` returns the direct product of right quasigroups
#! `Q1`, ..., `Qn`. We allow groups to be among the arguments.

#! <P/>If all arguments are groups then the standard &GAP; method is called and a group is returned.

#! <P/>Otherwise, if all non-group arguments are loops then a loop is returned,
#! else if all non-group arguments are quasigroups then a quasigroup is returned,
#! else a right quasigroup is returned. The underlying set is the carthesian product
#! of elements of `Q1`, ..., `Qn`. The restulting algebra is index based if and only if
#! all non-group arguments are index based. An effort is made to inherit common
#! properties of `Q1`, ...,. `Qn`.

#! @BeginExampleSession
#! gap> G := Group((1,2));;
#! gap> L := LoopByCayleyTable( [[1,2,3],[2,3,1],[3,1,2]] );;
#! gap> R := ProjectionRightQuasigroup( [1..3] );;
#! gap> D := DirectProduct( G, L );
#! <loop of size 6>
#! gap> D.1;
#! l[ (), l1 ]
#! gap> DirectProduct( G, L, R );
#! <right quasigroup of size 18>
#! @EndExampleSession

# OPPOSITE QUASIGROUPS
# _____________________________________________________________________________

#! @Section Opposite quasigroups

# RQ_OppositeAlgebra( category, Q )
# returns the algebra opposite of Q, with category in [ IsQuasigroup, IsLoop ]
DeclareGlobalFunction( "RQ_OppositeAlgebra" );

#! <P/>Given a quasigroup $(Q,\cdot)$, its <Index>opposite quasigroup</Index>**opposite**
#! is the quasigroup $(Q,\circ)$ with multiplication $x\circ y = y\cdot x$. If $(Q,\cdot)$ is a loop,
#! its opposite is also a loop. (Note that the opposite of a right quasigroup $Q$ is a right quasigroup
#! iff $Q$ is a quasigroup. We therefore do not support the opposite construction for right quasigroups.)

#! @BeginGroup
#! @GroupTitle Opposite quasigroups and loops

#! @Arguments Q
#! @Returns the opposite quasigroup (loop) of the quasigroup (loop) <Arg>Q</Arg>.
#! The resulting algebra is index based iff <Arg>Q</Arg> is index based. An effort is made
#! to inherit dual properties from <Arg>Q</Arg>
DeclareOperation( "OppositeQuasigroup", [ IsQuasigroup ] );

#! @Arguments Q
DeclareOperation( "OppositeLoop", [ IsLoop ] );

#! @BeginExampleSession
#! gap> Q := QuasigroupByFunction( GF(3), \- );;
#! gap> OQ := OppositeQuasigroup( Q );
#! <quasigroup of size 3>
#! gap> Display( MultiplicationTable( Q ) );
#! [ [  1,  3,  2 ],
#!   [  2,  1,  3 ],
#!   [  3,  2,  1 ] ]
#! gap> Display( MultiplicationTable( OQ ) );
#! [ [  1,  2,  3 ],
#!   [  3,  1,  2 ],
#!   [  2,  3,  1 ] ]
#! gap> B := LeftBolLoop( 8, 1 );
#! LeftBolLoop( 8, 1 )
#! gap> OppositeLoop( B ); # dual properties inherited
#! <right Bol loop of size 8>
#! @EndExampleSession

#! @EndGroup

# SUBALGEBRAS
# _____________________________________________________________________________

#! @Section Subalgebras

#! <P/>A subset $S$ of a right quasigroup $Q$ is a <Index>subrightquasigroup</Index>
#! **subrightquasigroup** of $Q$ if it is closed under multiplication and right division.
#! A subset $S$ of a quasigroup (resp. loop) $Q$ is a <Index>subquasigroup</Index> 
#! **subquasigroup** (resp. <Index>subloop</Index>**subloop**) if it is closed under
#! multiplication and both divisions. In all of the above cases, when $Q$ is finite,
#! $S$ is a subalgebra if it is closed under multiplicaton.

#! <P/>In &RightQuasigroups;, if a subalgebra `S` is created from an algebra `Q`,
#! the parent of `S` is set to the parent of `Q`, possibly `Q` itself, and the elements
#! of `S` are inherited from the parent of `Q`, cf. Section <Ref Sect="Section_Parent"/>.
#! If `A`, `B` are two algebras then `A` is a subalgebra of `B` iff `Parent( A ) = Parent( B )`
#! and the elements of `A` form a subset of `B`.

#! @BeginGroup
#! @GroupTitle Testing for subalgebras

#! @Arguments Q, S
DeclareOperation( "IsSubrightquasigroup", [ IsRightQuasigroup, IsRightQuasigroup ] );

#! @Arguments Q, S
DeclareOperation( "IsSubquasigroup", [ IsQuasigroup, IsQuasigroup ] );

#! @Arguments Q, S
#! @Returns true if a right quasigroup (quasigroup, loop) <Arg>S</Arg>
#! is a subrightquasigroup (subquasigroup, subloop) of a right quasigroup
#! (quasigroup, loop) <Arg>Q</Arg>, else returns `false`. 
DeclareOperation( "IsSubloop", [ IsLoop, IsLoop ] );

#! @EndGroup

#! @BeginGroup
#! @GroupTitle Constructing subalgebras

# Constructors for subquasigroups. No optional arguments here since index based in inherited from parent.

# RQ_InheritProperties( P, Q, isDual )
# auxiliary function
# sets properties for Q inherited from P 
# we also allow P to be a list of algebras
# if isDual is true, properties will be inherited dually (this is useful in Opposite)
DeclareGlobalFunction( "RQ_InheritProperties" ); # P, Q

# RQ_Subalgebra( Q, gens )
# returns the appropriate subalgebra of Q generated by gens.
# If gens is a list of elements of Q, returns the subquasigroup of Q generated by ls.
# If ls is a list of elements of the underlying set, returns the subquasigroup of Q generated by the elements corresponding to ls.
# Note: It is not neccessary to include "category" as an argument since this can be read from Q.
DeclareGlobalFunction( "RQ_Subalgebra" );

#! @Arguments Q, gens
DeclareOperation( "Subrightquasigroup", [IsRightQuasigroup, IsCollection ] );

#! @Arguments Q, gens
DeclareOperation( "Subquasigroup", [IsQuasigroup, IsCollection ] );

#! @Arguments Q, gens
#! @Returns the subrightquasigoup (subquasigroup, subloop) of a right quasigroup (quasigroup, loop) <Arg>Q</Arg>
#! generated by the list of elements <Arg>gens</Arg>. We allow <Arg>gens</Arg> to consist of elements of <Arg>Q</Arg> or 
#! of elements of the underlying set of <Arg>Q</Arg>. Note that there are no optional arguments in this constructor.
#! The resulting subalgebra will be index based (cf. Section <Ref Sect="Section_IndexBased"/>) iff
#! <Arg>Q</Arg> is index based. For subloops, we allow <Arg>gens</Arg> to be an empty set, in which case the trivial
#! subloop is returned.
#! @Description An effort is made for the subalgebra to inherit properties from <Arg>Q</Arg>. For instance,
#! if it is known that <Arg>Q</Arg> is commutative, the subalgebra will have an attribute that signifies it
#! is commutative.
DeclareOperation( "Subloop", [IsLoop, IsList ] ); 

#! @BeginExampleSession
#! gap> Q := LoopByFunction([0..7],function(x,y) return (x+y) mod 8; end);;
#! gap> S := Subrightquasigroup( Q, [4] ); # inherits loop property from parent
#! <loop of size 2>
#! gap> [ IsSubrightquasigroup( Q, S ), IsSubquasigroup( Q, S ), IsSubloop( Q, S ) ];
#! [ true, true, true ]
#! gap> Elements( S ); # note indexing of elements here and below
#! [ l0, l4 ]
#! gap> Elements( S )[ 2 ]; # the 2nd element of S
#! l4
#! gap> S.2; # the 2nd element of Q, the parent of S
#! l1
#! gap> S[4]; # the element of parent Q corresponding to the given element of the underlying set
#! l4
#! gap> Display( CayleyTable( S ) );
#! [ [  0,  4 ],
#!   [  4,  0 ] ]
#! gap> RightTranslation( Q, Q[4] ); # a permutation of the index set of Q
#! (1,5)(2,6)(3,7)(4,8)
#! gap> RightTranslation( S, S[4] ); # a permutation of the index set of S
#! (1,5)
#! gap> Subquasigroup( Q, [4] );
#! <loop of size 2>
#! gap> Subloop( Q, [4] );
#! <loop of size 2>
#! @EndExampleSession

#! @EndGroup

# ALL SUBALGEBRAS
# _____________________________________________________________________________

#! @BeginGroup
#! @GroupTitle All subalgebras

#! @Arguments Q
DeclareOperation( "AllSubrightquasigroups", [ IsRightQuasigroup ] );

#! @Arguments Q
DeclareOperation( "AllSubquasigroups", [ IsQuasigroup ] );

#! @Arguments Q
#! @Returns a list of all subrightquasigroups (subquasigroups, subloops) of
#! a right quasigroup (quasigroup, loop) <Arg>Q</Arg>.
DeclareOperation( "AllSubloops", [ IsLoop ] );

#! @BeginExampleSession
#! gap> AllSubloops( AsLoop( CyclicGroup( 3 ) ) );
#! [ <trivial group with 1 generator>, <associative loop of size 3> ]
#! gap> P := ProjectionRightQuasigroup( 2 );; 
#! gap> Length( AllSubrightquasigroups( P ) ); # every nonempty subset is a subrightquasigroup here
#! 3
#! @EndExampleSession

#! @EndGroup

# MINIMAL SUBALGEBRAS
# _____________________________________________________________________________

#! <P/>A subloop $S$ of a loop $Q$ is **minimal**<Index>minimal subloop</Index> if $S$ 
#! is nontrivial and $S$ contains no proper nontrivial subloops.
#! A sub(right)quasigroup $S$ of a (right) quasigroup $Q$ is
#! **minimal**<Index>minimal subquasigroup</Index><Index>minimal subquasigroup</Index> if $S$ contains
#! no proper sub(right)quasigroups.

#! @BeginGroup
#! @GroupTitle Testing minimal subalgebras

#! @Arguments [Q, ]S
DeclareOperation( "IsMinimalSubrightquasigroup", [ IsRightQuasigroup ] );

#! @Arguments [Q, ]S
DeclareOperation( "IsMinimalSubquasigroup", [ IsQuasigroup ] );

#! @Arguments [Q, ]S
#! @Returns `true` iff <Arg>S</Arg> is a minimal subrightquasigroup (subquasigroup, subloop), else returns false.
#! @Description Note that it is not necessary to specify the enveloping right quasigroup (quasigroup, loop)
#! since all needed information is contained already in <Arg>S</Arg>. In the version with two arguments
#! <Arg>Q</Arg>, <Arg>S</Arg>, it is first checked that <Arg>S</Arg> is a subalgebra of <Arg>Q</Arg>.
DeclareOperation( "IsMinimalSubloop", [ IsLoop ]);

#! @EndGroup

#! @BeginGroup
#! @GroupTitle All minimal subalgebras

#! @Arguments Q
DeclareOperation( "AllMinimalSubrightquasigroups", [ IsRightQuasigroup ] );

#! @Arguments Q
DeclareOperation( "AllMinimalSubquasigroups", [ IsQuasigroup ] );

#! @Arguments Q
#! @Returns a list of all minimal subrightquasigroups (subquasigroups, subloops) of
#! a right quasigroup (quasigroup, loop) <Arg>Q</Arg>.
DeclareOperation( "AllMinimalSubloops", [ IsLoop ] );

#! @EndGroup

# MAXIMAL SUBALGEBRAS
# _____________________________________________________________________________

#! <P/>A surighquasigroup $S$ of a right quasigroup $Q$ is
#! **maximal**<Index>maximal surightquasigroup</Index><Index>maximal suquasigroup</Index><Index>maximal subloop</Index>
#! if $S$ is propertly contained in $Q$ and if whenever $S&lt;A&lt;Q$ then either $A=S$ or $A=Q$.

#! @BeginGroup
#! @GroupTitle Testing maximal subalgebras

#! @Arguments Q, S
DeclareOperation( "IsMaximalSubrightquasigroup", [ IsRightQuasigroup , IsRightQuasigroup] );

#! @Arguments Q, S
DeclareOperation( "IsMaximalSubquasigroup", [ IsQuasigroup, IsQuasigroup ] );

#! @Arguments Q, S
#! @Returns `true` iff <Arg>S</Arg> is a maximal subrightquasigroup (subquasigroup, subloop)
#! of the right quasigroup (quasigroup, loop) <Arg>Q</Arg>, else returns false.
DeclareOperation( "IsMaximalSubloop", [ IsLoop, IsLoop ]);

#! @EndGroup

#! @BeginGroup
#! @GroupTitle All maximal subalgebras

#! @Arguments Q
DeclareOperation( "AllMaximalSubrightquasigroups", [ IsRightQuasigroup ] );

#! @Arguments Q
DeclareOperation( "AllMaximalSubquasigroups", [ IsQuasigroup ] );

#! @Arguments Q
#! @Returns a list of all maximal subrightquasigroups (subquasigroups, subloops) of
#! the right quasigroup (quasigroup, loop) <Arg>Q</Arg>.
DeclareOperation( "AllMaximalSubloops", [ IsLoop ]);

#! @BeginExampleSession
#! gap> Q := MoufangLoop(12,1);;
#! gap> S := Subloop(Q,[Q.2]);;
#! gap> IsMinimalSubloop(S);
#! true
#! gap> AllMinimalSubloops(Q);
#! [ <Moufang loop of size 2>, <Moufang loop of size 2>, 
#!   <Moufang loop of size 3>, <Moufang loop of size 2>, 
#!   <Moufang loop of size 2>, <Moufang loop of size 2>, 
#!   <Moufang loop of size 2>, <Moufang loop of size 2>, 
#!   <Moufang loop of size 2>, <Moufang loop of size 2> ]
#! gap> IsMaximalSubloop(Q,S);
#! false
#! gap> AllMaximalSubloops(Q);
#! [ <Moufang loop of size 6>, <Moufang loop of size 4>, 
#!   <Moufang loop of size 4>, <Moufang loop of size 4>, 
#!   <Moufang loop of size 6>, <Moufang loop of size 6>, 
#!   <Moufang loop of size 4>, <Moufang loop of size 4>, 
#!   <Moufang loop of size 4>, <Moufang loop of size 4>, 
#!   <Moufang loop of size 4>, <Moufang loop of size 4> ]
#! @EndExampleSession

#! @EndGroup


# COSETS AND TRANSVERSALS
# _____________________________________________________________________________

#! @Section Cosets and transversals

#! <P/>If $S$ is a subrightquasigroup of a right quasigroup $Q$, the <Index>right coset</Index>
#! **right cosets** are subsets of $Q$ of the form $Sx=\{sx:s\in S\}$, where $x\in Q$.
#! All right cosets of a subrightquasigroup $S$ of a right quasigroup $Q$
#! have the same cardinality, but they need not cover $Q$ and they can intersect
#! in nontrivial ways. In quasigroups and loops, the right cosest cover $Q$.

#! <P/> A <Index>right transversal</Index>**right transversal**
#! to $S$ in $Q$ is then a list of elements of `Q` containing one element from each right coset of $S$ in $Q$.

#! <P/>In &RightQuasigroups;, the right cosets and right transversals are mere lists, 
#! not special &GAP; objects.

# RightCosetsNC
# PROG: RightCosets(Q,S) is implemented as a global function in GAP. It
# checks if S is a subset of Q and then calls operation RightCosetsNC.
# REVISIT: Should RightCoset( S, x ) be implemented? See GAP for their extensive implementation.

#! <P/>The function `RightCosets( Q, S )` checks that `S` is a subrightquasigroup of `Q` and then
#! returns a list of all right cosets of `S` in `Q`. 

#! @Arguments Q, S
#! @Returns a right transversal to <Arg>S</Arg> in <Arg>Q</Arg>. 
DeclareOperation( "RightTransversal", [ IsRightQuasigroup, IsRightQuasigroup ] );
# REVISIT: This is a candidate for InParentFOA, as in the case of groups.

#! @BeginExampleSession
#! gap> P := ProjectionRightQuasigroup( 3 );;
#! gap> Display( MultiplicationTable( P ) );
#! [ [  1,  1,  1 ],
#!   [  2,  2,  2 ],
#!   [  3,  3,  3 ] ]
#! gap> S := Subrightquasigroup( P, [1,2] );;
#! gap> RightCosets( P, S ); # there is a single right coset of S in P
#! [ [ r1, r2 ] ]
#! gap> RightTransversal( P, S );
#! [ r1 ]
#! @EndExampleSession

#! <Index>left coset</Index>**Left cosets** $xS$ and <Index>left transversal</Index>**left transversals**
#! are defined dually to right cosets and right transversals. In right quasigroups, the left cosets of
#! $S$ need not have the same cardinality and can intersect in nontrivial ways, but they cover $Q$.
#! In quasigroups and loops, all left cosets of $S$ have the same cardinality.

#! <P/>The function `LeftCosets( Q, S )` checks that `S` is a subrightquasigroup of `Q` and then
#! returns a list of all left cosets of `S` in `Q`. 

#! @Arguments Q, S
#! @Returns a left transversal to <Arg>S</Arg> in <Arg>Q</Arg>.
DeclareOperation( "LeftTransversal", [ IsRightQuasigroup, IsRightQuasigroup ] );

# RIGHT QUASIGROUP BY GENERATORS
# _____________________________________________________________________________

#! @Section Right quasigroups by generators

#! <P/>In analogy with the `Group` function in &GAP;, we provide methods for generating
#! right quasigroups (quasigroups, loops) from a list of right quasigroup (quasigroup, loop) elements.

# RQ_AlgebraByGenerators( category, gens )
# PROG: Category is used only to check the argument gens. It does not determined the type
# of the resulting algebra. We allow category to be IsRack, IsQuandle, which really is a property.
DeclareOperation( "RQ_AlgebraByGenerators", [ IsOperation, IsList ] );

#! @BeginGroup
#! @GroupTitle RightQuasigroup, Quasigroup and Loop

#! @Arguments gens...
#! @Returns the right quasigroup (quasigroup, loop) generated by the given right quasigroup
#! (quasigroup, loop) elements. The generators can be given as `gen1`, `gen2`, `...`, or as
#! a single argument `[ gen1, gen2, ...]`. The generators must belong to the same parent algebra.
#! The attribute `GeneratorsOfMagma` (see Section <Ref Sect="Section_Generators"/>) is not set
#! to coincide with the given list of generators.
#! The resulting algebra is index based iff the parent algebra is index based.
DeclareGlobalFunction( "RightQuasigroup" );

#! @Arguments gens...
DeclareGlobalFunction( "Quasigroup" );

#! @Arguments gens...
DeclareGlobalFunction( "Loop" );

#! @EndGroup

#! @BeginGroup
#! @GroupTitle RightQuasigroupByGenerators, QuasigroupByGenerators and LoopByGenerators

#! @Arguments gens...
#! @Returns the right quasigroup (quasigroup, loop) generated by the given right quasigroup
#! (quasigroup, loop) elements. This is just like `RightQuasigroup` (`Quasigroup`, `Loop`).
DeclareGlobalFunction( "RightQuasigroupByGenerators" );

#! @Arguments gens...
DeclareGlobalFunction( "QuasigroupByGenerators" );

#! @Arguments gens...
DeclareGlobalFunction( "LoopByGenerators" );

#! @EndGroup

#! @BeginGroup
#! @GroupTitle RightQuasigroupWithGenerators, QuasigroupWithGenerators and LoopWithGenerators

DeclareOperation( "RQ_AlgebraWithGenerators", [ IsOperation, IsList ] ); # category/property, gens

#! @Arguments gens...
#! @Returns the right quasigroup (quasigroup, loop) generated by the given right quasigroup
#! (quasigroup, loop) elements. This is just like `RightQuasigroup` (`Quasigroup`, `Loop`)
#! except that it is guaranteed that the value of `GeneratorsOfMagma` will be set
#! to coincide with the given list of generators.
DeclareGlobalFunction( "RightQuasigroupWithGenerators" );

#! @Arguments gens...
DeclareGlobalFunction( "QuasigroupWithGenerators" );

#! @Arguments gens...
DeclareGlobalFunction( "LoopWithGenerators" );

#! @EndGroup

# INTERSECTIONS AND JOINS
# _____________________________________________________________________________

#! @Section Intersections and joins of right quasigroups

#! <P/>Given a list `algebras` of at least two right quasigroups (quasigroups, loops) with the same parent algebra,
#! `Intersection( algebras )` returns their intersection subalgebra. We also support
#! `Intersection( algebra1, algebra2, ... )`.

#! <P/>Passing of arguments for `Intersection` is handled in the standard &GAP; way.
#! Therefore the only method implemented in &RightQuasigroups; is `Intersection2` for the intersection of
#! two right quasigroups.

#! <P/>Given a list `algebras` of right quasigroups (quasigroups, loops) with the same parent algebra,
#! `Join( algebras )` returns the smallest subalgebra containing all algebras in the list. We also support
#! `Join( algebra1, algebra2, ... )`.

#! <P/>The function `Join` does not seem to be implemented in &GAP;. In
#! &RightQuasigroups;, `Join` and `Join2` are implemented in a way analogous to `Intersection` and `Intersection2`,
#! except that we also allow a single algebra as the argument, in which case that algebra is returned.

# Intersection2
# (PROG) Intersection2 is called by Intersection in GAP. It is supposed to return

DeclareGlobalFunction( "Join" );
DeclareOperation( "Join2", [ IsRightQuasigroup, IsRightQuasigroup ] );

#! @BeginExampleSession
#! gap> P := ProjectionRightQuasigroup( 10 );;
#! gap> A := Subrightquasigroup( P, [1..4] );;
#! gap> B := Subrightquasigroup( P, [3..7] );;
#! gap> Intersection( A, B );
#! <associative quandle of size 2>
#! gap> Elements( last );
#! [ r3, r4 ]
#! gap> Join( A, B );
#! <associative quandle of size 7>
#! gap> Elements( last );
#! [ r1, r2, r3, r4, r5, r6, r7 ]
#! @EndExampleSession

# CONGRUENCES
# _____________________________________________________________________________

#! @Section Congruences

# Equivalence relations are implemented in GAP as general mappings from X to X.

#! <P/>Let $A$ be an algebra in a variety $V$. Then $\sim$ is a <Index>congruence</Index>**congruence**
#! on $A$ if it is an equivalence relation on $A$ such that 
#! for every operation $f$ of arrity $m$ in the signature of $V$,
#! we have $f(x_1,\dots,x_m)\sim f(y_1,\dots,y_m)$ whenever $x_1,\dots,x_m,y_1,\dots,y_m\in A$
#! satisfy $x_1\sim y_1$, $\dots$, $x_m\sim y_m$. If $V$ is the variety of all right quasigroups
#! (resp. quasigroups, loops), we speak of a <Index>right quasigroup congruence</Index>
#! **right quasigroup congruence** (resp. <Index>quasigroup congruence</Index>**quasigroup congruence**,
#! <Index>loop congruence</Index>**loop congruence**).

#! <P/>It turns out that an equivalence relation $\sim$ on a finite right quasigroup (resp. quasigroup, loop)
#! is a right quasigroup (resp. quasigroup, loop) congruence
#! iff for every $x,y,u\in Q$ with $x\sim y$ we have $xu\sim yu$ and $ux\sim uy$.
#! Therefore, an equivalence relation $\sim$ on a finite loop (quasigroup, right quasigroup) is a
#! loop (quasigroup, right quasigroup) congruence iff it is a groupoid congruence.

#! <P/>In &GAP;, equivalence relations on  $A$ are represented as functions $f:A\to A$,
#! where $a,b\in A$ are related iff $f(a)=b$. Since equivalence relations are in one-to-one
#! correspondence with partitions, the &GAP; function `EquivalenceRelationByPartition` is
#! particularly convenient, as illustrated by the following example:

#! @BeginLogSession
#! gap> G := SymmetricGroup( 3 );;
#! gap> C := EquivalenceRelationByPartition( G, [[(),(1,2,3),(1,3,2)],[(1,2),(1,3),(2,3)]] );
#! <equivalence relation on SymmetricGroup( [ 1 .. 3 ] ) >
#! gap> Source( C );
#! Sym( [ 1 .. 3 ] )
#! gap> EquivalenceClasses( C );
#! [ {()}, {(1,2)} ]
#! gap> Elements( last[1] );
#! [ (), (1,2,3), (1,3,2) ]
#! @EndLogSession

DeclareGlobalFunction( "RQ_IsAlgebraCongruence" ); # category, C, reportErrors

#! @BeginGroup
#! @GroupTitle Checking right quasigroup congruences

#! @Arguments C
DeclareOperation( "IsRightQuasigroupCongruence", [ IsEquivalenceRelation ] ); 

#! @Arguments C
DeclareOperation( "IsQuasigroupCongruence", [ IsEquivalenceRelation ] );

#! @Arguments C
#! @Returns `true` if <Arg>C</Arg> is a right quasigroup (resp. quasigroup, loop) congruence
#! on the right quasigroup (resp. quasigroup, loop) `Source( `<Arg>C</Arg>` )`, else returns `false`.
#! Note that `false` is returned when a stronger algebra congruence is tested
#! on a weaker algebra, for instance, if `IsLoopCongruence( `<Arg>C</Arg>` )` is tested with
#! `Source( `<Arg>C</Arg>` )` that is not a declared loop.
DeclareOperation( "IsLoopCongruence", [ IsEquivalenceRelation ] );

#! @BeginExampleSession
#! gap> Q := QuasigroupByFunction( [0..3], function(x,y) return (x-y) mod 4; end );;
#! gap> C := EquivalenceRelationByPartition( Q, [ [Q[0],Q[2]], [Q[1],Q[3]] ] );
#! <equivalence relation on <quasigroup of size 4 on 0, 1, 2, 3> >
#! gap> IsQuasigroupCongruence( C );
#! true
#! gap> D := EquivalenceRelationByPartition( Q, [ [Q[0],Q[1],Q[2]], [Q[3]] ] );;
#! gap> IsQuasigroupCongruence( D );
#! false
#! @EndExampleSession

#! @EndGroup

#! @BeginGroup
#! @GroupTitle Congruences generated by partition

# RQ_AlgebraCongruenceByPairs( Q, partition )
# returns the congruence on Q generated by partition
DeclareOperation( "RQ_AlgebraCongruenceByPartition", [ IsRightQuasigroup, IsList ] );

#! @Arguments Q, partition
#! @Returns the right quasigroup (quasigroup, loop) congruence of the right quasigroup
#! (quasigroup, loop) <Arg>Q</Arg> generated by <Arg>partition</Arg>, that is, the smallest
#! congruence `C` such that every element of <Arg>partition</Arg> is a subset of 
#! an equivalence class of `C`. Here, <Arg>partition</Arg> must be a list of disjoint
#! subsets of <Arg>Q</Arg> (whose union is not necessarily all of <Arg>Q</Arg>).
DeclareOperation( "RightQuasigroupCongruenceByPartition", [ IsRightQuasigroup, IsList ] );

#! @Arguments Q, partition
DeclareOperation( "QuasigroupCongruenceByPartition", [ IsQuasigroup, IsList ] );

#! @Arguments Q, partition
DeclareOperation( "LoopCongruenceByPartition", [ IsLoop, IsList ] );

#! @BeginExampleSession
#! gap> Q := QuasigroupByFunction( GF(27), \- );;
#! gap> C := QuasigroupCongruenceByPartition( Q, [ [ Q.1, Q.2, Q.3 ], [ Q.4, Q.5 ] ] );; # merge Q.1, Q.2, Q.3 and also Q.4, Q.5
#! gap> List( EquivalenceClasses( C ), Size );
#! [ 9, 9, 9 ]
#! gap> G := AsLoop( SymmetricGroup( 5 ) );;
#! gap> C := LoopCongruenceByPartition( G, [ [ G[()], G[(1,2,3)] ] ] );; # merge (), (1,2,3)
#! gap> List( EquivalenceClasses( C ), Size );
#! [ 60, 60 ]
#! @EndExampleSession

#! @EndGroup

#! @BeginGroup
#! @GroupTitle Congruences generated by pairs

# RQ_AlgebraCongruenceByPairs( Q, pairs )
# returns the congruence on Q generated by pairs
DeclareOperation( "RQ_AlgebraCongruenceByPairs", [ IsRightQuasigroup, IsList ] );

#! @Arguments Q, pairs
#! @Returns the right quasigroup (quasigroup, loop) congruence of the right quasigroup
#! (quasigroup, loop) <Arg>Q</Arg> generated by <Arg>pairs</Arg>, that is, the smallest
#! congruence that contains <Arg>pairs</Arg> as a subset. Here, <Arg>pairs</Arg> must be
#! a list of pairs of elements of <Arg>Q</Arg>.
DeclareOperation( "RightQuasigroupCongruenceByPairs", [ IsRightQuasigroup, IsList ] );

#! @Arguments Q, pairs
DeclareOperation( "QuasigroupCongruenceByPairs", [ IsQuasigroup, IsList ] );

#! @Arguments Q, pairs
DeclareOperation( "LoopCongruenceByPairs", [ IsLoop, IsList ] );

#! @BeginExampleSession
#! gap> Q := RightQuasigroupByFunction([0..7], function(x,y) return (x+2*y) mod 8; end );;
#! gap> C := RightQuasigroupCongruenceByPairs( Q, [ [ Q[0],Q[2] ] ] );; # merge 0, 2
#! gap> List( EquivalenceClasses( C ), Elements );
#! [ [ r0, r2, r4, r6 ], [ r1, r5 ], [ r3, r7 ] ]
#! gap> C := RightQuasigroupCongruenceByPairs( Q, [ [ Q[0],Q[2] ], [ Q[0], Q[1] ] ] );; # merge 0, 2 and also 0, 1
#! gap> List( EquivalenceClasses( C ), Elements );
#! [ [ r0, r1, r2, r3, r4, r5, r6, r7 ] ]
#! @EndExampleSession

#! @EndGroup

#! @BeginGroup
#! @GroupTitle All congruences

#! @Arguments Q
DeclareOperation( "AllRightQuasigroupCongruences", [ IsRightQuasigroup ] );

#! @Arguments Q
DeclareOperation( "AllQuasigroupCongruences", [ IsQuasigroup ] );

#! @Arguments Q
#! @Returns a list of all right quasigroup (quasigroup, loop) congruences  of a right quasigroup
#! (quasigroup, loop) <Arg>Q</Arg>. The congruences  are returned as &GAP; objects suitable
#! as arguments of `FactorRightQuasigroup` (`FactorQuasigroup`, `FactorLoop`).
#! @Description Note: For a right quasigroup <Arg>Q</Arg>, there is no method yet for the case when
#! `RightMultiplicationGroup( Q )` does not act transitively on <Arg>Q</Arg>.
DeclareOperation( "AllLoopCongruences", [ IsLoop ] );

#! @EndGroup

# NORMALITY AND SIMPLICITY
# _____________________________________________________________________________

#! @Section Normality and simplicity

#! <P/>A subloop $S$ of a loop $Q$ is <Index>normal subloop</Index>**normal** in $Q$ if
#! $Sx=xS$, $S(xy)=(Sx)y$ and $(xy)S = x(yS)$ for every $x,y\in Q$. 
#! It can be shown that a subset $S$ of $Q$ is a normal subloop of $Q$ iff there is
#! a loop congruence $\sim$ on $Q$ such that $S$ is the congruence class of $\sim$
#! containing the neutral element of $Q$. 

#! <P/>If `S` is a subloop of a loop `Q`, the function `IsNormal( Q, S )` returns
#! `true` if `S` is normal in `Q`, else it returns `false`.

InParentFOA( "IsNormal", IsLoop, IsLoop, DeclareProperty );  # already declared for IsGroup, IsGroup
# PROG:
# (a) This declares operations IsNormal, IsNormalInParent and IsNormalOp
# (b) IsNormal( Q, S ) calls IsNormalInParent( S ) is Q = Parent( S )
# (c) otherwise IsNormal( Q, S ) calls IsNormalOp( Q, S )
# (d) the following method is automatically created for IsNormalInParent: S -> IsNormalOp( Parent( S ), S )
# Consequently, if only a method for IsNormalOp is installed, it is always called, no matter what.

#! <P/>If `S` is a subset or a subloop of a loop `Q`, `NormalClosure( Q, S )` returns
#! the normal closure of `S` in `Q`, that is, the smallest normal
#! subloop of `Q` containing `S`.

# NormalClosure( Q, S )
InParentFOA( "NormalClosure", IsLoop, IsLoop, DeclareAttribute );

#! @Arguments Q
#! @Returns a list of all normal subloops of a loop <Arg>Q</Arg>. Normal subloops
#! correspond to blocks of the multiplication group of <Arg>Q</Arg> that contain the neutral element.
DeclareOperation( "AllNormalSubloops", [ IsLoop ] );

#! <P/>A right quasigroup $Q$ is <Index>simple right quasigroup</Index>**simple** if the only
#! congruences on $Q$ are the diagonal congruence $\{(x,x):x\in Q\}$ and the full congruence
#! $Q\times Q$. It is well known that a quasigroup (loop) $Q$ is
#! simple iff its multiplication group $\mathrm{Mlt}(Q)=\langle R_x,L_x:x\in Q\rangle$ acts primitively
#! on $Q$ (see Section <Ref Sect="Section_Mlt"/>).

#! <P/>Note that in the finite case, which is the only case supported by &RightQuasigroups;,
#! a loop $Q$ is simple as a loop (no nontrivial loop congruences)
#! iff it is simple as a quasigroup (no nontrivial quasigroup congruences) iff it is simple as a right 
#! quasigroup (no nontrivial right quasigroup congruences) iff it is simple as a groupoid (no nontrivial
#! groupoid congruences). 

#! @BeginGroup
#! @GroupTitle Testing right quasigroups for simplicity

#! @Arguments Q
#! @Returns `true` if <Arg>Q</Arg> is a simple right quasigroup (quasigroup, loop), else returns `false`.
#! The non-qualified function `IsSimple` is also supported.
DeclareOperation( "IsSimpleRightQuasigroup", [ IsRightQuasigroup ] );

#! @Arguments Q
DeclareOperation( "IsSimpleQuasigroup", [ IsQuasigroup ] );

#! @Arguments Q
DeclareOperation( "IsSimpleLoop", [ IsLoop ] );

#! @BeginExampleSession
#! gap> # right quasigroup example
#! gap> R := RightQuasigroupByCayleyTable( [[2,2,1,1],[3,1,2,2],[4,3,3,3],[1,4,4,4]] );; 
#! gap> RMlt := RightMultiplicationGroup( R );
#! Group([ (1,2,3,4), (1,2) ])
#! gap> AllRightQuasigroupCongruences( R );
#! [ <equivalence relation on <right quasigroup of size 4 on 1, 2, 3, 4> >, 
#!   <equivalence relation on <right quasigroup of size 4 on 1, 2, 3, 4> > ]
#! gap> IsSimpleRightQuasigroup( R ); # IsSimple( R ) is also supported
#! true
#! gap> # quasigroup example
#! gap> Q := QuasigroupByFunction( [0..3], function(x,y) return (x-y) mod 4; end );;
#! gap> congruences := AllQuasigroupCongruences( Q );;
#! gap> List( congruences, EquivalenceClasses );
#! [ [ {q0}, {q1}, {q2}, {q3} ], [ {q0}, {q1} ], [ {q0} ] ]
#! gap> List( EquivalenceClasses( congruences[ 2 ] ), Elements );
#! [ [ q0, q2 ], [ q1, q3 ] ]
#! gap> IsSimpleQuasigroup( Q ); # IsSimple( Q ) is also supported
#! false
#! gap> # loop example
#! gap> L := AsLoop( Group((1,2,3,4)) );;
#! gap> AllNormalSubloops( L );
#! [ <trivial group with 1 generator>, <associative loop of size 2>, 
#!   <associative loop of size 4> ]
#! gap> IsSimpleLoop( L ); # IsSimple( L ) is also supported
#! false
#! gap> S := Subloop(  L, [ (1,3)(2,4) ] );;
#! gap> IsNormal( L, S );
#! true
#! @EndExampleSession

#! @EndGroup

# FACTOR ALGEBRAS
# _____________________________________________________________________________

#! @Section Factor algebras

#! <P/>When $\sim$ is a congruence on $A$, then the factor algebra $A/\sim$ is well defined
#! on the equivalence classes $[x]$ of $\sim$ by setting $f([x_1],\dots,[x_m]) = [f(x_1,\dots,x_m)]$
#! for every operation $f$ of arity $m$ in the signature of the enveloping variety $V$
#! and every $x_1,\dots,x_m\in A$.

#! <P/>In case of right quasigroups and quasigroups, the factor construction based on congruences
#! is the standard way of defining factor alegbras. In case of loops,
#! the equivalence classes of $\sim$ are precisely the cosets of the normal subloop 
#! $S$, the equivalence class of the identity element. The congruence-based factor algebra
#! construction is then equivalent to the standard coset-based construction $Sx\cdot Sy = S(xy)$ from group theory.

DeclareOperation( "RQ_FactorAlgebra", [ IsEquivalenceRelation, IsRecord ] ); # C, style

#! @BeginGroup
#! @GroupTitle Constructing factor algebras

#! @Arguments C[, constructorStyle ]
DeclareOperation( "FactorRightQuasigroup", [ IsEquivalenceRelation ] ); 

#! @Arguments C[, constructorStyle ]
DeclareOperation( "FactorQuasigroup", [ IsEquivalenceRelation ] );

#! @Arguments C[, constructorStyle ]
#! @Returns the factor algebra of `Source( `<Arg>C</Arg>` )` modulo the right quasigroup (resp. quasigroup,
#! loop) congruence <Arg>C</Arg>. In case of loops we also allow arguments <Arg>Q</Arg> and <Arg>N</Arg>
#! instead of <Arg>C</Arg>, where <Arg>Q</Arg> is a loop and <Arg>N</Arg> is a normal subloop of <Arg>Q</Arg>.
#! See Section <Ref Sect="Section_OptionalArguments"/> for the optional argument `constructorStyle`.
#! @Description An effort is made for the factor algebra to inherit properties from the enveloping algebra.
#! For instance, if it is known that the enveloping algebra is commutative, the factor algebra will
#! have an attribute that signifies it is commutative.
DeclareOperation( "FactorLoop", [ IsEquivalenceRelation ] );

# /
# declared for [ IsRightQuasigroup, IsEquivalenceRelation ] and for [ IsLoop, IsLoop ]

#! <P/>We also support infix notation for factor algebras, that is, `Q/C` or `Q/N`. In that version:
#! <List>
#!      <Item>the enveloping algebra `Q` must always be given,</Item>
#!      <Item>the optional argument `constructorStyle` cannot be given,</Item>
#!      <Item>the resulting algebra will be index based iff `Q` is index based.</Item>
#! </List>

#! @BeginExampleSession
#! gap> Q := ProjectionRightQuasigroup( 6 );;
#! gap> C := EquivalenceRelationByPartition( Q, [[Q.1,Q.2],[Q.3,Q.4,Q.5],[Q.6]] );;
#! gap> [ IsRightQuasigroupCongruence( C ), IsQuasigroupCongruence( C ), IsLoopCongruence( C ) ];
#! [ true, false, false ]
#! gap> F := Q/C;
#! <associative quandle of size 3>
#! gap> Elements( F ); # the inner "r" comes from Q, the outer "r" from F.
#! [ r<object>, r<object>, r<object> ]
#! gap> H := FactorRightQuasigroup( C, ConstructorStyle( false, false ) ); # non-index based version is supported (but not for /)
#! <associative quandle of size 3>
#! gap> HasMultiplicationTable( H );
#! false
#! gap> H.1*H.2;
#! r<object>
#! gap> CayleyTable( H );   
#! [ [ {r1}, {r1}, {r1} ], [ {r3}, {r3}, {r3} ], [ {r6}, {r6}, {r6} ] ]
#! @EndExampleSession

#! <P/>See Section <Ref Sect="Section_Homomorphisms"/> for natural projections onto factor algebras.

#! @EndGroup

#! @Section An example of the factor construction: Paige loops

#! <P/>We conclude with a larger example, the construction of finite simple Moufang loops, so-called
#! Paige loops. These are obtained as the factor of the multiplicative set $S$ of
#! elements of norm one in the Zorn vector matrix algebra modulo the center of $S$.

#! @BeginExampleSession
#! gap> # auxiliary functions
#! gap> DotProduct := function( x, y ) return Sum( [1..Length(x)], i -> x[i]*y[i] ); end;;
#! gap> CrossProduct := function( x, y ) return [ x[2]*y[3]-x[3]*y[2], x[3]*y[1]-x[1]*y[3], x[1]*y[2]-x[2]*y[1] ]; end;;
#! gap> PaigeNorm := function( x ) return x[1]*x[8] - DotProduct( x{[2,3,4]},x{[5,6,7]} ); end;;
#! gap> PaigeMult := function( x, y )
#! >   local a, b, c, d;
#! >   a := x[1]*y[1] + DotProduct(x{[2,3,4]},y{[5,6,7]});
#! >   b := x[1]*y{[2,3,4]} + x{[2,3,4]}*y[8] - CrossProduct(x{[5,6,7]},y{[5,6,7]});
#! >   c := x{[5,6,7]}*y[1] + x[8]*y{[5,6,7]} + CrossProduct(x{[2,3,4]},y{[2,3,4]});
#! >   d := DotProduct(x{[5,6,7]},y{[2,3,4]})+x[8]*y[8];
#! >   return Concatenation( [a], b, c, [d] );
#! >   end;;
#! gap> # Paige loop over GF(2) (index based approach in characteristic 2)
#! gap> F := GF(2);;
#! gap> S := Filtered( F^8, x -> PaigeNorm( x ) = One( F ) );;
#! gap> P := LoopByFunction( S, PaigeMult, ConstructorStyle( true, true ) ); 
#! <loop of size 120>
#! gap> # general approach (not index based, any characteristic, using congruences)
#! gap> n := 3;; # any prime power works but it will be very slow
#! gap> F := GF(n);;
#! gap> S := Filtered( F^8, x -> PaigeNorm( x ) = One( F ) );;
#! gap> M := LoopByFunction( S, PaigeMult, ConstructorStyle( false, false ) );;
#! gap> C := EquivalenceRelationByPartition( M, Set( S, x -> Set( [ M[x], M[-x] ] ) ) );; # factoring out +/- one
#! gap> P := FactorLoop( C, ConstructorStyle( false, false ) ); # 2000 ms
#! <loop of size 1080>
#! gap> # another approach using normal subloop
#! gap> n := 3;; F := GF(n);; S := Filtered( F^8, x -> PaigeNorm( x ) = One( F ) );;
#! gap> M := LoopByFunction( S, PaigeMult, ConstructorStyle( false, false ) );;
#! gap> one := [ Z(n)^0, 0*Z(n), 0*Z(n), 0*Z(n), 0*Z(n), 0*Z(n), 0*Z(n), Z(n)^0 ];;
#! gap> N := Subloop( M, [-one] );;
#! gap> P := FactorLoop( M, N, ConstructorStyle( false, false ) ); # 2000 ms, it takes a while to find the neutral element
#! <loop of size 1080>
#! @EndExampleSession