# Isomorphisms.gd
# Isomorphisms and isotopisms of right quasigroups
# =============================================================================

#! @Chapter Isomorphisms and isotopisms

# ISOMORPHISMS
# _____________________________________________________________________________

#! @Section Isomorphisms of right quasigroups

#! <P/>A mapping $f:(Q_1,\cdot)\to (Q_2,*)$ between (right) quasigroups is a **homomorphism**
#! <Index Subkey="of right quasigroups">homomorphis</Index> if $f(x)*f(y) = f(x\cdot y)$
#! for every $x,y\in Q_1$. The homomorphism $f$ then automatically preserves divisons.
#! If $f$ is also a bijection, it is an **isomorphism**<Index Subkey="of right quasigroups">isomorphism</Index>.

#! <P/>If $f:(Q_1,\cdot)\to (Q_2,*)$ is an isomorphism, then $x*y = f(f^{-1}(x)\cdot f^{-1}(y))$ for
#! all $x,y\in Q_1$, and $(Q_2,*)$ is called an **isomorphic copy**<Index>isomorphic copy</Index>
#! or an **isomorph**<Index>isomorph</Index> of $(Q_1,\cdot)$ via $f$.

#! In &RightQuasigroups;, all isomorphisms between two right quasigroups of size $n$ are returned
#! as permutations of $[1..n]$, that is, an analog of canonical permutations, cf. Section <Ref Sect="Section_ParentPerms"/>.

#! @BeginGroup
#! @GroupTitle Isomorphs

#! @Arguments Q, f[, constructorStyle]
#! @Returns an isomorphic copy of the right quasigroups `Q` via the permutation `f`.
#! If `Q` has size `n`, the permutation `f` must be a permutation on `[1..n]`.
#! The resulting right quasigroup will have the same underlying set as `Q` and will be
#! index based iff `Q` is index based (unless the optional argument `constructorStyle`
#! dictates otherwise). An effort is made for the copy to inherit properties from `Q`.
DeclareOperation( "IsomorphicCopyByPerm", [ IsRightQuasigroup, IsPerm ] );

#! @Arguments Q, f[, constructorStyle]
DeclareOperation( "RightQuasigroupIsomorph", [ IsRightQuasigroup, IsPerm ] );

#! @Arguments Q, f[, constructorStyle]
DeclareOperation( "QuasigroupIsomorph", [ IsQuasigroup, IsPerm ] );

#! @Arguments Q, f[, constructorStyle]
DeclareOperation( "LoopIsomorph", [ IsLoop, IsPerm ] );

#! @EndGroup

#! <P/>To decide if two right quasigroups are isomorphic, &RightQuasigroups; first
#! calculates some isomorphism invariants and a partition invariant under isomorphisms,
#! and then determines if there is an isomorphisms that respects the partition.

#! @Arguments Q
#! @Returns a data structure suitable for isomorphism searches from a right quasigroup `Q`.
DeclareOperation( "IsomorphismDiscriminator", [ IsRightQuasigroup ] );

# Auxiliary function (Q,dis)
# Given a right quasigroup <Q> with isomorphism discriminator <dis>,
# it returns a list of indices of generators of <Q> deemed best for
# an isomorphism filter. It mimics the function SmallGeneratingSet, but it
# considers the elements in order determined by block size of the disciminator.
DeclareOperation( "RQ_EfficientGenerators", [ IsRightQuasigroup, IsList ] );

#! @Arguments dis1, dis2
#! @Returns `true` if the two isomorphism discriminators `dis1`, `dis2` (each returned by `IsomorphismDiscriminator`)
#! are equal as discriminators, else returns `false`. If `false` is returned, the two
#! disriminators correspond to right quasigroups that are not isomorphic. If `true` is returned,
#! the corresponding right quasigroups might be isomorphic.
DeclareOperation( "AreEqualIsomorphismDiscriminators", [ IsList, IsList ] );

# EXTENDING MAPPINGS (AUXILIARY)
# We identity the map f: A --> B  with the triple [ m, a, b ], 
# where a is a subset of A, b[ i ] is the image of a[ i ], and m[ i ] > 0
# if and only if i is in a.

# Auxiliary function (f,t1,t2)
# <t1>, <t2> are multiplication tables of quasigroups, <f> is a partial map
# from a subset of elements of <t1> to a subset of elements of <t2>. 
# This function attempts to extend <f> into a homomorphism of right quasigroups by 
# extending the source of <f> into (the smallest possible) subrightqusigroup of <t1>.
DeclareOperation( "RQ_ExtendHomomorphismByClosingSource", [ IsList, IsRectangularTable, IsRectangularTable ] );

# Auxiliary function ( S, x )
# input: list of lists <S>, element <x>
# returns: smallest i such that x in S[i]; or fail.
DeclareOperation( "RQ_SublistPosition", [ IsList, IsObject ] );

# Auxiliary function ( f, Q1, gen1, dis1, Q2, dis2 ) 
# Given a partial map <f> from a right quasigroup <Q1> to a right quasigroup <Q2>,
# it attempts to extend <f> into an isomorphism betweem <Q1> and <Q2>.
# <gen1>, <dis1> and <dis2> are precalculated and stand for:
# efficient generators of <Q1>, discriminator of <Q1>, efficient generators of <Q2>, respectively.
DeclareOperation( "RQ_ExtendIsomorphism", [ IsList, IsRectangularTable, IsList, IsList, IsRectangularTable, IsList ] );

# Auxiliary function (category, Q1, gen1, dis1, Q2, dis2 )
# Given a right quasigroup <Q1>, its efficient generators <gen1>, the 
# disciminator <dis1> of <Q1>, and another right quasigroup <Q2> with discriminator
# <dis2>, it returns an isomorphism from <Q1> onto <Q2>, or it fails.
DeclareOperation( "RQ_IsomorphismAlgebrasWithPrecalculatedData",  [ IsObject, IsRightQuasigroup, IsList, IsList, IsRightQuasigroup, IsList ] );

#! @BeginGroup
#! @GroupTitle IsomorphismRightQuasigroups, IsomorphismQuasigroups, IsomorphismLoops

# Auxiliary function (category, Q1, Q2 )
# returns an isomorphism from <Q1> onto <Q2>, or fail.
DeclareOperation( "RQ_IsomorphismAlgebras", [ IsObject, IsRightQuasigroup, IsRightQuasigroup ] );

#! @Arguments Q1, Q2
#! @Returns an isomorphism between the right quasigroups (quasigroups, loops) `Q1` and `Q2`, if one exists,
#! else returns `fail`. If an isomorphism from `Q1` to `Q2` exists, it is returned as a permutation of $[1..n]$,
#! where $n$ is the size of `Q1` (and hence also the size of `Q2`).
DeclareOperation( "IsomorphismRightQuasigroups", [ IsRightQuasigroup, IsRightQuasigroup ] );

#! @Arguments Q1, Q2
DeclareOperation( "IsomorphismQuasigroups", [ IsQuasigroup, IsQuasigroup ] );

#! @Arguments Q1, Q2
DeclareOperation( "IsomorphismLoops", [ IsLoop, IsLoop ] );

#! @BeginExampleSession
#! gap> Q1 := CheinLoop( SymmetricGroup( 3 ) );
#! <Moufang loop of size 12>
#! gap> Q2 := IsomorphicCopyByPerm( Q1, (1,5,6)(4,8) ); # properties are inherited
#! <Moufang loop of size 12>
#! gap> IsomorphismLoops( Q1, Q2 ); # different isomorphism might be found
#! (1,5,10,8,12,11,4,6,3,2)
#! @EndExampleSession

#! @EndGroup

#! @BeginGroup
#! @GroupTitle Right quasigroups up to isomorphism

# auxiliary function ( category, ls )
# given a list <ls> of algebras of type <category>, returns a sublist of <ls> with algebras up to isomorphism
DeclareOperation( "RQ_AlgebrasUpToIsomorphism", [ IsObject, IsList ] );

#! @Arguments ls
#! @Returns a sublist of `ls` consisting of all right quasigroups (quasigroups, loops) in `ls` up to isomorphism.
DeclareOperation( "RightQuasigroupsUpToIsomorphism", [ IsList ] ); # all must be declared right quasigroups, not quasigroups or loops

#! @Arguments ls
DeclareOperation( "QuasigroupsUpToIsomorphism", [ IsList ] );

#! @Arguments ls
DeclareOperation( "LoopsUpToIsomorphism", [ IsList ] );

#! @BeginExampleSession
#! gap> Q1 := CheinLoop( SymmetricGroup( 3 ) );;
#! gap> Q2 := LoopIsomorph( Q1, (1,2) );;
#! gap> Q3 := AsLoop( CyclicGroup( 12 ) );;
#! gap> lps := LoopsUpToIsomorphism( [ Q1, Q2, Q3 ] );
#! [ <Moufang loop of size 12>, <associative loop of size 12> ]
#! gap> lps[1] = Q1; lps[2] = Q3;
#! true
#! true
#! @EndExampleSession

#! @EndGroup

# AUTOMORPHISM GROUPS
# _____________________________________________________________________________

#! @Section Automorphism groups of right quasigroups

# auxiliary function
# Arguments list S, right quasigroup Q, list GenQ, list DisQ
# Returns
#  Given a quasigroup <Q>, its subset <S>, the efficient generators <GenQ> of <Q>
#  and automorphism-invariant subsets <DisQ> of <Q>, it returns all
#  automorphisms of <Q> fixing the set <S> pointwise.
DeclareOperation( "RQ_AutomorphismsFixingSet", [ IsList, IsRightQuasigroup, IsList, IsList ] );

#! @Arguments Q
#! @Returns the automorphism group of the right quasigroup `Q`
#! as a subgroup of the symmetric group on `[1..Size(Q)]`.
DeclareAttribute( "AutomorphismGroup", IsRightQuasigroup );
# NOTE: The attribute AutomorphismGroup is already declared for groups.

#! @BeginExampleSession
#! gap> Q := CheinLoop( SymmetricGroup( 3 ) );;
#! gap> AutomorphismGroup( Q );
#! Group([ (2,3,4)(5,6,10,11,12,8)(7,9), (2,12,10,6,8,4)(3,11,5) ])
#! gap> Size( last );
#! 108
#! @EndExampleSession

# ISOTOPISMS
# _____________________________________________________________________________

#! @Section Isotopisms of right quasigroups

#! <P/>A triple of mappings $f,g,h:(Q_1,\cdot)\to (Q_2,*)$ between (right) quasigroups is an **homotopism**
#! <Index Subkey="of right quasigroups">homotopism</Index> if $f(x)*g(y) = h(x\cdot y)$
#! for every $x,y\in Q_1$. If $f$, $g$, $h$ are also bijection, the triple is
#! an **isotopism**<Index Subkey="of right quasigroups">isotopism</Index>.

#! <P/>If $f,g,h:(Q_1,\cdot)\to (Q_2,*)$ is an isotopism, then $x*y = h(f^{-1}(x)\cdot g^{-1}(y))$ for
#! all $x,y\in Q$, and $(Q_2,*)$ is called an **isotope**<Index>isotope</Index> of $(Q_1,\cdot)$ via $f$, $g$, $h$.
#! The isotope $(Q,\circ)$ is a (right) quasigroup iff $(Q,\cdot)$ is a (right) quasigroup, but an isotope
#! of a loop might not be a loop. 

# REVISIT: Add principal isotopisms (i.e., gamma=1)?

#! <P/>Given a quasigroup $(Q,\cdot,/,\backslash)$ and $a,b\in Q$, the <Index>principal loop isotope</Index>
#! **principal loop isotope** via $a,b$ is defined as $(Q,\circ)$, where $x\circ y = (x/a)\cdot(b\backslash y)$.
#! The principal loop isotope is automatically a loop with neutral element $b\cdot a$.

#! <P/>Every isotopism of loops can be written as the composition of a principal loop isotopism and an isomorphism.

# auxiliary function (category, Q, f, g, h, constructorStyle)
DeclareOperation( "RQ_AlgebraIsotopeByPerms", [ IsObject, IsRightQuasigroup, IsPerm, IsPerm, IsPerm, IsRecord ] );

#! @Arguments Q, f, g, h[, constructorStyle ]
#! @Returns a right quasigroup that is an isotope of the right quasigroup `Q` via the permutations `f`, `g`, `h`.
#! The permutations must belong to the symmetric group `[1..Size(Q)]`.
#! The resulting right quasigroup will have the same underlying set as `Q` and will be index based
#! iff `Q` is index based (unless the optional argument `constructorStyle` dictates otherwise).
#! @Description We do not provide a function `IsotopicCopyByPerms` (in analogy to `IsotopicCopyByPerm`)
#! since the category of the resulting algebra is not necessarily preserved.
DeclareOperation( "RightQuasigroupIsotope", [ IsRightQuasigroup, IsPerm, IsPerm, IsPerm ] );

#! @Arguments Q, f, g, h[, constructorStyle ]
#! @Returns a quasigroup that is an isotope of the quasigroup `Q` via the permutations `f`, `g`, `h`.
#! The permutations must belong to the symmetric group `[1..Size(Q)]`.
#! The resulting quasigroup will have the same underlying set as `Q` and will be index based
#! iff `Q` is index based (unless the optional argument `constructorStyle` dictates otherwise).
DeclareOperation( "QuasigroupIsotope", [ IsQuasigroup, IsPerm, IsPerm, IsPerm ] );

#! @Arguments Q, f, g, h[, constructorStyle ]
#! @Returns a loop that is an isotope of the quasigroup `Q` via the permutations `f`, `g`, `h`.
#! The permutations must belong to the symmetric group `[1..Size(Q)]`.
#! The resulting loop will have the same underlying set as `Q` and will be index based
#! iff `Q` is index based (unless the optional argument `constructorStyle` dictates otherwise).
#! If the arguments `Q`, `f`, `g`, `h` do not give a rise to a loop (but merely a quasigroup)
#! and if the arguments are checked, an error is generated.
DeclareOperation( "LoopIsotope", [ IsQuasigroup, IsPerm, IsPerm, IsPerm ] );

#! @Arguments Q, a, b
#! @Returns the loop isotope of a quasigroup <Arg>Q</Arg> via its elements <Arg>a</Arg>, <Arg>b</Arg>.
#! The resulting loop will have the same underlying set as <Arg>Q</Arg>,
#! will have neutral element <Arg>b</Arg>`*`<Arg>a</Arg>, 
#! and will be index based iff <Arg>Q</Arg> is index based.
DeclareOperation( "PrincipalLoopIsotope", [ IsQuasigroup, IsQuasigroupElement, IsQuasigroupElement ] );

#! @BeginGroup
#! @GroupTitle IsotopismRightQuasigroups, IsotopismQuasigroups, IsotopismLoops

# Auxiliary function (category, Q1, Q2 )
# returns an isotopism from <Q1> onto <Q2>, or fail.
DeclareOperation( "RQ_IsotopismAlgebras", [ IsObject, IsRightQuasigroup, IsRightQuasigroup ] );

#! @Arguments Q1, Q2
#! @Returns an isotopism from the right quasigroup (quasigroup, loop) `Q1` onto
#! the right quasigroup (quasigroup, loop) `Q2`, if it exists, else returns `fail`.
#! The isotopism is returned as a triple of permutations on $[1..n]$, where $n$
#! is the size of `Q1` (and hence also the size of `Q2`).
#! @Description NOTE: THERE IS NO METHOD FOR RIGHT QUASIGROUPS AND QUASIGROUPS YET.
DeclareOperation( "IsotopismRightQuasigroups", [ IsRightQuasigroup, IsRightQuasigroup ] );

#! @Arguments Q1, Q2
DeclareOperation( "IsotopismQuasigroups", [ IsQuasigroup, IsQuasigroup ] );

#! @Arguments Q1, Q2
DeclareOperation( "IsotopismLoops", [ IsLoop, IsLoop ] );

#! @BeginExampleSession
#! gap> t := [ [1,2,3,4,5,6], [2,1,6,5,3,4], [3,4,5,2,6,1], [4,6,1,3,2,5], [5,3,4,6,1,2], [6,5,2,1,4,3] ];;
#! gap> Q1 := LoopByCayleyTable( t );;
#! gap> RightQuasigroupIsotope( Q1, (1,2), (), (3,4) );
#! <right quasigroup of size 6>
#! gap> QuasigroupIsotope( Q1, (1,2), (), (3,4) ); # LoopIsotope with arguments checked will return an error 
#! <quasigroup or size 6>
#! gap> Q2 := LoopIsomorph( Q1, (3,4) );;
#! gap> Q3 := PrincipalLoopIsotope( Q2, Q2.5, Q2.6 );; # a loop isotopic to Q1
#! gap> IsomorphismLoops( Q1, Q2 );
#! (3,4)
#! gap> IsomorphismLoops( Q1, Q3 );
#! fail
#! gap> IsotopismLoops( Q1, Q3 );
#! [ (2,4,6,3), (1,3,4)(2,5,6), (2,4,6,3) ]
#! @EndExampleSession

#! @EndGroup

#! @BeginGroup
#! @GroupTitle Right quasigroups up to isotopism

# REVISIT: Implement methods for right quasigrops and quasigroups

# auxiliary function ( category, ls )
# given a list <ls> of algebras of type <category>, returns a sublist of <ls> with algebras up to isotopism
DeclareOperation( "RQ_AlgebrasUpToIsotopism", [ IsObject, IsList ] );

#! @Arguments ls
#! @Returns a sublist of `ls` consisting of all right quasigroups (quasigroups, loops) in `ls` up to isotopism.
#! @Description NOTE: THERE IS NO METHOD FOR RIGHT QUASIGROUPS AND QUASIGROUPS YET.
DeclareOperation( "RightQuasigroupsUpToIsotopism", [ IsList ] );

#! @Arguments ls
DeclareOperation( "QuasigroupsUpToIsotopism", [ IsList ] );

#! @Arguments ls
DeclareOperation( "LoopsUpToIsotopism", [ IsList ] );

#! @EndGroup

# REVISIT: Add IsIsotopicToGroup function?