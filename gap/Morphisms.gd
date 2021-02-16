# Morphisms.gd
# Homomorphisms, isomorphisms and automorphisms of right quasigroups
# =============================================================================

#! @Chapter Homomorphisms, isomorphisms and automorphisms


# HOMOMORPHISMS OF RIGHT QUASIGROUPS
# _____________________________________________________________________________

#! @Section Homomorphisms of right quasigroups

#! <P/>A mapping $f:(Q_1,\cdot)\to (Q_2,*)$ between (right) quasigroups is a **homomorphism**
#! <Index Subkey="of right quasigroups">homomorphis</Index> if $f(x)*f(y) = f(x\cdot y)$
#! for every $x,y\in Q_1$. The homomorphism $f$ then automatically preserves divisons.
#! If $f$ is also a bijection, it is an **isomorphism**<Index Subkey="of right quasigroups">isomorphism</Index>.

#! <P/>In &RightQuasigroups;, homomorphisms are represented as right quasigroup mappings.
#! See Chapter <Ref Chap="Chapter_Mappings"/> for creating right quasigroup mappings from
#! permutations and transformations.

#! @BeginGroup
#! @GroupTitle Testing homomorphisms

#! @Arguments f
#! @Returns `true` if the mapping <Arg>f</Arg> is a homomorphism of right quasigroups (quasigroups, loops),
#! else returns `false`. The function checks that the source and range of <Arg>f</Arg> are right quasigroups
#! (quasigroups, loops) and that <Arg>f</Arg> respects multiplication.
DeclareOperation( "IsRightQuasigroupHomomorphism", [ IsMapping ] );

#! @Arguments f
DeclareOperation( "IsQuasigroupHomomorphism", [ IsMapping ] );

#! @Arguments f
DeclareOperation( "IsLoopHomomorphism", [ IsMapping ] );

#! @EndGroup

#! @BeginGroup
#! @GroupTitle Testing isomorphisms

#! @Arguments f
#! @Returns `true` if the mapping <Arg>f</Arg> is an isomorphism of right quasigroups (quasigroups, loops),
#! else returns `false`. The function checks <Arg>f</Arg> is a bijective right quasigroup (quasigroup, loop) homomorphism.
#! @Description Mappings returned by `IsomorphismRightQuasigroups` are right quasigroup isomorphisms.
DeclareOperation( "IsRightQuasigroupIsomorphism", [ IsMapping ] );

#! @Arguments f
DeclareOperation( "IsQuasigroupIsomorphism", [ IsMapping ] );

#! @Arguments f
DeclareOperation( "IsLoopIsomorphism", [ IsMapping ] );

#! @BeginExampleSession
#! gap> A := QuasigroupByFunction( [0..7], function(x,y) return (x-y) mod 8; end );;
#! gap> B := QuasigroupByFunction( [0..3], function(x,y) return (x-y) mod 4; end );;
#! gap> SetQuasigroupElementsName(B, "b" );;
#! gap> f := function( x ) return B[ UnderlyingSetElm( x ) mod 4 ]; end;; 
#! gap> List( A, f );
#! [ b0, b1, b2, b3, b0, b1, b2, b3 ]
#! gap> m := MappingByFunction( A, B, f );;
#! gap> IsRightQuasigroupHomomorphism( m );
#! true
#! gap> IsRightQuasigroupIsomorphism( m );
#! false
#! gap> IsSurjective( m );
#! true
#! @EndExampleSession

#! @EndGroup

#! @BeginGroup
#! @GroupTitle Testing endomorphisms

#! @Arguments f
#! @Returns `true` if the mapping <Arg>f</Arg> is an endomorphism of a right quasigroup (quasigroup, loop),
#! else returns `false`. The function checks that <Arg>f</Arg> is a right quasigroup (quasigroup, loop)
#! homomorphism with identical source and range.
DeclareOperation( "IsRightQuasigroupEndomorphism", [ IsMapping ] );

#! @Arguments f
DeclareOperation( "IsQuasigroupEndomorphism", [ IsMapping ] );

#! @Arguments f
DeclareOperation( "IsLoopEndomorphism", [ IsMapping ] );

#! @EndGroup

#! @BeginGroup
#! @GroupTitle Testing automorphisms

#! @Arguments f
#! @Returns `true` if the mapping <Arg>f</Arg> is an automorphism of a right quasigroup (quasigroup, loop),
#! else returns `false`. The function checks that <Arg>f</Arg> is a bijective right quasigroup
#! (quasigroup, loop) endomorphism.
#! @Description Note that elements of automorphism groups of right quasigroups are not
#! right quasigroup automorphisms in this sense and must be converted via `AsRightQuasigroupMapping`.
DeclareOperation( "IsRightQuasigroupAutomorphism", [ IsMapping ] );

#! @Arguments f
DeclareOperation( "IsQuasigroupAutomorphism", [ IsMapping ] );

#! @Arguments f
DeclareOperation( "IsLoopAutomorphism", [ IsMapping ] );

#! @BeginExampleSession
#! gap> Q := AutomorphicLoop( 15, 1 );;
#! gap> f := LeftInnerMapping( Q, Q.6, Q.2 );
#! 9,8,7,10)(11,15,12,13,14)
#! gap> m := AsRightQuasigroupMapping( Q, f );;
#! MappingByFunction( <automorphic loop 15/1>, <automorphic loop 15/1>, function( x ) ... end )
#! gap> IsRightQuasigroupAutomorphism( m );
#! true
#! @EndExampleSession

#! @EndGroup

#! @BeginGroup
#! @GroupTitle Homomorphisms by images

#! @Arguments  Q1, Q2[[, gens], imgs]
#! @Returns the homomorphism from right quasigroup (quasigroup, loop) <Arg>Q1</Arg>
#! to right quasigroup (quasigroup, loop) <Arg>Q2</Arg>
#! determined by the images <Arg>imgs</Arg> on the generators <Arg>gens</Arg> of <Arg>Q1</Arg>.
#! If the list <Arg>gens</Arg> is omitted, `GeneratorsOfRightQuasigroup(`<Arg>Q1</Arg>`)` is used instead.
#! If the list <Arg>imgs</Arg> is omitted, `GeneratorsOfRightQuasigroup(`<Arg>Q2</Arg>`)` is used instead.
#! If <Arg>gens</Arg>, <Arg>imgs</Arg> are not of the same length, error is returned.
#! If <Arg>gens</Arg> does not generate <Arg>Q1</Arg>, `fail` is returned.
#! If the arguments do not define a homomorphism, `fail` is returned.
DeclareOperation( "RightQuasigroupHomomorphismByImages", [ IsRightQuasigroup, IsRightQuasigroup ] );

#! @Arguments  Q1, Q2[[, gens], imgs]
DeclareOperation( "QuasigroupHomomorphismByImages", [ IsQuasigroup, IsQuasigroup ] );

#! @Arguments  Q1, Q2[[, gens], imgs]
DeclareOperation( "LoopHomomorphismByImages", [ IsLoop, IsLoop ] );

#! @BeginExampleSession
#! gap> Q := CheinLoop( SymmetricGroup( 3 ) );;
#! gap> gens := SmallGeneratingSet( Q ); # also sets GeneratorsOfMagma in this case
#! [ l[ 0, (2,3) ], l[ 0, (1,2,3) ], l[ 1, () ] ]
#! gap> S := AsLoop( Group((1,2)) );;
#! gap> f := LoopHomomorphismByImages( Q, S, gens, [ S[()],S[()],S[(1,2)] ] );
#! MappingByFunction( <Moufang loop of size 12>, <associative loop of size 2>, function( x ) ... end )
#! gap> Q[ [1,(1,2,3)] ]^f;
#! l(1,2)
#! @EndExampleSession

#! @EndGroup

#! @Arguments f
#! @Returns the kernel relation of the mapping <Arg>f</Arg>, that is, the
#! equivalence relation `E` on `Source( `<Arg>f</Arg>` )` such that `xEy` iff `x^f = y^f`.
DeclareOperation( "KernelRelationOfMapping", [ IsMapping ] );

#! @BeginExampleSession
#! gap> G := SymmetricGroup( 3 );;
#! gap> f := MappingByFunction( G, Domain([1,-1]), SignPerm );;
#! gap> ker := KernelRelationOfMapping( f );
#! <equivalence relation on SymmetricGroup( [ 1 .. 3 ] ) >
#! gap> EquivalenceClasses( ker );
#! [ {(2,3)}, {()} ]
#! @EndExampleSession

#! @BeginGroup
#! @GroupTitle Kernel of loop homomorphisms

#! @Arguments f
#! @Returns the kernel of the loop homomorphism <Arg>f</Arg>, that is, the subloop
#! of the loop `Source( `<Arg>f</Arg>` )` consisting of all elements `x` such that 
#! `x^f` is the identity element of the loop `Range( `<Arg>f</Arg>` )`.
#! The `NC` version does not check whether <Arg>f</Arg> is a loop homomorphism. 
DeclareOperation( "KernelOfLoopHomomorphism", [ IsMapping ] );

#! @Arguments f
DeclareOperation( "KernelOfLoopHomomorphismNC", [ IsMapping ] );

#! @EndGroup

#! @BeginGroup
#! @GroupTitle Natural homomorphism by congruence

#! @Arguments C
#! @Returns the natural projection corresponding to the right quasigroup congruence <Arg>C</Arg>.
#! With `Q = Source( `<Arg>C</Arg>` )`, it returns the endomorphism `f` from `Q` to `Q/`<Arg>C</Arg>
#! such that `x^f` is the element of `Q/`<Arg>C</Arg> containing `x` (as a congruence class).
#! The `NC` version does not check whether <Arg>C</Arg> is a congruence.
DeclareOperation( "NaturalHomomorphismByCongruence", [ IsEquivalenceRelation ] );

#! @Arguments C
DeclareOperation( "NaturalHomomorphismByCongruenceNC", [ IsEquivalenceRelation ] );

#! @BeginExampleSession
#! gap> Q := QuasigroupByFunction( [0..3], function(x,y) return (x-y) mod 4; end );;
#! gap> C := EquivalenceRelationByPartition( Q, [ [Q[0],Q[2]], [Q[1],Q[3]] ] );
#! gap> f := NaturalHomomorphismByCongruence( C );
#! MappingByFunction( <quasigroup of size 4>, <quasigroup of size 2>, function( x ) ... end )
#! gap> UnderlyingSet( Range( f ) );
#! [ {q0}, {q1} ]
#! @EndExampleSession

#! @EndGroup

#! @BeginGroup
#! @GroupTitle Natural homomorphism by normal subloop

#! @Arguments Q, S
#! @Returns the natural projection from the loop <Arg>Q</Arg> to the factor loop <Arg>Q</Arg>`/`<Arg>S</Arg>.
#! The `NC` version does not check whether <Arg>S</Arg> is a normal subloop of <Arg>Q</Arg>.
DeclareOperation( "NaturalHomomorphismByNormalSubloop", [ IsLoop, IsLoop ] );

#! @Arguments Q, S
DeclareOperation( "NaturalHomomorphismByNormalSubloopNC", [ IsLoop, IsLoop ] );

#! @BeginExampleSession
#! gap> Q := MoufangLoop( 12, 1 );
#! <Moufang loop 12/1>
#! gap> f := NaturalHomomorphismByNormalSubloop( Q, Subloop(Q, [Q.2,Q.3] ) );
#! MappingByFunction( <Moufang loop 12/1>, <Moufang loop of size 2>, function( x ) ... end )
#! @EndExampleSession

#! @EndGroup

# ISOMORPHISMS OF RIGHT QUASIGROUPS
# _____________________________________________________________________________

#! @Section Isomorphisms of right quasigroups

#! <P/>If $f:(Q_1,\cdot)\to (Q_2,*)$ is an isomorphism, then $x*y = f(f^{-1}(x)\cdot f^{-1}(y))$ for
#! all $x,y\in Q_1$, and $(Q_2,*)$ is called an **isomorphic copy**<Index>isomorphic copy</Index>
#! or an **isomorph**<Index>isomorph</Index> of $(Q_1,\cdot)$ via $f$.

#! In &RightQuasigroups;, all isomorphisms between two right quasigroups of size $n$ are returned
#! as right quasigroup mappings, while all automorphisms of a right quasigroup are return as
#! parent permutations, cf. Section <Ref Sect="Section_MappingsIntro"/>.

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
#! else returns `fail`. If an isomorphism from `Q1` to `Q2` exists, it is returned as a right quasigroup mapping.
#! See Chapter <Ref Chap="Chapter_Mappings"/> for conversion options of the returned mapping.
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
#! gap> f := IsomorphismLoops( Q1, Q2 ); 
#! MappingByFunction( <Moufang loop of size 12>, <Moufang loop of size 12>, function( x ) ... end )
#! gap> AsCanonicalTransformation( f );
#! Transformation( [ 5, 1, 3, 6, 8, 2, 4, 11, 10, 12, 9, 7 ] ) # transformation on [1..12]
#! gap> PermutationOfImage( last ); # as permutation
#! (1,5,8,11,9,10,12,7,4,6,2)
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

# AUTOMORPHISM GROUPS OF RIGHT QUASIGROUPS
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
#! as a subgroup of the symmetric group on `[1..Size(Parent(Q))]`.
DeclareAttribute( "AutomorphismGroup", IsRightQuasigroup );
# NOTE: The attribute AutomorphismGroup is already declared for groups.

#! @BeginExampleSession
#! gap> Q := MoufangLoop( 12, 1 );
#! <Moufang loop 12/1>
#! gap> AutomorphismGroup( Q );
#! Group([ (2,8)(4,12)(6,10), (2,9,10,4,11,8)(3,5)(6,7,12) ])
#! gap> S := Subloop( Q, [Q.3] );; ParentInd( S );
#! [ 1, 3, 5 ]
#! gap> AutomorphismGroup( S ); # consists of parent permutations
#! Group([ (3,5) ])
#! @EndExampleSession