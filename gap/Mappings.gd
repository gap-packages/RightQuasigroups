# Mappings.gd
# Mappings, tranformations and permutations of right quasigroups
# =============================================================================

#! @Chapter Mappings, transformations and permutations of right quasigroups

# MAPPINGS, TRANSFORMATIONS AND PERMUTATIONS
# _____________________________________________________________________________

#! @Section Mappings, transformations and permutations

#! <P/>Given right quasigroups `Q1` and `Q2`, a mappings from `Q1` to `Q2` might be represented in three ways:
#! <List>
#! <Item>as a &GAP; mapping with source `Q1` and range `Q2` (used for mappings between two distinct right quasigroups,
#! for instance for homomorphisms),</Item>
#! <Item>as transformations (used as numerical analogs of &GAP; mappings,
#! for instance for left translations of right quasigroups),</Item>
#! <Item>as permutations (used for bijective mappings when `Q1 = Q2`, for instance for elements of
#! the right multiplication groups, automorphisms, etc.).</Item>
#! </List>

#! <P/>For a fixed right quasigroup `Q`, there are two kinds of permutations:
#! <List>
#! <Item>**Canonical permutations**<Index Subkey="canonical">permutation</Index>,
#! whose indexing is based on the position of elements among elements of `Q`. More precisely,
#! a permutation `f` is a canonical permutation on `Q` if it restricts to a permutation 
#! on `[1..Size(Q)]`. (Note that it is not required for `f` to fix all points outside of `[1..Size(Q)]`,
#! not even to fix all points outside of `[1..Size(Parent(Q))]`.)</Item>
#! <Item>**Parent permutations**<Index Subkey="parent">permutation</Index>, 
#! whose indexing is based on the parent indices of elements of `Q`. More precisely,
#! a permutation `f` is a parent permutation of `Q` if it restricts to a permutation of `ParentInd( Q )`.
#! (Note that it is not required for `f` to fix all points outside of `ParentInd( Q )`.)
#! </Item>
#! </List>
#! Note that a permutation does not keep track of `Q`, of course. The right quasigroup `Q` must therefore
#! be provided in order to interpret a given permutation as a bijection from `Q` to `Q`.

#! <P/>Similarly, for fixed right quasigroups `Q1`, `Q2`, there are two kinds of transformations:
#! <List>
#! <Item>**Canonical transformations**<Index Subkey="canonical">transformtation</Index>,
#! whose indexing is based on the position of elements among elements of `Q1` and `Q2`. More precisely,
#! a transformation `t` is a canonical transformation from `Q1` to `Q2` if
#! for every `i` in `[1..Size(Q1)]` we have `i^t` in `[1..Size(Q2)]`.</Item>
#! <Item>**Parent transformations**<Index Subkey="parent">transformation</Index>, 
#! whose indexing is based on the parent indices of elements of `Q1` and `Q2`. More precisely,
#! a transformation `t` is a parent transformation from `Q1` to `Q2` if
#! for every `i` in `ParentInt( Q1 )` we have `i^t` in `ParentInd( Q2 )`.</Item>
#! </List>
#! Note that transformations do not keep track of `Q1` nor `Q2`. The right quasigroups `Q1` and `Q2` must therefore
#! be provided in order to interpret a given transformation as a mapping from `Q1` to `Q2`.

#! <P/>Most permutations encountered in &RightQuasigroups; are parent permutations, such as right translations,
#! right inner mappings, automorphisms, etc. However, canonical permutations
#! are useful, too, for instance while working with multiplication tables.

#! <P/>Any permutation can be made into a transformation by the &GAP; function `AsTransformation`.
#! This can be used to convert canonical permutations to canonical transformations.

#! <P/>Conversely, if a transformation happens to permute the points in its image,
#! the corresponding permutation is returned by the &GAP; function `PermutationOfImage`.
#! This can be used to convert bijective canonical transformations into canonical permutations.

#! <P/>The default permutation action in &RightQuasigroups; treats permutations as parent permutations. That is,
#! if `Q` is a right quasigroup and `f` is a permutation then `(Q.i)^f` returns `Q.(i^f)`.

# RIGHT QUASIGROUP MAPPINGS
# _____________________________________________________________________________

#! @Section Right quasigroup mapppings

#! <P/>Mappings between right quasigroups are &GAP; mappings and the standard methods for mappings therefore apply.
#! The following example creates the squaring mapping on a loop `Q` and calculates the image of an element of `Q`.

#! @BeginExampleSession
#! gap> Q := MoufangLoop( 12, 1 );;
#! gap> m := MappingByFunction( Q, Q, x -> x*x );
#! MappingByFunction( <Moufang loop 12/1>, <Moufang loop 12/1>, function( x ) ... end )
#! gap> [ Source( m ) = Q, Range( m ) = Q ];
#! [ true, true ]
#! gap> Q.2*Q.2 = Q.2^m;
#! true
#! @EndExampleSession

#! @BeginGroup
#! @GroupTitle IsRightQuasigroupMapping, IsQuasigroupMapping, IsLoopMapping

# RQ_IsAlgebraMapping( category, m )
DeclareOperation( "RQ_IsAlgebraMapping", [ IsObject, IsMapping ] );

#! @Arguments m
#! @Returns `true` if <Arg>m</Arg> is a right quasigroup (quasigroup, loop) mapping,
#! that is, a &GAP; mapping in which both the source and the range are right quasigroups (quasigroups, loops).
DeclareOperation( "IsRightQuasigroupMapping", [ IsMapping ] );

#! @Arguments m
DeclareOperation( "IsQuasigroupMapping", [ IsMapping ] );

#! @Arguments m
DeclareOperation( "IsLoopMapping", [ IsMapping ] );

#! @EndGroup

#! @Arguments Q1, Q2, f[, isCanonical]
#! @Returns In this form, returns a right quasigroup mapping with source <Arg>Q1</Arg>
#! and range <Arg>Q2</Arg> determined by the transformation <Arg>f</Arg>.
#! If the optional argument is not given, it is checked that <Arg>f</Arg> is a parent transformation
#! from `Q1` to `Q2`, and then the returned mapping `m` satisfies `(Q1.i)^m = Q2.j` iff `i^f=j`.
#! If the optional argument is set to `true`, is it checked that <Arg>f</Arg> is a canonical
#! transformation, and then the returned mapping `m` satisfies
#! `(Elements(Q1)[i])^m = Elements(Q2)[j]` iff `i^f=j`.
#! @Description In the form `AsRightQuasigroupMapping( Q, f[, isCanonical] )`,
#! returns a right quasigroup mapping with source and range equal to <Arg>Q</Arg>,
#! determined by the permutation <Arg>f</Arg>.
DeclareOperation( "AsRightQuasigroupMapping", [ IsRightQuasigroup, IsRightQuasigroup, IsTransformation ] );

# CANONICAL PERMUTATIONS AND PARENT PERMUTATIONS
# _____________________________________________________________________________

#! @Section Canonical permutations and parent permutations

#! @Arguments Q, f 
#! @Returns `true` if the permutation <Arg>f</Arg> is a canonical permutation on the right quasigroup <Arg>Q</Arg>,
#! (that is, if <Arg>f</Arg> restricts to a permutation of `[1..Size(`<Arg>Q</Arg>`)]`), else returns `false`.
DeclareOperation( "IsCanonicalPerm", [ IsRightQuasigroup, IsPerm ] );

#! @Arguments Q, f 
#! @Returns `true` if <Arg>f</Arg> is a parent permutation on right quasigroup `Q`
#! (that is, if <Arg>f</Arg> restricts to a permutation of `ParentInd(`<Arg>Q</Arg>`)`), else returns `false`.
DeclareOperation( "IsParentPerm", [ IsRightQuasigroup, IsPerm ] );

#! @BeginExampleSession
#! gap> Q := MoufangLoop( 12, 1 );; S := Subloop( Q, [Q.3] );;
#! gap> ParentInd( S );
#! [ 1, 3, 5 ]
#! gap> (Q.3)^(3,4) = Q.4;
#! true
#! gap> IsParentPerm( S, (3,4) ); # does not act on [ 1, 3, 5 ] 
#! false
#! gap> IsParentPerm( S, (3,5) ); # acts on [ 1, 3, 5 ]
#! true
#! gap> IsParentPerm( S, (3,5)(7,8) ); # action on the complement of S is ignored
#! true
#! gap> IsCanonicalPerm( S, (1,2,3) ); # acts on [1..Size(S)]
#! true
#! gap> IsCanonicalPerm( S, (1,3,5) ); # does not act on [1..Size(S)]
#! false
#! @EndExampleSession

#! @Arguments arg
#! @Returns the canonical permutation determined by the argument(s) <Arg>arg</Arg>.
#! If the argument is a bijective right quasigroup mapping `m` on `Q`,
#! returns a permutation `f` such that `i^f=j` iff `Elements(Q)[i]^m = Elements(Q)[j]`.
#! If the arguments are right quasigroup `Q` and its bijective parent transformation `m`,
#! returns a permutation `f` such that `i^f=j` iff `ParentInd(Q)[i]^m = ParentInd(Q)[j]`.
#! If the arguments are a right quasigroup `Q` and its parent permutation `m`,
#! returns a permutation `f` such that `i^f=j` iff `ParentInd(Q)[i]^m = ParentInd(Q)[j]`.
DeclareOperation( "AsCanonicalPerm", [ IsMapping ] );

#! @Returns the parent permutation determined by the argument(s) <Arg>arg</Arg>.
#! If the argument is a bijective right quasigroup mapping `m` on `Q`, returns
#! a permutation `f` such that `i^f=j` iff `(Q.i)^m = Q.j` (for `i` in `ParentInd(Q)`).
#! If the arguments are a right quasigroup `Q` and its bijective canonical transformation `m`,
#! returns a permutation `f` such that `ParentInd(Q)[i]^f=ParentInd(Q)[j]` iff `i^m = j`.
#! If the arguments are a right quasigroup `Q` and its canonical permutation `m`,
#! returns a permutation `f` such that `ParentInd(Q)[i]^f=ParentInd(Q)[j]` iff `i^m = j`.
#! @Description See `ParentInd`, too.
DeclareOperation( "AsParentPerm", [ IsMapping ] );

#! @BeginExampleSession
#! gap> Q := MoufangLoop( 12, 1 );; S := Subloop( Q, [Q.3] );; ParentInd( S );
#! [ 1, 3, 5 ]
#! gap> m := AsRightQuasigroupMapping( S, (1,3,5) );
#! MappingByFunction( <Moufang loop of size 3>, <Moufang loop of size 3>, function( x ) ... end )
#! gap> m2 := AsRightQuasigroupMapping( S, (1,2,3), true ); # argument is a canonical permutation
#! MappingByFunction( <Moufang loop of size 3>, <Moufang loop of size 3>, function( x ) ... end )
#! gap> m = m2;
#! true
#! gap> f := AsParentPerm( m ); # ParentInd( m ) has the same effect
#! (1,3,5)
#! gap> f2 := AsCanonicalPerm( m );
#! (1,2,3)
#! gap> AsParentPerm( S, f2 );
#! (1,3,5)
#! gap> AsCanonicalPerm( S, f );
#! (1,2,3)
#! @EndExampleSession

# CANONICAL TRANSFORMATIONS AND PARENT TRANSFORMATIONS
# _____________________________________________________________________________

#! @Section Canonical transformations and parent transformations

# RQ_IsTransformation( Q1, Q2, f, isCanonical, reportErrors )
# returns true if f is a transformation from Q1 to Q2, either canonical or parent, as determined by isCanonical 
DeclareOperation( "RQ_IsTransformation", [ IsRightQuasigroup, IsRightQuasigroup, IsTransformation, IsBool, IsBool ] );

#! @Arguments Q1, Q2, t 
#! @Returns `true` if <Arg>t</Arg> is a canonical transformation from right quasigroup <Arg>Q1</Arg> to
#! right quasigroup <Arg>Q2</Arg>, that is, if `i^f` is in `[1..Size(Q2)]` for all `i` in `[1..Size(Q1)]`.
DeclareOperation( "IsCanonicalTransformation", [ IsRightQuasigroup, IsRightQuasigroup, IsTransformation ] );

#! @Arguments Q1, Q2, t 
#! @Returns `true` if <Arg>t</Arg> is a parent transformation from right quasigroup <Arg>Q1</Arg> to
#! right quasigroup <Arg>Q2</Arg> (that is, if `i^f` is in `ParentInd( Q2 )` for all `i` in `ParentInd( Q1 )`),
#! else returns `false`.
DeclareOperation( "IsParentTransformation", [ IsRightQuasigroup, IsRightQuasigroup, IsTransformation ] );

#! @Arguments arg
#! @Returns the canonical transformation determined by the argument(s) <Arg>arg</Arg>.
#! If the argument is a right quasigroup mapping `m` from `Q1` to `Q2`, returns
#! a transformation `t` such that `i^t=j` iff `Elements(Q1)[i]^m = Elements(Q2)[j]`.
#! If the arguments are two right quasigroups `Q1`, `Q2` and their parent transformation `m`,
#! returns a tranformation `t` such that `i^t=j` iff `ParentInd(Q1)[i]^m = ParentInd(Q2)[j]`.
#! If the arguments are a right quasigroup `Q` and its parent permutation `m`,
#! returns a tranformation `t` such that `i^t=j` iff `ParentInd(Q)[i]^m = ParentInd(Q)[j]`.
DeclareOperation( "AsCanonicalTransformation", [ IsMapping ] );

#! @Returns the parent transformation determined by the argument(s) <Arg>arg</Arg>.
#! If the argument is a right quasigroup mapping `m` from `Q1` to `Q2`, returns
#! a transformation `t` such that `i^t=j` iff `(Q1.i)^m = Q2.j` (for `i` in `ParentInd(Q1)`).
#! If the arguments are two right quasigroups `Q1`, `Q2` and their canonical transformation `m`,
#! returns a tranformation `t` such that `ParentInd(Q1)[i]^t=ParentInd(Q2)[j]` iff `i^m = j`.
#! If the arguments are a right quasigroup `Q` and its canonical permutation `m`,
#! returns a tranformation `t` such that `ParentInd(Q)[i]^t=ParentInd(Q)[j]` iff `i^m = j`.
#! @Description See `ParentInd`, too.
DeclareOperation( "AsParentTransformation", [ IsMapping ] );

#! @BeginExampleSession
#! gap> Q := AsLoop( SymmetricGroup( 4 ) );;
#! gap> S1 := Subloop( Q, [ Q[(1,2,3)] ] );;
#! gap> S2 := Subloop( Q, [ Q[(1,4)]*Q[(1,2,3)]*Q[(1,4)] ] );; # conjugate subloop
#! gap> m := MappingByFunction( S1, S2, x-> Q[(1,4)]*x*Q[(1,4)] ); # conjugation S1 -> S2
#! MappingByFunction( <associative loop of size 3>, <associative loop of size 3>, function( x ) ... end )
#! gap> ParentInd( S1 );
#! [ 1, 9, 13 ]
#! gap> ParentInd( S2 );
#! [ 1, 4, 5 ]
#! gap> t := AsParentTransformation( m ); 
#! Transformation( [ 1, 2, 3, 4, 5, 6, 7, 8, 4, 10, 11, 12, 5 ] )
#! gap> IsParentTransformation( S1, S2, t );
#! true
#! gap> AsCanonicalTransformation( m );
#! IdentityTransformation
#! gap> IsCanonicalTransformation( S1, S2, last );
#! true
#! @EndExampleSession