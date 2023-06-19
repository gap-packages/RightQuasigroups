# MltSearch.gd
# Realizing permutation groups as multiplication groups of loops
# =============================================================================

# chapter continues
#! @Chapter Associated permutation groups

#! @Section Realizing permutation groups as multiplication groups of loops

#! <P/>The multiplication group $\mathrm{Mlt}(Q)$ of a loop $Q$ is the permutation
#! group generated by all left and right translations of $Q$, cf. <Ref Sect="Section_Mlt"/>.
#! It is a difficult question in loop theory to determine which permutation groups
#! can be realized as multiplication groups of loops.
#! The following functions search for loops such that their multiplication group
#! is contained in or equal to a given permutation group `G`.
#
#! <P/>Since the emphasis here is on the multiplication groups, the results
#! are returned not as loops but as right sections of loops, cf. <Ref Sect="Section_Translations"/>.
#! A right section `sec` contained in the symmetric group on `[1..n]` can then be
#! converted to a loop by the command `LoopByRightSection( [1..n], sec )`.

#! <P/>There are two optional parameters in the searches.
#! <List>
#! <Item>One can speed up the search by setting the argument `depth` higher;
#! the price is a much bigger memory consumption. The value of `depth`
#! is automatically and optimally chosen if in the permutation group `G`
#! there are "not many" permutations fixing `depth` elements. The optional
#! argument `depth` can be ommmitted, or set to `depth=2` with no harm.</Item>
#! <Item>The second optional argument `infolevel` determines the amount of information
#! reported during the search. With `infolevel=0`, no information is reported.
#! With `infolevel=1`, information on timing and hits is reported.
#! With `infolevel=2`, intermediate results are printed as well.</Item>
#! </List>

#! @Arguments G[, depth[, infolevel]]
#! @Returns a (possibly empty) list of right sections of all loops $Q$ such that $\mathrm{Mlt}(Q)\le G$.
#! Returns `fail` is something is wrong with the arguments.
DeclareOperation( "AllLoopsWithMltInGroup", [IsGroup] );

#! @Arguments G[, depth[, infolevel]]
#! @Returns a (possibly empty) list of right sections of all nonassociative loops $Q$ such that $\mathrm{Mlt}(Q)\le G$.
#! Returns `fail` if something is wrong with the arguments.
DeclareOperation( "AllNonassociativeLoopsWithMltInGroup", [IsGroup] );

#! @Arguments G[, depth[, infolevel]]
#! @Returns a list that is either empty or contains the right section of a loop $Q$ such that $\mathrm{Mlt}(Q)\le G$.
#! Returns `fail` if something is wrong with the arguments.
DeclareOperation( "OneLoopWithMltInGroup", [IsGroup] );

#! @Arguments G[, depth[, infolevel]]
#! @Returns a list that is either empty or contains the right section of a nonassociative loop $Q$ such that $\mathrm{Mlt}(Q)\le G$.
#! Returns `fail` if something is wrong with the arguments.
DeclareOperation( "OneNonassociativeLoopWithMltInGroup", [IsGroup] );

#! @Arguments G[, depth[, infolevel]]
#! @Returns a (possibly empty) list of right sections of all loops $Q$ such that $\mathrm{Mlt}(Q)=G$.
#! Returns `fail` if something is wrong with the arguments.
DeclareOperation( "AllLoopsWithMltGroup", [IsGroup] );

#! @Arguments G[, depth[, infolevel]]
#! @Returns a list that is either empty or contains the right section of a loop $Q$ such that $\mathrm{Mlt}(Q)=G$.
#! Returns `fail` if something is wrong with the arguments.
DeclareOperation( "OneLoopWithMltGroup", [IsGroup] );

#! @BeginExampleSession
#! gap> G := AlternatingGroup( 6 );;
#! gap> OneLoopWithMltGroup( G, 2, 0 );
#! [ [ (), (1,2)(3,4,5,6), (1,3)(2,4,6,5), (1,4)(2,5,3,6), 
#!       (1,5)(2,6,4,3), (1,6)(2,3,5,4) ] ]
#! gap> Q := LoopByRightSection( [1..6], last[ 1 ] );
#! <loop of size 6>
#! gap> MultiplicationGroup( Q ) = G;
#! true
#! gap> AllLoopsWithMltInGroup( SymmetricGroup( 4 ), 2, 0);
#! [ [ (), (1,2)(3,4), (1,3)(2,4), (1,4)(2,3) ], 
#!   [ (), (1,2)(3,4), (1,3,2,4), (1,4,2,3) ], 
#!   [ (), (1,2,3,4), (1,3)(2,4), (1,4,3,2) ] ]
#! gap> a := AllLoopsWithMltInGroup( PGL(3,3), 3, 0 );; Size(a);
#! 56
#! gap> a := AllLoopsWithMltGroup(PGL(3,3), 3, 0);; Size(a);
#! 52
#! @EndExampleSession