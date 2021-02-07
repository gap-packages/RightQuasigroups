# PermutationGroups.gd
# Permutation groups associated with right quasigroups
# =============================================================================

#! @Chapter Associated permutation groups

# PARENT PERMUTATIONS AND CANONICAL PERMUTATIONS
# _____________________________________________________________________________

#! @Section Parent permutations and canonical permutations

#! <P/>Suppose that `Q` is a right quasigroup with parent `P`, let `nQ = Size(Q)` and `nP = Size(P)`.
#! Two types of permutations can be understood as permutations of `Q`:
#! <List>
#! <Item>a permutation on `[1..nQ]`, which we will call
#! **canonical permutation**<Index Subkey="canonical">permutation</Index><Index>canonical permutation</Index>, and</Item>
#! <Item>a permuation on `[1..nP]` that only moves points within `ParentInd( Q )`, which we will call
#! **parent permutation**<Index Subkey="parent">permutation</Index><Index>parent permutation</Index>.</Item>
#! </List>
#! <P/>

#! <P/>Most permutations encountered in &RightQuasigroups; are parent permutations, such as all right translations,
#! all elements of multiplication groups, etc. 
#! However, it is often useful to work with canonical permutations, for instance while considering right translations
#! in the context of multiplication tables (which, by definition, contain entries from `[1..nQ]`), or when
#! dealing with isomorphisms between right quasigroups on different underlying sets.

#! <P/>The following functions convert between the two types of permutations.

#! @Arguments Q, f
#! @Returns the parent permutation `g` corresponding to the canonical permutation <Arg>f</Arg>
#! on the right quasigroup <Arg>Q</Arg>. That is, `g` fixes all points outside of `ParentInd( `<Arg>Q</Arg>` )`
#! and if `i` is an element of `ParentInd( `<Arg>Q</Arg>` )` then `i^g = j` iff `u^`<Arg>f</Arg>` = v`,
#! `ParentInd( Elements( `<Arg>Q</Arg>` )[ u ] ) = i` and `ParentInd( Elements( `<Arg>Q</Arg>` )[ v ] ) = j`.
DeclareOperation( "AsParentPerm", [ IsRightQuasigroup, IsPerm ] );

#! @Arguments Q, f
#! @Returns the canonical permutation `g` corresponding to the parent permutation <Arg>f</Arg>
#! on the parent of the right quasigroup <Arg>Q</Arg>. That is, `g` is a permutation on `[1..Size( `<Arg>Q</Arg>` )]`
#! and if `i` is an element of `[1..Size( `<Arg>Q</Arg>` )]` then `i^g = j` iff
#! `ParentInd( Elements( `<Arg>Q</Arg>` )[i] ) = u`, `ParentInd( Elements( `<Arg>Q</Arg>` )[ j ] ) = v` and `u^`<Arg>f</Arg>` = v`.
#! @Description Note that the returned permutation is independent of how <Arg>f</Arg> operates outside
#! of `ParentInd( `<Arg>Q</Arg>` )`. We therefore allow <Arg>f</Arg> to move points outside of `ParentInd( `<Arg>Q</Arg>` )`.
DeclareOperation( "AsCanonicalPerm", [ IsRightQuasigroup, IsPerm ] );

#! @BeginExampleSession
#! gap> P := ProjectionRightQuasigroup( 10 );;
#! gap> Q := Subrightquasigroup( P, [3,5,7] );;
#! gap> ParentInd( Q );
#! [ 3, 5, 7 ]
#! gap> AsParentPerm( Q, (1,2,3) );
#! (3,5,7)
#! gap> AsCanonicalPerm( Q, (3,5,7)(8,9) ); # moved points outside of ParentInd( Q ) are ignored
#! (1,2,3)
#! @EndExampleSession

# TRANSLATIONS AND SECTIONS
# _____________________________________________________________________________

#! @Section Translations and sections

#! <P/>Let `Q` be a right quasigroup with a parernt of size `nP`.
#! Right translations of right quasigroups and left translations of quasigroups
#! are represented as parent permutations in &RightQuasigroups;, cf. Section <Ref Sect = "Section_ParentPerms"/>.
#! Consequently, such translations are elements of the symmetric group on `[1..nP]`.

#! <P/>A left translation of a right quasigroup `Q` is not necessarily a permutation
#! and it is represented as a transformation on `[1..nP]` that fixes all
#! points outside of `ParentInd( Q )`. Consequently, such translations are
#! elements of the full transformation semigroup on `[1..nP]`.

#! @Arguments Q, x
#! @Returns the right translation by the element <Arg>x</Arg> in the right quasigroup <Arg>Q</Arg>.
#! We allow <Arg>x</Arg> to be an element of <Arg>Q</Arg> or an element of the underlying set of <Arg>Q</Arg>.
#! The right translation is returned as a parent permutation, cf. 
#! Section <Ref Sect="Section_Parent"/>.
DeclareOperation( "RightTranslation", [ IsRightQuasigroup, IsObject ] );

#! @Arguments Q, x
#! @Returns the left translation by the element <Arg>x</Arg> in the right quasigroup <Arg>Q</Arg>.
#! We allow <Arg>x</Arg> to be an element of <Arg>Q</Arg> or an element of the underlying set of <Arg>Q</Arg>.
#! If <Arg>Q</Arg> is a quasigroup, the translation is returned as a parent permutation,
#! cf. Section <Ref Sect="Section_Parent"/>. If <Arg>Q</Arg> is merely 
#! a right quasigroup, the left translation is returned as a transformation on `[1..Size( Parent( `<Arg>Q</Arg>` ) )]`,
#! even if the left translation happens to be a permutation.
DeclareOperation( "LeftTranslation", [ IsRightQuasigroup, IsObject ] );

#! @BeginGroup
#! @GroupTitle Sections

#! @Arguments Q
DeclareAttribute( "RightSection", IsRightQuasigroup );

#! @Arguments Q
#! @Returns the right (left) section of a right quasigroup `Q`, i.e., the list
#! of all right (left) translations by elements of `Q`.
DeclareAttribute( "LeftSection", IsRightQuasigroup );

#! @EndGroup

# MULTIPLICATION GROUPS AND RELATIVE MULTIPLICATION GROUPS
# _____________________________________________________________________________

#! @Section Multiplication groups and relative multiplication groups

#! <P/>The <Index>right multiplication group</Index>**right multiplication group** $\textrm{Mlt}_r(Q)$
#! of a right quasigroup $Q$ is the permutation group generated by all right translations of $Q$.
#! The <Index>left multiplication group</Index>**left multiplication group** $\textrm{Mlt}_\ell(Q)$
#! of a left quasigroup $Q$ is defined dually. The <Index>multiplication group</Index>
#! **multiplication group** $\textrm{Mlt}(Q)$ of a quasigroup `Q` is the permutation group generated
#! by all left and right translations of $Q$.

#! <P/>If $S$ is a subrightquasigroup of a right quasigroup $Q$, the
#! <Index>relative right multiplication group</Index>**relative right multiplication group**
#! of $S$ in $Q$ is the subgroup of $\textrm{Mlt}_r(Q)$ generated by all right translations 
#! in $Q$ by elements of $S$. Note that there is a difference betwee the relative right multiplication
#! group of $S$ in $Q$ and the right multiplication group of $S$.
#! The <Index>relative left multiplication group</Index>**relative left multiplication group**
#! and the <Index>relative multiplication group</Index>**relative multiplication group**
#! of $S$ in $Q$ are defined analogously.

#! @BeginGroup 
#! @GroupTitle Multiplication groups 

#! @Arguments Q
DeclareAttribute( "RightMultiplicationGroup", IsRightQuasigroup );

#! @Arguments Q
DeclareAttribute( "LeftMultiplicationGroup", IsQuasigroup );

#! @Arguments Q
#! @Returns the (right, left) multiplication group of the right quasigroup
#! `Q` as a subgroup of the symmetric group on the set `[1..Size(Parent(Q))]`.
#! For the left and two-sided versions, `Q` must be a quasigroup.
#! An effort is made to return the group with a precalculated small generating set.
DeclareAttribute( "MultiplicationGroup", IsQuasigroup );

#! @EndGroup

#! @Arguments Q
#! @Returns the left multiplication semigroup of the right quasigoup `Q`
#! as a subsemigroup of the full transformation semigroup on the set `[1..Size(Parent(Q))]`.
#! If `Q` is a quasigorup, the function returns the group `LeftMultiplicationGroup( Q )`.
DeclareAttribute( "LeftMultiplicationSemigroup", IsRightQuasigroup );

#! @BeginGroup
#! @GroupTitle Relative multiplication groups

#! @Arguments Q, S
DeclareOperation( "RelativeRightMultiplicationGroup", [ IsRightQuasigroup, IsRightQuasigroup ] );

#! @Arguments Q, S
DeclareOperation( "RelativeLeftMultiplicationGroup", [ IsQuasigroup, IsQuasigroup ] );

#! @Arguments Q, S
#! @Returns the relative (right, left) multiplication group of a subrightquasigroup `S`
#! in the right quasigroup `Q`. For the left an two-sided versions, `Q` must be a quasigroup.
#! An effort is made to return the group with a precalculated small generating set.
DeclareOperation( "RelativeMultiplicationGroup", [ IsQuasigroup, IsQuasigroup ] );

#! @BeginExampleSession
#! gap> Q := AsQuasigroup( Group( (1,2,3,4,5,6 ) ) );;
#! gap> Size( RightMultiplicationGroup( Q ) );
#! 6
#! gap> S := Subquasigroup( Q, [ Q[(1,3,5)(2,4,6)] ] );
#! <associative quasigroup of size 3>
#! gap> RightTranslation( S, (1,3,5)(2,4,6) );
#! (1,3,5)
#! gap> LeftTranslation( S, (1,3,5)(2,4,6) );
#! (1,3,5)
#! gap> Size( LeftMultiplicationGroup( S ) );
#! 3
#! gap> RelativeLeftMultiplicationGroup( Q, S );
#! Group([ (1,3,5)(2,4,6) ])
#! gap> LeftMultiplicationGroup( S );
#! Group([ (1,3,5) ])
#! gap> RelativeRightMultiplicationGroup( Q, S );
#! Group([ (1,3,5)(2,4,6) ])
#! gap> Size( MultiplicationGroup( Q ) );
#! 6
#! @EndExampleSession

#! @EndGroup

# INNER MAPPING GROUPS
# ____________________________________________________________________________

#! @Section Inner mappings and inner mapping groups

#! <P/>Elements of the multiplication groups that fix the identity element
#! are called <Index>inner mappings</Index>**inner mappings** in loop theory.
#! We extend the definition of traditional inner mappings to right quasigroups and
#! quasigroups 

#! <P/>When $Q$ is a right quasigroup and $x,y\in Q$, the <Index>right inner mapping</Index>
#! **right inner mapping** determined by $x$ and $y$ is the permutation $R(x,y) = R_{xy}^{-1} R_y R_x$,
#! where we compose mappings from right to left. The <Index>right inner mapping group</Index>
#! **right inner mapping group** of $Q$ is the subgroup of the right multiplication group $\textrm{Mlt}_r(Q)$ of $Q$
#! generated by all right inner mappings. When $Q$ is a loop, the right inner mapping group
#! is the stabilizer of the neutral element of $Q$ in $\textrm{Mlt}_r(Q)$.

#! <P/>When $Q$ is a quasigroup and $x,y\in Q$, the <Index>middle inner mapping</Index>
#! **middle inner mapping** determined by $x$ is the permutation $L(x)^{-1} R(x)$,
#! and the <Index>left inner mapping</Index>**left inner mapping** determined by $x$ and $y$ is the
#! permutation $L(x,y) = L_{yx}^{-1} L_y L_x$. The <Index>middle inner mapping group</Index>
#! **middle inner mapping group** of $Q$ is the subgroup of the multiplication group of $Q$
#! generated by all middle inner mappings. The <Index>left inner mapping group</Index>
#! **left inner mapping group** of $Q$ is the subgroup of the left multiplication group $\textrm{Mlt}_\ell(Q)$ of $Q$
#! generarted by all left inner mappings. When $Q$ is a loop, the left inner mapping grup
#! is the stabilizer of the neutral element of $Q$ in $\textrm{Mlt}_\ell(Q)$.
#! Finally, the <Index>inner mapping group</Index>**inner mapping group** of $Q$ is the
#! subroup of the multiplication group $\textrm{Mlt}(Q)$ of $Q$ generated by all right, middle and left
#! inner mappings. When $Q$ is a loop, the inner mapping group is the stabilizer of the
#! neutral element of $Q$ in $\textrm{Mlt}(Q)$.

#! @BeginGroup
#! @GroupTitle Inner mappings

#! @Arguments Q, x, y
DeclareOperation( "RightInnerMapping", [ IsRightQuasigroup, IsRightQuasigroupElement, IsRightQuasigroupElement ] );

#! @Arguments Q, x
DeclareOperation( "MiddleInnerMapping", [ IsQuasigroup, IsQuasigroupElement ] );

#! @Arguments Q, x, y
#! @Returns the right, middle and left inner mappings in `Q` determined by `x` (and `y`).
#! For the right inner mappings, `Q` can be a right quasigroups. For the middle and left inner
#! mappings, `Q` must be a quasigroup.
DeclareOperation( "LeftInnerMapping", [ IsQuasigroup, IsQuasigroupElement, IsQuasigroupElement ] );

#! @EndGroup

#! @BeginGroup
#! @GroupTitle Inner mapping groups

#! @Arguments Q
DeclareAttribute( "RightInnerMappingGroup", IsRightQuasigroup );

#! @Arguments Q
DeclareAttribute( "MiddleInnerMappingGroup", IsQuasigroup );

#! @Arguments Q
DeclareAttribute( "LeftInnerMappingGroup", IsLoop );

#! @Arguments Q
#! @Returns the (right, middle, left) inner mapping group of a quasigroup `Q`. For the
#! right inner mapping group, we alow `Q` to be a right quasigroup.
DeclareAttribute( "InnerMappingGroup", IsLoop );

#! @BeginExampleSession
#! gap> ct := [[1,2,3,4,5],[2,1,4,5,3],[3,5,1,2,4],[4,3,5,1,2],[5,4,2,3,1]];;
#! gap> Q := LoopByCayleyTable( ct );; # not commutative
#! gap> LeftInnerMapping( Q, Q.3, Q.3 );
#! (2,4,5)
#! gap> L := LeftMultiplicationGroup( Q );;
#! gap> LInn := LeftInnerMappingGroup( Q );;
#! gap> [ Size( L ), Size( LInn ) ];
#! [ 120, 24 ]
#! gap> (2,4,5) in L;
#! true
#! gap> LInn = Stabilizer( L, 1 );
#! true
#! gap> RInn := RightInnerMappingGroup( Q );;
#! gap> Size( RInn );
#! 24
#! gap> ForAll( Q, x -> ForAll( Q, y -> RightInnerMapping( Q, x, y ) in RInn ) );
#! true
#! gap> LInn = RInn;
#! true
#! gap> Size( MiddleInnerMappingGroup( Q ) );
#! 12
#! @EndExampleSession

#! @EndGroup

# DISPLACEMENT GROUPS
# ____________________________________________________________________________

#! @Section Displacement groups

#! <P/>For a right quasigroup $(Q,\cdot)$, define the **right positive displacement group** $\mathrm{Dis}_r^+(Q) =\langle R_xR_y^{-1}:x,y\in Q\rangle$,
#! the **right negative displacement group** $\mathrm{Dis}_r^-(Q) =\langle R_x^{-1}R_y:x,y\in Q\rangle$,
#! and the **right displacement group** $\mathrm{Dis}_r(Q) = \langle R_xR_y^{-1},R_x^{-1}R_y:x,y\in Q\rangle$.

#! <P/>For a fixed $e\in Q$ we then have $\mathrm{Dis}_r^+(Q)=\langle R_eR_x^{-1}:x\in Q\rangle = \langle R_xR_e^{-1}:x\in Q\rangle$ and
#! $\mathrm{Dis}_r^-(Q)=\langle R_x^{-1}R_e:x\in Q\rangle = \langle R_e^{-1}R_x:x\in Q\rangle$.

#! <P/>The **left displacement groups** are defined analogously for a left quasigroup $(Q,\cdot)$ by 
#! $\mathrm{Dis}_\ell^+(Q) =\langle L_xL_y^{-1}:x,y\in Q\rangle$, $\mathrm{Dis}_\ell^-(Q) =\langle L_x^{-1}L_y:x,y\in Q\rangle$
#! and $\mathrm{Dis}_\ell(Q) = \langle L_xL_y^{-1},L_x^{-1}L_y:x,y\in Q\rangle$.

#! <P/>For a fixed $e\in Q$ we then have $\mathrm{Dis}_\ell^+(Q)=\langle L_eL_x^{-1}:x\in Q\rangle = \langle L_xL_e^{-1}:x\in Q\rangle$ and
#! $\mathrm{Dis}_\ell^-(Q)=\langle L_x^{-1}L_e:x\in Q\rangle = \langle L_e^{-1}L_x:x\in Q\rangle$.

#! @BeginGroup
#! @GroupTitle Right displacement groups

#! @Arguments Q
#! @Returns the various right displacement groups of the right quasigroup <Arg>Q</Arg>.
DeclareAttribute( "RightPositiveDisplacementGroup", IsRightQuasigroup );

#! @Arguments Q
DeclareAttribute( "RightNegativeDisplacementGroup", IsRightQuasigroup );

#! @Arguments Q
DeclareAttribute( "RightDisplacementGroup", IsRightQuasigroup );

#! @EndGroup

#! @BeginGroup
#! @GroupTitle Left displacement groups

#! @Arguments Q
#! @Returns the various left displacement groups of the quasigroup <Arg>Q</Arg>.
DeclareAttribute( "LeftPositiveDisplacementGroup", IsQuasigroup );

#! @Arguments Q
DeclareAttribute( "LeftNegativeDisplacementGroup", IsQuasigroup );

#! @Arguments Q
DeclareAttribute( "LeftDisplacementGroup", IsQuasigroup );

#! @EndGroup

#! @Arguments Q
#! @Returns `true` if the quasigroup <Arg>Q</Arg> is isotopic to a group, else returns `false`.
#! @Description Note that a quasigroup $(Q,\cdot)$ is isotopic to a group iff $\mathrm{Dis}_\ell^+(Q)$
#! acts regularly on $Q$, in which case $(Q,\cdot)$ is isotopic to $\mathrm{Dis}_\ell^+(Q)$ (cf. Chapter <Ref Chap="Chapter_Iso"/>).
DeclareProperty( "IsIsotopicToGroup", IsQuasigroup );

#! @BeginExampleSession
#! gap> Q := QuasigroupByFunction( GF(5), \- );
#! <quasigroup of size 5>
#! gap> RightPositiveDisplacementGroup( Q );
#! Group([ (1,4,5,3,2) ])
#! gap> LeftDisplacementGroup( Q );
#! Group([ (1,2,3,5,4) ])
#! gap> IsIsotopicToGroup( Q );
#! true
#! @EndExampleSession