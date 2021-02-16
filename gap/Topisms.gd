# Topisms.gd
# Homotopisms, isotopisms and autotopisms of right quasigroups
# =============================================================================

#! @Chapter Homotopisms, isotopisms and autotopisms

# HOMOTOPISMS OF RIGHT QUASIGROUPS
# _____________________________________________________________________________

#! @Section Homotopisms of right quasigroups

#! <P/>Blah blah.

# ISOTOPISMS OF RIGHT QUASIGROUPS
# _____________________________________________________________________________

#! @Section Isotopisms of right quasigroups

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




# ISOTOPES OF RIGHT QUASIGROUPS
# _____________________________________________________________________________

#! @Section Isotopes of right quasigroups

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

# AFFINE RIGHT QUASIGROUPS
# _____________________________________________________________________________

# REVISIS: ACTIVATE WHEN READY

#! @Section Affine right quasigroups

#! <P/>Affine (right) quasigroups are instances of (right) quasigroup isotopes of a loop.

#! <P/>Let $(Q,\cdot)$ be a loop, let $f,g$ be endomorphisms of $(Q,\cdot)$ and $c\in Z(Q,\cdot)$. Define
#! a new multiplication $*$ on $Q$ by $x*y = f(x)g(y)c$ and denote the resulting magma by $\mathrm{Aff}(Q,\cdot,f,g,c)$.
#! Then $\mathrm{Aff}(Q,\cdot,f,g,c)$ is a right quasigroup iff $f$ is an automorphism of $(Q,\cdot)$
#! and it is a quasigroup iff both $f$ and $g$ are automorphisms of $(Q,\cdot)$. Any such (right) quasigroup
#! is said to be **affine over the loop** $(Q,\cdot)$<Index>affine right quasigroup</Index>
#! <Index>affine quasigroup</Index> and the parameters $(Q,\cdot,f,g,c)$ constitute its **arithmetic form**
#! <Index>arithmetic form of affine (right) quasigroup</Index>.

#! <P/>Several different formats of arithmetic forms are supported in &RightQuasigroups;:
#! <List>
#! <Item> `(n,f,g,c)` for four integers such that `n` is positive, `Gcd(n,f)=1` (and `Gcd(n,g)=1` for quasigroups).
#! The multiplication is then defined on `[0..n-1]` by `x*y = (f*x + g*y +c) mod n`.</Item>
#! <Item> `(G,f,g,c)`, where `G` is an additive group, `f` is an additive automorphisms of `G`,
#! `g` is an additive endomorphisms of `G` and `c` is a central element of `G`
#! (and `g` is bijective for quasigroups).
#! The multiplication is then defined on `G` by `x*y = x^f + y^g + c`.</Item>
#! <Item> `(G,f,g,c)`, where `G` is a loop or a group, `f` is an automorphisms of `G`,
#! `g` is an endomorphisms of `G` and `c` is a central element of `G`
#! (and `g` is bijective for quasigroups).
#! The multiplication is then defined on `G` by `x*y = x^f*y^g*c`.</Item>
#! </List>

# RQ_IsAffineAlgebraArithmeticForm( category, Q, f, g, c, reportErrors )
DeclareGlobalFunction( "RQ_IsAffineAlgebraArithmeticForm" );

#! @Arguments G, f, g, c
#! @Returns `true` if the arguments constitute an arithmetic form for an affine right quasigroup,
#! else returns `false`. See above for the possible formats of arithmetic forms.
DeclareGlobalFunction( "IsAffineRightQuasigroupArithmeticForm" );

#! @Arguments G, f, g, c
#! @Returns `true` if the arguments constitute an arithmetic form for an affine quasigroup,
#! else returns `false`. See above for the possible formats of arithmetic forms.
DeclareGlobalFunction( "IsAffineQuasigroupArithmeticForm" );

# RQ_AffineAlgebra( category, Q, f, g, c, style )
DeclareGlobalFunction( "RQ_AffineAlgebra" );

#! @Arguments G, f, g, c
#! @Returns the affine right quasigroup by the arithmetic form <Arg>G</Arg>, <Arg>f</Arg>, <Arg>g</Arg>, <Arg>c</Arg>.
#! See above for the possible formats of arithmetic forms.
DeclareGlobalFunction( "AffineRightQuasigroup" );

#! @Arguments G, f, g, c
#! @Returns the affine quasigroup by the arithmetic form <Arg>G</Arg>, <Arg>f</Arg>, <Arg>g</Arg>, <Arg>c</Arg>.
#! See above for the possible formats of arithmetic forms.
DeclareGlobalFunction( "AffineQuasigroup" );