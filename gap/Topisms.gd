# Topisms.gd
# Homotopisms, isotopisms and autotopisms of right quasigroups
# =============================================================================

#! @Chapter Homotopisms, isotopisms and autotopisms

# HOMOTOPISMS OF RIGHT QUASIGROUPS
# _____________________________________________________________________________

#! @Section Homotopisms, isotopism and autotopisms of right quasigroups

#! <P/>A triple of mappings $f,g,h:(Q_1,\cdot)\to (Q_2,*)$ between right quasigroups is a **homotopism**
#! <Index Subkey="right quasigroups">homotopism</Index> if $f(x)*g(y) = h(x\cdot y)$
#! for every $x,y\in Q_1$. If $f$, $g$, $h$ are also bijections, the triple is
#! an **isotopism**<Index Subkey="right quasigroups">isotopism</Index>. An isotopism with
#! $(Q_1,\cdot) = (Q_2,*)$ is an **autotopism**<Index Subkey="right quasigroups">autotopism</Index>.

#! <P/>In &RightQuasigroups;, homotopisms are represented by three right quasigroup mappings,
#! while autotopisms are represented by three parent permutations. 
#! See Chapter <Ref Chap="Chapter_Mappings"/> for conversions between right quasigroup mappings,
#! permutations and transformations. Many functions working with homotopisms and autotopisms
#! accept either three arguments `f`, `g`, `h`, or a single argument `[f,g,h]`.

#! <P/>A homotopism $(f,g,h)$ of quasigroups $Q_1\to Q_2$ is determined by the values of one of the
#! three components on $Q_1$ and by one more value of one of the other two components. For instance,
#! if $c\in Q_1$, a homotopism $(f,g,h)$ is determined by the value $g(c)$ and by $f(x)$ for $x\in Q_1$.
#! This fact is used in the search for isotopisms of quasigroups.

DeclareCategory( "IsRightQuasigroupHomotopism", IsMultiplicativeElementWithInverse ); 
# PROG: Not every homotopism is invertible but we allow it as an option.

DeclareCategoryCollections( "IsRightQuasigroupHomotopism" );
# InstallTrueMethod( IsGeneratorsOfMagmaWithInverses, IsRightQuasigroupHomotopismCollection );

DeclareRepresentation( "IsRightQuasigroupHomotopismRep",
    IsComponentObjectRep and IsMultiplicativeElementWithInverse, [ "source", "range", "f", "g", "h" ] 
);

BindGlobal( "RQ_HomotopismFamily", NewFamily( "RQ_HomotopismFamily", IsObject, IsRightQuasigroupHomotopism ) );
BindGlobal( "RQ_HomotopismType", NewType( RQ_HomotopismFamily, IsRightQuasigroupHomotopism and IsRightQuasigroupHomotopismRep ) );

# constructor
DeclareOperation( "HomotopismRightQuasigroups", [ IsMapping, IsMapping, IsMapping ] );

DeclareAttribute( "Source", IsRightQuasigroupHomotopism );
DeclareAttribute( "Range", IsRightQuasigroupHomotopism );
DeclareOperation( "ComponentOfHomotopism", [ IsRightQuasigroupHomotopism, IsInt ] );

DeclareGlobalFunction( "IsHomotopismRightQuasigroups" );
DeclareGlobalFunction( "IsHomotopismQuasigroups" );
DeclareGlobalFunction( "IsHomotopismLoops" );

DeclareProperty( "IsInjective", IsRightQuasigroupHomotopism );
DeclareProperty( "IsSurjective", IsRightQuasigroupHomotopism );
DeclareProperty( "IsBijective", IsRightQuasigroupHomotopism );

DeclareGlobalFunction( "IsIsotopismRightQuasigroups");
DeclareGlobalFunction( "IsIsotopismQuasigroups" );
DeclareGlobalFunction( "IsIsotopismLoops" );

DeclareGlobalFunction( "IsAutotopismRightQuasigroups");
DeclareGlobalFunction( "IsAutotopismQuasigroups" );
DeclareGlobalFunction( "IsAutotopismLoops" );

#! @BeginGroup 
#! @GroupTitle IsRightQuasigroupHomotopism, IsQuasigroupHomotopism, IsLoopHomotopism

# RQ_IsAlgebraHomotopism( category, f, g, h )
DeclareOperation( "RQ_IsAlgebraHomotopism", [ IsOperation, IsMapping, IsMapping, IsMapping ] );

#! @Arguments f, g, h
#! @Returns `true` if the three right quasigroup (quasigroup, loop) mappings <Arg>f</Arg>, <Arg>g</Arg>, <Arg>h</Arg>
#! form a homotopism from `Q1 = Source( f ) = Source( g ) = Source( h )` to
#! `Q2 = Range( f ) = Range( g ) = Range( h )`, else returns `false`.
#! @Description The function also accepts a single argument `[f,g,h]`.
#DeclareOperation( "IsRightQuasigroupHomotopism", [ IsMapping, IsMapping, IsMapping ] );

#! @Arguments f, g, h
#DeclareOperation( "IsQuasigroupHomotopism", [ IsMapping, IsMapping, IsMapping ] );

#! @Arguments f, g, h
#DeclareOperation( "IsLoopHomotopism", [ IsMapping, IsMapping, IsMapping ] );

#! @EndGroup

#! @BeginGroup 
#! @GroupTitle IsRightQuasigroupIsotopism, IsQuasigroupIsotopism, IsLoopIsotopism

# RQ_IsAlgebraIsotopism( category, f, g, h )
#DeclareOperation( "RQ_IsAlgebraIsotopism", [ IsOperation, IsMapping, IsMapping, IsMapping ] );

#! @Arguments f, g, h
#! @Returns `true` if the three right quasigroup (quasigroup, loop) mappings <Arg>f</Arg>, <Arg>g</Arg>, <Arg>h</Arg>
#! form an isotopism from `Q1 = Source( f ) = Source( g ) = Source( h )` onto
#! `Q2 = Range( f ) = Range( g ) = Range( h )`, else returns `false`.
#! @Description The function also accepts a single argument `[f,g,h]`.
#DeclareOperation( "IsRightQuasigroupIsotopism", [ IsMapping, IsMapping, IsMapping ] );

#! @Arguments f, g, h
#DeclareOperation( "IsQuasigroupIsotopism", [ IsMapping, IsMapping, IsMapping ] );

#! @Arguments f, g, h
#DeclareOperation( "IsLoopIsotopism", [ IsMapping, IsMapping, IsMapping ] );

#! @EndGroup

#! @BeginGroup 
#! @GroupTitle IsRightQuasigroupAutotopism, IsQuasigroupAutotopism, IsLoopAutotopism

# RQ_IsAlgebraHomotopism( category, f, g, h )
#DeclareOperation( "RQ_IsAlgebraAutotopism", [ IsOperation, IsMapping, IsMapping, IsMapping ] );

#! @Arguments f, g, h
#! @Returns `true` if the three right quasigroup (quasigroup, loop) mappings <Arg>f</Arg>, <Arg>g</Arg>, <Arg>h</Arg>
#! form an autotopism of `Q1 = Source( f ) = Source( g ) = Source( h ) = Range( g ) = Range( h )`, else returns `false`.
#! @Description The function also accepts a single argument `[f,g,h]`.
#DeclareOperation( "IsRightQuasigroupAutotopism", [ IsMapping, IsMapping, IsMapping ] );

#! @Arguments f, g, h
#DeclareOperation( "IsQuasigroupAutotopism", [ IsMapping, IsMapping, IsMapping ] );

#! @Arguments f, g, h
#DeclareOperation( "IsLoopAutotopism", [ IsMapping, IsMapping, IsMapping ] );

#! @EndGroup

# TWISTS OF RIGHT QUASIGROUPS
# _____________________________________________________________________________

#! @Section Twists of right quasigroups

#! <P/>In this section we introduce a natural construction that encompasses isomorphs, isotopes and affine 
#! constructions as special cases.

#! <P/>Given a magma $Q$ and three mappings $f,g,h:Q\to Q$, the **twist**<Index>twist</Index>
#! $\mathrm{Tw}(Q,f,g,h)$ of $Q$ via $(f,g,h)$ is defined to be the magma $(Q,*)$ with multiplication
#! $x*y = h(f(x)g(y))$.

#! <P/>If $Q$ is a right quasigroup, the twist $\mathrm{Tw}(Q,f,g,h)$ is a right quasigroup iff both $f$ and $h$
#! are bijections of $Q$. Moreover, the twist $\mathrm{Tw}(Q,f,g,h)$ is a quasigroup iff all three $f$, $g$ and $h$
#! are bijections of $Q$. Finally, if $\mathrm{Tw}(Q,f,g,h)$ is a quasigroup then it is a loop iff
#! $g^{-1}(f(x)\backslash h^{-1}(x))$ is equal to $f^{-1}(h^{-1}(x)/g(x))$ and independent of $x$.

#! <P/>For the convenience of the reader, functions that work with twists accept a wide variety
#! of arguments representing mappings. The conventions on arguments here are analogous to those for isomorphs,
#! cf. Section <Ref Sect="Section_Isomorphs"/>. In more detail:
#! <List>
#! <Item>The functions are named according to the type of algebra they return, not
#! according to the type of algebra they require as input. For instance, `LoopTwist` returns a loop
#! but it accepts a quasigroup (or loop).</Item>
#! <Item>There are four mandatory arguments, given as <Arg>Q</Arg>, <Arg>f</Arg>, <Arg>g</Arg>, <Arg>h</Arg>
#! or as <Arg>Q</Arg>, `[`<Arg>f</Arg>`,`<Arg>g</Arg>`,`<Arg>h</Arg>`]`.</Item>
#! <Item>There are two optional arguments <Arg>isCanonical</Arg> and <Arg>constructorStyle</Arg>. 
#! Any subset of the optional arguments can be given.</Item>
#! <Item>The argument <Arg>Q</Arg> must be a right quasigroup or quasigroup. The returned 
#! algebra will have the same underlying set as <Arg>Q</Arg>.</Item>
#! <Item>Each of the arguments <Arg>f</Arg>, <Arg>g</Arg>, <Arg>h</Arg> can be given as a right quasigroup mapping
#! from <Arg>Q</Arg> to <Arg>Q</Arg> or as a canonical or parent permutation of <Arg>Q</Arg> or 
#! as a canonical or parent transformation of <Arg>Q</Arg>. (See Chapter <Ref Chap="Chapter_Mappings"/>.)
#! The arguments <Arg>f</Arg> and <Arg>h</Arg> must always give rise to bijections. If a quasigroup is supposed
#! to be returned, then also <Arg>g</Arg> must give rise to a bijection.
#! Finally, if a loop is supposed to be returned then
#! $g^{-1}(f(x)\backslash h^{-1}(x))$ must be equal to $f^{-1}(h^{-1}(x)/g(x))$ and independent of $x$.</Item>
#! <Item>If the optional argument <Arg>isCanonical</Arg> is given and set to `true`, all permutations/transformations
#! from among <Arg>f</Arg>, <Arg>g</Arg>, <Arg>h</Arg> are interpreted as canonical permutations/transformations,
#! else they are by default interpreted as parent permutations/transformations.</Item>
#! <Item>See Section <Ref Sect="Section_OptionalArguments"/> for the optional argument <Arg>constructorStyle</Arg>.</Item>
#! </List>

# RQ_AlgebraTwistByParentTransformations( category, Q, f, g, h, style )
# Auxiliary. This is the main constructor here.
DeclareOperation( "RQ_AlgebraTwistByParentTransformations",
    [ IsOperation, IsRightQuasigroup, IsTransformation, IsTransformation, IsTransformation, IsRecord ] );

# RQ_AlebraTwist( category, arg )
# arg is expected as Q, f, g, h [, isCanonical, style] or as Q, [f,g,h][, isCanonical, style]
# Auxiliary. Processes a wide range of arguments and calls RQ_AlgebraTwistByParentTransformations.
DeclareOperation( "RQ_AlgebraTwist", [ IsOperation, IsList ] );

#! @Arguments Q, f, g, h[, isCanonical, constructorStyle]
#! @Returns the twist of the right quasigroup <Arg>Q</Arg> via <Arg>f</Arg>, <Arg>g</Arg>, <Arg>h</Arg>,
#! that is, the right quasigroup on the underlying set of <Arg>Q</Arg> with multiplication $x*y = h(f(x)g(y))$.
#! See above for conventions on the arguments.
DeclareGlobalFunction( "RightQuasigroupTwist" );

#! @Arguments Q, f, g, h[, isCanonical, constructorStyle]
#! @Returns the twist of the quasigroup <Arg>Q</Arg> via <Arg>f</Arg>, <Arg>g</Arg>, <Arg>h</Arg>,
#! that is, the quasigroup on the underlying set of <Arg>Q</Arg> with multiplication $x*y = h(f(x)g(y))$.
#! See above for conventions on the arguments.
DeclareGlobalFunction( "QuasigroupTwist" );

#! @Arguments Q, f, g, h[, isCanonical, constructorStyle]
#! @Returns the twist of the quasigroup <Arg>Q</Arg> via <Arg>f</Arg>, <Arg>g</Arg>, <Arg>h</Arg>,
#! that is, the loop on the underlying set of <Arg>Q</Arg> with multiplication $x*y = h(f(x)g(y))$.
#! See above for conventions on the arguments.
DeclareGlobalFunction( "LoopTwist" );

#! @BeginExampleSession
#! gap> Q := MoufangLoop( 12, 1 );;
#! gap> f := (1,2,3);; g := Transformation( [3,3,3] );; h := MappingByFunction( Q, Q, x->x^-1 );; # note various formats
#! gap> T := RightQuasigroupTwist( Q, f, g, h ); # f and h must be bijective 
#! <right quasigroup of size 12>
#! gap> QuasigroupTwist( Q, [ f, (1,12), h ] ); # g must be bijective for quasigroups
#! <quasigroup of size 12>
#! gap> f := RightTranslation( Q, Q.2 )^-1;; g := LeftTranslation( Q, Q.3 )^-1;;
#! gap> LoopTwist( Q, f, g, (), ConstructorStyle( true, true ) ); # principal loop isotope
#! <loop of size 12>
#! @EndExampleSession

# ISOTOPES OF RIGHT QUASIGROUPS
# _____________________________________________________________________________

#! @Section Isotopes of right quasigroups

#! <P/>If $f,g,h:(Q,\cdot)\to (Q,*)$ is an isotopism, then $x*y = h(f^{-1}(x)\cdot g^{-1}(y))$ for
#! all $x,y\in Q$, and $(Q,*)$ is called an **isotope**<Index>isotope</Index> of $(Q,\cdot)$ via $f$, $g$, $h$.
#! (Note that isotopes are special cases of twists, as introduced in Section <Ref Sect="Section_Twists"/>.)
#! The isotope $(Q,*)$ is a (right) quasigroup iff $(Q,\cdot)$ is a (right) quasigroup, but an isotope
#! of a loop might not be a loop. 

#! <P/>Given a quasigroup $(Q,\cdot,/,\backslash)$ and $a,b\in Q$, the <Index>principal loop isotope</Index>
#! **principal loop isotope** via $a,b$ is defined as $(Q,\circ)$, where $x\circ y = (x/a)\cdot(b\backslash y)$.
#! The principal loop isotope is automatically a loop with neutral element $b\cdot a$.

# auxiliary function (category, data ), data = [ Q, f, g, h, [, isCanonical, constructorStyle ] ]
DeclareOperation( "RQ_AlgebraIsotope", [ IsOperation, IsList ] );

#! @Arguments Q, f, g, h[, isCanonical, constructorStyle ]
#! @Returns the isotope of the right quasigroup <Arg>Q</Arg> via isotopism <Arg>f</Arg>, <Arg>g</Arg>, <Arg>h</Arg>,
#! that is, the right quasigroup on the underlying set of <Arg>Q</Arg> with multiplication $x*y = h(f^{-1}(x)g^{-1}(y))$.
#! See Section <Ref Sect="Section_Twists"/> for conventions on the arguments.
#! @Description Note that the function `IsotopicCopyByPerms` (in analogy to `IsotopicCopyByPerm`)
#! is not provided since the category of the resulting algebra is not necessarily preserved.
DeclareGlobalFunction( "RightQuasigroupIsotope" );

#! @Arguments Q, f, g, h[, isCanonical, constructorStyle ]
#! @Returns the isotope of the quasigroup <Arg>Q</Arg> via isotopism <Arg>f</Arg>, <Arg>g</Arg>, <Arg>h</Arg>,
#! that is, the quasigroup on the underlying set of <Arg>Q</Arg> with multiplication $x*y = h(f^{-1}(x)g^{-1}(y))$.
#! See Section <Ref Sect="Section_Twists"/> for conventions on the arguments.
DeclareGlobalFunction( "QuasigroupIsotope" );

#! @Arguments Q, f, g, h[, isCanonical, constructorStyle ]
#! @Returns the loop isotope of the quasigroup <Arg>Q</Arg> via isotopism <Arg>f</Arg>, <Arg>g</Arg>, <Arg>h</Arg>,
#! that is, the loop on the underlying set of <Arg>Q</Arg> with multiplication $x*y = h(f^{-1}(x)g^{-1}(y))$.
#! See Section <Ref Sect="Section_Twists"/> for conventions on the arguments.
DeclareGlobalFunction( "LoopIsotope" );

#! @BeginExampleSession
#! gap> style := ConstructorStyle( false, false );;
#! gap> P := QuasigroupByFunction( [0..99999], function(x,y) return (x-y) mod 10^5; end, style );
#! <quasigroup of size 100000>
#! gap> f := (1,10,100,1000,10000,100000);; g := (3,4);; h := ();;
#! gap> R := QuasigroupIsotope( P, f, g, h, style );
#! <quasigroup of size 100000>
#! gap> R.1000*R.4;
#! q97
#! @EndExampleSession

#! @Arguments Q, a, b
#! @Returns the loop isotope of the quasigroup <Arg>Q</Arg> via its elements <Arg>a</Arg>, <Arg>b</Arg>.
#! The resulting loop has the same underlying set as <Arg>Q</Arg> and
#! its neutral element corresponds to <Arg>b</Arg>`*`<Arg>a</Arg>. 
DeclareOperation( "PrincipalLoopIsotope", [ IsQuasigroup, IsQuasigroupElement, IsQuasigroupElement ] );

#! @BeginExampleSession
#! gap> Q := QuasigroupByFunction( GF(11), \- );;
#! gap> P := PrincipalLoopIsotope( Q, Q.3, Q.4 );
#! <loop of size 11>
#! gap> UnderlyingSetElm( Q.4*Q.3 ) = UnderlyingSetElm( One( P ) );
#! true
#! @EndExampleSession

# AFFINE RIGHT QUASIGROUPS
# _____________________________________________________________________________

#! @Section Affine right quasigroups

#! <P/>Let $Q$ be a loop, let $f,g$ be endomorphisms of $Q$ and $u,v\in Q$. Define
#! a new multiplication $*$ on $Q$ by $x*y = (f(x)u)(g(y)v)$ and denote the resulting magma by $\mathrm{Aff}(Q,f,u,g,v)$.
#! Then $\mathrm{Aff}(Q,f,u,g,v)$ is a right quasigroup iff $f$ is an automorphism of $Q$
#! and it is a quasigroup iff both $f$ and $g$ are automorphisms of $Q$.
#! Analogous comments hold for $\mathrm{Aff}(Q,f,u,v,g)$, $\mathrm{Aff}(Q,u,f,g,v)$ and $\mathrm{Aff}(Q,u,f,v,g)$,
#! where the multiplication formula is obtained according to the order of the arguments. For instance, 
#! in $\mathrm{Aff}(Q,u,f,g,v)$ the multiplication is given by $x*y = (uf(x))(g(y)v)$.

#! <P/>Any such (right) quasigroup is said to be **affine over the loop** $Q$<Index>affine right quasigroup</Index>
#! <Index>affine quasigroup</Index> and the parameters constitute its **arithmetic form**
#! <Index>arithmetic form of affine (right) quasigroup</Index>. 

#! <P/>Note that affine right quasigroups are instances of twists of right quasigroups,
#! cf. Section <Ref Sect="Section_Twists"/>. Also note that if $Q$ is an abelian group
#! then all four formulas reduce to $x*y = f(x)g(y)uv$, and we can replace $u$, $v$ with a single element $c$.

#! <P/>Several different formats of arithmetic forms are supported in &RightQuasigroups;:
#! <List>
#! <Item> `(n,f,g,c)` for four integers such that `n` is positive, `Gcd(n,f)=1` (and `Gcd(n,g)=1` for quasigroups).
#! The multiplication is then defined on `[0..n-1]` by `(f*x + g*y +c) mod n`.</Item>
#! <Item> `(F,f,g,c)`, where `F` is a field, `f` is a nonzero element of `F` and `g`, `c` are elements of `F`
#! (and `g` is nonzero for quasigroups).
#! The multiplication is then defined on `F` by `f*x + g*y +c`.</Item>
#! <Item> `(G,f,g,c)`, where `G` is an abelian group or an abelian additive group,
#! `f` is an automorphism of `G`, `g` is an endomorphism of `G`,
#! and `c` is an element of `G` (and `g` is bijective for quasigroups). The multiplication is then defined on `G`
#! by `x^f+y^g+c` if `G` is an additive group and by `x^f*y^g*c` otherwise.</Item>
#! <Item> `(G,f,u,g,v)`, where `G` is a loop, a group, or an additive group,
#! `f` is an automorphisms of `G`, `g` is an endomorphisms of `G` and `u`, `v` are elements of `G`
#! (and `g` is bijective for quasigroups).
#! The multiplication is then defined on `G` by `(x^f+u)+(y^g+v)` in the additive case and by
#! `(x^f*u)*(y^g*v)` otherwise.</Item>
#! <Item>The three variations `(G,f,u,v,g)`, `(G,u,f,g,v)` and `(G,u,f,v,g)` are also also supported.</Item>
#! </List>

# RQ_IsAffineAlgebraArithmeticForm( category, data, reportErrors )
DeclareGlobalFunction( "RQ_IsAffineAlgebraArithmeticForm" );

#! @Arguments arg
#! @Returns `true` if the arguments constitute an arithmetic form for an affine right quasigroup,
#! else returns `false`. See above for the possible formats of arithmetic forms.
DeclareGlobalFunction( "IsAffineRightQuasigroupArithmeticForm" );

#! @Arguments arg
#! @Returns `true` if the arguments constitute an arithmetic form for an affine quasigroup,
#! else returns `false`. See above for the possible formats of arithmetic forms.
DeclareGlobalFunction( "IsAffineQuasigroupArithmeticForm" );

# RQ_AffineAlgebra( category, data, style )
DeclareGlobalFunction( "RQ_AffineAlgebra" );

#! @Arguments arg[, constructorStyle]
#! @Returns the affine right quasigroup by the arithmetic form <Arg>arg</Arg>.
#! See above for the possible formats of arithmetic forms. If a loop `G` is part of the arithmetic form,
#! the right quasigroup is returned as an isotope of <Arg>G</Arg>. In all other cases
#! the code first internally creates a suitble right quasigroup `G`and then returns
#! an isotope of `Q`.
DeclareGlobalFunction( "AffineRightQuasigroup" );

#! @Arguments arg[, constructorStyle]
#! @Returns the affine quasigroup by the arithmetic form <Arg>arg</Arg>.
#! See above for the possible formats of arithmetic forms. Also see the remark on isotopy under `AffineRightQuasigroup`.
DeclareGlobalFunction( "AffineQuasigroup" );

#! <P/>Here are four examples of affine (right) quasigroups, constructed from various arithmetic forms.
#! First over `([0..n-1],+)`:

#! @BeginExampleSession
#! gap> IsAffineRightQuasigroupArithmeticForm( 10, 3, 5, 1 ); # suitable for (3*x+5*y+1) mod 10
#! true
#! gap> IsAffineQuasigroupArithmeticForm( 10, 3, 5, 1 ); # gcd(10,5) <> 1
#! false
#! gap> Q := AffineRightQuasigroup( 10, 3, 5, 1 );
#! <right quasigroup of size 10>
#! gap> Q := AffineRightQuasigroup( 100000, 333, 777, 5, ConstructorStyle(false,false) ); # non-index based example
#! <right quasigroup of size 100000>
#! @EndExampleSession

#! <P/>Next over a finite field:

#! @BeginExampleSession
#! gap> F := GF(9);; f := Z(9);; g := Z(9)^3;; c := Z(9)^5;;
#! gap> IsAffineQuasigroupArithmeticForm( F, f, g, c ); # suitable for f*x+g*y+c
#! true
#! gap> AffineQuasigroup( F, f, g, c );
#! <quasigroup of size 9>
#! @EndExampleSession

#! <P/>Next over an abelian group:

#! @BeginExampleSession
#! gap> G := CyclicGroup(10);; A := AutomorphismGroup( G );; f := A.1;; g := A.2;; c := G.1;;
#! gap> IsAffineQuasigroupArithmeticForm( G, f, g, c ); # suitable for x^f*y^g*c
#! true
#! gap> AffineQuasigroup( G, f, g, c );
#! <quasigroup of size 10>
#! @EndExampleSession

#! <P/>Next over a group:

#! @BeginExampleSession
#! gap> G := DihedralGroup(12);;
#! gap> f := MappingByFunction( G, G, x->x^(G.1) );; g := MappingByFunction( G, G, x->x^(G.2) );; u := G.1;; v := G.2;;
#! gap> IsAffineQuasigroupArithmeticForm( G, f, u, g, v ); # suitable for (x^f*u)*(y^g*v)
#! true
#! gap> AffineQuasigroup( G, f, u, g, v ); 
#! <quasigroup of size 12>
#! @EndExampleSession

#! <P/>And finally over a loop. Note that the left translations (which are parent permutations) must be converted
#! into loop mappings in the example.

#! @BeginExampleSession
#! gap> G := AutomorphicLoop( 10, 1 );; # inner mappings are automorphisms here
#! gap> f := AsRightQuasigroupMapping( G, LeftInnerMapping( G, G.2, G.3 ) );;
#! gap> g := f*f;; u := G.1;; v := G.2;;
#! gap> IsAffineQuasigroupArithmeticForm( G, u, f, v, g ); # suitable for (u*x^f)*(v*y^g)
#! true 
#! gap> Q := AffineQuasigroup( G, u, f, v, g ); 
#! <quasigroup of size 10>
#! @EndExampleSession

# RIGHT QUASIGROUPS UP TO ISOTOPISM
# _____________________________________________________________________________

#! @Section Right quasigroups up to isotopism

# RQ_ArePossiblyIsotopicLoops( Q1, Q2 )
# Returns true if Q1, Q2 are possibly isotopic loops.
# Rhe function checks a few invariants of loops under isotopisms.
DeclareOperation( "RQ_ArePossiblyIsotopicLoops", [ IsLoop, IsLoop ] );

# RQ_ExtendIsotopismByClosingSource( f, g, h, tables1, tables2 )
# extends partial isotopism [f,g,h]
# each component is represented as a list m of length n, where m[i]=0 means m is not defined on i
# tables1 = [ multtable1, rdivtable1, ldivtable1 ], similarly for tables2
# MATH: [f,g,h] is determined by the values of f[i] for all i and g[1].
DeclareGlobalFunction( "RQ_ExtendIsotopismByClosingSource" );

# RQ_ExtendIsotopism( f, g, h, tables1, tables2, gens1 )
DeclareGlobalFunction( "RQ_ExtendIsotopism" );

# RQ_IsotopismAlgebras( category, Q1, Q2, viaPrincipalLoopIsotopes )
# returns an isotopism from <Q1> onto <Q2>, or fail.
DeclareOperation( "RQ_IsotopismAlgebras", [ IsOperation, IsRightQuasigroup, IsRightQuasigroup, IsBool ] );

#! @Arguments Q1, Q2
#! @Returns an isotopism from the right quasigroup <Arg>Q1</Arg> onto
#! the right quasigroup <Arg>Q2</Arg>, if it exists, else returns `fail`.
#! @Description NOTE: THERE IS NO METHOD FOR RIGHT QUASIGROUPS YET.
DeclareOperation( "IsotopismRightQuasigroups", [ IsRightQuasigroup, IsRightQuasigroup ] );

#! @Arguments Q1, Q2
#! @Returns an isotopism from the quasigroup <Arg>Q1</Arg> onto
#! the quasigroup <Arg>Q2</Arg>, if it exists, else returns `fail`.
DeclareOperation( "IsotopismQuasigroups", [ IsQuasigroup, IsQuasigroup ] );

#! @BeginExampleSession
#! gap> Q1 := RandomQuasigroup( 32 );;
#! gap> G := SymmetricGroup( 32 );;
#! gap> Q2 := QuasigroupIsotope( Q1, Random( G ), Random( G ), Random( G ) );;
#! gap> IsotopismQuasigroups( Q1, Q2 );
#! [ MappingByFunction( <quasigroup of size 32>, <quasigroup of size 32>, function( x ) ... end ),
#!   MappingByFunction( <quasigroup of size 32>, <quasigroup of size 32>, function( x ) ... end ),
#!   MappingByFunction( <quasigroup of size 32>, <quasigroup of size 32>, function( x ) ... end ) ]
#! gap> IsQuasigroupIsotopism( last );
#! true
#! @EndExampleSession

#! @Arguments Q1, Q2[, viaPrincipalLoopIsotopes]
#! @Returns an isotopism from the loop <Arg>Q1</Arg> onto the loop <Arg>Q2</Arg>,
#! if it exists, else returns `fail`.
#! @Description If the optional argument <Arg>viaPrincipalLoopIsotopes</Arg> is set to `true`, the method
#! constructs all principal loop isotopes `Q3` of <Arg>Q1</Arg>, one by one, and checks
#! for an isomorphism from `Q3` to <Arg>Q2</Arg>.
DeclareOperation( "IsotopismLoops", [ IsLoop, IsLoop ] );

#! @BeginExampleSession
#! gap> t := [ [1,2,3,4,5,6], [2,1,6,5,3,4], [3,4,5,2,6,1], [4,6,1,3,2,5], [5,3,4,6,1,2], [6,5,2,1,4,3] ];;
#! gap> Q1 := LoopByCayleyTable( t );;
#! gap> Q2 := LoopIsomorph( Q1, (3,4) );;
#! gap> Q3 := PrincipalLoopIsotope( Q2, Q2.5, Q2.6 );; # a loop isotopic to Q1
#! gap> IsotopismLoops( Q1, Q3 ); # add optional argument `true` to use a method via principal loop isotopes
#! [ MappingByFunction( <loop of size 6>, <loop of size 6>, function( x ) ... end ),
#!   MappingByFunction( <loop of size 6>, <loop of size 6>, function( x ) ... end ),
#!   MappingByFunction( <loop of size 6>, <loop of size 6>, function( x ) ... end ) ]
#! @EndExampleSession

#! @BeginGroup
#! @GroupTitle Right quasigroups up to isotopism

# REVISIT: Implement methods for right quasigrops and quasigroups

# auxiliary function ( category, ls, viaPrincipalLoopIsotopes )
# given a list <ls> of algebras of type <category>, returns a sublist of <ls> with algebras up to isotopism
DeclareOperation( "RQ_AlgebrasUpToIsotopism", [ IsOperation, IsList, IsBool ] );

#! @Arguments ls
#! @Returns a sublist of `ls` consisting of all right quasigroups (quasigroups, loops) in `ls` up to isotopism.
#! In case of loops, if the optional argument <Arg>viaPrincipalLoopIsotopes</Arg> is set to `true`, uses
#! a method based on principal loop isotopes.
#! @Description NOTE: THERE IS NO METHOD FOR RIGHT QUASIGROUPS YET.
DeclareOperation( "RightQuasigroupsUpToIsotopism", [ IsList ] );

#! @Arguments ls
DeclareOperation( "QuasigroupsUpToIsotopism", [ IsList ] );

#! @Arguments ls[, viaPrincipalLoopIsotopes]
DeclareOperation( "LoopsUpToIsotopism", [ IsList ] );

#! @EndGroup

# REVISIT: Add IsIsotopicToGroup function?

# AUTOTOPISM GROUPS OF RIGHT QUASIGROUPS
# _____________________________________________________________________________

#! @Section Autotopism groups of right quasigroups.

#! <P/>NOT IMPLEMENTED YET.

#! @BeginExampleSession
#! gap> q:=LoopByCayleyTable([[1,2,3,4,5 ],[2,1,4,5,3],[3,4,5,1,2],[4,5,2,3,1],[5,3,1,2,4]]);
#! <loop of size 5>
#! gap> f:=[(1,5,4), (2,4,3), (1,5,4)];
#! [ (1,5,4), (2,4,3), (1,5,4) ]
#! gap> atp:=AutotopismObject@RightQuasigroups(q,f[1],f[2],f[3]);
#! IsRightQuasigroupAutotopismObject((1,5,4), (2,4,3), (1,5,4))
#! gap> AmbientRightQuasigroup(atp);
#! <loop of size 5>
#! gap> One(atp);
#! IsRightQuasigroupAutotopismObject((), (), ())
#! gap> atp^-4;
#! IsRightQuasigroupAutotopismObject((1,4,5), (2,3,4), (1,4,5))
#! @EndExampleSession

#! @Arguments Q,f,g
DeclareOperation( "AutotopismFromPrincipalLoopIsotope", [ IsLoop, IsLoopElement, IsLoopElement ] );

#! @Arguments i,atp
DeclareOperation( "AtpOn3nElms@", [ IsPosInt, IsRightQuasigroupHomotopism ] );

#! @Arguments p,atp
DeclareOperation( "AtpOnnSquare@", [ IsList, IsRightQuasigroupHomotopism ] );

#! @Arguments gens
DeclareOperation( "AutotopismGroupByGenerators", [ IsList and IsRightQuasigroupHomotopismCollection ] );

#! @Arguments Q, gens, green, yellow, red
DeclareGlobalFunction( "ExtendAtpGrp" );

#! @Arguments Q
DeclareAttribute( "AutotopismGroup", IsLoop );

#! @BeginExampleSession
#! gap> Q := RightBolLoop(8,1);
#! <right Bol loop 8/1>
#! gap> AutotopismFromPrincipalLoopIsotope( Q, Q.4, Q.3 );
#! IsRightQuasigroupAutotopismObject((1,3)(2,4)(5,7)(6,8), (1,4)(2,3)(5,8), (1,2)(3,4)(5,6)(7,8))
#! gap> AutotopismGroup( Q );
#! <autotopism group with 5 generators>
#! gap> Size( last );
#! 128
#! @EndExampleSession

#! @Arguments Q
DeclareGlobalFunction( "LeftAtpInvariant@" );

#! @Arguments Q
DeclareGlobalFunction( "RightAtpInvariant@" );

#! @Arguments Q
DeclareAttribute( "AtpInvariant@", IsLoop );

#! @Arguments Q,S,a,b
DeclareGlobalFunction( "CheckAtpInvariant@" );
