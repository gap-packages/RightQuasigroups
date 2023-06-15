# Topisms.gd
# Homotopisms, isotopisms and autotopisms of right quasigroups
# =============================================================================

#! @Chapter Homotopisms, isotopisms and autotopisms

# HOMOTOPISMS OF RIGHT QUASIGROUPS
# _____________________________________________________________________________

#! @Section Homotopisms, isotopism and autotopisms of right quasigroups


#! <P/>A triple of mappings $t = (f,g,h):(Q_1,\cdot)\to (Q_2,*)$ between right quasigroups is a **homotopism**
#! <Index Subkey="right quasigroups">homotopism</Index> if $f(x)*g(y) = h(x\cdot y)$
#! for every $x,y\in Q_1$. The right quasigroup $Q_1$ is the source of $t$, $Q_2$ is the range of $t$, and
#! the mappings $f$, $g$, $h$ are the components of $t$. If $f$, $g$, $h$ are also bijections, $t$ is
#! an **isotopism**<Index Subkey="right quasigroups">isotopism</Index>. An isotopism with
#! $(Q_1,\cdot) = (Q_2,*)$ is an **autotopism**<Index Subkey="right quasigroups">autotopism</Index>.

#! @BeginGroup 
#! @GroupTitle Representing homotopisms in &GAP;

#! @Arguments object
#! @Returns `true` or `false`.
#! @Description In &RightQuasigroups;, homotopisms are represented by a &GAP; category `IsRightQuasigroupHomotopism`
#! that stores attributes and whose objects can be multiplied and inverted. Homotopisms keep track of the source, the range,
#! and the three mappings. If the source is equal to the range, the mappins are stored as parent permutations, otherwise
#! they are stored as parent transformations.
DeclareCategory( "IsRightQuasigroupHomotopism", IsMultiplicativeElementWithInverse ); 
# PROG: Not every homotopism is invertible but we allow it as an option.


#! @EndGroup

DeclareCategoryCollections( "IsRightQuasigroupHomotopism" );
# InstallTrueMethod( IsGeneratorsOfMagmaWithInverses, IsRightQuasigroupHomotopismCollection );

DeclareRepresentation( "IsRightQuasigroupHomotopismRep",
    IsComponentObjectRep and IsMultiplicativeElementWithInverse, [ "source", "range", "f", "g", "h" ] 
);

BindGlobal( "RQ_HomotopismFamily", NewFamily( "RQ_HomotopismFamily", IsObject, IsRightQuasigroupHomotopism ) );
BindGlobal( "RQ_HomotopismType", NewType( RQ_HomotopismFamily, IsRightQuasigroupHomotopism and IsRightQuasigroupHomotopismRep ) );

#! @BeginGroup 
#! @GroupTitle Attributes of homotopisms

#! @Arguments t
DeclareAttribute( "Source", IsRightQuasigroupHomotopism );

#! @Arguments t
DeclareAttribute( "Range", IsRightQuasigroupHomotopism );

#! @Arguments t, i
#! @Returns the source, range and the <Arg>i</Arg>th component of a homotopism <Arg>t</Arg>, respectively. The attributes can
#! be also accessed directly via `t!.source`, `t!.range`, `t!.f`, `t!.g` and `t!.h`.
#! @Description The three components are stored as parent transformations or as parent permutations (when the source equals the range).
DeclareOperation( "ComponentOfHomotopism", [ IsRightQuasigroupHomotopism, IsInt ] );
#! @EndGroup

#! @BeginGroup
#! @GroupTitle Properties of homotopisms

#! @Arguments t
DeclareProperty( "IsInjective", IsRightQuasigroupHomotopism );

#! @Arguments t
DeclareProperty( "IsSurjective", IsRightQuasigroupHomotopism );

#! @Arguments t
#! @Description Returns `true` if (every component of) the homotopism <Arg>t</Arg> is injective, surjective and bijective, respectively.
DeclareProperty( "IsBijective", IsRightQuasigroupHomotopism );

#! @EndGroup

#! @BeginGroup 
#! @GroupTitle Creating homotopisms

#! @Arguments Q1, Q2, f, g, h[, isCanonical ]
#! @Returns a homotopism from right quasigroup <Arg>Q1</Arg> to right quasigroup <Arg>Q2</Arg>
#! according to the three mappings <Arg>f</Arg>, <Arg>g</Arg>, <Arg>h</Arg>.
#! Each of the mappings can be a right quasigroup mappings (with source <Arg>Q1</Arg> and range <Arg>Q2</Arg>),
#! or a transformation, or a permutation (where the latter only makes sense if <Arg>Q1</Arg> equals <Arg>Q2</Arg>).
#! If the optional argument <Arg>isCanonical</Arg> is set to `true`, all transformations/permutations
#! are understood as canonical (not parent) transformations/permutations.
#! The function performs an explicit check that the provided data give rise to a homotopism.
#! @Description We also support other formats of the arguments, namely `HomotopismRightQuasigroups( Q, f, g, h[, isCanonical])`
#! when `Q` is both the source and the range, and `HomotopismRightQuasigroups( f, g, h )`
#! when the three mappings are right quasigroup mappings (which keep track of the source and the range). Note that in
#! the last format the optional argument <Arg>isCanonical</Arg> is not supported because it is meaningless.
DeclareOperation( "HomotopismRightQuasigroups", [ IsMapping, IsMapping, IsMapping ] ); # constructor

#! <P/>Here is an example of a constructor for a homotopism:

#! @BeginExampleSession
#! gap> Q1 := ProjectionRightQuasigroup( 3 );;
#! gap> Q2 := ProjectionRightQuasigroup( 2 );;
#! gap> f := Transformation( [1,1,2] );; g := Transformation( [2,1,2] );; h := f;;
#! gap> t := HomotopismRightQuasigroups( Q1, Q2, f, g, h );
#! <homotopism of right quasigroups>
#! gap> IsRightQuasigroupHomotopism( t ); # category/filter check
#! true
#! gap> Display( t );
#! <homotopism of right quasigroups
#!    source = <associative quandle of size 3>
#!    range = <associative quandle of size 2>
#!    f = Transformation( [ 1, 1, 2 ] )
#!    g = Transformation( [ 2, 1, 2 ] )
#!    h = Transformation( [ 1, 1, 2 ] )
#! >
#! gap> [ Source( t ), Range( t ) ];
#! [ <associative quandle of size 3>, <associative quandle of size 2> ]
#! gap> ComponentOfHomotopism( t, 2 ); # the second component, g
#! Transformation( [ 2, 1, 2 ] )
#! @EndExampleSession

#! <P/>And here is an example of a constructor for a homotopism with the same source and range.
#! The homotopism is immediately recognized to be an autotopism since the three given
#! mappings are permutations.

#! @BeginExampleSession
#! gap> Q := QuasigroupByFunction( [0..4], function( x,y ) return (x+y) mod 5; end );;
#! gap> f := (1,2,3,4,5);; g := (2,4,1,3,5);; h := (3,1,4,2,5);; # +1, +2, +3
#! gap> t := HomotopismRightQuasigroups( Q, f, g, h );
#! <autotopism of quasigroups>
#! gap> Display( t );
#! <autotopism of quasigroups
#!    source = range = <quasigroup of size 5>
#!    f = (1,2,3,4,5)
#!    g = (1,3,5,2,4)
#!    h = (1,4,2,5,3)
#! >
#! @EndExampleSession

#! @EndGroup

#! @BeginGroup
#! @GroupTitle Creating autotopisms

#! @Arguments Q, f, g, h[, isCanonical ]
#! @Returns an autotopism of a right quasigroup <Arg>Q</Arg>
#! according to the three bijections <Arg>f</Arg>, <Arg>g</Arg>, <Arg>h</Arg>.
#! The function differs from `HomotopismRightQuasigroups( Q, f, g, h )` in that it explicitly checks 
#! that the provided mappings are bijective. The format of the mappings and the optional argument
#! `isCanonical` behave as for `HomotopismRightQuasigroups`.
DeclareGlobalFunction( "AutotopismRightQuasigroup" ); 

#! @EndGroup

#! @BeginGroup 
#! @GroupTitle Operations with homotopisms

#! <P/>Homotopism can be compared, multiplied (composed from left to right) and inverted (if they are bijective).

#! @Arguments Q
#! @Returns the identity autotopism on the right quasigroup <Arg>Q</Arg>. The identity autotopism can also be obtained
#! by calling `HomotopismRightQuasigroups( Q, (), (), () )`. Finally, given any homotopism `t` with source `Q`,
#! the method `One( t )` returns the identity autotopism on `Q`.
DeclareOperation( "IdentityAutotopism", [ IsRightQuasigroup ] );

#! @BeginExampleSession
#! gap> Q := QuasigroupByFunction( [0..4], function( x, y ) return (x+y) mod 5; end );;
#! gap> f := RightTranslation( Q, 1 );; g := LeftTranslation( Q, 2 );; h := RightTranslation( Q, 3 );;
#! gap> t := HomotopismRightQuasigroups( Q, f, g, h );;
#! gap> Display( t*t );
#! <autotopism of quasigroups
#!    source = range = <quasigroup of size 5>
#!    f = (1,3,5,2,4)
#!    g = (1,5,4,3,2)
#!    h = (1,2,3,4,5)
#! >
#! gap> Display( t^-1 );
#! <autotopism of quasigroups
#!    source = range = <quasigroup of size 5>
#!    f = (1,5,4,3,2)
#!    g = (1,4,2,5,3)
#!    h = (1,3,5,2,4)
#! >
#! gap> One( t );
#! <identity autotopism>
#! gap> Display( last );
#! <identity autotopism on <quasigroup of size 5>>
#! @EndExampleSession

#! @EndGroup

#! @BeginGroup 
#! @GroupTitle Testing homotopism data

#! @Arguments Q1, Q2, f, g, h[, isCanonical]
DeclareGlobalFunction( "IsHomotopismRightQuasigroups" );

#! @Arguments Q1, Q2, f, g, h[, isCanonical]
DeclareGlobalFunction( "IsHomotopismQuasigroups" );

#! @Arguments Q1, Q2, f, g, h[, isCanonical]
#! @Returns `true` if the data provided in the arguments gives rise to a homotopism of right quasigroups (quasigroups, loops).
#! Accepts the same type of arguments as the constructor `HomotopismRightQuasigroups`.
DeclareGlobalFunction( "IsHomotopismLoops" );

#! @BeginGroup 
#! @GroupTitle Testing isotopism data

#! @Arguments Q1, Q2, f, g, h[, isCanonical]
DeclareGlobalFunction( "IsIsotopismRightQuasigroups" );

#! @Arguments Q1, Q2, f, g, h[, isCanonical]
DeclareGlobalFunction( "IsIsotopismQuasigroups" );

#! @Arguments Q1, Q2, f, g, h[, isCanonical]
#! @Returns `true` if the data provided by in the arguments gives rise to an isotopism of right quasigroups (quasigroups, loops).
#! Accepts the same type of arguments as the constructor `HomotopismRightQuasigroups`.
DeclareGlobalFunction( "IsIsotopismLoops" );

#! @EndGroup

#! @BeginGroup 
#! @GroupTitle Testing autotopism data

#! @Arguments Q, f, g, h[, isCanonical]
DeclareGlobalFunction( "IsAutotopismRightQuasigroups" );

#! @Arguments Q, f, g, h[, isCanonical]
DeclareGlobalFunction( "IsAutotopismQuasigroups" );

#! @Arguments Q, f, g, h[, isCanonical]
#! @Returns `true` if the data provided by in the arguments gives rise to an isotopism of right quasigroups (quasigroups, loops).
#! Accepts the same type of arguments as the constructor `HomotopismRightQuasigroups` with the same source and range.
DeclareGlobalFunction( "IsAutotopismLoops" );

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
#! <Item>The arguments <Arg>f</Arg>, <Arg>g</Arg>, <Arg>h</Arg> are mandatory and each can be given as
#! a right quasigroup mapping, a transformation or a permutation.
#! The arguments <Arg>f</Arg> and <Arg>h</Arg> must always give rise to bijections. If a quasigroup is supposed
#! to be returned, then also <Arg>g</Arg> must give rise to a bijection. Finally, if a loop is supposed to be returned then
#! $g^{-1}(f(x)\backslash h^{-1}(x))$ must be equal to $f^{-1}(h^{-1}(x)/g(x))$ and independent of $x$.</Item>
#! <Item> If <Arg>f</Arg> is given as a transformation/permutation, the argument <Arg>Q</Arg> is also required,
#! and it must be a right quasigroup, quasigroup or loop. The returned algebra will have the same
#! underlying set as <Arg>Q</Arg>. If <Arg>f</Arg> is given as a right quasigroup mapping,
#! its source will be used as <Arg>Q</Arg>.</Item>
#! <Item>Any subset of the two optional arguments <Arg>isCanonical</Arg> and <Arg>constructorStyle</Arg>
#! can be given.</Item>
#! <Item>If the optional argument <Arg>isCanonical</Arg> is given and set to `true`, the permutation/transformation
#! <Arg>f</Arg> is interpreted as a canonical permutation/transformation,
#! else it is by default interpreted as parent permutation/transformation.
#! (See Chapter <Ref Chap="Chapter_Mappings"/>.) </Item>
#! <Item>See Section <Ref Sect="Section_OptionalArguments"/> for the optional argument <Arg>constructorStyle</Arg>.</Item>
#! </List>

#! @BeginGroup
#! @GroupTitle RightQuasigroupTwist, QuasigroupTwist and LoopTwist 

# RQ_AlgebraTwistByParentTransformations( category, Q, f, g, h, style )
# Auxiliary. This is the main constructor here.
DeclareOperation( "RQ_AlgebraTwistByParentTransformations",
    [ IsOperation, IsRightQuasigroup, IsTransformation, IsTransformation, IsTransformation, IsRecord ] );

# RQ_AlebraTwist( category, arg )
# arg is expected as Q, f, g, h [, isCanonical, style] or as Q, [f,g,h][, isCanonical, style]
# Auxiliary. Processes a wide range of arguments and calls RQ_AlgebraTwistByParentTransformations.
DeclareOperation( "RQ_AlgebraTwist", [ IsOperation, IsList ] );

#! @Arguments [Q,] f, g, h[, isCanonical, constructorStyle]
DeclareGlobalFunction( "RightQuasigroupTwist" );

#! @Arguments [Q,] f, g, h[, isCanonical, constructorStyle]
DeclareGlobalFunction( "QuasigroupTwist" );

#! @Arguments [Q,] f, g, h[, isCanonical, constructorStyle]
#! @Returns the twist of the right quasigroup <Arg>Q</Arg> via <Arg>f</Arg>, <Arg>g</Arg>, <Arg>h</Arg>,
#! that is, the right quasigroup (quasigroup, loop) on the underlying set of <Arg>Q</Arg> with multiplication $x*y = h(f(x)g(y))$.
#! See above for conventions on the arguments.
DeclareGlobalFunction( "LoopTwist" );

#! @BeginExampleSession
#! gap> Q := MoufangLoop( 12, 1 );;
#! gap> f := (1,2,3);; g := Transformation( [3,3,3] );; h := MappingByFunction( Q, Q, x->x^-1 );; # note various formats
#! gap> T := RightQuasigroupTwist( Q, f, g, h ); # f and h must be bijective 
#! <right quasigroup of size 12>
#! gap> QuasigroupTwist( Q,  f, (1,12), h ); # g must be bijective for quasigroups
#! <quasigroup of size 12>
#! gap> f := RightTranslation( Q, Q.2 )^-1;; g := LeftTranslation( Q, Q.3 )^-1;;
#! gap> LoopTwist( Q, f, g, (), ConstructorStyle( true, true ) ); # principal loop isotope
#! <loop of size 12>
#! @EndExampleSession

#! @EndGroup

# ISOTOPES OF RIGHT QUASIGROUPS
# _____________________________________________________________________________

#! @Section Isotopes of right quasigroups

#! <P/>If $t=(f,g,h):(Q,\cdot)\to (Q,*)$ is an isotopism, then $x*y = h(f^{-1}(x)\cdot g^{-1}(y))$ for
#! all $x,y\in Q$, and $(Q,*)$ is called an **isotope**<Index>isotope</Index> of $(Q,\cdot)$ via $f$, $g$, $h$.
#! (Note that isotopes are special cases of twists, as introduced in Section <Ref Sect="Section_Twists"/>.)
#! The isotope $(Q,*)$ is a (right) quasigroup iff $(Q,\cdot)$ is a (right) quasigroup, but an isotope
#! of a loop might not be a loop. 

#! <P/>Given a quasigroup $(Q,\cdot,/,\backslash)$ and $a,b\in Q$, the <Index>principal loop isotope</Index>
#! **principal loop isotope** via $a,b$ is defined as $(Q,\circ)$, where $x\circ y = (x/a)\cdot(b\backslash y)$.
#! The principal loop isotope is automatically a loop with neutral element $b\cdot a$.

# auxiliary function (category, data ), data = [ Q, f, g, h, [, isCanonical, constructorStyle ] ]
DeclareOperation( "RQ_AlgebraIsotope", [ IsOperation, IsList ] );

#! @Arguments [Q,] f, g, h[, isCanonical, constructorStyle ]
#! @Returns the isotope of the right quasigroup <Arg>Q</Arg> via isotopism <Arg>f</Arg>, <Arg>g</Arg>, <Arg>h</Arg>,
#! that is, the right quasigroup on the underlying set of <Arg>Q</Arg> with multiplication $x*y = h(f^{-1}(x)g^{-1}(y))$.
#! See Section <Ref Sect="Section_Twists"/> for conventions on the arguments.
DeclareGlobalFunction( "RightQuasigroupIsotope" );

#! @Arguments [Q,] f, g, h[, isCanonical, constructorStyle ]
#! @Returns the isotope of the quasigroup <Arg>Q</Arg> via isotopism <Arg>f</Arg>, <Arg>g</Arg>, <Arg>h</Arg>,
#! that is, the quasigroup on the underlying set of <Arg>Q</Arg> with multiplication $x*y = h(f^{-1}(x)g^{-1}(y))$.
#! See Section <Ref Sect="Section_Twists"/> for conventions on the arguments.
DeclareGlobalFunction( "QuasigroupIsotope" );

#! @Arguments [Q,] f, g, h[, isCanonical, constructorStyle ]
#! @Returns the loop isotope of the quasigroup <Arg>Q</Arg> via isotopism <Arg>f</Arg>, <Arg>g</Arg>, <Arg>h</Arg>,
#! that is, the loop on the underlying set of <Arg>Q</Arg> with multiplication $x*y = h(f^{-1}(x)g^{-1}(y))$.
#! See Section <Ref Sect="Section_Twists"/> for conventions on the arguments.
#! @Description Warning: Unless the constructor style specifies that arguments should be checked, they will
#! not be checked and the returned algebra might not have an identity element despite being declared a loop.
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
#! <Item>The three variations `(G,f,u,v,g)`, `(G,u,f,g,v)` and `(G,u,f,v,g)` are also supported.</Item>
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

#! <P/>We support four methods for calculating isotopisms between loops, quasigroups and right quasigroups $Q_1$, $Q_2$:
#! <List>
#! <Item> "via perfect matchings with invariants" (default method for right quasigroups):
#! considers all possible $f$ that preserve certain invariants of right quasigroups, then completes the isotopism
#! by finding a perfect matching in a bipartite graph,</Item>
#! <Item> "via perfect matchings with automorphism group": considers all possible $f$ modulo the automorphism group of $Q1$,
#! then employs perfect matchings as above,</Item>
#! <Item> "via domain extension" (for quasigroups and loops only, default method for quasigroups and loops):
#! attempts to find an isotopism $(f,g,h)$ by iteratively enlarging the domains of $f$, $g$ and $h$,</Item>
#! <Item> "via principal loop isotopes" (for loops only): constructs principal loop isotopes of $Q1$
#! one by one and checks if any one of them is isomrphic to $Q2$.</Item>
#! </List>

#! @Arguments Q
#! @Returns a certain invariant $m$ of the right quasigroup <Arg>Q</Arg> that is preserved by isotopisms.
#! (Strictly speaking, only the sorted version of $m$ is preserved under isotopisms, see `AreEqualIsotopismDscriminators`.)
#! In more detail, $m$ is the list $(m_x:x\in Q)$, $m_x$ is the sorted list $(m_{x,y}:y\in Q)$, and $m_{x,y}$
#! is the number of occurrences of $y$ in the row indexed by $x$.
DeclareAttribute( "IsotopismDiscriminator", IsRightQuasigroup );

#! @BeginExampleSession
#! gap> Q := RightQuasigroupByCayleyTable([[1,1,1],[2,3,2],[3,2,3]]);;
#! gap> Display(IsotopismDiscriminator( Q ) );
#! [ [  0,  0,  3 ],
#!   [  0,  1,  2 ],
#!   [  0,  1,  2 ] ]
#! @EndExampleSession

#! @Arguments D1, D2
#! @Returns `true` when the two isotopism discriminators <Arg>D1</Arg>, <Arg>D2</Arg> calculated via
#! `IsotopismDiscriminator` are the same. When `false` is returned, it is guaranteed that
#! the corresponding right quasigroups are not isotopic.
DeclareOperation( "AreEqualIsotopismDiscriminators", [ IsList, IsList ] );

#! @Arguments Q1, Q2
#! @Returns `false` if the method determined that <Arg>Q1</Arg>, <Arg>Q2</Arg> are not isotopic right 
#! quasigroups, based on their isotopism discrininators (and some additional invariants in the case of loops).
#! When `true` is returned, the two right quasigroups might be isotopic.
DeclareOperation( "ArePossiblyIsotopicRightQuasigroups", [ IsRightQuasigroup, IsRightQuasigroup ] );

# RQ_PerfectBipartiteMatching( A )
# returns a perfect matching in the bipartite graph determined by the square matrix A
# A[i,j]=1 iff there is an edge between U[i] and V[j], where U,V are the two parts (of equal size).
# If no perfect matching exists, fail is returned.
# This is the standard Hall's algorithm.
DeclareOperation( "RQ_PerfectBipartiteMatching", [ IsMatrix ] );

# What follows are some general methods for selecting from blocks B1, ..., Bn of size b1, ..., bn
# in all possible ways and in all possible bijective ways.
# The "selector" (odometer-like) starts at [1,...,1] and goes all the way to [b1,...,bn] 

# RQ_Selector_Increment( selector, blocks )
# Increments and returns the selector. If it is initialized as [0,...0], the next state will be [1,...1].
# If it overflows, `fail` is returned.
DeclareOperation( "RQ_Selector_Increment", [ IsList, IsList ] ); 

# RQ_Selector_FirstConflictWithBijectivity( selector, blocks )
# Returns the first position p in the selector [x1,...,xn] such that
# blocks[1][x1], ..., blocks[p][xp] has a repetition. If there is no repetition, returns fail.
DeclareOperation( "RQ_Selector_FirstConflictWithBijectivity", [ IsList, IsList ] );

# RQ_Selector_NextBijective( selector, blocks )
# Returns the first state after <selector> that results ina  bijective selection from the blocks
DeclareOperation( "RQ_Selector_NextBijective", [ IsList, IsList ] );

# this is the end of the selector methods

# RQ_IsotopismRighQuasigroupsPM( Q1, Q2, method ) 
# returns an isotopism (f,g,h): Q1 --> Q2
# In the method "via perfect matchings with invariants", it first caclulates blocks of Q1 that f must preserve,
# while in he method "via perfect matchings with automorphism group", it first calculates the automorphism group of Q2.
# It tries all choices of f (preserving the blocks, or modulo Aut(Q2)), then all choices of g[1],
# completes h, and then completes g by finding a perfect matching, if possible.
DeclareOperation( "RQ_IsotopismRightQuasigroupsPM", [ IsRightQuasigroup, IsRightQuasigroup, IsString ] );  

# now for some auxiliary functions for domain extension of isotopisms

# RQ_ExtendIsotopismByClosingSource( f, g, h, tables1, tables2 )
# extends partial isotopism [f,g,h]
# each component is represented as a list m of length n, where m[i]=0 means m is not defined on i
# tables1 = [ multtable1, rdivtable1, ldivtable1 ], similarly for tables2
# MATH: [f,g,h] is determined by the values of f[i] for all i and g[1].
DeclareGlobalFunction( "RQ_ExtendIsotopismByClosingSource" );

# RQ_ExtendIsotopism( f, g, h, tables1, tables2, gens1 )
DeclareGlobalFunction( "RQ_ExtendIsotopism" );

# RQ_IsotopismQuasigroupsDE( Q1, Q2 )
# this is the isotopism search "via domain extension"
DeclareOperation( "RQ_IsotopismQuasigroupsDE", [ IsRightQuasigroup, IsRightQuasigroup ] );

# RQ_IsotopismLoopsPLI
# this is the isotopism search "via principal loop isotopes"
DeclareOperation( "RQ_IsotopismLoopsPLI", [ IsLoop, IsLoop ] );

#! @Arguments Q1, Q2[, method]
#! @Returns an isotopism from the right quasigroup <Arg>Q1</Arg> onto
#! the right quasigroup <Arg>Q2</Arg>, if it exists, else returns `fail`.
#! The optional argument <Arg>method</Arg> must be set to one of the values
#! "via perfect matchings with invariants", "via perfect matchings with automorphism group",
#! "via domain extension" or "via principal loop isotopes". If no method is provided by the user,
#! "via "via domain extension" for loops and quasigroups, while
#!  "via perfect matchings with invariants" for right quasigroups.
DeclareOperation( "IsotopismRightQuasigroups", [ IsRightQuasigroup, IsRightQuasigroup, IsString ] );

#! @BeginExampleSession
#! gap> Q1 := RandomRightQuasigroup( 30 );;
#! gap> Q2 := RightQuasigroupIsotope( Q1, (1,2,3), (10,20,30), (4,30) );;
#! gap> t := IsotopismRightQuasigroups( Q1, Q2 );
#! <isotopism of right quasigroups>
#! gap> Q3 := RightQuasigroupIsotope( Q1, t!.f, t!.g, t!.h );;
#! gap> MultiplicationTable( Q2 ) = MultiplicationTable( Q3 );
#! true
#! @EndExampleSession

#! @Arguments Q1, Q2[, method]
#! @Returns an isotopism from the quasigroup <Arg>Q1</Arg> onto
#! the quasigroup <Arg>Q2</Arg>, if it exists, else returns `fail`.
#! The only difference from `IsotopismRightQuasigroups` is that the arguments <Arg>Q1</Arg>, <Arg>Q2</Arg>
#! must be quasigroups.
DeclareOperation( "IsotopismQuasigroups", [ IsQuasigroup, IsQuasigroup, IsString ] );

#! @Arguments Q1, Q2[, method]
#! @Returns an isotopism from the loop <Arg>Q1</Arg> onto
#! the loopp <Arg>Q2</Arg>, if it exists, else returns `fail`.
#! The only difference from `IsotopismRightQuasigroups` is that the arguments <Arg>Q1</Arg>, <Arg>Q2</Arg>
#! must be loops.
DeclareOperation( "IsotopismLoops", [ IsLoop, IsLoop, IsString ] );

#! @BeginExampleSession
#! gap> Q1 := MoufangLoop( 32, 10 );
#! MoufangLoop( 32, 10 )
#! gap> Q2 := LoopIsomorph( Q1, (3,4) );;
#! gap> Q3 := PrincipalLoopIsotope( Q2, Q2.10, Q2.20 );; # a loop isotopic to Q1
#! gap> IsotopismLoops( Q1, Q3 ); 
#! <isotopism of loops>
#! @EndExampleSession

#! @BeginGroup
#! @GroupTitle Right quasigroups up to isotopism

# RQ_AlgebrasUpToIsotopism( category, ls, viaPrincipalLoopIsotopes )
# given a list <ls> of algebras of type <category>, returns a sublist of <ls> with algebras up to isotopism
DeclareOperation( "RQ_AlgebrasUpToIsotopism", [ IsOperation, IsList, IsString ] );

#! @Arguments ls[, method]
#! @Returns a sublist of `ls` consisting of all right quasigroups (quasigroups, loops) in `ls` up to isotopism.
#! In case of loops, if the optional argument <Arg>viaPrincipalLoopIsotopes</Arg> is set to `true`, uses
#! a method based on principal loop isotopes.
DeclareOperation( "RightQuasigroupsUpToIsotopism", [ IsList, IsString ] );

#! @Arguments ls[, method]
DeclareOperation( "QuasigroupsUpToIsotopism", [ IsList, IsString ] );

#! @Arguments ls[, method]
DeclareOperation( "LoopsUpToIsotopism", [ IsList, IsString ] );

#! @EndGroup

# AUTOTOPISM GROUPS OF RIGHT QUASIGROUPS
# _____________________________________________________________________________

#! @Section Autotopism groups of right quasigroups.

# RQ_HtpOnPairs( ls, t )
# applies the first two components of the right quasigroup homotopism t to the list ls of length 2
DeclareOperation( "RQ_HtpOnPairs", [ IsList, IsRightQuasigroupHomotopism ] );

#! <P/>The autotopism group of a right quasigroup consist of abstract homotopism &GAP; objects. While the group 
#! operations are defined for them, the computation of size, center and other important group theoretical 
#! concepts is very slow. In this case, &GAP; recommends to use a `NiceMonomorphism` into a group $G$, whose 
#! representation allows fast generic methods. For autotopisms of a right quasigroup $Q$, $G$ is the 
#! symmetric group on $3n$ points, where $n$ is the size of the parent of $Q$. If the triple $(f,g,h)$
#! defines an autotopism of $Q$, then its nice monomorphic image acts on $\{1,\ldots,n\}$ as the parent
#! permutation of $f$, on $\{n+1,\ldots,2n\}$ as the conjugate of the parent permutation of $g$, and on 
#! $\{2n+1,\ldots,3n\}$ as the conjugate of the parent permutation of $h$.


#! @Arguments t
#! @Returns a permutation of degree $3n$, where $n$ is the size of the parent right quasigroup
#! of the source of <Arg>t</Arg>. 
#! @Description The input is a <C>HomotopismRightQuasigroups</C> object. This map is a nice
#! monomorphism for this category.
DeclareGlobalFunction( "RQ_NiceMonomorphism" );

# Input: a parent permutation of degree 3*n
# Output: a triple of permutations of degree n
#! @Arguments Q
#! @Returns the inverse function $f$ of the nice monomorphism <C>HomotopismRightQuasigroups</C> objects
#! whose source is the right quasigroup <Arg>Q</Arg>. The function $f$ maps permutations of degree $3n$ to 
#! isotopisms of <Arg>Q</Arg>, where $n$ is the size of the parent of <Arg>Q</Arg>. 
DeclareGlobalFunction( "RQ_NiceMonomorphismInverse" );

#! @Arguments Q,gens
#! @Returns a group consisting of <C>HomotopismRightQuasigroups</C> objects. This group has a
#! a nice monomorphism to a permutation group of degree $3n$ where $n$ is the order of the 
#! parent group of <Arg>Q</Arg>.
DeclareOperation( "RQ_AutotopismGroupByGeneratorsNC", [ IsRightQuasigroup, IsRightQuasigroupHomotopismCollection ] );

#! @Arguments Q
#! @Returns the autotopism group of a loop <Arg>Q</Arg>.
#! Note: There is no generic method implemented yet for right quasigroups and quasigroups.
DeclareAttribute( "AutotopismGroup", IsRightQuasigroup );

#! @BeginExampleSession
#! gap> q := RightBolLoop( 16, 3 );
#! RightBolLoop( 16, 3 )
#! gap> ag := AutotopismGroup( q );
#! <group with 5 generators>
#! gap> Size( ag );
#! 768
#! @EndExampleSession
