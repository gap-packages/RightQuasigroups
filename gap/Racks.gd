# Racks.gd
# Racks and quandles
# =============================================================================

#! @Chapter Racks and quandles

# TOWARD RACKS AND QUANDLES
# _____________________________________________________________________________

# IsIdempotent already declared in GAP for an element
# DeclareProperty( "IsIdempotent", IsRightQuasigroup );

#! @Section Testing properties of racks and quandles

#! @BeginGroup
#! @GroupTitle IsRack and IsQuandle

#! @Arguments Q
#! @Returns `true` if the right quasigroup <Arg>Q</Arg> is a rack/quandle, else returns `false`.
DeclareProperty( "IsRack", IsRightQuasigroup );

#! @Arguments Q
DeclareProperty( "IsQuandle", IsRightQuasigroup );

#! @EndGroup

#! @BeginGroup
#! @GroupTitle IsHomogeneousRack and IsHomogeneousQuandle

#! @Arguments Q
#! @Returns `true` if the right quasigroup <Arg>Q</Arg> is a homogeneous rack/quandle,
#! that is, a rack/quandle on which the automorphism group acts transitively.
DeclareProperty( "IsHomogeneousRack", IsRightQuasigroup );

#! @Arguments Q
DeclareProperty( "IsHomogeneousQuandle", IsRightQuasigroup );

#! @EndGroup

#! @BeginGroup
#! @GroupTitle IsConnectedRack and IsConnectedQuandle

#! @Arguments Q
#! @Returns `true` if the right quasigroup <Arg>Q</Arg> is a connected rack/quandle,
#! that is, a rack/quandle on which the right multiplication group acts transitively.
DeclareProperty( "IsConnectedRack", IsRightQuasigroup );

#! @Arguments Q
DeclareProperty( "IsConnectedQuandle", IsRightQuasigroup );

#! @EndGroup

#! @BeginGroup
#! @GroupTitle IsLatinRack and IsLatinQuandle

#! @Arguments Q
#! @Returns `true` if the right quasigroup <Arg>Q</Arg> is a latin rack/quandle, else returns `false`.
DeclareProperty( "IsLatinRack", IsRightQuasigroup );

#! @Arguments Q
DeclareProperty( "IsLatinQuandle", IsRightQuasigroup );

#! @EndGroup

#! @BeginGroup
#! @GroupTitle IsProjectionRack and IsProjectionQuandle

#! @Arguments Q
#! @Returns `true` if the right quasigroup <Arg>Q</Arg> is a projection rack/quandle,
#! that is, a rack/quandle satisfying $x*y=x$.
DeclareProperty( "IsProjectionRack", IsRightQuasigroup );

#! @Arguments Q
DeclareProperty( "IsProjectionQuandle", IsRightQuasigroup );

#! @EndGroup

#! @BeginGroup
#! @GroupTitle IsPermutationalRack and IsPermutationalQuandle

#! @Arguments Q
#! @Returns `true` if the right quasigroup <Arg>Q</Arg> is a permutational rack/quandle,
#! that is, a rack/quandle satisfying $x*y=f(x)$ for some permutation $f$ of <Arg>Q</Arg>.
DeclareProperty( "IsPermutationalRack", IsRightQuasigroup );

#! @Arguments Q
DeclareProperty( "IsPermutationalQuandle", IsRightQuasigroup );

#! @EndGroup


#! @BeginGroup
#! @GroupTitle IsFaithfulRack and IsFaithfulQuandle

#! @Arguments Q
#! @Returns `true` if the right quasigroup <Arg>Q</Arg> is a faithful rack/quandle,
#! that is, a rack/quandle with $x\mapsto R_x$ injective on <Arg>Q</Arg>.
DeclareProperty( "IsFaithfulRack", IsRightQuasigroup );

#! @Arguments Q
DeclareProperty( "IsFaithfulQuandle", IsRightQuasigroup );

#! @EndGroup

# CONTRUCTORS FOR RACKS
# _____________________________________________________________________________

#! @Section Constructors for racks

#! <P/>The projection right quasigroup is automatically a quandle. We support `ProjectionRack` and
#! `ProjectionQuandle` as a synonym of `ProjectionRightQuasigroup`.

DeclareSynonym( "ProjectionRack", ProjectionRightQuasigroup );
DeclareSynonym( "ProjectionQuandle", ProjectionRightQuasigroup );

#! @Arguments n, f[, constructorStyle]
#! @Returns the permutational rack on `[1..`<Arg>n</Arg>`]` via the permutation <Arg>f</Arg>, that is, the
#! rack on `[1..`<Arg>n</Arg>`]` with multiplication `x*y = x^f`. The permutation <Arg>f</Arg> must restrict
#! to `[1..`<Arg>n</Arg>`]`.
#! @Description Note that for index based right quasigroups it is possible to change the underlying set
#! via `ChangeUnderlyingSet`, cf. Section <Ref Sect="Section_UnderlyingSet"/>.
DeclareOperation( "PermutationalRack", [ IsPosInt, IsPerm ] );

#! @BeginExampleSession
#! gap> PermutationalRack( 10, (3,4,5) );
#! <rack of size 10>
#! gap> Q := PermutationalRack( 100000, (1,100000), ConstructorStyle( false, true ) );
#! <rack of size 100000>
#! gap> Q[1]*Q[2];
#! r100000
#! @EndExampleSession

#! @Arguments  n [, constructorStyle]
#! @Returns the cyclic rack on `[1..`<Arg>n</Arg>`]`, that is, the
#! rack on `[1..`<Arg>n</Arg>`]` with multiplication `x*y = x+1`, where the addition is with wraparound. 
#! This is the same as permutational rack via the <Arg>n</Arg>-cycle `(1,2,...,`<Arg>n</Arg>`)`.
DeclareOperation( "CyclicRack", [ IsPosInt ] );

#! <P/>A rack is **affine**<Index>affine rack</Index> if it is an affine right quasigroup over an abelian group
#! (that happens to be a rack). See Section <Ref Sect="Section_Affine"/> for affine right quasigroups
#! and their arithmetic forms. For affine racks, we allow only arithmetic forms `(n,f,g,c)`, `(F,f,g,c)` and `(G,f,g,c)`.

#! <P/>If $G$ is an (additive) abelian group, $f$ an automorphism of $G$, $g$ an endomorphism of $G$ and $c\in G$,
#! then the affine right quasigroup on $G$ with arithmetic form $(G,f,g,c)$ and multiplication $x*y = f(x)+g(y)+c$
#! is a rack iff $g(c)=0$, $fg = gf$ and $g(f+g-I)$ is the zero mapping, where $I$ is the identity mapping on $G$.

#! <P/>In particular, if $n$ is a positive integer, $f$ is an integer relatively prime to $n$, and $g$, $c$ are integers,
#! then the affine right quasigroup on $[0..n-1]$ with arithmetic form $(n,f,g,c)$ and
#! multiplication $x*y = (f*x+g*y+c)\ \mathrm{mod}\ n$ is a rack iff $gc\equiv 0\pmod n$ and $g(f+g-1)\equiv 0\pmod n$.

#! @Arguments arg
#! @Returns `true` if <Arg>arg</Arg> is an arithmetic form of an affine rack. See above for allowed
#! arithmetic forms.
DeclareOperation( "IsAffineRackArithmeticForm", [ IsPosInt, IsInt, IsInt, IsInt ] );

#! @Arguments arg[, constructorStyle]
#! @Returns the affine rack with arithmetic form <Arg>arg</Arg>. See above for allowed
#! arithmetic forms.
DeclareGlobalFunction( "AffineRack" );

#! @BeginExampleSession
#! gap> # affine rack on [0..11]
#! gap> [ IsAffineRackArithmeticForm( 12, 5, 2, 6 ), AffineRack( 12, 5, 2, 6 ) ];
#! [ true, <rack of size 12> ]
#! gap> # affine rack on GF(9)
#! gap> F := GF(9);; f := 2*Z(9);; g := Z(9)+One(F);; c := Zero(F);;
#! gap> [ IsAffineRackArithmeticForm( F, f, g, c ), AffineRack( F, f, g, c ) ]; # latin racks are quandles
#! [ true, <latin quandle of size 9> ] 
#! gap> # affine rack on cyclic group of order 4
#! gap> x := (1,2,3,4);; G := Group( x );;
#! gap> f := GroupHomomorphismByImages( G, G, [x], [x^-1] );;
#! gap> g := GroupHomomorphismByImages( G, G, [x], [x^2] );;
#! gap> c := x^2;;
#! gap> [ IsAffineRackArithmeticForm( G, f, g, c ), AffineRack( G, f, g, c ) ];
#! [ true, <rack of size 4> ]
#! @EndExampleSession

# CONTRUCTORS FOR QUANDLES
# _____________________________________________________________________________

#! @Section Constructors for quandles

#! <P/>A quandle is **affine**<Index>affine quandle</Index> if it is an affine right quasigroup over an abelian group
#! (that happens to be a quandle). See Section <Ref Sect="Section_Affine"/> for affine right quasigroups
#! and their arithmetic forms. 

#! <P/>If $G$ is an (additive) abelian group, $f$ an automorphism of $G$, $g$ an endomorphism of $G$ and $c\in G$,
#! then the affine right quasigroup on $G$ with arithmetic form $(G,f,g,c)$ and multiplication $x*y = f(x)+g(y)+c$
#! is a quandle iff $c=0$ and $g=I-f$, where $I$ is the identity mapping on $G$.
#! Therefore, for affine quandles, we allow only "shortened" arithmetic forms `(n,f)`, `(F,f)` and `(G,f)`.

#! @Arguments arg
#! @Returns `true` if <Arg>arg</Arg> is an arithmetic form of an affine quandle. See above for allowed
#! arithmetic forms.
DeclareOperation( "IsAffineQuandleArithmeticForm", [ IsPosInt, IsInt ] );

#! @Arguments arg[, constructorStyle]
#! @Returns the affine quande with arithmetic form <Arg>arg</Arg>. See above for allowed
#! arithmetic forms. The synonym `AlexanderQuandle` is also supported.
DeclareGlobalFunction( "AffineQuandle" );

DeclareSynonym( "AlexanderQuandle", AffineQuandle );

#! @BeginExampleSession
#! gap> # affine quandle on [0..9]
#! gap> [ IsAffineQuandleArithmeticForm( 10, 3 ), AffineQuandle( 10, 3 ) ];
#! [ true, <quandle of size 10> ]
#! gap> # affine quandle on GF(9)
#! gap> [ IsAffineQuandleArithmeticForm( GF(9), 2*Z(9) ), AffineQuandle( GF(9), 2*Z(9) ) ];
#! [ true, <latin quandle of size 9> ]
#! gap> # affine quandle on cyclic group of order 5
#! gap> G := CyclicGroup(5);; f := Elements( AutomorphismGroup( G ) )[2];
#! [ f1 ] -> [ f1^2 ]
#! gap> [ IsAffineQuandleArithmeticForm( G, f ), AffineQuandle( G, f ) ];
#! [ true, <latin quandle of size 5> ]
#! @EndExampleSession

#! @Arguments n[, constructorstyle ]
#! @Returns the dihedral quandle of size <Arg>n</Arg>, that is, the quandle on `[0..`<Arg>n</Arg>`-1]`
#! with multiplication `x*y = (-x+2y) mod `<Arg>n</Arg>.
DeclareOperation( "DihedralQuandle", [ IsPosInt ] );

# RQ_CoreOfAlgebra( category, G, style )
DeclareOperation( "RQ_CoreOfAlgebra", [ IsObject, IsDomain, IsRecord ] );

#! @Arguments G[, constructorStyle]
#! @Returns the core of the group (resp. additive group) <Arg>G</Arg> defined by $x*y = (yx^{-1})y$
#! (resp. $x*y=y-x+y$). The core is always a quandle.
#! @Description Note: The value of `constructorStyle.checkArguments` of the optional argument `constructorStyle`
#! does not come into play and need not be specified.
DeclareOperation( "CoreOfGroup", [ IsGroup ] );

#! @Arguments Q[, constructorStyle]
#! @Returns the core of the right Bol loop <Arg>Q</Arg> defined by $x*y = (yx^{-1})y$.
#! The core is always a quandle.
#! @Description Note: The value of `constructorStyle.checkArguments` of the optional argument `constructorStyle`
#! does not come into play and need not be specified.
DeclareOperation( "CoreOfRightBolLoop", [ IsRightBolLoop ] );

#! @BeginExampleSession
#! gap> G := AlternatingGroup( 5 );
#! gap> Q := CoreOfGroup( G );
#! <quandle of size 60>
#! gap> Q[(1,2,3)]*Q[(1,2,3,4,5)] = Q[(1,2,3)*(1,2,3,4,5)^(-1)*(1,2,3)];
#! true
#! gap> CoreOfRightBolLoop( RightBolLoop(8,1) );
#! <quandle of size 8>
#! @EndExampleSession

#! <P/>Given a group $G$, subgroup $H$ and an automorphism $f$ of $G$ that centralizes $H$, 
#! the **Galkin quandle**<Index>Galkin quandle</Index> (aka **coset quandle** or **homogeneous quandle**)
#! is defined on the right cosets $\{Hx:x\in G\}$ by $Hx*Hy = H f(xy^{-1})y$. 

#! @BeginGroup
#! @GroupTitle GalkinQuandle and HomogeneousQuandle

#! @Arguments G, H, f[, constructorStyle]
#! @Returns the Galkin quandle constructed from the group <Arg>G</Arg>, subgroup <Arg>H</Arg>
#! and automorphism <Arg>f</Arg> of <Arg>G</Arg> that centralizes <Arg>H</Arg>.
#! The synonym `HomogeneousQuandle` is also supported.
DeclareOperation( "GalkinQuandle", [ IsGroup, IsGroup, IsMapping ] );

DeclareSynonym( "HomogeneousQuandle", GalkinQuandle );

#! @BeginExampleSession
#! gap> G := SymmetricGroup( 4 );; H := Subgroup( G, [(1,2)] );;
#! gap> f := Filtered( AutomorphismGroup( G ), g -> (1,2)^g = (1,2) )[3];
#! ^(3,4)
#! gap> Q := GalkinQuandle( G, H, f );
#! <quandle of size 12>
#! @EndExampleSession

#! @EndGroup

# RQ_ConjugationQuandle( category, S, m, style )
DeclareOperation( "RQ_ConjugationQuandle", [ IsObject, IsCollection, IsInt, IsRecord ] );

#! @Arguments G[, m[, constructorStyle] ]
#! @Returns the conjugation quandle on <Arg>G</Arg> defined by $x*y = y^{-m}*x*y^m$.
#! The argument <Arg>G</Arg> can either be a group or a collection of group elements closed under
#! the above operation.
#! If the optional argument <Arg>m</Arg> is omitted, the multiplication is given by $x*y=y^{-1}*x*y$.
DeclareGlobalFunction( "ConjugationQuandle" ); # PROG: too many possibilities of arguments

#! @BeginExampleSession
#! gap> G := SymmetricGroup( 3 );; 
#! gap> ConjugationQuandle( G ); # y^-1*x*y on G
#! <quandle of size 6>
#! gap> ConjugationQuandle( ConjugacyClass( G, (1,2,3) ) ); # y^-1*x*y on [ (1,2,3), (1,3,2) ]
#! <quandle of size 2>
#! gap> ConjugationQuandle( G, 2 ); # y^-2*x*y^2 on G
#! <quandle of size 6>
#! @EndExampleSession

# RACK AND QUANDLE ENVELOPES (JOYCE-BLACKBURN REPRESENTATION)
# _____________________________________________________________________________

#! @Section Rack envelopes and quandle envelopes: The Joyce-Blackburn representation

#! <P/>The triple $(G,S,R)$ is a **rack envelope**<Index>rack envelope</Index> if 
#! $G$ is a (permutation) group, $S$ is a set of orbit representatives of $G$, and $R=(r_x:x\in S)$
#! is a collection of elements of $G$ such that $r_x\in C_G(G_x)$ for every $x\in S$,
#! and $\langle \bigcup_{x\in S}r_x^G\rangle = G$. Here $r_x^G$ is the conjugacy class of $r_x$ in $G$.

#! <P/>The triple $(G,S,R)$ is a **quandle envelope**<Index>quandle envelope</Index> if
#! it is a rack envelope and $r_x(x)=x$ (that is, $r_x\in Z(G_x)$) for every $x\in S$.

#! <P/>There is a one-to-one correspondence between racks (quandles) and rack envelopes (quandle envelopes)
#! on a given set. This is sometimes called the **Joyce-Blackburn representation** of racks (quandles).
#! Given a rack/quandle $(Q,\cdot)$, let $G=\mathrm{Mlt}_r(Q)$ be the right multiplication group of $Q$,
#! $S$ a set of orbit representatives of $G$ on $Q$, and $R=(R_x:x\in S)$ (a collection of right transversals).
#! Then $(G,S,R)$ is a rack/quandle envelope. Conversely, given a rack/quandle envelope $(G,S,R)$ on a set $Q$
#! with $R=(r_x:x\in S)$, we can define right translations $R_y = r_x^{g_y} = g_y^{-1}r_xg_y$,
#! where $g_y$ is any element of $G$ such that $g_y(x)=y$. Then $(Q,\cdot)$ with multiplication $x\cdot y = R_y(x)$
#! is a rack/quandle.

#! <P/>In &RightQuasigroups;, a rack/quandle envelope is represented as a list `[G,S,R]`, where
#! `G` is a permutation group, `S` is a list of points (orbit representatives) and `R` is a list of
#! elements of `G`, one for each `x` in `S`. Functions that use rack/quandle envelopes accept
#! either a single argument `[G,S,R]` or three arguments `G`, `S`, `R`.

# RQ_IsRackOrQuandleEnvelope( category, G, reps, perms, reportErrors )
# checks if [ G, reps, perms ] is a rack/quandle envelope
# PROG: the permutations need not be canonical
DeclareOperation( "RQ_IsRackOrQuandleEnvelope", [ IsObject, IsGroup, IsList, IsList, IsBool ] );

#! @BeginGroup
#! @GroupTitle IsRackEnvelope and IsQuandleEnvelope

#! @Arguments G, reps, perms 
#! @Returns `true` if <Arg>G</Arg>, <Arg>reps</Arg>, <Arg>perms</Arg> is a rack envelope (resp. quandle envelope).
#! For the method to apply, <Arg>G</Arg> must be a group and <Arg>reps</Arg>, <Arg>perms</Arg> must be lists.
#! To return `true`, <Arg>G</Arg> must be a permutation group with orbit representatives <Arg>reps</Arg> and
#! <Arg>perms</Arg>`[i]` must be an element of $C_G(G_x)$ (resp. $Z(G_x)$), where `x=reps[i]`, and
#! the union of the conjugacy classes of the permutations in <Arg>perms</Arg> must generate <Arg>G</Arg>.
#! A version with a single argument `[`<Arg>G</Arg>`,`<Arg>reps</Arg>`,`<Arg>perms</Arg>`]` is also supported.
DeclareOperation( "IsRackEnvelope", [ IsGroup, IsList, IsList ] );

#! @Arguments G, reps, perms
DeclareOperation( "IsQuandleEnvelope", [ IsGroup, IsList, IsList ] );

#! @EndGroup

#! @BeginGroup
#! @GroupTitle RackEnvelope and QuandleEnvelope

#! @Arguments Q
#! @Returns the rack envelope (quandle envelope) of the rack (quandle) as a list `[G,reps,perms]`.
DeclareOperation( "RackEnvelope", [ IsRack ] );

#! @Arguments Q
DeclareOperation( "QuandleEnvelope", [ IsQuandle ] );

#! @EndGroup

#! @BeginGroup RackByRackEnvelope and QuandleByQuandleEnvelope

# RQ_RackOrQuandleByEnvelope( category, G, reps, perms, style )
# creates rack/quandle from the envelope [ G, reps, perms ]
DeclareOperation( "RQ_RackOrQuandleByEnvelope", [ IsObject, IsGroup, IsList, IsList, IsRecord ] );

#! @Arguments G, reps, perms[, constructorStyle]
#! @Returns the rack (quandle) corresponding to the rack envelope (quandle envelope)
#! `[`<Arg>G</Arg>`,`<Arg>reps</Arg>`,`<Arg>perms</Arg>`]`. The underlying set will be
#! the union of the orbits of <Arg>G</Arg> on the representatives <Arg>reps</Arg>.
#! The resulting rack (quandle) is connected if <Arg>G</Arg> has a single orbit.
#! A version with a single non-optional argument `[`<Arg>G</Arg>`,`<Arg>reps</Arg>`,`<Arg>perms</Arg>`]`
#! is also supported.
DeclareOperation( "RackByRackEnvelope", [ IsGroup, IsList, IsList ] );

#! @Arguments G, reps, perms[, constructorStyle]
DeclareOperation( "QuandleByQuandleEnvelope", [ IsGroup, IsList, IsList ] );

#! @EndGroup

#! @BeginExampleSession
#! gap> Q := SmallQuandle( 10, 1000 );
#! #I   - reading data file
#! <small quandle 10/1000>
#! gap> env := QuandleEnvelope( Q );
#! [ Group([ (1,3,2)(7,8) ]), [ 1, 4, 5, 6, 7, 9, 10 ],
#!   [ (), (), (1,2,3), (1,3,2)(7,8), (1,2,3), (1,3,2)(7,8), (1,3,2)(7,8) ] ]
#! gap> Q2 := QuandleByQuandleEnvelope( env );
#! <quandle of size 10>
#! gap> IsomorphismQuandles( Q, Q2 );
#! MappingByFunction( <small quandle 10/1000>, <quandle of size 10>, function( x ) ... end )
#! gap> AsParentTransformation( last );
#! IdentityTransformation
#! gap> QuandleByQuandleEnvelope( env[1], env[2], env[3] ); # separate arguments also supported for envelopes
#! <quandle of size 10>
#! @EndExampleSession

# SUBRACKS AND SUBQUANDLES
# _____________________________________________________________________________

#! @Section Subracks and subquandles

#! @BeginGroup
#! @GroupTitle Testing subracks and subquandles

#! @Arguments Q, S
DeclareOperation( "IsSubrack", [ IsRack, IsRack ] );

#! @Arguments Q, S
#! @Returns `true` if <Arg>S</Arg> is a subrack (subquandle) of the rack (quandle) <Arg>Q</Arg>, else returns `false`.
DeclareOperation( "IsSubquandle", [ IsQuandle, IsQuandle ] );

#! @EndGroup

#! @BeginGroup
#! @GroupTitle Creating subracks and subquandles

#! @Arguments Q, gens
DeclareOperation( "Subrack", [ IsRack, IsCollection ] );

#! @Arguments Q, gens
#! @Returns the subrack (subquandle) of a rack (quandle) <Arg>Q</Arg>
#! generated by the list of elements <Arg>gens</Arg>. We allow <Arg>gens</Arg> to consist of elements of <Arg>Q</Arg> or 
#! of elements of the underlying set of <Arg>Q</Arg>. 
#! The resulting subalgebra will be index based (cf. Section <Ref Sect="Section_IndexBased"/>) iff
#! <Arg>Q</Arg> is index based. 
DeclareOperation( "Subquandle", [IsQuandle, IsCollection ] );

#! @BeginExampleSession
#! gap> Q := AffineRack( 12, 5, 2, 6 );;
#! gap> S := Subrack( Q, [ Q[2] ] );
#! <rack of size 2>
#! gap> IsSubrack( Q, S );
#! true
#! @EndExampleSession

#! @EndGroup

#! @BeginGroup
#! @GroupTitle All subracks and subquandles

#! @Arguments Q
DeclareOperation( "AllSubracks", [ IsRack ] );

#! @Arguments Q
#! @Returns a list of all subracks (subquandles) of a rack (quandle) <Arg>Q</Arg>.
DeclareOperation( "AllSubquandles", [ IsQuandle ] );

#! @BeginExampleSession
#! gap> Q := ConjugationQuandle( SymmetricGroup( 3 ) );;
#! gap> List( AllSubquandles( Q ), Size );
#! [ 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 3, 5, 2, 4, 6, 3 ]
#! @EndExampleSession

#! @EndGroup

#! @BeginGroup
#! @GroupTitle Racks and quandles by generators 

#! <P/>These functions are analogous to those described in Section <Ref Sect="Section_ByGenerators"/>

#! @Arguments gens...
DeclareGlobalFunction( "Rack" );

#! @Arguments gens...
DeclareGlobalFunction( "Quandle" );

#! @Arguments gens...
DeclareGlobalFunction( "RackByGenerators" );

#! @Arguments gens...
DeclareGlobalFunction( "QuandleByGenerators" );

#! @Arguments gens...
DeclareGlobalFunction( "RackWithGenerators" );

#! @Arguments gens...
DeclareGlobalFunction( "QuandleWithGenerators" );

#! @BeginExampleSession
#! gap> Q := ConjugationQuandle( SymmetricGroup( 3 ) );;
#! gap> S := QuandleWithGenerators( [ Q[(1,2)], Q[(1,2,3)] ] );
#! <quandle of size 5>
#! gap> GeneratorsOfMagma( S );
#! [ r(1,2), r(1,2,3) ]
#! @EndExampleSession

#! @EndGroup

# ISOMORPHISMS AND ISOTOPISMS FOR RACKS AND QUANDLES
# _____________________________________________________________________________

#! @Section Isomorphisms and isotopisms of racks and quandles

#! @BeginGroup
#! @GroupTitle IsomorphismRacks, IsomorphismQuandles

#! @Arguments Q1, Q2
#! @Returns an isomorphism between the racks (quandles) <Arg>Q1</Arg> and <Arg>Q2</Arg>, if one exists,
#! else returns `fail`. If an isomorphism from <Arg>Q1</Arg> to <Arg>Q2</Arg> exists,
#! it is returned as a permutation of $[1..n]$, where $n$ is the size of <Arg>Q1</Arg>
#! (and hence also the size of <Arg>Q2</Arg>).
DeclareOperation( "IsomorphismRacks", [ IsRack, IsRack ] );

#! @Arguments Q1, Q2
DeclareOperation( "IsomorphismQuandles", [ IsQuandle, IsQuandle ] );

#!@EndGroup

#! @BeginGroup
#! @GroupTitle Racks and quandles up to isomorphism

#! @Arguments ls
#! @Returns a sublist of `ls` consisting of all racks (quandles) in `ls` up to isomorphism.
DeclareOperation( "RacksUpToIsomorphism", [ IsList ] );

#! @Arguments ls
DeclareOperation( "QuandlesUpToIsomorphism", [ IsList ] );

#!@EndGroup

# GROUPS ASSOCIATED WITH RACKS AND QUANDLES
# _____________________________________________________________________________

#! @Section Groups associated with racks and quandles

#! @Arguments Q
#! @Returns the adjoint group associated with the rack <Arg>Q</Arg>, that is, the free group
#! on <Arg>Q</Arg> with relations $b^{-1}ab = a*b$, where $a*b$ is the product in the rack
#! <Arg>Q</Arg>. The generators inherit names from the elements of <Arg>Q</Arg>.
DeclareAttribute( "AdjointGroup", IsRack ); 
# PROG: AdjointGroup is already declared in GAP as an attribute for finite radical algebras

#! @BeginExampleSession
#! gap> g := AdjointGroup( DihedralQuandle( 3 ) );
#! <fp group on the generators [ q0, q1 ]>
#! gap> GeneratorsOfGroup( g );
#! [ q0, q1 ]
#! gap> RelatorsOfFpGroup( g );
#! [ q0^-1*q1^2*q0^-1, (q1^-1*q0)^3 ]
#! gap> Size( g );
#! infinity
#! @EndExampleSession

##  IMPLIED FILTERS
##  ___________________________________________________________________________

InstallTrueMethod( IsSelfDistributive, IsRightSelfDistributive and IsLeftSelfDistributive );
InstallTrueMethod( IsSelfDistributive, IsRightSelfDistributive and IsCommutative );
InstallTrueMethod( IsSelfDistributive, IsLeftSelfDistributive and IsCommutative );
InstallTrueMethod( IsLeftSelfDistributive, IsSelfDistributive );
InstallTrueMethod( IsRightSelfDistributive, IsSelfDistributive );
InstallTrueMethod( IsRack, IsRightQuasigroup and IsRightSelfDistributive );
InstallTrueMethod( IsRightSelfDistributive, IsRack ); # this is useful when IsRack is set to true in constructor, say
InstallTrueMethod( IsQuandle, IsRack and IsIdempotent );
InstallTrueMethod( IsRack, IsQuandle );
InstallTrueMethod( IsIdempotent, IsQuandle );
InstallTrueMethod( IsLatinRack, IsRack and IsLeftQuasigroupMagma );
InstallTrueMethod( IsQuandle, IsLatinRack );
InstallTrueMethod( IsLatinQuandle, IsQuandle and IsLeftQuasigroupMagma );
InstallTrueMethod( IsLeftQuasigroupMagma, IsQuasigroup ); # PROG: Dangerous if one wants to test something?
InstallTrueMethod( IsConnectedRack, IsLatinRack );
InstallTrueMethod( IsConnectedQuandle, IsLatinQuandle );
InstallTrueMethod( IsHomogeneousRack, IsConnectedRack );
InstallTrueMethod( IsHomogeneousQuandle, IsConnectedRack );
InstallTrueMethod( IsProjectionRack, IsProjectionRightQuasigroup and IsRack );
InstallTrueMethod( IsProjectionQuandle, IsProjectionRightQuasigroup and IsQuandle );
InstallTrueMethod( IsProjectionRack, IsProjectionQuandle );
InstallTrueMethod( IsPermutationalQuandle, IsPermutationalRack and IsQuandle );
InstallTrueMethod( IsPermutationalRack, IsProjectionRack );
InstallTrueMethod( IsAssociative, IsPermutationalRack );
InstallTrueMethod( IsFaithfulRack, IsRack and IsFaithfulRightQuasigroup );
InstallTrueMethod( IsFaithfulQuandle, IsQuandle and IsFaithfulRightQuasigroup );
InstallTrueMethod( IsFaithfulRack, IsLatinRack );
InstallTrueMethod( IsFaithfulQuandle, IsLatinQuandle );