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
#! @GroupTitle IsHomegeneousRack and IsHomogeneousQuandle

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
#! that is, a rack/quandle with $x\mapsto R_x$ injective <Arg>Q</Arg>.
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

#! @Arguments S, f[, constructorStyle]
#! @Returns the permutational rack on <Arg>S</Arg> via the permutation <Arg>f</Arg>, that is, the
#! rack on <Arg>S</Arg> with multiplication `x*y = x^f`. The permutation <Arg>f</Arg> must restrict
#! to <Arg>S</Arg>.
DeclareOperation( "PermutationalRack", [ IsCollection, IsPerm ] );

# CONTRUCTORS FOR QUANDLES
# _____________________________________________________________________________

#! @Section Constructors for quandles

# RQ_AffineQuandle( A, f, g, mult, style ) # g = 1-f to check latin property
DeclareOperation( "RQ_AffineQuandle", [ IsDomain, IsFunction, IsFunction, IsFunction, IsRecord ] );

#! @Arguments S, f[, constructorStyle]
#! @Returns the affine quandle on <Arg>S</Arg> via <Arg>f</Arg>. Supported arguments are:
#! <Arg>S</Arg> a group and <Arg>f</Arg> its automorphism (in which case $x*y = f(x)*y*f(y)^{-1}$ and
#! the resulting quandle is latin iff $y\mapsto y*f(y)^{-1}$ is bijective),
#! <Arg>S</Arg> an additive group and <Arg>f</Arg> its automorphism (in which case $x*y = f(x)+y-f(y)$ and
#! the resulting quandle is latin iff $y\mapsto y-f(y)$ is bijective),
#! <Arg>S</Arg> a field and <Arg>f</Arg> its nonzero element (in which case $x*y = f*x+(1-f)*y$ and
#! the resulting quandle is latin iff $1-f$ is a nonzero element of <Arg>S</Arg>),
#! <Arg>S</Arg> a positive integer and <Arg>f</Arg> a positive integer relatively prime to <Arg>S</Arg>
#! (in which case $x*y = f*x+(1-f)*y$ on `[0..`<Arg>S</Arg>`]` and 
#! the resulting quandle is latin iff $1-f$ is relatively prime modulo <Arg>S</Arg>).
#! @Description The synonym `AlexanderQuandle` is supported.
DeclareOperation( "AffineQuandle", [ IsGroup, IsMapping ] );

DeclareSynonym( "AlexanderQuandle", AffineQuandle );

#! @BeginExampleSession
#! gap> G := CyclicGroup(5);; f := Elements(AutomorphismGroup(G))[2];
#! [ f1 ] -> [ f1^2 ]
#! gap> AffineQuandle( G, f );
#! <latin quandle of size 5>
#! gap> AffineQuandle( GF(8), One(GF(8)) );
#! <quandle of size 8>
#! gap> AffineQuandle( 5, 2 ); # both 2 and 1-2 are invertible mod 5
#! <latin quandle of size 5>
#! gap> AffineQuandle( 10, 3 ); # 1-3 is not invetible mod 10
#! <quandle of size 10>
#! @EndExampleSession

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

#! @BeginGroup
#! @GroupTitle GalkinQuandle and HomogeneousQuandle

#! @Arguments G, H, f[, constructorStyle]
#! @Returns the Galkin quandle constructed from the group <Arg>G</Arg>, subgroup <Arg>H</Arg>
#! and automorphism <Arg>f</Arg> of <Arg>G</Arg> that fixes <Arg>H</Arg> pointwise.
#! The underlying set of the returned quandle is a right transversal to <Arg>H</Arg> in <Arg>G</Arg>. 
#! @Description The Galkin quandle is defined on the right cosets $\{Hx:x\in G\}$ by
#! $Hx*Hy = H f(xy^{-1})y$. We also support the synonym `HomogeneousQuandle` since Galkin quandles
#! are precisely homogeneous quandles, that is, quandles with a transitive automorphism group.
DeclareOperation( "GalkinQuandle", [ IsGroup, IsGroup, IsMapping ] );

DeclareSynonym( "HomogeneousQuandle", GalkinQuandle );

#! @BeginExampleSession
#! gap> G := SymmetricGroup( 4 );;
#! gap> H := Subgroup( G, [(1,2)] );;
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
#! ()
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

#! @EndGroup

#! @BeginGroup
#! @GroupTitle All subracks and subquandles

#! @Arguments Q
DeclareOperation( "AllSubracks", [ IsRack ] );

#! @Arguments Q
#! @Returns a list of all subracks (subquandles) of a rack (quandle) <Arg>Q</Arg>.
DeclareOperation( "AllSubquandles", [ IsQuandle ] );

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