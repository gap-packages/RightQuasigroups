# Properties.gd
# Properties of right quasigroups, quasigroups and loops
# =============================================================================

#! @Chapter Properties of right quasigroups, quasigroups and loops

# PROPERTIES OF RIGHT QUASIGROUPS
# _____________________________________________________________________________

#! @Section Properties of magmas and right quasigroups

#! <P/>The following list summarizes properties of magmas tested in &RightQuasigroups;. All identities 
#! are universally quantified with respect to all variables.
#! <List>
#! <Item><Index>3-power associative magma</Index>**3-power associative**: $x(xx)=(xx)x$</Item>
#! <Item><Index>alternative magma</Index>**alternative**: $x(xy) = (xx)y$ and $x(yy)=(xy)y$</Item>
#! <Item><Index>associative magma</Index>**associative**: $x(yz)=(xy)z$</Item>
#! <Item><Index>commutative magma</Index>**commutative**: $xy=yx$</Item>
#! <Item><Index>flexible magma</Index>**flexible**: $x(yx)=(xy)x$</Item>
#! <Item><Index>idempotent magma</Index>**idempotent**: $xx=x$</Item>
#! <Item><Index>left alternative magma</Index>**left alternative**: $x(xy)=(xx)y$</Item>
#! <Item><Index>left self-distributive magma</Index>**left self-distributive**: $x(yz)=(xy)(xz)$</Item>
#! <Item><Index>right alternative magma</Index>**right alternative**: $x(yy)=(xy)y$</Item>
#! <Item><Index>right self-distributive magma</Index>**right self-distributive**: $(xy)z=(xz)(yz)$</Item>
#! <Item><Index>self-distributive magma</Index>**self-distributive**: $x(yz)=(xy)(xz)$ and $(xy)z=(xz)(yz)$</Item>
#! <Item><Index>unipotent magma</Index>**unipotent**: $xx=yy$</Item>
#! </List>

#! <P/>Some of these methods for magmas are already available in &GAP;, e.g., `IsCommutative`. In such a case,
#! we provide a new method for right quasigroups that takes advantage of right translations as permutations.

#! @BeginGroup
#! @GroupTitle Testing properties of magma and right quasigroups

#! @Arguments Q
#! @Returns `true` if `Q` is a magma with the property in the title of the function, else returns `false`
DeclareProperty( "Is3PowerAssociative", IsMagma );

#! @Arguments Q
DeclareProperty( "IsAlternative", IsMagma );

#! @Arguments Q
DeclareProperty( "IsAssociative", IsRightQuasigroup );

#! @Arguments Q
DeclareProperty( "IsCommutative", IsRightQuasigroup );

#! @Arguments Q
DeclareProperty( "IsFlexible", IsMagma ); 

#! @Arguments Q
DeclareProperty( "IsIdempotent", IsMagma );

#! @Arguments Q
DeclareProperty( "IsLeftAlternative", IsMagma );

#! @Arguments Q
DeclareProperty( "IsLeftSelfDistributive", IsMagma );

#! @Arguments Q
DeclareProperty( "IsRightAlternative", IsMagma );

#! @Arguments Q
DeclareProperty( "IsRightSelfDistributive", IsMagma ); # REVISIT: Should we introduce IsRSelfDistributive synonym?

#! @Arguments Q
DeclareProperty( "IsSelfDistributive", IsMagma );

#! @Arguments Q
DeclareProperty( "IsUnipotent", IsMagma ); # xx=yy

#! @EndGroup

# PROPERTIES OF QUASIGROUPS
# _____________________________________________________________________________

#! @Section Properties of quasigroups

# Note: These notions are equational for quasigroups but in general magmas look different.

#! @Arguments Q
#! @Returns `true` if `Q` is a semisymmetric quasigroup (that is, a quasigroup
#! satisfying `(x*y)*x=y` for all `x`, `y`), else returns `false`.
DeclareProperty( "IsSemisymmetric", IsQuasigroup );

#! @Arguments Q
#! @Returns `true` if `Q` is a totally symmetric quasigroup (that is, a quasigroup
#! that is semisymmetric and commutative), else returns `false`.
DeclareProperty( "IsTotallySymmetric", IsQuasigroup );

#! @Arguments Q
#! @Returns `true` if `Q` is a Steiner quasigroup (that is, a quasigroup
#! that is idempotent and totally symmetric), else returns `false`.
#! We also support the synonym `IsMedial`.
DeclareProperty( "IsSteinerQuasigroup", IsQuasigroup );

#! @Arguments Q
#! @Returns `true` if `Q` is an entropic quasigroup (that is, a quasigroup
#! satisfying `(x*u)*(v*y) = (x*v)*(u*y)` for all `x`, `y`, `u`, `v`), else returns `false`.
#! We also support the synonym `IsMedial`.
DeclareProperty( "IsEntropic", IsQuasigroup );

DeclareSynonymAttr( "IsMedial", IsEntropic );

#! @Arguments Q
#! @Returns `true` if `Q` is a power associative quasigroup (that is, a quasigroup
#! in which every element generates an associative subquasigroup), else returns `false`.
DeclareProperty( "IsPowerAssociative", IsQuasigroup );

#! @Arguments Q
#! @Returns `true` if `Q` is a diassociative quasigroup (that is, a quasigroup
#! in which every two elements generate an associative subquasigroup), else returns `false`.
DeclareProperty( "IsDiassociative", IsQuasigroup );

# INVERSE PROPERTIES OF LOOPS
# _____________________________________________________________________________

#! @Section Inverse properties of loops

#! <P/>In a loop $Q$ with neutral element $e$, for every $x\in Q$ there is a unique left inverse
#! $x^\ell$ of $x$ satisfying $x^\ell x = e$ and a unique right inverse $x^r$ of $x$ satisfying
#! $xx^r=e$, cf. Section <Ref Sect="Section_Elementwise"/>. If $x^ell = x^r$ then the two-sided
#! inverse of $x$ is denoted by $x^{-1}$.

#! @Arguments Q
#! @Returns `true` if `Q` is a loop with two-sided inverses, else returns `false`.
DeclareProperty( "HasTwosidedInverses", IsLoop );

#! @BeginGroup
#! @GroupTitle Right and left inverse properties

# It looks like there is a bug in AutoDoc. If there are several properties in a group, 
# the @Returns is only displayed if it is given for the first property.

#! @Arguments Q
#! @Returns `true` if the loop `Q` has the right inverse property (resp. left inverse property,
#! inverse property), else returns `false`.
DeclareProperty( "HasRightInverseProperty", IsLoop );

#! @Arguments Q
DeclareProperty( "HasLeftInverseProperty", IsLoop );

#! @Arguments Q
#! @Description A loop $Q$ has the <Index>right inverse property</Index>**right inverse property**
#! (resp. <Index>left inverse property</Index>**left inverse property**)
#! if $(xy)y^r=x$ (resp. $x^\ell(xy)=y$) holds for all $x,y\in Q$. It has the
#! <Index>inverse property</Index>**inverse property** if it is has the right inverse property
#! and the left inverse property. In all three cases, the loop automatically has two-sided inverses.
DeclareProperty( "HasInverseProperty", IsLoop );

#! @EndGroup

#! @Arguments Q
#! @Returns `true` if the loop `Q` has the weak inverse property.
#! @Description A loop $Q$ has the <Index>weak inverse property</Index>**weak inverse property**
#! if $(xy)^\ell x = y^\ell$ holds for all $x,y\in Q$. The dual identity $x(yx)^r = y^r$ is equivalent
#! to the weak inverse property. A weak inverse property loop does not necessarily posses 
#! two-sided inverses.
DeclareProperty( "HasWeakInverseProperty", IsLoop );

#! @BeginGroup
#! @GroupTitle Automorphic and antiautomorphic inverse properties

#! @Arguments Q
#! @Returns `true` if the loop `Q` has the automorphic inverse property (resp. antiautomorphic
#! inverse property), else returns `false`.
DeclareProperty( "HasAutomorphicInverseProperty", IsLoop );

#! @Arguments Q
#! @Description A loop $Q$ has the <Index>automorphic inverse property</Index>**automorphic
#! inverse property** if $(xy)^r = x^r y^r$ holds for all $x,y\in Q$ (the dual identity
#! $(xy)^\ell = x^\ell y^\ell$ is equivalent to it in loops). An automorphic inverse property loop does
#! not necessarily posses two-sided inverses. A loop $Q$ has the <Index>antiautomorphic inverse
#! property</Index>**antiautomorphic inverse property** if $(xy)^r = y^r x^r$ holds
#! for all $x,y\in Q$ (the dual identity $(xy)^\ell = y^\ell x^\ell$ is equivalent to it in loops). 
#! An antiautomorphic inverse property loop automatically possesses two-sided inverses and
#! the definining identity can therefore be restated as $(xy)^{-1}=y^{-1}x^{-1}$.
DeclareProperty( "HasAntiautomorphicInverseProperty", IsLoop );

#! @EndGroup

# LOOPS OF BOL-MOUFANG TYPE
# _____________________________________________________________________________

#! @Section Loops of Bol-Moufang type

#! <P/>A loop identity is said to be of <Index>identity of Bol-Moufang type</Index>**Bol-Moufang type**
#! if two of its three variables occur once on each side, the third variable occurs twice on each side,
#! and the order in which the variables appear on both sides is the same, cf. $((xy)x)z=x(y(xz))$.
#! A variety of loops is of <Index>variety of Bol-Moufang type</Index>**Bol-Moufang type**
#! if it is defined by a single identity of Bol-Moufang type. It can be shown that there are precisey
#! 14 varieties of loops of Bol-Moufang type; the 11 varieties included in the list below plus the already
#! introduced associative loops (aka groups), flexible loops, left alternative loops and right alternative loops.

#! <P/>Here are the 11 varieties of loops of Bol-Moufang type not yet introduced. In some cases
#! there are several equivalent identities of Bol-Moufang type for the given variety but we only list one such identity.
#! <List>
#! <Item><Index>C loops</Index>**C loops**: $x(y(yz))=((xy)y)z$</Item>
#! <Item><Index>extra loops</Index>**extra loops**: $x(y(zx))=((xy)z)x$</Item>
#! <Item><Index>LC loops</Index>**LC loops**: $(xx)(yz)=(x(xy))z$</Item>
#! <Item><Index>left Bol loops</Index>**left Bol loops**: $x(y(xz)) = (x(yx))z$</Item>
#! <Item><Index>left nuclear square loops</Index>**left nuclear square loops**: $(xx)(yz)=((xx)y)z$</Item>
#! <Item><Index>middle nuclear square loops</Index>**middle nuclear square loops**: $x((yy)z)=(x(yy))z$</Item>
#! <Item><Index>Moufang loops</Index>**Moufang loops**: $(xy)(zx)=(x(yz))x$</Item>
#! <Item><Index>RC loops</Index>**RC loops**: $x((yz)z)=(xy)(zz)$</Item>
#! <Item><Index>right Bol loops</Index>**right Bol loops**: $x((yz)y)=((xy)z)y$</Item>
#! <Item><Index>right nuclear square loops</Index>**right nuclear square loops**: $x(y(zz))=(xy)(zz)$</Item>
#! </List>

#! @BeginGroup
#! @GroupTitle Testing for loops of Bol-Moufang type

#! @Arguments Q
#! @Returns `true` if the loop `Q` has the property in the title of the function, else returns `false`.
DeclareProperty( "IsCLoop", IsLoop );

#! @Arguments Q
DeclareProperty( "IsExtraLoop", IsLoop );

#! @Arguments Q
DeclareProperty( "IsLCLoop", IsLoop );

#! @Arguments Q
DeclareProperty( "IsLeftBolLoop", IsLoop );

#! @Arguments Q
DeclareProperty( "IsLeftNuclearSquareLoop", IsLoop );

#! @Arguments Q
DeclareProperty( "IsMiddleNuclearSquareLoop", IsLoop );

#! @Arguments Q
DeclareProperty( "IsMoufangLoop", IsLoop );

#! @Arguments Q
DeclareProperty( "IsRCLoop", IsLoop );

#! @Arguments Q
DeclareProperty( "IsRightBolLoop", IsLoop );

#! @Arguments Q
DeclareProperty( "IsRightNuclearSquareLoop", IsLoop );

#! @EndGroup

#! @Arguments Q
#! @Returns `true` if `Q` is a <Index>nuclear square loop</Index>**nuclear square loop**
#! (that is, a loop where the square of every element lies in the nucleus), else returns `false`.
#! The variety of nuclear square loops is technically not of Bol-Moufang type,
#! but can be obtained by interesecting varieties of Bol-Moufang type.
DeclareProperty( "IsNuclearSquareLoop", IsLoop );

# POWER ALTERNATIVE LOOPS
# _____________________________________________________________________________

#! @Section Power alternative loops

#! <P/>A loop $Q$ is <Index>right power alternative loop</Index><Index>right power alternative loops</Index>
#! **right power alternative** (resp. **left power alternative**) if it is power associative and 
#! $(xy^n)y^m = xy^{n+m}$ (resp. $x^n(x^my) = x^{n+m}y$) holds for all $x,y\in Q$ and all integers $n$, $m$.
#! A loop is <Index>power alternative loop</Index>**power alternative** if it is both right and left power
#! alternative.

#! @BeginGroup
#! @GroupTitle Testing for power alternative loops

#! @Arguments Q
#! @Returns `true` if the loop `Q` is right power alternative (resp. left power alternative, power alternative),
#! else returns `false`.
DeclareProperty( "IsRightPowerAlternative", IsLoop );

#! @Arguments Q
DeclareProperty( "IsLeftPowerAlternative", IsLoop );

#! @Arguments Q
DeclareProperty( "IsPowerAlternative", IsLoop );

#! @EndGroup

# CC-LOOPS AND OSBORN LOOPS
# _____________________________________________________________________________

#! @Section Conjugacy closed loops and Osborn loops

#! <P/>A loop $Q$ is <Index>right conjugacy closed loop</Index>**right conjugacy closed**
#! (aka **RCC**) if for all $x,y\in Q$ the permutation $R_x^{-1}R_yR_x$ is a right translation.
#! Dually, $Q$ is <Index>left conjugacy closed loop></Index>**left conjugacy closed**
#! (aka **LCC**) if for all $x,y\in Q$ the permutation $L_x^{-1}L_yL_x$ is a left translation.
#! A loop $Q$ is <Index>conjugacy closed loop</Index>**conjugacy closed** (aka **CC**)
#! if it is both RCC and LCC.

#! @BeginGroup 
#! @GroupTitle Testing for conjugacy closed loops

#! @Arguments Q
DeclareProperty( "IsRightConjugacyClosedLoop", IsLoop );
#! @Returns `true` if the loop `Q` is right conjugacy closed (resp. left conjugacy closed,
#! conjugacy closed), else returns `false`.

#! @Arguments Q
DeclareProperty( "IsLeftConjugacyClosedLoop", IsLoop );

#! @Arguments Q
#! @Description The synonyms `IsRCCLoop`, `IsLCCLoop` and `IsCCLoop` are also supported.
DeclareProperty( "IsConjugacyClosedLoop", IsLoop );

DeclareSynonymAttr( "IsRCCLoop", IsRightConjugacyClosedLoop );
DeclareSynonymAttr( "IsLCCLoop", IsLeftConjugacyClosedLoop );
DeclareSynonymAttr( "IsCCLoop", IsConjugacyClosedLoop );

#! @EndGroup

#! @Arguments Q
#! @Returns `true` if `Q` is an Osborn loop<Index>Osborn loop</Index> (that is, a loop
#! satisfying $y\backslash (x(zx)) = ((y/x)z)x$), else returns `false`.
DeclareProperty( "IsOsbornLoop", IsLoop );

# ADDITIONAL VARIETIES OF LOOPS
# _____________________________________________________________________________

#! @Section Additional varieties of loops

#! @Arguments Q
#! @Returns `true` if `Q` is a code loop, else returns `false`.
#! A loop $Q$ is a <Index>code loop</Index>**code loop** if it a Moufang loop containing a central subloop $Z$ of size $2$
#! such that $Q/Z$ is an elementary abelian $2$-group.
DeclareProperty( "IsCodeLoop", IsLoop );

#! @Arguments Q
#! @Returns `true` if `Q` is a Steiner loop, else retunrs `false`.
#! A loop $Q$ is a <Index>Steiner loop</Index>**Steiner loop** if it is an inverse property loop
#! of exponent at most $2$.
DeclareProperty( "IsSteinerLoop", IsLoop );

#! @BeginGroup
#! @GroupTitle Bruck loops

#! <P/>A loop $Q$ is a <Index>right Bruck loop</Index>**right Bruck loop** (aka **right K-loop**) if
#! it is right Bol an has the automorphic inverse property. A loop $Q$ is a <Index>left Bruck loop</Index>
#! **left Bruck loop** (aka **left K-loop**) if it is left Bol and has the automorphic inverse property.

#! @Arguments Q
#! @Returns `true` if `Q` is a right Bruck loop (resp. left Bruck loop), else returns `false`.
DeclareProperty( "IsRightBruckLoop", IsLoop );

#! @Arguments Q
#! @Description The synonyms `IsRightKLoop` and `IsLeftKLoop` are also supported.
DeclareProperty( "IsLeftBruckLoop", IsLoop );

DeclareSynonymAttr( "IsRightKLoop", IsRightBruckLoop );
DeclareSynonymAttr( "IsLeftKLoop", IsLeftBruckLoop );

#! @EndGroup

#! @BeginGroup
#! @GroupTitle Automorphic loops

#! <P/>A loop $Q$ is a <Index>right automorphic loop</Index>**right automorphic loop** (aka **right A-loop**) if
#! all right inner mapping of $Q$ are automorphisms of $Q$. 
#! A loop $Q$ is a <Index>middle automorphic loop</Index>
#! **middle automorphic loop** (aka **middle A-loop**) if all its middle inner mappings are automorphisms of $Q$.
#! A loop $Q$ is a <Index>left automorphic loop</Index>
#! **left automorphic loop** (aka **left A-loop**) if all its left inner mappings are automorphisms of $Q$.
#! A loop $Q$ is an <Index>automorphic loop</Index>**automorphic loop** (aka **A-loop**) if all
#! its inner mappings are automorphisms of $Q$.

#! @Arguments Q
#! @Returns `true` if `Q` is a right automorphic loop (resp. middle automorphic loop, left automorphic loop,
#! automorphic loop), else returns `false`.
DeclareProperty( "IsRightAutomorphicLoop", IsLoop );

#! @Arguments Q
DeclareProperty( "IsMiddleAutomorphicLoop", IsLoop );

#! @Arguments Q
DeclareProperty( "IsLeftAutomorphicLoop", IsLoop );

#! @Arguments Q
#! @Description The synonyms `IsRightALoop`, `IsMiddleALoop`, `IsLeftALoop` and `IsALoop` are also supported.
DeclareProperty( "IsAutomorphicLoop", IsLoop );

DeclareSynonymAttr( "IsRightALoop", IsRightAutomorphicLoop );
DeclareSynonymAttr( "IsMiddleALoop", IsMiddleAutomorphicLoop );
DeclareSynonymAttr( "IsLeftALoop", IsLeftAutomorphicLoop );
DeclareSynonymAttr( "IsALoop", IsAutomorphicLoop );

#! @EndGroup

############################################################################
##  IMPLICATIONS for InstallTrueMethod
##  -------------------------------------------------------------------------

# implies
InstallTrueMethod( IsExtraLoop, IsAssociative and IsLoop );
InstallTrueMethod( HasTwosidedInverses, IsPowerAssociative and IsLoop );
InstallTrueMethod( IsPowerAlternative, IsDiassociative );
InstallTrueMethod( IsFlexible, IsDiassociative );
InstallTrueMethod( IsMoufangLoop, IsExtraLoop );
InstallTrueMethod( IsCLoop, IsExtraLoop );
InstallTrueMethod( IsLeftBolLoop, IsMoufangLoop );
InstallTrueMethod( IsRightBolLoop, IsMoufangLoop );
InstallTrueMethod( IsDiassociative, IsMoufangLoop );
InstallTrueMethod( IsLCLoop, IsCLoop );
InstallTrueMethod( IsRCLoop, IsCLoop );
InstallTrueMethod( IsDiassociative, IsCLoop and IsFlexible);
InstallTrueMethod( IsRightBolLoop, IsLeftBolLoop and IsCommutative );
InstallTrueMethod( IsLeftPowerAlternative, IsLeftBolLoop );
InstallTrueMethod( IsLeftBolLoop, IsRightBolLoop and IsCommutative );
InstallTrueMethod( IsRightPowerAlternative, IsRightBolLoop );
InstallTrueMethod( IsLeftPowerAlternative, IsLCLoop );
InstallTrueMethod( IsLeftNuclearSquareLoop, IsLCLoop );
InstallTrueMethod( IsMiddleNuclearSquareLoop, IsLCLoop );
InstallTrueMethod( IsRCLoop, IsLCLoop and IsCommutative );
InstallTrueMethod( IsRightPowerAlternative, IsRCLoop );
InstallTrueMethod( IsRightNuclearSquareLoop, IsRCLoop );
InstallTrueMethod( IsMiddleNuclearSquareLoop, IsRCLoop );
InstallTrueMethod( IsLCLoop, IsRCLoop and IsCommutative );
InstallTrueMethod( IsRightNuclearSquareLoop, IsLeftNuclearSquareLoop and IsCommutative );
InstallTrueMethod( IsLeftNuclearSquareLoop, IsRightNuclearSquareLoop and IsCommutative );
InstallTrueMethod( IsLeftNuclearSquareLoop, IsNuclearSquareLoop );
InstallTrueMethod( IsRightNuclearSquareLoop, IsNuclearSquareLoop );
InstallTrueMethod( IsMiddleNuclearSquareLoop, IsNuclearSquareLoop );
InstallTrueMethod( IsRightAlternative, IsLeftAlternative and IsCommutative );
InstallTrueMethod( IsLeftAlternative, IsRightAlternative and IsCommutative );
InstallTrueMethod( IsLeftAlternative, IsAlternative );
InstallTrueMethod( IsRightAlternative, IsAlternative );
InstallTrueMethod( IsLeftAlternative, IsLeftPowerAlternative );
InstallTrueMethod( HasLeftInverseProperty, IsLeftPowerAlternative );
InstallTrueMethod( IsPowerAssociative, IsLeftPowerAlternative );
InstallTrueMethod( IsRightAlternative, IsRightPowerAlternative );
InstallTrueMethod( HasRightInverseProperty, IsRightPowerAlternative );
InstallTrueMethod( IsPowerAssociative, IsRightPowerAlternative );
InstallTrueMethod( IsLeftPowerAlternative, IsPowerAlternative );
InstallTrueMethod( IsRightPowerAlternative, IsPowerAlternative );
InstallTrueMethod( IsAssociative, IsLCCLoop and IsCommutative );
InstallTrueMethod( IsExtraLoop, IsLCCLoop and IsMoufangLoop );
InstallTrueMethod( IsAssociative, IsRCCLoop and IsCommutative );
InstallTrueMethod( IsExtraLoop, IsRCCLoop and IsMoufangLoop );
InstallTrueMethod( IsLCCLoop, IsCCLoop );
InstallTrueMethod( IsRCCLoop, IsCCLoop );
InstallTrueMethod( IsExtraLoop, IsCodeLoop );
InstallTrueMethod( IsCCLoop, IsCodeLoop );
InstallTrueMethod( IsCommutative, IsSteinerLoop );
InstallTrueMethod( IsCLoop, IsSteinerLoop );
InstallTrueMethod( HasAutomorphicInverseProperty, IsLeftBruckLoop );
InstallTrueMethod( IsLeftBolLoop, IsLeftBruckLoop );
InstallTrueMethod( IsRightBruckLoop, IsLeftBruckLoop and IsCommutative );
InstallTrueMethod( HasAutomorphicInverseProperty, IsRightBruckLoop );
InstallTrueMethod( IsRightBolLoop, IsRightBruckLoop );
InstallTrueMethod( IsLeftBruckLoop, IsRightBruckLoop and IsCommutative );
InstallTrueMethod( IsLeftALoop, IsALoop );
InstallTrueMethod( IsRightALoop, IsALoop );
InstallTrueMethod( IsMiddleALoop, IsALoop );
InstallTrueMethod( IsLeftALoop, IsRightALoop and HasAntiautomorphicInverseProperty );
InstallTrueMethod( IsRightALoop, IsLeftALoop and HasAntiautomorphicInverseProperty );
InstallTrueMethod( IsFlexible, IsMiddleALoop );
InstallTrueMethod( HasAntiautomorphicInverseProperty, IsFlexible and IsLeftALoop );
InstallTrueMethod( HasAntiautomorphicInverseProperty, IsFlexible and IsRightALoop );
InstallTrueMethod( IsMoufangLoop, IsALoop and IsLeftAlternative );
InstallTrueMethod( IsMoufangLoop, IsALoop and IsRightAlternative );
InstallTrueMethod( IsMoufangLoop, IsALoop and HasLeftInverseProperty );
InstallTrueMethod( IsMoufangLoop, IsALoop and HasRightInverseProperty );
InstallTrueMethod( IsMoufangLoop, IsALoop and HasWeakInverseProperty );

# is implied by
InstallTrueMethod( IsExtraLoop, IsMoufangLoop and IsLeftNuclearSquareLoop );
InstallTrueMethod( IsExtraLoop, IsMoufangLoop and IsMiddleNuclearSquareLoop );
InstallTrueMethod( IsExtraLoop, IsMoufangLoop and IsRightNuclearSquareLoop );
InstallTrueMethod( IsMoufangLoop, IsLeftBolLoop and IsRightBolLoop );
InstallTrueMethod( IsCLoop, IsLCLoop and IsRCLoop );
InstallTrueMethod( IsNuclearSquareLoop, IsLeftNuclearSquareLoop
        and IsRightNuclearSquareLoop and IsMiddleNuclearSquareLoop );
InstallTrueMethod( IsFlexible, IsCommutative );
InstallTrueMethod( Is3PowerAssociative, IsPowerAssociative );
InstallTrueMethod( Is3PowerAssociative, IsCommutative );
InstallTrueMethod( Is3PowerAssociative, IsLeftAlternative );
InstallTrueMethod( Is3PowerAssociative, IsRightAlternative );
InstallTrueMethod( IsAlternative, IsLeftAlternative and IsRightAlternative );
InstallTrueMethod( IsCCLoop, IsLCCLoop and IsRCCLoop );
InstallTrueMethod( IsOsbornLoop, IsMoufangLoop );
InstallTrueMethod( IsOsbornLoop, IsCCLoop );
InstallTrueMethod( IsLeftBruckLoop, IsLeftBolLoop and HasAutomorphicInverseProperty );
InstallTrueMethod( IsRightBruckLoop, IsRightBolLoop and HasAutomorphicInverseProperty );
InstallTrueMethod( IsMiddleALoop, IsCommutative and IsLoop);
InstallTrueMethod( IsLeftALoop, IsLeftBruckLoop );
InstallTrueMethod( IsLeftALoop, IsLCCLoop );
InstallTrueMethod( IsRightALoop, IsRightBruckLoop );
InstallTrueMethod( IsRightALoop, IsRCCLoop );
InstallTrueMethod( IsALoop, IsCommutative and IsMoufangLoop );
InstallTrueMethod( IsALoop, IsLeftALoop and IsMiddleALoop );
InstallTrueMethod( IsALoop, IsRightALoop and IsMiddleALoop );
InstallTrueMethod( IsALoop, IsAssociative and IsLoop);

# implies and is implied by (for inverse properties)
InstallTrueMethod( HasAntiautomorphicInverseProperty, HasAutomorphicInverseProperty and IsCommutative );
InstallTrueMethod( HasAutomorphicInverseProperty, HasAntiautomorphicInverseProperty and IsCommutative );
InstallTrueMethod( HasLeftInverseProperty, HasInverseProperty );
InstallTrueMethod( HasRightInverseProperty, HasInverseProperty );
InstallTrueMethod( HasWeakInverseProperty, HasInverseProperty );
InstallTrueMethod( HasAntiautomorphicInverseProperty, HasInverseProperty );
InstallTrueMethod( HasTwosidedInverses, HasAntiautomorphicInverseProperty );
InstallTrueMethod( HasInverseProperty, HasLeftInverseProperty and IsCommutative );
InstallTrueMethod( HasInverseProperty, HasRightInverseProperty and IsCommutative );
InstallTrueMethod( HasInverseProperty, HasLeftInverseProperty and HasRightInverseProperty );
InstallTrueMethod( HasInverseProperty, HasLeftInverseProperty and HasWeakInverseProperty );
InstallTrueMethod( HasInverseProperty, HasRightInverseProperty and HasWeakInverseProperty );
InstallTrueMethod( HasInverseProperty, HasLeftInverseProperty and HasAntiautomorphicInverseProperty );
InstallTrueMethod( HasInverseProperty, HasRightInverseProperty and HasAntiautomorphicInverseProperty );
InstallTrueMethod( HasInverseProperty, HasWeakInverseProperty and HasAntiautomorphicInverseProperty );
InstallTrueMethod( HasTwosidedInverses, HasLeftInverseProperty );
InstallTrueMethod( HasTwosidedInverses, HasRightInverseProperty );
InstallTrueMethod( HasTwosidedInverses, IsFlexible and IsLoop );