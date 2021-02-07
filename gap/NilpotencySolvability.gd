# NilpotencySolvability.gd
# Nilpotency and solvability for right quasigroups
# =============================================================================

#! @Chapter Nilpotency and solvability

# NUCLEI, COMMUTANT, CENTER
# _____________________________________________________________________________

#! @Section Nuclei, commutant and center

#! <P/>The <Index>right nucleus</Index>**right nucleus** (resp. <Index>middle nucleus</Index>**middle nucleus**,
#! <Index>left nucleus</Index>**left nucleus**) of a right quasigroup $Q$ is the set
#! $\mathrm{Nuc}_r(Q) = \{z\in Q:x(yz)=(xy)z$ for all $x,y\in Q\}$ (resp.
#! $\mathrm{Nuc}_m(Q) = \{y\in Q:x(yz)=(xy)z$ for all $x,z\in Q\}$,
#! $\mathrm{Nuc}_\ell(Q) = \{x\in Q:x(yz)=(xy)z$ for all $y,z\in Q\}$).
#! The <Index>nucles</Index>**nucleus** $\mathrm{Nuc}(Q)$ of $Q$ is the intesection of the three nuclei.

#! <P/>In the finite case, each of the four nuclei is either an empty set or a subalgebra. 
#! For loops, each of the four nuclei is always a subloop.

#! <P/>The <Index>commutant</Index>**commutant** of a right quasigroup $Q$ is the
#! possibly empty set $\{x\in Q:xy=yx$ for all $y\in Q\}$. 
#! In the case of loops, the neutral element always belongs to the commutant.

#! <P/>The <Index>center</Index>**center** of a right quasigroup is the
#! intersection of the nucleus and the commutant. In the case of loops, the
#! center is always a normal subloop.

#! @BeginGroup
#! @GroupTitle Nuclei

#! @Arguments Q
DeclareAttribute( "RightNucleus", IsRightQuasigroup );

#! @Arguments Q
DeclareAttribute( "MiddleNucleus", IsRightQuasigroup );

#! @Arguments Q
DeclareAttribute( "LeftNucleus", IsRightQuasigroup );

#! @Arguments Q
#! @Returns the (right, middle, left) nucleus of a right quasigroup `Q`,
#! either as the empty set or as a subalgebra of `Q`.
DeclareAttribute( "Nuc", IsRightQuasigroup );

#! <P/>We also support the synonyms `NucleusOfRightQuasigroup`, `NucleusOfQuasigroup`
#! and `NucleusOfLoop` of `Nuc`.

DeclareSynonymAttr( "NucleusOfRightQuasigroup", Nuc );
DeclareSynonymAttr( "NucleusOfQuasigroup", Nuc );
DeclareSynonymAttr( "NucleusOfLoop", Nuc );

#! @EndGroup

#! @Arguments Q
#! @Returns the commutant of the rigt quasigroup `Q`, a possibly empty subset of `Q`.
DeclareAttribute( "Commutant", IsRightQuasigroup );

#! @Arguments Q
#! @Returns the center of the right quasigroup `Q`.
DeclareAttribute( "Center", IsRightQuasigroup );

# NILPOTENCY
# _____________________________________________________________________________

#! @Section Nilpotency for loops

#! The <Index>nilpotency class</Index>**nilpotency class** of a loop $Q$ is defined inductively
#! by $cl(Q)=1$ if $Q$ is an abelian group and by $cl(Q)=cl(Q/Z(Q))+1$, if this process terminates,
#! in which case `Q` is <Index>nilpotent loop</Index>**nilpotent**.

#! @Arguments Q
#! @Returns the nilpotency class of a loop `Q`. When `Q` is not nilpotent, returns `fail`.
DeclareAttribute( "NilpotencyClassOfLoop", IsLoop );

#! @Arguments Q
#! @Returns `true` if `Q` is a nilpotent loop, else returns `false`.
#! The non-qualified version `IsNilpotent` is also supported, cf. Section <Ref Sect="Section_Nonqualified"/>.
DeclareOperation( "IsNilpotentLoop", [ IsLoop ] );
# already declared for groups

#! @Arguments Q
#! @Returns the upper central series of the loop `Q`, that is, the series 
#! $[Z_0(Q), Z_1(Q), Z_2(Q), ...]$, where $Z_0(Q)=1$ and $Z_{n+1}(Q)$ is the preimage of $Z(Q/Z_n(Q))$
#! under the canonical projection $Q\to Q/Z_n(Q)$. The function returns the longest strictly increasing segment
#! of the upper central series. The non-qualified version `UpperCentralSeries` is also supported.
DeclareOperation( "UpperCentralSeriesOfLoop", [ IsLoop ] );
# UpperCentralSeriesOfGroup declared for groups

#! @Arguments Q
#! @Returns the lower central series of the loop `Q`, that is, the series
#! $Q=Q_{(0)}\ge Q_{(1)}\ge Q_{(2)}\ge \dots$ such that $Q_{(i+1)}$ is the smallest normal subloop of $Q$
#! such that $Q_{(i)}/Q_{(i+1)}\le Z(Q/Q_{(i+1)})$.
#! The function returns the longest strictly decreasing segment of the lower central series.
#! The non-qualified version `LowerCentralSeries` is also supported.
DeclareOperation( "LowerCentralSeriesOfLoop", [ IsLoop ] );
# LowerCentralSeriesOfGroup declared for groups

# SOLVABILITY AND CONGRUENCE SOLVABILITY FOR LOOPS
# _____________________________________________________________________________

#! @Section Solvability and congruence solvability for loops

#! <P/>There are two competing notions of solvability in loop theory and both are supported in
#! &RightQuasigroups;.

#! <P/>A loop $Q$ is <Index>classically solvable loop</Index>**classically solvable**
#! if there exists a series $Q=Q_0&gt; Q_1&gt;\dots&gt;Q_n=1$ such that $Q_i\unlhd Q$ and $Q_i/Q_{i+1}$ 
#! is a commutative group. To conform with historical terminology, we call classically solvable loops
#! **solvable**.

#! <P/>There is a well-established universal-algebraic notion of commutators of congruences in
#! congruence-modular varieties, which includes the variety of loops. We denote by $[A,B]_Q$ this
#! commutator of two normal subloops $A$, $B$ of a loop $Q$. A normal subloop $A$ is 
#! <Index>abelian subloop</Index>**abelian in** $Q$ if $[A,A]_Q=1$. If a normal subloop
#! $A$ is abelian in $Q$ then $A$ is a commutative group, but not necessarily vice versa.

#! <P/>A loop $Q$ is then <Index>congruence solvable loop</Index>**congruence solvable** if there
#! exists a series $Q=Q_0&gt; Q_1&gt;\dots&gt;Q_n=1$ such that $Q_i\unlhd Q$ and $Q_i/Q_{i+1}$ is abelian
#! in $Q/Q_{i+1}$. Universally-algebraically speaking, this is the correct notion of solvablity
#! for loops. Every congruence solvable loop is classically solvable but not necessarily
#! vice versa. A group is congruence solvable iff it is classically solvable.

#! @Arguments Q
#! @Returns the <Index>associator subloop</Index>**associator subloop** of `Q`, that is,
#! the smallest normal subloop of `Q` containing all associators.
DeclareAttribute( "AssociatorSubloop", IsLoop );

#! @Arguments Q
#! @Returns `true` if `Q` is a (classically) solvable loop, else returns `false`. The non-qualified
#! version `IsSolvable` is also supported.
DeclareOperation( "IsSolvableLoop", [ IsLoop ] );

#! @Arguments Q
#! @Returns the <Index>derived subloop</Index>**derived subloop** of `Q`, that is, the smallest
#! normal subloop `A` of `Q` such that `Q/A` is an abelian group. Note that this coincides
#! with the congruence derived subloop $[Q,Q]_Q$ and there is therefore no need for
#! `CongruenceDerivedSubloop`.
DeclareAttribute( "DerivedSubloop", IsLoop );

#! @Arguments Q
#! @Returns the derived series of `Q`. If $Q'$ denotes the derived subloop of $Q$, then
#! the derived series is the series $[Q,Q',Q'',\dots]$. The function returns the longest
#! strictly decreasing initial segment of the derived series. The non-qualified version
#! `DerivedSeries` is also supported.
DeclareOperation( "DerivedSeriesOfLoop", [ IsLoop ] );

#! @Arguments Q
#! @Returns the number of steps in the derived series of the loop `Q` if `Q` is solvable, else
#! returns `fail`.
DeclareAttribute( "DerivedLength", IsLoop );
# already declared for groups

#!<P/>The following methods deal with congruence solvability of loops.

#! @Arguments Q, A, B
#! @Returns the commutator of normal subloops `A`, `B` of the loop `Q`, that is, $[A,B]_Q$. For finite loops,
#! $[A,B]_Q$ is the smallest normal subloop of $Q$ contaning the elements $R_{b_1,c_1}(a)/R_{b_2,c_2}(a)$,
#! $L_{b_1,c_1}(a)/L_{b_2,c_2}(a)$, $T_{b_1}(a)/T_{b_2}(a)$, where $a\in A$, $b_1/b_2\in B$, $c_1/c_2\in B$
#! and where $R_{x,y}$, $L_{x,y}$, $T_x$ are the standard inner mappings of $Q$.
DeclareOperation( "CommutatorOfNormalSubloops", [ IsLoop, IsLoop, IsLoop ] );

#! @Arguments Q, A
#! @Returns `true` if `A` is an abelian (in the universal-algebraic sense) normal subloop of the loop `Q`, else returns `false`.
#! A normal subloop $A$ of $Q$ is abelian in $Q$ if $[A,A]_Q=1$.
DeclareOperation( "IsAbelianNormalSubloop", [ IsLoop, IsLoop ] );

#! @Arguments Q
#! @Returns `true` if `Q` is a congruence solvable loop, else returns `false`. 
DeclareOperation( "IsCongruenceSolvableLoop", [ IsLoop ] );

#! @Arguments Q
#! @Returns the congruence derived series of `Q`. With $Q^{(0)}=Q$ and $Q^{(i+1)} = [Q^{(i)},Q^{(i)}]_Q$,
#! this is the series $[Q^{(0)},Q^{(1)},Q^{(2)},\dots]$. The function returns the longest
#! strictly decreasing initial segment of the congruence derived series. 
DeclareOperation( "CongruenceDerivedSeriesOfLoop", [ IsLoop ] );

#! @Arguments Q
#! @Returns the number of steps in the congruence derived series of the loop `Q` if `Q` is congruence
#! solvable, else returns `fail`.
DeclareAttribute( "CongruenceDerivedLength", IsLoop );

#! @BeginExampleSession
#! gap> uset := Union( List([0..3], i-> [[i,0],[i,1]] ) ); # the underlying set
#! [ [ 0, 0 ], [ 0, 1 ], [ 1, 0 ], [ 1, 1 ], [ 2, 0 ], [ 2, 1 ], [ 3, 0 ], [ 3, 1 ] ]
#! gap> ct := [[0,1,2,3],[1,3,0,2],[2,0,3,1],[3,2,1,0]];;
#! gap> mult := function( x, y )
#! >       if x[2]=0 or y[2]=0 then
#! >               return [ (x[1]+y[1]) mod 4, (x[2]+y[2]) mod 2 ];
#! >       else
#! >               return [ ct[x[1]+1,y[1]+1], (x[2]+y[2]) mod 2 ];
#! >       fi;
#! > end;
#! function( x, y ) ... end
#! gap> Q := LoopByFunction( uset, mult ); # Z_4 x Z_2 with one quandrant "replaced" with ct
#! <loop of size 8>
#! gap> DerivedSeries( Q );
#! [ <loop of size 8>, <loop of size 4>, <loop of size 1> ]
#! gap> IsSolvable( Q );
#! true
#! gap> DerivedLength( Q );
#! 2
#! gap> C := CommutatorOfNormalSubloops(Q,Q,Q); # congruence derived subloop = derived subloop
#! <loop of size 4>
#! gap> D := CommutatorOfNormalSubloops(Q,C,C); # 2nd congruence derived subloop differs from 2nd derived subloop
#! <loop of size 4>
#! gap> CongruenceDerivedSeriesOfLoop( Q );
#! [ <loop of size 8>, <loop of size 4> ]
#! gap> IsCongruenceSolvableLoop( Q );
#! false
#! gap> CongruenceDerivedLength( Q );
#! fail
#! gap> IsCommutative( C ) and IsAssociative( C ) and IsNormal( Q, C ); # commutative group, normal in Q
#! true
#! gap> IsAbelianNormalSubloop( Q, C ); # but not abelian in Q
#! false
#! @EndExampleSession

# FRATTINI SUBLOOP
# _____________________________________________________________________________

#! @Section Frattini subloop

# FrattiniSubloop( Q ) 
# Returns the Frattini subloop of a strongly nilpotent loop <Q>.
DeclareAttribute( "FrattiniSubloop", IsLoop );

# REVISIT: DO we really need this? It is easy to call Size(Q)/Size(FrattiniSubloop(Q)).
# FrattinifactorSize( Q )
# Returns the Frattini factor size of loop <Q>, i.e., the index of
# the Frattini subloop of <Q> in <Q>.
# FrattinifactorSize already declared for groups