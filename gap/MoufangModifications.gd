# MoufangModifications.gd
# Quarter modifications of Moufang loops
# =============================================================================

# chapter continues
#! @Chapter Bol loops, Bruck loops and Moufang loops

#! @Section Quarter modifications of Moufang loops

#! <P/>Dr&#225;pal 
# <Cite Key="DrapalCD"/>
#! described two prominent families of extensions of Moufang loops. The common feature of these extensions is that
#! they modify multiplication tables of groups or other Moufang loops in exactly one quarter of the mutliplication table.

#! It turns out that these extensions suffice to obtain all nonassociative Moufang loops of order at most 64 if one starts with so-called Chein loops.
#! We call the two constructions **Moufang modifications**<Index Subkey="Moufang">modification</Index>. The library of Moufang loops 
#! used to be based on Moufang modifications. See
# <Cite Key="DrVo"/>
#! for details.

#! @Arguments G[, constructorStyle]
#! @Returns the Chein loop constructed from the group `G`, that is, the Moufang loop on `[0,1] x G` with multiplication
#! $$ (0,x)(0,y) = (0,xy),\ (0,x)(1,y) = (1,yx),\, (1,x)(0,y) = (1,xy^{-1}),\ (1,x)(1,y) = (0,yx^{-1}). $$
#! The resulting Moufang loop is associative iff `G` is commutative.
DeclareOperation( "CheinLoop", [ IsGroup ] );

# auxiliary functions used in Moufang modifications

# @Arguments A, B (lists)
# @Returns list P, where P[i] is the position of B[i] in A. Here, A, B are lists.
DeclareOperation( "RQ_PositionList", [ IsList, IsList ] );

# @Arguments i, m
# @Returns i modulo the set [-m+1..m]
DeclareOperation( "RQ_Modular", [ IsInt, IsInt ] );

# @Arguments i, m
# @Returns overflow of i modulo [-m+1..m]. See documentation for more details.
DeclareOperation( "RQ_DVSigma", [ IsInt, IsInt ] );

#! @BeginGroup
#! @GroupTitle *Cyclic modification

#! @Arguments Q, S, a, h
#! @Returns The cyclic modification of a Moufang loop <Arg>Q</Arg> obtained from <Arg>S</Arg>, <Arg>a</Arg>$=\alpha$ and <Arg>h</Arg> as described below.
#! We allow <Arg>S</Arg> to be normal subloop of <Arg>L</Arg> or a list of elements of <Arg>L</Arg> generating a normal subloop.
#! The returned loop will always be index based.
#! @Description Assume that $Q$ is a Moufang loop with a normal subloop $S$ such that $Q/S$ is a cyclic group of order $2m$.
#! Let $h\in S\cap Z(L)$. Let $\alpha$ be a generator of $Q/S$ and write $Q = \bigcup_{i\in M} \alpha^i$, where
#! $M=\{-m+1$, $\dots$, $m\}$. Let $\sigma:\mathbb{Z}\to M$ be defined by
#! $\sigma(i)=0$ if $i\in M$, $\sigma(i)=1$ if $i>m$, and $\sigma(i)=-1$ if $i&lt;-m+1$.
#! Introduce a new multiplication $*$ on $Q$ by $x*y = xyh^{\sigma(i+j)}$, where $x\in \alpha^i$, $y\in\alpha^j$,
#! $i\in M$ and $j\in M$.
#! Then $(Q,*)$ is a Moufang loop, a **cyclic modification**<Index Subkey="cyclic">modification</Index> of $Q$.
DeclareGlobalFunction( "LoopByCyclicModification" );

#! @EndGroup

#! @BeginGroup
#! @GroupTitle *Dihedral modification

#! @Arguments Q, S, e, f, h
#! @Returns The dihedral modification of a Moufang loop <Arg>Q</Arg> obtained from <Arg>S</Arg>, <Arg>e</Arg>, <Arg>f</Arg> and <Arg>h</Arg> as described below.
#! We allow <Arg>S</Arg> to be normal subloop of <Arg>L</Arg> or a list of elements of <Arg>L</Arg> generating a normal subloop.
#! The returned loop will always be index based.
#! @Description Let $Q$ be a Moufang loop with a normal subloop $S$ such that $Q/S$ is a dihedral group of order $4m$,
#! with $m\ge 1$. Let $M$ and $\sigma$ be efined as in the cyclic case. Let $\beta$, $\gamma$ be two involutions
#! of $Q/S$ such that $\alpha=\beta\gamma$ generates a cyclic subgroup of $Q/S$ of order $2m$. Let $e\in\beta$
#! and $f\in\gamma$ be arbitrary. Then $Q$ can be written as a disjoint union $Q=\bigcup_{i\in M}(\alpha^i\cup e\alpha^i)$,
#! and also $Q=\bigcup_{i\in M}(\alpha^i\cup \alpha^if)$. Let $G_0=\bigcup_{i\in M}\alpha^i$, and $G_1=L\setminus G_0$.
#! Let $h\in S\cap N(L)\cap Z(G_0)$. Introduce a new multiplication $*$ on $Q$ by $x*y = xyh^{(-1)^r\sigma(i+j)}$,
#! where $x\in\alpha^i\cup e\alpha^i$, $y\in\alpha^j\cup \alpha^jf$, $i\in M$, $j\in M$, $y\in G_r$ and $r\in\{0,1\}$.
#! Then $(Q,*)$ is a Moufang loop, a **dihedral modification**<Index Subkey="dihedral">modification</Index> of $Q$.
DeclareGlobalFunction( "LoopByDihedralModification" );

#! @EndGroup