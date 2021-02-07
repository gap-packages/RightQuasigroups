# BolLoops.gd
# Methods for Bol loops
# ============================================================================

#! @Chapter Bol loops, Bruck loops and Moufang loops

#! @Section Bol loops and Bruck loops

#! <P/>A right Bruck loop is a right Bol loop satisfying the automorphic inverse property.
#! If $Q$ is a right Bol loop in which the squaring map $x\mapsto x^2$ is a permutation,
#! then $(Q,\circ)$ with multiplication $x\circ y = ((x(yy))x)^{1/2}$ is a right Bruck loop,
#! the right Bruck loop <Index>associated right Bruck loop</Index>**associated**
#! with $Q$. Here, $x^{1/2}$ is the square-root of $x$, the unique element $y$ of $Q$ such
#! that $y^2=x$.

#! <P/>For a left Bol loop in which squaring is a permutation, the 
#! associated left Bruck loop is defined by $x\circ y = (x((yy)x))^{1/2}$. 

#! @BeginGroup
#! @GroupTitle Associated Bruck loop

#! @Arguments Q
#! @Returns the right (left) Bruck loop associated with the right (left) Bol loop `Q`.
#! The returned loop is index based iff `Q` is index based.
DeclareAttribute( "AssociatedRightBruckLoop", IsRightBolLoop );

#! @Arguments Q
DeclareAttribute( "AssociatedLeftBruckLoop", IsLeftBolLoop );

#! @EndGroup

#! <P/>A triple $G$, $H_1$, $H_2$ is an <Index>exact group factorization</Index>**exact group factorization**
#! if $H_1$, $H_2$ are subgroups of $G$ such that $H_1\cap H_2=1$ and $H_1H_2=G$.

#! <P/>Given an exact group factorization $G$, $H_1$, $H_2$, the triple $(G\times G, H_1\times H_2, \{(g,g^{-1}):g\in G\})$
#! is a loop right folder and the resulting group is automatically a right Bol loop.

#! @Arguments G, H1, H2 
#! @Returns `true` if `G`, `H1`, `H2` is an exact group factorization, else returns `false`.
DeclareOperation( "IsExactGroupFactorization", [ IsGroup, IsGroup, IsGroup ] );

#! @Arguments G, H1, H2[, constructorStyle]
#! @Returns the right Bol loop obtained from the exact group factorization `G`, `H1`, `H2`.
DeclareOperation( "RightBolLoopByExactGroupFactorization", [ IsGroup, IsGroup, IsGroup ] ); 

#! @BeginExampleSession
#! gap> G := SymmetricGroup( 5 );;
#! gap> H1 := Subgroup( G, [(1,2),(1,3),(1,4)] );;
#! gap> H2 := Subgroup( G, [(1,2,3,4,5)] );;
#! gap> IsExactGroupFactorization( G, H1, H2 );
#! true
#! gap> RightBolLoopByExactGroupFactorization( G, H1, H2 );
#! <right Bol loop of order 120>
#! @EndExampleSession