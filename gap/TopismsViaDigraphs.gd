# TopismsViaDigraphs.gd
# Methods for topisms and morphism using the associated colored digraph
# ============================================================================

#! @Chapter Homotopisms, isotopisms and autotopisms

#! @Section Topisms using the associated colored digraph

#! <P/>Let $(M,*)$ be a magma of order $n$, $M=\{m_1,\ldots,m_n\}$. Define the set 
#! $T$, which consists of triples $\{i,j,k\}$, $1\leq i \leq n &lt; j \leq 2n 
#! &lt; k \leq 3n$, such that $\{i,j,k\} \in T$ if and only if $m_i*m_{j-n} = 
#! m_{k-2n}$. Clearly, $|T|=n^2$. Define $V=\{1,\ldots,3n\} \cup T$ and 
#! $E=\{(i,t) : 1\leq i \leq 3n, i \in t\in T\}$. Then $\Gamma=(V,E)$ is a directed graph 
#! on $3n+n^2$ vertices. We call $\Gamma$ the **digraph associated to $M$**. 
#! If we define the color classes $\{1,\ldots,n\}$, $\{n+1,\ldots,2n\}$, $\{2n+1,\ldots,3n\}$ 
#! and $T$ on the vertices of $\Gamma$, then we speak of the **colored digraph associated to** 
#! $M$. 

#! <P/>Two colored digraphs are isomorphic, if there is a bijection between their vertices, which 
#! preserves adjacency and colors. Two magmas are isotopic if and only if their associated
#! colored digraphs are isomorphic. There is a one-to-one correspondance between autotopisms
#! of a magma and the (color preserving) automorphisms of the associated colored digraph. 

#! <P/>We use the BLISS interface of the <Package>Digraphs</Package> package to compute isomorphisms and automorphisms
#! of colored digraphs. As a by-product of Brendon McKay's graph automorphism algorithm, we obtain 
#! a **canonical labeling** of $\Gamma$. Canonical labelings may speed up the search for isomorphisms
#! between colored digraphs. However, canonical labeling of graphs are not uniquely defined, they
#! may depend on the software version and the hardware specifications. 

##########################
###    DECLARATIONS    ###
##########################

#! @Arguments Q
#! @Returns the colored directed graph on $3n+n^2$ vertices, where $n=|Q|$. 
DeclareAttribute( "RQ_Digraph", IsRightQuasigroup );

#! @BeginGroup
#! @Returns a permutation of degree $n$ or $3n$, where $n=|Q|$.
#! @Description Let $Q,S$ be two right quasigroups and $u,v$ the permutations returned by 
#! `RQ_BlissCanonicalLabeling4Morphism`. Let $Q'$ be the isomorph of $Q$ via $uv^{-1}$. 
#! Then $Q$ and $S$ are isomorphic if and only if $Q'$ and $S$ have the same multiplication table.
#! Similarly, let $w,z$ be the permutations returned by `RQ_BlissCanonicalLabeling4Topism`.
#! The map $wz^{-1}$ induces three mappings $g,f,h:Q\to S$. Let $Q'$ be the right quasigroup 
#! isotope of $Q$ w.r.t. the mappings $f,g,h$. Then $Q$ and $S$ are isotopic if and only if
#! $Q'$ and $S$ have the same multiplication table. 
#! @Arguments Q
DeclareAttribute( "RQ_BlissCanonicalLabeling4Morphism", IsRightQuasigroup );
#! @Arguments Q
DeclareAttribute( "RQ_BlissCanonicalLabeling4Topism", IsRightQuasigroup );
#! @EndGroup