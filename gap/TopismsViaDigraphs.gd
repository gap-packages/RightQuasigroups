# TopismsViaDigraphs.gd
# Methods for topisms and morphism using the associated colored digraph
# ============================================================================

#! @Chapter Homotopisms, isotopisms and autotopisms

#! @Section Topisms using the associated colored digraph

#! <P/>TODO: EXPLAIN ASSOCIATED COLORED DIGRAPHS

##########################
###    DECLARATIONS    ###
##########################

#! @Arguments Q
#! @Returns a directed graph on $3n+n^2$ vertices, where $n=|Q|$. The vertices $\{1,\ldots,n\}$,
#! $\{n+1,\ldots,2n\}$, $\{2n+1,\ldots,3n\}$ represent the rows, columns and symbols of the
#! multiplication table of $Q$. The remaining $n^2$ vertices stand for the triples $(x,y,x*y)$. 
DeclareAttribute( "RQ_Digraph", IsRightQuasigroup );

#! @Arguments Q
#! @Returns a permutation of degree $n$, where $n=|Q|$.
DeclareAttribute( "RQ_BlissCanonicalLabeling4Morphism", IsRightQuasigroup );

#! @Arguments Q
#! @Returns a permutation of degree $3n$, where $n=|Q|$. 
DeclareAttribute( "RQ_BlissCanonicalLabeling4Topism", IsRightQuasigroup );

