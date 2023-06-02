LoadPackage("rightquasi"); 
LoadPackage("digraph");

InstallMethod( IsGeneratorsOfMagmaWithInverses,
    "for a collection of right quasigroup homotopisms",
    [ IsRightQuasigroupHomotopismCollection ],
    elms -> ForAll( elms, x -> Inverse( x ) <> fail ) 
);


#! @Arguments Q
#! @Returns a permutation group <C>G</C> of degree <C>3*n</C>, where <C>n=|Q|</C>. The actions on
#! <C>[1..n]</C>, <C>[n+1..2*n]</C> and <C>[2*n+1..3*n]</C> correspond to the permutations
#! <C>f,g,h</C> such that <C>x^f*y^g=(x*y)^h</C> holds. 
DeclareAttribute( "RQ_AutotopismGroup", IsRightQuasigroup );
DeclareAttribute( "RQ_AutGroupByGraph", IsRightQuasigroup );
#! @Arguments Q
#! @Returns a directed graph on $3n+n^2$ vertices, where $n=|Q|$. The vertices $\{1,\ldots,n\}$,
#! $\{n+1,\ldots,2n\}$, $\{2n+1,\ldots,3n\}$ represent the rows, columns and symbols of the
#! multiplication table of $Q$. The remaining $n^2$ vertices stand for the triples $(x,y,x*y)$. 
DeclareAttribute( "RQ_Digraph", IsRightQuasigroup );

# Input: a triple of permutations of degree n
# Output: a parent permutation of degree 3*n
DeclareGlobalFunction( "RQ_NiceMonomorphismWithDegree" );
InstallGlobalFunction( RQ_NiceMonomorphismWithDegree, 
function( htop )
    local n;
    n := Size( Parent( Source( htop ) ) );
    return PermList( 
        Concatenation( List( [1,2,3], i -> (i-1)*n + ListPerm( ComponentOfHomotopism( htop, i), n ) ) ) 
    );
end);

# Input: a parent permutation of degree 3*n
# Output: a triple of permutations of degree n
DeclareGlobalFunction( "RQ_NiceMonomorphismInverseWithDegree" );
InstallGlobalFunction( RQ_NiceMonomorphismInverseWithDegree, 
function( rq )
    local n, fun;
    n := Size( Parent( rq ) );
    fun := function( perm )
        local li;
        li := List( [0,1,2], i -> PermList( OnTuples( i*n + [1..n], perm ) - i*n ) );
        return HomotopismRightQuasigroups( rq, rq, li[1], li[2], li[3] );
    end;
    return fun;
end);

InstallMethod( RQ_Digraph, "for right quasigroups",
    [ IsRightQuasigroup ],
function( rq )
    local n, mt, src, ran, i, j, k, block_pos;
    n := Size( rq );
    mt := MultiplicationTable( rq );
    src := [];
    ran := [];
    for i in [1..n] do
        for j in [1..n] do
            k := mt[i][j];
            block_pos := (2+i)*n+j;
            Append( src, [ i, n+j, 2*n+k ] );
            Append( ran, [block_pos, block_pos, block_pos] );
        od;
    od;
    return Digraph( (3+n)*n, src, ran );
end );

InstallMethod( RQ_AutotopismGroup, "for right quasigroups",
    [ IsRightQuasigroup ], 
function( rq )
    local n, npar, digr, ag, vert_colors, gens, atpgr, nice;
    n := Size( rq );
    npar := Size( Parent( rq ) );
    vert_colors := List( [1..(3+n)*n], i->Minimum( 4, 1+Int((i-1)/n) ) );
    ag := AutomorphismGroup( RQ_Digraph( rq ), vert_colors );
    gens := List( GeneratorsOfGroup( ag ), perm -> 
        List( [0,1,2], i -> PermList( OnTuples( i*n + [1..n], perm ) - i*n ) ) );
    gens := List( gens, li -> 
        List( li, y -> AsParentPerm( rq, y ) ) );
    ag := Group( 
        List( gens, li ->
            PermList( 
                Concatenation( List( [1,2,3], i -> (i-1)*npar + ListPerm( li[i], npar ) ) ) 
            )
        ) 
    );
    gens := List( gens, li -> 
        HomotopismRightQuasigroups( rq, rq, li[1], li[2], li[3] ) );
    atpgr := Group( gens );
    nice := GroupHomomorphismByFunction( 
        atpgr, 
        ag, 
        RQ_NiceMonomorphismWithDegree, 
        RQ_NiceMonomorphismInverseWithDegree( rq )
    );
    SetNiceMonomorphism( atpgr, nice );
    SetIsHandledByNiceMonomorphism( atpgr, true );
    return atpgr;
end );

InstallMethod( RQ_AutGroupByGraph, "for right quasigroups",
    [ IsRightQuasigroup ], 
function( rq )
    local n, digr, i, ag, vert_colors;
    n := Size( rq );
    digr := DigraphMutableCopy( RQ_Digraph( rq ) );
    DigraphAddVertices( digr, n );
    for i in [1..n] do 
        DigraphAddEdge( digr, [ i, (3+n)*n+i] ); 
        DigraphAddEdge( digr, [ n+i, (3+n)*n+i] ); 
        DigraphAddEdge( digr, [ 2*n+i, (3+n)*n+i] ); 
    od;
    vert_colors := List( [1..(3+n)*n], i->Minimum( 4, 1+Int((i-1)/n) ) );;
    for i in [1..n] do 
        Add( vert_colors, 5 ); 
    od;
    ag := AutomorphismGroup( digr, vert_colors );
    ag := Action( ag, [1..n] );
    ag := Group( List( GeneratorsOfGroup( ag ), x -> AsParentPerm( rq, x ) ) );
    return ag;
end );

###

if not IsBound(n) then n:=36; fi;

rq:=ConnectedQuandle(n,5);
ig:=RQ_AutotopismGroup(rq); Print( "#time = ", time, "\n"); Size(ig);

###

rq:=ConnectedQuandle(n,5);

ag2:=RQ_AutGroupByGraph(rq); Print( "#time = ", time, "\n");
ag2=AutomorphismGroup(rq);

###

n2:=32;
rq:=RandomNilpotentLoop([ElementaryAbelianGroup(2),ElementaryAbelianGroup(n2/2)]);

ag2:=RQ_AutGroupByGraph(rq); Print( "#time = ", time, "\n");
AutomorphismGroup(rq); Print( "#time = ", time, "\n");
ag2=AutomorphismGroup(rq);

ig:=RQ_AutotopismGroup(rq); Print( "#time = ", time, "\n");
Size(ig);
