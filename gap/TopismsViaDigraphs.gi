# TopismsViaDigraphs.gi
# Methods for topisms and morphism using the associated colored digraph
# ============================================================================

#############################
###    IMPLEMENTATIONS    ###
###    DIGRAPHS MAGIC     ###
#############################

InstallMethod( RQ_Digraph, "for a right quasigroup",
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

InstallMethod( RQ_BlissCanonicalLabeling4Morphism, "for a right quasigroup",
    [ IsRightQuasigroup ],
function( rq )
    local n, vert_colors, i, digr, cl;
    n := Size( rq );
    vert_colors := List( [1..(3+n)*n], i->Minimum( 4, 1+Int((i-1)/n) ) );;
    for i in [1..n] do 
        Add( vert_colors, 5 ); 
    od;
    digr := DigraphMutableCopy( RQ_Digraph( rq ) );
    DigraphAddVertices( digr, n );
    for i in [1..n] do 
        DigraphAddEdge( digr, [ i, (3+n)*n+i] ); 
        DigraphAddEdge( digr, [ n+i, (3+n)*n+i] ); 
        DigraphAddEdge( digr, [ 2*n+i, (3+n)*n+i] ); 
    od;
    cl := BlissCanonicalLabelling( digr, vert_colors );
    cl := RestrictedPerm( cl, [1..n] );
    return cl;
end );

InstallMethod( RQ_BlissCanonicalLabeling4Topism, "for a right quasigroup",
    [ IsRightQuasigroup ],
function( rq )
    local n, vert_colors, cl;
    n := Size( rq );
    vert_colors := List( [1..(3+n)*n], i->Minimum( 4, 1+Int((i-1)/n) ) );
    cl := BlissCanonicalLabelling( RQ_Digraph( rq ), vert_colors );
    return RestrictedPerm( cl, [1..3*n] );
end );

#####################################
###    IMPLEMENTATIONS            ###
###    PACKAGE EXTENSION MAGIC    ###
#####################################

# Returns a right quasigroup mapping
InstallMethod( IsomorphismRightQuasigroups, "for two right quasigroups, via digraphs",
    [ IsRightQuasigroup, IsRightQuasigroup ], 10,
function( Q, S )
    local iso;
    if Size( Q ) <> Size( S ) then return fail; fi;
    iso := RQ_BlissCanonicalLabeling4Morphism( Q ) / RQ_BlissCanonicalLabeling4Morphism( S );
    iso := AsRightQuasigroupMapping( Q, S, AsTransformation( iso ), true ); # canonical perm
    if RespectsMultiplication( iso ) then 
        return iso;
    else
        return fail;
    fi;
end );

# Returns a right quasigroup homotopism object
InstallOtherMethod( IsotopismRightQuasigroups, "for two right quasigroups, via digraphs",
    [ IsRightQuasigroup, IsRightQuasigroup ], 10,
function( Q, S )
    local n, clQ, clS, iso;
    if Size( Q ) <> Size( S ) then return fail; fi;
    n := Size( Q );
    clQ := RQ_BlissCanonicalLabeling4Topism( Q );
    clQ := List( [0,1,2], i -> PermList( OnTuples( i*n + [1..n], clQ ) - i*n ) );
    clS := RQ_BlissCanonicalLabeling4Topism( S );
    clS := List( [0,1,2], i -> PermList( OnTuples( i*n + [1..n], clS ) - i*n ) );
    iso := HomotopismRightQuasigroups( 
        Q, 
        S, 
        AsTransformation( clQ[1]/clS[1] ),  
        AsTransformation( clQ[2]/clS[2] ),  
        AsTransformation( clQ[3]/clS[3] ), 
        true # canonical perms
    ); 
    SetIsBijective( iso, true );
    return iso;
end );

InstallMethod( AutotopismGroup, "for a right quasigroup, via digraphs",
    [ IsRightQuasigroup ], 10,
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
    gens := List( gens, li -> 
        HomotopismRightQuasigroups( rq, rq, li[1], li[2], li[3] ) );
    return RQ_AutotopismGroupByGeneratorsNC( rq, gens );
end );

InstallOtherMethod( AutomorphismGroup, "for a right quasigroup, via digraphs",
    [ IsRightQuasigroup ], 
function( rq )
    local n, digr, i, ag, vert_colors;
    n := Size( rq );
    vert_colors := List( [1..(3+n)*n], i->Minimum( 4, 1+Int((i-1)/n) ) );;
    for i in [1..n] do 
        Add( vert_colors, 5 ); 
    od;
    digr := DigraphMutableCopy( RQ_Digraph( rq ) );
    DigraphAddVertices( digr, n );
    for i in [1..n] do 
        DigraphAddEdge( digr, [ i, (3+n)*n+i] ); 
        DigraphAddEdge( digr, [ n+i, (3+n)*n+i] ); 
        DigraphAddEdge( digr, [ 2*n+i, (3+n)*n+i] ); 
    od;
    ag := AutomorphismGroup( digr, vert_colors );
    ag := Action( ag, [1..n] );
    ag := Group( List( GeneratorsOfGroup( ag ), x -> AsParentPerm( rq, x ) ) );
    return ag;
end );

