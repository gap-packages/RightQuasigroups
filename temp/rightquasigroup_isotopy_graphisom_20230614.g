LoadPackage("rightquasi"); 
LoadPackage("digraph");

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

###########################
###        TESTS        ###
###########################

TraceMethods([AutotopismGroup,AutomorphismGroup,IsomorphismRightQuasigroups,IsotopismRightQuasigroups]);
AutotopismGroup(RightBolLoop(8,2));

if not IsBound(n) then n:=36; fi;

rq:=ConnectedQuandle(n,5);
ig:=AutotopismGroup(rq); Print( "#time = ", time, "\n"); Size(ig);

###

rq:=ConnectedQuandle(n,5);

ag2:=AutomorphismGroup(rq); Print( "#time = ", time, "\n");
ag2=AutomorphismGroup(rq);

###

n2:=32;
rq:=RandomNilpotentLoop([ElementaryAbelianGroup(2),ElementaryAbelianGroup(n2/2)]);

ag2:=AutomorphismGroup(rq); Print( "#time = ", time, "\n");
AutomorphismGroup(rq); Print( "#time = ", time, "\n");
ag2=AutomorphismGroup(rq);

ig:=AutotopismGroup(rq); Print( "#time = ", time, "\n");
Size(ig);
Exponent(ig);

li:=List([1..5],i->Random(ig));
h:=Group(li);
Size(h);
KnownAttributesOfObject(h);

###

Q1 := RightQuasigroupByFunction( [0..9], function(x,y) return (x+2*y) mod 10; end );
Q2 := RightQuasigroupIsomorph( Q1, (3,4,5) );
IsomorphismRightQuasigroups( Q1, Q2 );

Q1:=MoufangLoop(64,222);
f:=List([1,2,3],i->Random(SymmetricGroup([2..64])));
Q2:=LoopIsotope(Q1,f[1],f[2],f[3]);
iso:=IsotopismRightQuasigroups(Q1,Q2); Print( "#time = ", time, "\n");
# IsotopismRightQuasigroups(Q1,Q2); time; # this takes very very long
ff:=List([1,2,3],i->AsPermutation(ComponentOfHomotopism(iso,i)));
AutotopismRightQuasigroup(Q1,f[1]/ff[1],f[2]/ff[2],f[3]/ff[3]);
