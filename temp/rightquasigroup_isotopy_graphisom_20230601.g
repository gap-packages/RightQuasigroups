LoadPackage("rightquasi"); 
LoadPackage("digraph");

##########################
###    DECLARATIONS    ###
##########################

#! Arguments elms
#! @Description A method to check if the elements of the list <A>elms</A> do generate a 
#! group of autotopisms of a given right quasigroup. 
InstallMethod( IsGeneratorsOfMagmaWithInverses,
    "for a collection of right quasigroup homotopisms",
    [ IsRightQuasigroupHomotopismCollection ],
    elms -> ForAll( elms, x -> 
        Range( x ) = Range( elms[1] ) 
        and Source( x ) = Range( elms[1] ) 
        and Inverse( x ) <> fail 
    ) 
);

DeclareOperation( "RQ_AutotopismGroupByGeneratorsNC", [ IsRightQuasigroup, IsRightQuasigroupHomotopismCollection ] );
#! @Arguments Q
#! @Returns a group consisting of <C>HomotopismRightQuasigroups</C> objects. This group has a
#! a nice monomorphism to a permutation group of degree $3n$ where $n$ is the order of the 
#! parent group of <A>Q</A>.
#! @Arguments Q
DeclareAttribute( "RQ_AutotopismGroup", IsRightQuasigroup );
#! @Returns a permutation group whose degree is the order of the parent of <A>Q</A>. 
DeclareAttribute( "RQ_AutGroupByDigraph", IsRightQuasigroup );
#! @Arguments Q
#! @Returns a directed graph on $3n+n^2$ vertices, where $n=|Q|$. The vertices $\{1,\ldots,n\}$,
#! $\{n+1,\ldots,2n\}$, $\{2n+1,\ldots,3n\}$ represent the rows, columns and symbols of the
#! multiplication table of $Q$. The remaining $n^2$ vertices stand for the triples $(x,y,x*y)$. 
DeclareAttribute( "RQ_Digraph", IsRightQuasigroup );

#! @Arguments t
#! @Returns a permutation of degree $3n$, where $n$ is the size of the parent right quasigroup
#! of the source of <A>t</A>. 
#! @Description The input is a <C>HomotopismRightQuasigroups</C> object. This map is a nice
#! monomorphism for this category.
DeclareGlobalFunction( "RQ_NiceMonomorphism" );
# Input: a parent permutation of degree 3*n
# Output: a triple of permutations of degree n
#! @Arguments Q
#! @Returns the inverse function $f$ of the nice monomorphism <C>HomotopismRightQuasigroups</C> objects
#! whose source is the right quasigroup <A>Q</A>. The function $f$ maps permutations of degree $3n$ to 
#! isotopisms of <A>Q</A>, where $n$ is the size of the parent of <A>Q</A>. 
DeclareGlobalFunction( "RQ_NiceMonomorphismInverse" );


#! @Arguments Q,S,t
#! @Returns a permutation between the parents, mapping <A>Q</A> to <A>S</A>. 
DeclareOperation( "AsParentPerm", [ IsRightQuasigroup, IsRightQuasigroup, IsPerm ] );

#! @Arguments Q
#! @Returns a permutation of degree $n$, where $n=|Q|$.
DeclareAttribute( "RQ_BlissCanonicalLabeling4Morphism", IsRightQuasigroup );
#! @Arguments Q
#! @Returns a permutation of degree $3n$, where $n=|Q|$. 
DeclareAttribute( "RQ_BlissCanonicalLabeling4Topism", IsRightQuasigroup );
#! @Arguments Q,S
#! @Returns a right quasigroup mapping
DeclareOperation( "RQ_IsomorphismByDigraph", [ IsRightQuasigroup, IsRightQuasigroup ] );
#! @Arguments Q,S
#! @Returns a right quasigroup homotopism object
DeclareOperation( "RQ_IsotopismByDigraph", [ IsRightQuasigroup, IsRightQuasigroup ] );

#############################
###    IMPLEMENTATIONS    ###
#############################

InstallGlobalFunction( RQ_NiceMonomorphism, 
function( htop )
    local n;
    n := Size( Parent( Source( htop ) ) );
    return PermList( 
        Concatenation( List( [1,2,3], i -> (i-1)*n + ListPerm( ComponentOfHomotopism( htop, i), n ) ) ) 
    );
end);

InstallGlobalFunction( RQ_NiceMonomorphismInverse, 
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

InstallMethod( AsParentPerm, "for two right quasigroups and canonical permutation",
    [ IsRightQuasigroup, IsRightQuasigroup, IsPerm ],
function( Q, S, t )
    local indQ, indS, imgs, parperm;
    indQ := ParentInd( Q );
    indS := ParentInd( S );
    imgs := List( [1..Size(Q)], i -> indS[ i^t ] );
    parperm := MappingPermListList( indQ, imgs );
    if parperm = fail then 
        Error( "RQ: <3> must be a bijective transformation from <1> to <2>." );
    else
        return parperm;
    fi;
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

InstallMethod( RQ_IsomorphismByDigraph, "for two right quasigroups",
    [ IsRightQuasigroup, IsRightQuasigroup ],
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

InstallMethod( RQ_BlissCanonicalLabeling4Topism, "for a right quasigroup",
    [ IsRightQuasigroup ],
function( rq )
    local n, vert_colors, cl;
    n := Size( rq );
    vert_colors := List( [1..(3+n)*n], i->Minimum( 4, 1+Int((i-1)/n) ) );
    cl := BlissCanonicalLabelling( RQ_Digraph( rq ), vert_colors );
    return RestrictedPerm( cl, [1..3*n] );
end );

InstallMethod( RQ_IsotopismByDigraph, "for two right quasigroups",
    [ IsRightQuasigroup, IsRightQuasigroup ],
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
    return iso;
end );

InstallMethod( RQ_AutotopismGroupByGeneratorsNC, "for a right quasigroup and a list of autotopisms",
    [ IsRightQuasigroup, IsRightQuasigroupHomotopismCollection ],
function( rq, gens )
    local atpgr, ag, nice;
    if gens = [ ] then gens := [ IdentityAutotopism( rq ) ]; fi;
    atpgr := MakeGroupyObj( FamilyObj( gens ), IsGroup, gens, false );
    ag := Group( List( gens, RQ_NiceMonomorphism ) );
    nice := GroupHomomorphismByFunction( 
        atpgr, 
        ag, 
        RQ_NiceMonomorphism, 
        RQ_NiceMonomorphismInverse( rq )
    );
    SetNiceMonomorphism( atpgr, nice );
    SetIsHandledByNiceMonomorphism( atpgr, true );
    return atpgr;
end );

InstallMethod( GroupByGenerators, "for a list of autotopisms", 
    [ IsRightQuasigroupHomotopismCollection ],
function( gens )
    return RQ_AutotopismGroupByGeneratorsNC( Source( gens[1] ), AsList( gens ) );
end );

InstallMethod( RQ_AutotopismGroup, "for a right quasigroup",
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
    gens := List( gens, li -> 
        HomotopismRightQuasigroups( rq, rq, li[1], li[2], li[3] ) );
    return RQ_AutotopismGroupByGeneratorsNC( rq, gens );
end );

InstallMethod( RQ_AutGroupByDigraph, "for a right quasigroup",
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

if not IsBound(n) then n:=36; fi;

rq:=ConnectedQuandle(n,5);
ig:=RQ_AutotopismGroup(rq); Print( "#time = ", time, "\n"); Size(ig);

###

rq:=ConnectedQuandle(n,5);

ag2:=RQ_AutGroupByDigraph(rq); Print( "#time = ", time, "\n");
ag2=AutomorphismGroup(rq);

###

n2:=32;
rq:=RandomNilpotentLoop([ElementaryAbelianGroup(2),ElementaryAbelianGroup(n2/2)]);

ag2:=RQ_AutGroupByDigraph(rq); Print( "#time = ", time, "\n");
AutomorphismGroup(rq); Print( "#time = ", time, "\n");
ag2=AutomorphismGroup(rq);

ig:=RQ_AutotopismGroup(rq); Print( "#time = ", time, "\n");
Size(ig);
Exponent(ig);

li:=List([1..5],i->Random(ig));
h:=Group(li);
Size(h);
KnownAttributesOfObject(h);

###

Q1 := RightQuasigroupByFunction( [0..9], function(x,y) return (x+2*y) mod 10; end );
Q2 := RightQuasigroupIsomorph( Q1, (3,4,5) );
RQ_IsomorphismByDigraph( Q1, Q2 );

Q1:=MoufangLoop(64,222);
f:=List([1,2,3],i->Random(SymmetricGroup([2..64])));
Q2:=LoopIsotope(Q1,f[1],f[2],f[3]);
iso:=RQ_IsotopismByDigraph(Q1,Q2); Print( "#time = ", time, "\n");
# IsotopismRightQuasigroups(Q1,Q2); time; # this takes very very long
ff:=List([1,2,3],i->AsPermutation(ComponentOfHomotopism(iso,i)));
AutotopismRightQuasigroup(Q1,f[1]/ff[1],f[2]/ff[2],f[3]/ff[3]);
