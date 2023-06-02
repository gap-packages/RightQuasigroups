LoadPackage("rightquasi"); 
LoadPackage("digraph");

#! @Arguments Q
#! @Returns a permutation group <C>G</C> of degree <C>3*n</C>, where <C>n=|Q|</C>. The actions on
#! <C>[1..n]</C>, <C>[n+1..2*n]</C> and <C>[2*n+1..3*n]</C> correspond to the permutations
#! <C>f,g,h</C> such that <C>x^f*y^g=(x*y)^h</C> holds. 
DeclareAttribute( "RQ_AutotopismGroup", IsRightQuasigroup );
DeclareAttribute( "RQ_AutGroupByGraph", IsRightQuasigroup );
DeclareAttribute( "RQ_Graph", IsRightQuasigroup );

InstallMethod( RQ_Graph, "for right quasigroups",
    [ IsRightQuasigroup ],
function( rq )
    local n, mt, src, ran, i, j, k, block_pos;
    n := Size( rq );
    mt := MultiplicationTable( rq );
    src := [];
    ran := [];
    for i in [1..n] do
        for j in [1..n] do
            #k := Position( Elements(rq), rq[i]*rq[j] );
            k := mt[i][j];
            block_pos := (2+i)*n+j;
            #Append( edges, [ [i,block_pos], [n+j,block_pos], [2*n+k,block_pos] ] );
            Append( src, [ i, n+j, 2*n+k ] );
            Append( ran, [block_pos, block_pos, block_pos] );
        od;
    od;
    return Digraph( (3+n)*n, src, ran );
end );

InstallMethod( RQ_AutotopismGroup, "for right quasigroups",
    [ IsRightQuasigroup ], 
function( rq )
    local n, digr, ag, vert_colors;
    n := Size( rq );
    vert_colors := List( [1..(3+n)*n], i->Minimum( 4, 1+Int((i-1)/n) ) );
    ag:=AutomorphismGroup( RQ_Graph( rq ), vert_colors );
    return Action( ag, [1..3*n] );
end );

InstallMethod( RQ_AutGroupByGraph, "for right quasigroups",
    [ IsRightQuasigroup ], 
function( rq )
    local n, digr, i, ag, vert_colors;
    n := Size( rq );
    digr := DigraphMutableCopy( RQ_Graph( rq ) );
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
    return Action( ag, [1..n] );
end );

###

if not IsBound(n) then n:=36; fi;

rq:=ConnectedQuandle(n,5);
ig:=RQ_AutotopismGroup(rq); Print( "#time = ", time, "\n"); Size(ig);
ag1:=Stabilizer(ig,List([1..n],i->[i,n+i,2*n+i]),OnSetsTuples); time;

AutomorphismGroup(rq); Print( "#time = ", time, "\n"); 
Size(AutomorphismGroup(rq))=Size(ag1);

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
