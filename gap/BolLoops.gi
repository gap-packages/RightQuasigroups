# BolLoops.gi
# Methods for Bol loops
# ============================================================================

# AssociatedRightBruckLoop
InstallMethod( AssociatedRightBruckLoop,
    [ IsRightBolLoop ],
function( Q )
    local n, elms, squares, roots, uSet, t, ct, L, F;
    n := Size( Q );
    elms := Elements( Q );
    squares := List( Q, x -> PositionSorted( elms, x*x ) );
    if not Size( Set( squares ) ) = n then
        Error( "RQ: <1> must be a right Bol loop in which squaring is a bijection." );
    fi;
    roots := Inverse( PermList( squares ) );
    uSet := UnderlyingSet( Q );
    if IsIndexBased( Q ) then
        t := MultiplicationTable( Q );
        ct := List( [1..n], i -> List( [1..n], j -> uSet[ (t[t[j,t[i,i]],j])^roots ] ) ); # ((y*(x*x))*y)^(1/2)
        L := LoopByCayleyTable( ct, ConstructorStyle( true, false ) );
    else # not index based
        L := RQ_AlgebraShell( IsLoop, uSet, ConstructorStyle( false, false ) );
        F := FamilyObj( L.1 );
        F!.roots := roots;
        F!.origElms := ShallowCopy( elms );
        F!.mult := function( x, y )
            x := F!.origElms[ PositionSorted( F!.uSet, x ) ];
            y := F!.origElms[ PositionSorted( F!.uSet, y ) ];
            return F!.uSet[ PositionSorted( F!.origElms, (y*(x*x))*y )^F!.roots ];           
        end;
        RQ_AddDefaultOperations( L );
    fi;
    SetIsRightBruckLoop( L, true );
    return L;
end );

# AssociatedLeftBruckLoop
InstallMethod( AssociatedLeftBruckLoop,
    [ IsLeftBolLoop ],
function( Q )     
    # REVISIT: we could also call OppositeLoop( AssociatedRightBruckLoop( OppositeLoop( Q ) ) )
    local n, elms, squares, roots, uSet, t, ct, L, F;
    n := Size( Q );
    elms := Elements( Q );
    squares := List( Q, x -> PositionSorted( elms, x*x ) );
    if not Size( Set( squares ) ) = n then
        Error( "RQ: <1> must be a right Bol loop in which squaring is a bijection." );
    fi;
    roots := Inverse( PermList( squares ) );
    uSet := UnderlyingSet( Q );
    if IsIndexBased( Q ) then
        t := MultiplicationTable( Q );
        ct := List( [1..n], i -> List( [1..n], j -> uSet[ (t[i,t[t[j,j],i]])^roots  ] ) ); # (x*((y*y)*x))^(1/2)
        L := LoopByCayleyTable( ct, ConstructorStyle( true, false ) );
    else # not index based
        L := RQ_AlgebraShell( IsLoop, uSet, ConstructorStyle( false, false ) );
        F := FamilyObj( L.1 );
        F!.roots := roots;
        F!.origElms := ShallowCopy( elms );
        F!.mult := function( x, y )
            x := F!.origElms[ PositionSorted( F!.uSet, x ) ];
            y := F!.origElms[ PositionSorted( F!.uSet, y ) ];
            return F!.uSet[ PositionSorted( F!.origElms, x*((y*y)*x) )^F!.roots ];           
        end;
        RQ_AddDefaultOperations( L );
    fi;
    SetIsLeftBruckLoop( L, true );
    return L;
end );

# IsExactGroupFactorization( G, H1, H2 )
InstallMethod( IsExactGroupFactorization, "for a group and two subgroups",
    [ IsGroup, IsGroup, IsGroup ],
function( G, H1, H2 )
    return IsSubgroup(G,H1) and IsSubgroup(G,H2) and Size(G)=Size(H1)*Size(H2) and IsTrivial(Intersection(H1,H2));
end);

# RightBolLoopByExactGroupFactorization
# PROG: constructor OK, calls LoopByRightFolder
InstallMethod( RightBolLoopByExactGroupFactorization, "for group and two subgroups",
    [IsGroup, IsGroup, IsGroup ],
function( g, h1, h2 )
    return RightBolLoopByExactGroupFactorization( g, h1, h2, RQ_defaultConstructorStyle );
end );

InstallOtherMethod( RightBolLoopByExactGroupFactorization, "for group, two subgroups and record",
    [ IsGroup, IsGroup, IsGroup, IsRecord ],
function( g, h1, h2, style )
    local G, H, T, Q;
    if not IsSubgroup( g, h1 ) and IsSubgroup( g, h2 ) then
        Error( "RQ: <2> and <3> must be subgroups of <1>." );
    fi;
    if style.checkArguments and not IsExactGroupFactorization( g, h1, h2 ) then
        Error("RQ: <1>, <2>, <3> must be an exact group factorization.");
    fi;
	G := DirectProduct( g, g );
    H := ClosureGroup( Image( Embedding( G, 1 ), h1 ), Image( Embedding( G, 2 ), h2 ) );
    T := List( g, x -> Image( Embedding( G, 1 ), x )*Image( Embedding( G, 2 ), x^-1 ) );
    Q := LoopByRightFolder( G, H, T, ConstructorStyle( style.indexBased, false ) ); # nothing to check
    SetIsRightBolLoop( Q, true );
    return Q;
end);