# Random.gi
# Random right quasigroups, quasigroups and loops
# =============================================================================

# RANDOM RIGHT QUASIGROUPS, QUASIGROUPS AND LOOPS
# _____________________________________________________________________________

# RandomRightQuasigroup
# PROG: constructor OK, calls RQ_AlgebraBySection
InstallMethod( RandomRightQuasigroup, "for collection",
    [ IsCollection ],
    S -> RandomRightQuasigroup( S, RQ_defaultConstructorStyle )
);

InstallOtherMethod( RandomRightQuasigroup, "for collection and record",
    [ IsCollection, IsRecord ],
function( S, style )
    local n, rsection;
    RQ_CompleteConstructorStyle( style );
    S := Set( S );
    n := Size( S );
    rsection := List( [1..n], i -> Random( SymmetricGroup( n ) ) );
    return RQ_AlgebraByRightSection( IsRightQuasigroup, S, rsection, ConstructorStyle( style.indexBased, false ) );
end );

InstallOtherMethod( RandomRightQuasigroup, "for positive integer",
    [ IsPosInt ],
    n -> RandomRightQuasigroup( [1..n], RQ_defaultConstructorStyle )
);

InstallOtherMethod( RandomRightQuasigroup, "for positive integer and record",
    [ IsPosInt, IsRecord ],
function( n, style )
    return RandomRightQuasigroup( [1..n], style );
end );

# RQ_RandomAlgebra
# PROG: constructor OK, calls RQ_AlgebraByCayleyTable
InstallMethod( RQ_RandomAlgebra, "for category, collection, positive integer and record",
    [ IsObject, IsCollection, IsPosInt ], 
function( category, S, iter )
    local n, style, f, x, y, z, is_proper, xx, yy, zz, random_walk_step, i, ct;
    
    # processing arguments
    if IsPosInt( S ) then # order given instead of the underlying set
        S := [1..S];
    fi;
    S := Set( S );
    n := Size( S );
    style := ConstructorStyle( true, false );
    # special case n=1
    if n=1 then
        return RQ_AlgebraByCayleyTable( category, [[S[1]]], style );
    fi;

    # initializing funciton for proper and improper Latin squares
    # the meaning of f(x,y,z)=1 is that there is symbol z in row x and column y
    f := List([1..n], i -> List([1..n], j -> 0*[1..n]));
    # cyclic group of order n on symbols [1..n]
    for x in [1..n] do for y in [1..n] do 
        z := x+y-1;
        if z > n then
            z := z - n;
        fi;
        f[x][y][z] := 1;
    od; od;
    is_proper := true; # proper latin square to start with
    
    # one random walk step
    random_walk_step := function()
        local x, y, z, triples, x2, y2, z2, triple;
        if is_proper then
            repeat 
                x := Random([1..n]);
                y := Random([1..n]);
                z := Random([1..n]);
            until f[x][y][z]=0;
        fi;
        if not is_proper then # use unique point with f(x,y,z)=-1
            x := xx; 
            y := yy; 
            z := zz;
        fi;
        # find all suitable triples
        x2 := Filtered( [1..n], a -> f[a][y][z] = 1 );
        y2 := Filtered( [1..n], a -> f[x][a][z] = 1 );
        z2 := Filtered( [1..n], a -> f[x][y][a] = 1 );
        # pick a random suitable triple
        x2 := Random( x2 );
        y2 := Random( y2 );
        z2 := Random( z2 );
        # shuffle values
        f[x][y][z] := f[x][y][z] + 1;
        f[x][y2][z2] := f[x][y2][z2] + 1;
        f[x2][y][z2] := f[x2][y][z2] + 1;
        f[x2][y2][z] := f[x2][y2][z] + 1;
        f[x2][y][z] := f[x2][y][z] - 1;
        f[x][y2][z] := f[x][y2][z] - 1;
        f[x][y][z2] := f[x][y][z2] - 1;
        f[x2][y2][z2] := f[x2][y2][z2] - 1;
        # determine properness
        if f[x2][y2][z2] = 0 then
            is_proper := true;
        else
            is_proper := false;
            xx := x2;
            yy := y2;
            zz := z2;
        fi;
    end;
    
    # move into an initial point in the graph
    for i in [1..iter] do
        random_walk_step();
    od;
    # find a proper square nearby
    while not is_proper do
        random_walk_step();
    od;
    # construct the multiplication table from the function
    ct := List([1..n], i->[1..n]);
    for x in [1..n] do for y in [1..n] do
        ct[x,y] := Filtered([1..n], z -> f[x][y][z] = 1)[ 1 ];
    od; od;
    ct := List( ct, row -> List(row, i -> S[i] ) ); 
    if category = IsLoop then # normalizing the Cayley table
        ct := NormalizedQuasigroupCayleyTable( ct );
    fi;
    return RQ_AlgebraByCayleyTable( category, ct, style );
end);

# RandomQuasigroup
InstallMethod( RandomQuasigroup, "for collection",
    [ IsCollection ],
    S -> RQ_RandomAlgebra( IsQuasigroup, S, Length(S)^3 )
);

InstallOtherMethod( RandomQuasigroup, "for positive integer",
    [ IsPosInt ],
    n -> RQ_RandomAlgebra( IsQuasigroup, [1..n], n^3 )
);

InstallOtherMethod( RandomQuasigroup, "for collection and positive integer",
    [ IsCollection, IsPosInt ],
function( S, iter )
    return RQ_RandomAlgebra( IsQuasigroup, S, iter );
end);

InstallOtherMethod( RandomQuasigroup, "for two positive integers",
    [ IsPosInt, IsPosInt ],
function( n, iter )
    return RQ_RandomAlgebra( IsQuasigroup, [1..n], iter );
end);

# RandomLoop
InstallMethod( RandomLoop, "for collection",
    [ IsCollection ],
    S -> RQ_RandomAlgebra( IsLoop, S, Length(S)^3 )
);

InstallOtherMethod( RandomLoop, "for positive integer",
    [ IsPosInt ],
    n -> RQ_RandomAlgebra( IsLoop, [1..n], n^3 )
);

InstallOtherMethod( RandomLoop, "for collection and positive integer",
    [ IsCollection, IsPosInt ],
function( S, iter )
    return RQ_RandomAlgebra( IsLoop, S, iter );
end);

InstallOtherMethod( RandomLoop, "for two positive integers",
    [ IsPosInt, IsPosInt ],
function( n, iter )
    return RQ_RandomAlgebra( IsLoop, [1..n], iter );
end);

# RandomNilpotentLoop
# PROG: constructor OK, calls LoopByCentralExtension
InstallMethod( RandomNilpotentLoop, "for a list of abelian groups and positive integers",
    [ IsList ],
function( lst )                
    local n, K, F, f, theta, i, j;
    if IsEmpty( lst ) then
        Error("RQ: the argument must be a list of finite abelian groups and/or positive integers.");
    fi;
    n := lst[1];
    if not ( IsPosInt( n ) or (IsGroup( n ) and IsAbelian( n ) and IsFinite( n ) ) ) then
        Error("RQ: the argument must be a list of finite abelian groups and/or positive integers.");
    fi;
    # central subloop
    if IsInt( n ) then # first argument is a positive integer
        K := AsLoop( Random( AllGroups( n, IsAbelian ) ) );
    else # first argument is an abelian group
        K := AsLoop( n );
    fi;
    # factor loop
    if Length( lst ) = 1 then # trivial factor
        F := LoopByCayleyTable( [ [ 1 ] ] ); 
    else
        F := RandomNilpotentLoop( lst{[2..Length(lst)]} );
    fi;
    # cocycle (random)
    f := Size( F );
    theta := List([1..f], i->[1..f]);
    for i in [2..f] do
        theta[1,i]:=1;
    od;
    for i in [2..f] do for j in [2..f] do
        theta[i,j] := Random( [1..Size(K)] );
    od; od;
    # To guarantee that the resulting loop has maximal nilpotency class,
    # it suffices to make sure that theta is not symmetric.
    if f>2 then
        i := Random([2..f]);
        j := Random( Difference( [2..f], [i] ) );
        theta[i,j] := Random( Difference( [1..Size(K)], [ theta[j,i] ] ) );
    fi;
    # the loop
    return LoopByCentralExtension( K, F, theta );
end);
