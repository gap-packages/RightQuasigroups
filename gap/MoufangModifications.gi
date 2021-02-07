# MoufangModifications.gi
# Quarter modifications of Moufang loops
# =============================================================================

# CheinLoop

InstallMethod( CheinLoop, "for group",
    [ IsGroup ],
    G -> CheinLoop( G, RQ_defaultConstructorStyle )
);

InstallOtherMethod( CheinLoop, "for group and record",
    [ IsGroup, IsRecord ],
function( G, style )    
    local S, mult, Q;
    RQ_CompleteConstructorStyle( style );
    S := Union( List( Elements( G ), x -> [ [0,x],[1,x] ] ) );
    mult := function( x, y )
        if x[1]=0 and y[1]=0 then return [ 0, x[2]*y[2] ];      # x*y = xy
        elif x[1]=0 and y[1]=1 then return [ 1, y[2]*x[2] ];    # x*yu = (yx)u
        elif y[1]=0 then return [ 1, x[2]*y[2]^-1 ];            # xu*y = (xy^{-1})u
        else return [ 0, y[2]^-1*x[2] ];                        # xu*yu = y^{-1}x
        fi;
    end;
    Q := LoopByFunction( S, mult, ConstructorStyle( false, false ) );    
    if style.indexBased then
        Q := IndexBasedCopy( Q );
    fi;
    SetIsMoufangLoop( Q, true );
    if IsCommutative( G ) then
        SetIsAssociative( Q, true );
    fi;
    return Q;
end);

# auxiliary functions for Moufang modifications

# RQ_PositionList( A, B ) 
InstallMethod( RQ_PositionList, "for two lists",
    [ IsList, IsList ],
function( A, B )
    local P, b;
    P := [];
    for b in B do Add( P, Position( A, b ) ); od;
    return P;
end);

# RQ_Modular( i, m ) 
InstallMethod( RQ_Modular, "for two integers",
    [ IsInt, IsInt ],
function(i, m)
    while i>m do i:=i-2*m; od;
    while i<1-m do i:=i+2*m; od;
    return i;
end);

# RQ_DVSigma( i, m ) 
InstallMethod( RQ_DVSigma, "for two integers",
    [ IsInt, IsInt ],
function( i, m )
    if i > m then return 1; fi;
    if i < 1 - m then return -1; fi;
    return 0;
end);

# LoopByCyclicModification( L, S, a, h) 
# MATH: 
# L is a loop,
# S is a normal subloop of L, or a set generating a normal subloop of L,
# L/S is cyclic of order 2m, generated by aS,
# h is an element of Z(L) and S
# PROG: nothing is checked
InstallGlobalFunction( LoopByCyclicModification,
function( L, S, a, h )

    local n, ih, m, aP, aa, i, iL, T, x, y, z, exponent;

    # making sure that S is a subloop of L
    if not IsLoop( S ) then S := Subloop( L, S ); fi;
    
    # converting all into numbers for faster calculation
    n := Size( L );
    ih := Position( Elements( L ), h^(-1) ); #inverse of h
    h := Position( Elements( L ), h );
    a := Position( Elements( L ), a );
    S := RQ_PositionList( Elements( L ), Elements( S ) );
    L := MultiplicationTable( L );
    
    # setting parameter m of the construction
    m := n / ( 2 * Length( S ) );

    # calculating all cosets a^i, for i in M
    aP := []; aa := 1;
    for i in [ 0..2*m-1 ] do
        Add( aP, List( S, j -> L[ aa, j ] ) );
        aa := L[ aa, a ];
    od;

    # into which cosets belong elements of L
    iL := List( [ 1..n ], x -> RQ_Modular( RQ_SublistPosition(aP, x ) - 1, m ) );

    # setting up the multiplication table
    T := List( [ 1..n ], i->[] );
    for x in [ 1..n ] do for y in [ 1..n ] do
        z := L[ x, y ];
        exponent := RQ_DVSigma( iL[ x ] + iL[ y ], m );
        if exponent = 1 then z := L[ z, h ]; fi;
        if exponent = -1 then z := L[ z, ih ]; fi;
        T[ x, y ] := z;
    od; od;

    return LoopByCayleyTable( T, rec( indexBased := true, checkArguments := false ) );
end);

# LoopByDihedralModification( L, S, e, f, h) 
# MATH:
# L is a loop,
# S is a normal subloop of L, or a set generating a normal subloop of L,
# L/S is dihedral of order 4m, 
# eS, fS are involutions of L/S such that eS*fS is of order 2m,
# let G0 be the union of cosets of L/S generated by eS*fS,
# h is an element of N(L), S and Z(G0).
# PROG: nothing is checked
InstallGlobalFunction( LoopByDihedralModification,
function( L, S, e, f, h )

    local a, G0, n, m, ih, aP, aa, i, eP, fP, eL, fL, T, x, y, z, exp;

    # making sure that S is a subloop of L
    if not IsLoop( S ) then S := Subloop( L, S ); fi;
    
    # obtaining a and G0
    a := e * f; 
    G0:= Subloop( L, a * Elements( S ) );

    # all in numbers
    n := Size( L );
    ih := Position( Elements( L ), h^(-1) ); #inverse of h
    h := Position( Elements( L ), h );
    a := Position( Elements( L ), a );
    e := Position( Elements( L ), e );
    f := Position( Elements( L ), f );
    S := RQ_PositionList( Elements( L ), Elements( S ) );
    G0 := RQ_PositionList( Elements( L ), Elements( G0 ) );
    L := MultiplicationTable( L );

    # setting parameter m
    m := n / ( 4 * Length( S ) );

    # powers of aS, eS and fS
    aP := []; aa := 1;
    for i in [ 0..2*m-1 ] do
        Add( aP, List( S, i -> L[ aa, i ] ) );
        aa := L[ aa, a ];
    od;
    eP := List( aP, x -> Union( List( x, y -> [ y, L[ e, y ] ] ) ) );
    fP := List( aP, x -> Union( List( x, y -> [ y, L[ y, f ] ] ) ) );
    
    # into which cosets belong elements of L
    eL := List( [ 1..n ], x -> RQ_Modular( RQ_SublistPosition(eP, x ) - 1, m ) );
    fL := List( [ 1..n ], x -> RQ_Modular( RQ_SublistPosition(fP, x ) - 1, m ) );

    # setting up multiplication table
    T := List( L, x->[] );
    for x in [ 1..n ] do for y in [ 1..n ] do
        if y in G0 then exp := RQ_DVSigma( eL[ x ] + fL[ y ], m );
        else exp := (-1)*RQ_DVSigma( eL[ x ] + fL[ y ], m ); fi;
        z := L[ x, y ];
        if exp = 1 then z := L[ z, h ]; fi;
        if exp = -1 then z := L[ z, ih ]; fi;
        T[ x, y ] := z;
    od; od;

    return LoopByCayleyTable( T, rec( indexBased := true, checkArguments := false ) );
end);
