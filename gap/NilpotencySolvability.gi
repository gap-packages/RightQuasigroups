# NilpotencySolvability.gi
# Nilpotency and solvability for right quasigroups.
# =============================================================================

# PROG: constructor OK, all rely on Subloop, NormalClosure, etc.

# NUCLEI, COMMUTANT, CENTER
# _____________________________________________________________________________

# MATH: For finite right quasigroups, each of the for nuclei is empty or a subrightquasigroup.

# RightNucleus
InstallMethod( RightNucleus, "for right quasigroup",
    [ IsRightQuasigroup ],
function( Q )
    local S, RS, n, elms;
    RS := RightSection( Q );
    n := Size( Q );
    elms := Elements( Q );
    S := Filtered( [ 1..n ], i -> ForAll( [ 1..n ], j ->
        RS[ j ]*RS[ i ] = RS[ PositionSorted( elms, elms[ j ]*elms[ i ] ) ] ) );
    S := RQ_Subalgebra( Q, Elements( Q ){ S } );
    if Size(S) > 0 then
        SetIsAssociative( S, true );
    fi;
    return S;
end );

# MiddleNucleus
InstallMethod( MiddleNucleus, "for right quasigroup",
    [ IsRightQuasigroup ],
function( Q )
    local S, RS, n, elms;
    RS := RightSection( Q );
    n := Size( Q );
    elms := Elements( Q );
    S := Filtered( [ 1..n ], i -> ForAll( [ 1..n ], j ->
        RS[ i ]*RS[ j ] = RS[ PositionSorted( elms, elms[ i ]*elms[ j ] ) ] ) );
    S := RQ_Subalgebra( Q, Elements( Q ){ S } );
    if Size(S) > 0 then
        SetIsAssociative( S, true );
    fi;   
    return S;
end );

# LeftNucleus
# PROG: We need left translations or a direct approach here.
InstallMethod( LeftNucleus, "for right quasigroup",
    [ IsRightQuasigroup ],
function( Q )
    local S, LS, n, elms;
    if CategoryOfRightQuasigroup( Q ) = IsRightQuasigroup then # no left tranlations available, direct approach
        S := Filtered( Q, x -> ForAll( Q, y -> ForAll( Q, z -> x*(y*z) = (x*y)*z ) ) );
        S := Subrightquasigroup( Q, S );
    else # quasigroup or loop
        LS := LeftSection( Q );
        n := Size( Q );
        elms := Elements( Q );
        S := Filtered( [ 1..n ], i -> ForAll( [ 1..n ], j ->
            LS[ j ]*LS[ i ] = LS[ PositionSorted( elms, elms[ i ]*elms[ j ] ) ] ) );
        S := RQ_Subalgebra( Q, Elements( Q ){ S } );
    fi;
    if Size(S) > 0 then
        SetIsAssociative( S, true );
    fi;
    return S;
end );

# Nuc
InstallMethod( Nuc, "for right quasigroup",
    [ IsRightQuasigroup ],
function( Q )
    local N, S;
    # PROG: Elements( [] ) = [];
    N := Intersection( Elements( LeftNucleus( Q ) ), Elements( RightNucleus( Q ) ), Elements( MiddleNucleus( Q ) ) );
    S := RQ_Subalgebra( Q, N );    
    if Size(S) > 0 then 
        SetIsAssociative( S, true );
    fi;
    return S;
end );

# Commutant
InstallMethod( Commutant, "for right quasigroup",
    [ IsRightQuasigroup ],
function( Q )
    if CategoryOfRightQuasigroup( Q ) = IsRightQuasigroup then # left translations not available
        return Filtered( Q, x -> ForAll( Q, y -> x*y=y*x ) );
    fi;
    # quasigroup or loop
    return Elements( Q ){ Filtered( [1..Size( Q )], i -> LeftSection( Q )[ i ] = RightSection( Q )[ i ] ) };
end );

# Center
# (PROG) setting rank high to make sure that Center for IsMagma and IsCommutative is not called first
InstallOtherMethod( Center, "for right quasigroup",
    [ IsRightQuasigroup ], 1*SUM_FLAGS + 20,
function( Q )
    local S;
    S := Intersection( Nuc( Q ), Commutant( Q ) );
    S := RQ_Subalgebra( Q, S );
    if Size( S ) > 0 then
        SetIsAssociative( S, true );
        SetIsCommutative( S, true );
    fi;
    return S;
end );

# NILPOTENCY
# _____________________________________________________________________________

# NilpotencyClassOfLoop 
InstallMethod( NilpotencyClassOfLoop, "for loop",
    [ IsLoop ],
function( Q )
    local n;
    if Size( Q ) = 1 then return 0; fi;
    if Size( Center( Q ) ) = 1 then return fail; fi;
    n := NilpotencyClassOfLoop( Q / Center( Q ) );
    if n = fail then 
        return fail;
    fi;
    return n + 1;
end );

# IsNilpotentLoop 
InstallMethod( IsNilpotentLoop, "for loop", 
    [ IsLoop ],
    Q -> NilpotencyClassOfLoop( Q ) <> fail
);

# IsNilpotent
InstallOtherMethod( IsNilpotent, "for loop", [ IsLoop ], IsNilpotentLoop );

# UpperCentralSeriesOfLoop
InstallMethod( UpperCentralSeriesOfLoop, "for loop",  
    [ IsLoop ], 
function( Q )
    local f, Qbar, ucs;
    if Size( Center( Q ) ) = 1 then
        return [ Subloop( Q, [] ) ];
    fi;
    f := NaturalHomomorphismByNormalSubloop( Q, Center( Q ) );
    Qbar := Range( f );
    ucs := UpperCentralSeriesOfLoop( Qbar );
    return Concatenation( List( ucs, s -> Subloop(Q, PreImage( f, s ) ), [ Subloop( Q, [] ) ] ) );
end );

# UpperCentralSeries
InstallOtherMethod( UpperCentralSeries, "for loop", [ IsLoop ], UpperCentralSeriesOfLoop );

# LowerCentralSeriesOfLoop
InstallOtherMethod( LowerCentralSeriesOfLoop, "for loop",  
    [ IsLoop ], 
function( Q )
    # MATH: Although lower central series is based on commutators, we only need
    # commutators of the form [A,Q]_Q, which happen to be equal to the smallest
    # normal subloop of Q contaning f(a)/a for all a in A and f in Inn(Q)
    local series, G, gens, last_loop, positions, next_loop;
    series := [ Q ];
    G := InnerMappingGroup( Q );
    gens := List( Q, x -> Orbit( G, x )/x ); # list of lists
    repeat
        last_loop := Last( series );
        positions := List( last_loop, x -> PositionSorted( Elements( Q ), x ) );
        next_loop := NormalClosure( Q, Union( gens{positions} ) );
        if last_loop <> next_loop then
            Add( series, next_loop );
        fi;
    until last_loop = next_loop;
    return series;
end );

# LowerCentralSeries
InstallOtherMethod( LowerCentralSeries, "for loop", [ IsLoop ], LowerCentralSeriesOfLoop );

# SOLVABILITY FOR LOOPS
# _____________________________________________________________________________

# AssociatorSubloop
# MATH: Asc( Q ) happens to be the smallest normal subloop of Q
# generated by all elements f(x)/x, where f is in the right inner mapping group of Q
# and x is in Q.
InstallOtherMethod( AssociatorSubloop, "for Loop",
    [ IsLoop ],
function( Q )
    local G, gens;
    G := RightInnerMappingGroup( Q );
    gens := List( Q, x-> Orbit( G, x )/x ); 
    return NormalClosure( Q, Union( gens ) );
end );

# IsSolvableLoop
InstallMethod( IsSolvableLoop, "for loop",
    [ IsLoop ],
function( Q )
    local N;
    N := DerivedSubloop( Q );
    if Size( N ) = 1 then return true; fi;
    if N = Q then return false; fi;
    return IsSolvable( N ) and IsSolvable( Q / N );
end );

# IsSolvable
InstallOtherMethod( IsSolvable, "for loop", [ IsLoop ], IsSolvableLoop );

# DerivedSubloop
# MATH: Q' = Mlt(Q)'(1)
InstallMethod( DerivedSubloop, "for loop", 
    [ IsLoop ],
function( Q )
    local D;
    D := Orbit( DerivedSubgroup( MultiplicationGroup( Q ) ), One( Q ) );
    return Subloop( Q, D ); # automatically normal
end );

# DerivedSeriesOfLoop
InstallMethod( DerivedSeriesOfLoop, "for loop", 
    [ IsLoop ],
function( Q )
    local series, last_loop, D;
    series := [ Q ];
    repeat
        last_loop := Last( series );
        D := DerivedSubloop( last_loop );
        if D <> last_loop then
            Add( series, D );
        fi;
    until D = last_loop;
    return series;
end );

# DerivedSeries
InstallOtherMethod( DerivedSeries, "for loop", [ IsLoop ], DerivedSeriesOfLoop );

# DerivedLength
InstallOtherMethod( DerivedLength, "for loop", 
    [ IsLoop ], 
function( Q ) 
    local series;
    series := DerivedSeriesOfLoop( Q );
    if not Size( Last( series ) ) = 1 then # not solvable
        return fail;
    fi;
    return Length( series ) - 1;
end );

# CommutatorOfNormalSubloops
InstallMethod( CommutatorOfNormalSubloops, "for loop and two normal subloops",
    [ IsLoop, IsLoop, IsLoop ],
function( Q, A, B )
	local QmodZ, ZcapB, BmodZ, gens, a, b1, b2, c1, c2;
	# it suffices to take one element in every coset of the center (the inner mappings will not change )
	# QmodZ is a transversal of Z in Q
	QmodZ := List( RightCosets( Q, Center(Q) ), c -> c[1] );
	# BmodZ is a transversal of Z\cap B in B
	ZcapB := Subloop( Q, Elements( Intersection( Center(Q), B ) ) );
	BmodZ := List( RightCosets( B, ZcapB ), c -> c[1] );
	# cycle over certain inner mappings
	gens := [];
    for a in A do
	    for b1 in QmodZ do for b2 in b1*BmodZ do
            AddSet( gens, LeftDivision(b1,a*b1)/LeftDivision(b2,a*b2) ); # T_{b1}(a)/T_{b2}(a)
		    for c1 in QmodZ do for c2 in c1*BmodZ do
			    AddSet( gens, RightQuotient((a*b1)*c1,b1*c1)/RightQuotient((a*b2)*c2, b2*c2 ) ); # R_{b1,c1}(a)/R_{b2,c2}(a)
                AddSet( gens, LeftQuotient(c1*b1, c1*(b1*a))/LeftQuotient(c2*b2, c2*(b2*a)) ); # L_{b1,c1}(a)/L_{b2,c2}(a)
            od; od;
        od; od;
    od;
	return NormalClosure( Q, gens );
end );

# IsAbelianNormalSubloop
InstallMethod( IsAbelianNormalSubloop, "for loop and its normal subloop",
    [ IsLoop, IsLoop ],
function( Q, A )
    if not IsNormal( Q, A ) then
        Error( "RQ: <2> must be a normal subloop of <1>.");
    fi;
    return Size( CommutatorOfNormalSubloops( Q, A, A ) ) = 1; # REVISIT: Faster method?
end );

# IsCongruenceSolvableLoop
InstallMethod( IsCongruenceSolvableLoop, "for loop",
    [ IsLoop ],
function( Q )
    local series;
    series := CongruenceDerivedSeriesOfLoop( Q );
    return Size( Last( series ) ) = 1;
end );

# CongruenceDerivedSeriesOfLoop
InstallMethod( CongruenceDerivedSeriesOfLoop, "for loop", 
    [ IsLoop ],
function( Q )
    local series, last_loop, D;
    series := [ Q ];
    repeat
        last_loop := Last( series );
        D := CommutatorOfNormalSubloops( Q, last_loop, last_loop );
        if D <> last_loop then
            Add( series, D );
        fi;
    until D = last_loop;
    return series;
end );

# CongruenceDerivedLength 
InstallMethod( CongruenceDerivedLength, "for loop", 
    [ IsLoop ], 
function( Q ) 
    local series;
    series := CongruenceDerivedSeriesOfLoop( Q );
    if not Size( Last( series ) ) = 1 then # not congruence solvable
        return fail;
    fi;
    return Length( series ) - 1;
end );

# FRATTINI SUBALGEBRA
# _____________________________________________________________________________

# FrattiniSubrightquasigroup
InstallMethod( FrattiniSubrightquasigroup, "for right quasigroup", 
    [ IsRightQuasigroup ],
function( Q )
    # slow
    return Intersection( AllMaximalSubrightquasigroups( Q ) );
end );

# FrattiniSubquasigroup
InstallMethod( FrattiniSubquasigroup, "for quasigroup",
    [ IsQuasigroup ],
    Q -> FrattiniSubrightquasigroup( Q )
);

# FrattiniSubloop
InstallMethod( FrattiniSubloop, "for loop",
    [ IsLoop ],
function( Q )
    local S;
    if IsNilpotent( MultiplicationGroup( Q ) ) then
        # faster method
        S := Orbit( FrattiniSubgroup( MultiplicationGroup( Q ) ), One( Q ) );
        return Subloop( Q, S );
    fi;
    # call generic method
    return FrattiniSubrightquasigroup( Q );
end );

# REVISIT: Activate this later?
# FrattinifactorSize( Q ) 
#InstallOtherMethod( FrattinifactorSize, "for a strongly nilpotent loop", 
#    [ IsLoop ],
#function( Q )
#    if IsNilpotent( MultiplicationGroup( Q ) ) then
#        return Size( Q ) / Size( FrattiniSubloop( Q ) );
#    else
#        TryNextMethod();
#        return;
#    fi;
#end );