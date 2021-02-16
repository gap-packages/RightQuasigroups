# Properties.gi
# Properties of right quasigroups, quasigroups and loops
# =============================================================================

# PROPERTIES OF RIGHT QUASIGROUPS
# _____________________________________________________________________________

# Is3PowerAssociative
InstallMethod( Is3PowerAssociative, "for magma",
    [ IsMagma ],
function( Q )
    return ForAll( Q, x -> x*(x*x)=(x*x)*x );
end);

# is implied by
# InstallTrueMethod( Is3PowerAssociative, IsPowerAssociative );
# InstallTrueMethod( Is3PowerAssociative, IsCommutative );
# InstallTrueMethod( Is3PowerAssociative, IsLeftAlternative );
# InstallTrueMethod( Is3PowerAssociative, IsRightAlternative );

# IsAlternative
InstallMethod( IsAlternative, "for magma",
    [ IsMagma ],
    Q -> IsLeftAlternative( Q ) and IsRightAlternative( Q )
);

# implies
# InstallTrueMethod( IsLeftAlternative, IsAlternative );
# InstallTrueMethod( IsRightAlternative, IsAlternative );

# is implied by
# InstallTrueMethod( IsAlternative, IsLeftAlternative and IsRightAlternative );

# IsAssociative
InstallOtherMethod( IsAssociative, "for loop",
    [ IsLoop ], 2,
function( Q )
    local sec;
    if IsIndexBased( Q ) or HasRightSection( Q ) then
        sec := RightSection( Q );
        return ForAll( sec, x -> ForAll( sec, y -> x*y in sec ) );
    fi;
    TryNextMethod();
end);

InstallOtherMethod( IsAssociative, "for right quasigroup",
    [ IsRightQuasigroup ], 1,
function( Q )
    local n, sec, elms;
    if IsIndexBased( Q ) or HasRightSection( Q ) then
        n := Size( Q );
        sec := RightSection( Q );
        elms := Elements( Q );
        return ForAll( [1..n], i -> ForAll( [1..n], j ->
            sec[i]*sec[j] = sec[ PositionSorted( elms, elms[i]*elms[j] ) ]
        ) );
    fi;
    TryNextMethod(); # will likely call IsAssociative for magmas
end);

# implies
# InstallTrueMethod( IsExtraLoop, IsAssociative and IsLoop );

# IsCommutative
InstallOtherMethod( IsCommutative, "for quasigroup",
    [ IsRightQuasigroup ], 1,
function( Q )
    if HasLeftSection( Q ) and HasRightSection( Q ) then
        return LeftSection( Q ) = RightSection( Q );
    fi;
    TryNextMethod();
end );

# IsFlexible
InstallMethod( IsFlexible, "for magma",
    [ IsMagma ], 0,
    Q -> ForAll( Q, x -> ForAll( Q, y -> x*(y*x) = (x*y)*x ) )
);

InstallOtherMethod( IsFlexible, "for quasigroup",
    [ IsQuasigroup ], 1,
function( Q )
    local LS, RS;
    if HasLeftSection( Q ) and HasRightSection( Q ) then
        LS := LeftSection( Q );
        RS := RightSection( Q );
        return ForAll( [1..Size( Q )], i -> LS[ i ] * RS[ i ] = RS[ i ] * LS[ i ] );
    fi;
    TryNextMethod();
end );

# is implied by
# InstallTrueMethod( IsFlexible, IsCommutative );

# IsIdempotent
InstallOtherMethod( IsIdempotent, "for magma",
    [ IsMagma ],
    Q -> ForAll( Q, x -> x*x = x )
);

# IsLeftAlternative
InstallMethod( IsLeftAlternative, "for magma",
    [ IsMagma ], 0,
    Q -> ForAll( Q, x -> ForAll( Q, y -> x*(x*y) = (x*x)*y ) )
);

InstallOtherMethod( IsLeftAlternative, "for loop",
    [ IsLoop ], 1,
function( Q )
    if HasLeftSection( Q ) then 
        return ForAll( LeftSection( Q ), a -> a*a in LeftSection( Q ) );
    fi;
    TryNextMethod();
end );

# implies
# InstallTrueMethod( IsRightAlternative, IsLeftAlternative and IsCommutative );

# IsLeftSelfDistributive
InstallMethod( IsLeftSelfDistributive, "for magma",
    [ IsMagma ],
    Q -> ForAll( Q, x -> ForAll( Q, y -> ForAll( Q, z -> x*(y*z) = (x*y)*(x*z) ) ) )
);

# IsRightAlternative
InstallMethod( IsRightAlternative, "for magma",
    [ IsMagma ], 0,
    Q -> ForAll( Q, x -> ForAll( Q, y -> (x*y)*y = x*(y*y) ) )
);

InstallOtherMethod( IsRightAlternative, "for loop",
    [ IsLoop ], 1,
function( Q )
    if HasRightSection( Q ) then
        return ForAll( RightSection( Q ), a -> a*a in RightSection( Q ) );
    fi;
    TryNextMethod();
end );

# implies
# InstallTrueMethod( IsLeftAlternative, IsRightAlternative and IsCommutative );

# IsRightSelfDistributive
InstallMethod( IsRightSelfDistributive, "for magma",
    [ IsMagma ],
    Q -> ForAll( Q, x -> ForAll( Q, y -> ForAll( Q, z -> (x*y)*z = (x*z)*(y*z) ) ) )
);

# IsSelfDistributive
InstallMethod( IsSelfDistributive, "for magma",
    [ IsMagma ],
    Q -> IsRightSelfDistributive( Q ) and IsLeftSelfDistributive( Q )
);

# IsUnipotent
InstallMethod( IsUnipotent, "for magma",
    [ IsMagma ],
function( Q )
    local square;
    square :=  Elements(Q)[1]^2;
    return ForAll( Q, y -> y^2 = square );
end );

# PROPERTIES OF RIGHT QUASIGROUPS
# _____________________________________________________________________________

# IsFaithfulRightQuasigroup 
InstallMethod( IsFaithfulRightQuasigroup, "for right quasigroup",
    [ IsRightQuasigroup ],
    Q -> Size( Set( RightSection( Q ) ) ) = Size( Q )
);

# IsProjectionRightQuasigroup
InstallMethod( IsProjectionRightQuasigroup, "for right quasigroup",
    [ IsRightQuasigroup ],
function( Q )
    return ForAll( Q, x -> ForAll( Q, y -> x*y = x ) );
end );

# PROPERTIES OF QUASIGROUPS
# _____________________________________________________________________________

# IsSemisymmetric
InstallMethod( IsSemisymmetric, "for quasigroup",
    [ IsQuasigroup ],
function( Q )
    if HasLeftSection( Q ) and HasRightSection( Q ) then
        return ForAll( Q, x -> LeftTranslation( Q, x ) * RightTranslation( Q, x ) = () );
    fi;
    return ForAll( Q, x -> ForAll( Q, y -> (x*y)*x=y ) );
end );

# IsTotallySymmetric
InstallMethod( IsTotallySymmetric, "for quasigroup",
    [ IsQuasigroup ],
    Q -> IsCommutative( Q ) and IsSemisymmetric( Q )
);

# IsSteinerQuasigroup
InstallMethod( IsSteinerQuasigroup, "for quasigroup",
    [ IsQuasigroup ],
    Q -> IsIdempotent( Q ) and IsTotallySymmetric( Q )
);

# IsEntropic
InstallMethod( IsEntropic, "for quasigroup",
    [ IsQuasigroup ],
    Q -> ForAll( Q, x -> ForAll(Q, u -> ForAll( Q, v -> ForAll( Q, y -> (x*u)*(v*y) = (x*v)*(u*y) ) ) ) )
);

# IsPowerAssociative
InstallOtherMethod( IsPowerAssociative, "for quasigroup", # REVISIT: Does the concept make sense for right quasigroups?
    [ IsQuasigroup ],
function( Q )
    local category, elms, S;
    category := CategoryOfRightQuasigroup( Q );
    elms := Elements( Q );
    while not IsEmpty( elms ) do
        S := RQ_Subalgebra( Q, [ elms[1] ] );
        if not IsAssociative( S ) then
            return false;
        fi;
        elms := Difference( elms, S ); # S is a group so we need not test its elements
    od;
    return true;
end );

# implies
# InstallTrueMethod( HasTwosidedInverses, IsPowerAssociative and IsLoop );

# IsDiassociative( Q )

InstallOtherMethod( IsDiassociative, "for quasigroup",
    [ IsQuasigroup ],
function( Q )
    local category, pairs, S;
    category := CategoryOfRightQuasigroup( Q );
    pairs := Combinations( Elements( Q ), 2 );
    while not IsEmpty( pairs ) do
        S := RQ_Subalgebra( category, Q, pairs[ 1 ] );
        if not IsAssociative( S ) then
            return false;
        fi;
        pairs := Difference( pairs, Combinations( Elements( S ), 2 ) );
    od;
    return true;
end );

# implies
# InstallTrueMethod( IsPowerAlternative, IsDiassociative );
# InstallTrueMethod( IsFlexible, IsDiassociative );

# INVERSE PROPERTIES OF LOOPS
# _____________________________________________________________________________

# HasTwosidedInverses
InstallMethod( HasTwosidedInverses, "for loop",
    [ IsLoop ],
    Q -> ForAll( Q, x -> LeftInverse( x ) = RightInverse( x ) )
);

# HasRightInverseProperty
InstallMethod( HasRightInverseProperty, "for loop",
    [ IsLoop ],
function( Q )
    if HasRightSection( Q ) then
        return ForAll( RightSection( Q ), x -> x^-1 in RightSection( Q ) );
    fi;
    return ForAll( Q, x -> ForAll( Q, y -> (y*x)*RightInverse(x) = y ) );
end );

# HasLeftInverseProperty
InstallMethod( HasLeftInverseProperty, "for loop",
    [ IsLoop ],
function( Q )
    if HasLeftSection( Q ) then
        ForAll( LeftSection( Q ), x -> x^-1 in LeftSection( Q ) );
    fi;
    return ForAll( Q, x -> ForAll( Q, y -> LeftInverse(x)*(x*y) = y ) );
end );

# HasInverseProperty
InstallMethod( HasInverseProperty, "for loop",
    [ IsLoop ],
    Q -> HasRightInverseProperty( Q ) and HasLeftInverseProperty( Q )
);

# HasWeakInverseProperty
InstallMethod( HasWeakInverseProperty, "for loop",
    [ IsLoop ],
    Q -> ForAll( Q, x -> ForAll( Q, y -> LeftInverse(x*y)*x=LeftInverse(y) ))
);

# HasAutomorphicInverseProperty
InstallMethod( HasAutomorphicInverseProperty, "for loop",
    [ IsLoop ],
    Q -> ForAll( Q, x -> ForAll( Q, y -> LeftInverse( x*y ) = LeftInverse( x )*LeftInverse( y ) ) )
);

# HasAntiautomorphicInverseProperty
InstallMethod( HasAntiautomorphicInverseProperty, "for loop",
    [ IsLoop ],
    Q -> ForAll( Q, x -> ForAll( Q, y -> LeftInverse( x*y ) = LeftInverse( y )*LeftInverse( x ) ) )
);

# implies and is implied by (for inverse properties)
# InstallTrueMethod( HasAntiautomorphicInverseProperty, HasAutomorphicInverseProperty and IsCommutative );
# InstallTrueMethod( HasAutomorphicInverseProperty, HasAntiautomorphicInverseProperty and IsCommutative );
# InstallTrueMethod( HasLeftInverseProperty, HasInverseProperty );
# InstallTrueMethod( HasRightInverseProperty, HasInverseProperty );
# InstallTrueMethod( HasWeakInverseProperty, HasInverseProperty );
# InstallTrueMethod( HasAntiautomorphicInverseProperty, HasInverseProperty );
# InstallTrueMethod( HasTwosidedInverses, HasAntiautomorphicInverseProperty );
# InstallTrueMethod( HasInverseProperty, HasLeftInverseProperty and IsCommutative );
# InstallTrueMethod( HasInverseProperty, HasRightInverseProperty and IsCommutative );
# InstallTrueMethod( HasInverseProperty, HasLeftInverseProperty and HasRightInverseProperty );
# InstallTrueMethod( HasInverseProperty, HasLeftInverseProperty and HasWeakInverseProperty );
# InstallTrueMethod( HasInverseProperty, HasRightInverseProperty and HasWeakInverseProperty );
# InstallTrueMethod( HasInverseProperty, HasLeftInverseProperty and HasAntiautomorphicInverseProperty );
# InstallTrueMethod( HasInverseProperty, HasRightInverseProperty and HasAntiautomorphicInverseProperty );
# InstallTrueMethod( HasInverseProperty, HasWeakInverseProperty and HasAntiautomorphicInverseProperty );
# InstallTrueMethod( HasTwosidedInverses, HasLeftInverseProperty );
# InstallTrueMethod( HasTwosidedInverses, HasRightInverseProperty );
# InstallTrueMethod( HasTwosidedInverses, IsFlexible and IsLoop );

# LOOPS OF BOL-MOUFANG TYPE
# _____________________________________________________________________________

# IsCLoop
InstallMethod( IsCLoop, "for loop",
    [ IsLoop ],
    Q -> IsLCLoop( Q ) and IsRCLoop( Q )
);

# implies
# InstallTrueMethod( IsLCLoop, IsCLoop );
# InstallTrueMethod( IsRCLoop, IsCLoop );
# InstallTrueMethod( IsDiassociative, IsCLoop and IsFlexible);

# is implied by
# InstallTrueMethod( IsCLoop, IsLCLoop and IsRCLoop );


# IsExtraLoop
InstallMethod( IsExtraLoop, "for loop",
    [ IsLoop ],
    Q -> IsMoufangLoop( Q ) and IsNuclearSquareLoop( Q )
);

# implies
# InstallTrueMethod( IsMoufangLoop, IsExtraLoop );
# InstallTrueMethod( IsCLoop, IsExtraLoop );

# is implied by
# InstallTrueMethod( IsExtraLoop, IsMoufangLoop and IsLeftNuclearSquareLoop );
# InstallTrueMethod( IsExtraLoop, IsMoufangLoop and IsMiddleNuclearSquareLoop );
# InstallTrueMethod( IsExtraLoop, IsMoufangLoop and IsRightNuclearSquareLoop );

# IsLCLoop
InstallMethod( IsLCLoop, "for loop",
    [ IsLoop ],
function( Q )
    if HasRightSection( Q ) and HasLeftSection( Q ) then
        return ForAll( LeftSection( Q ), a -> ForAll( RightSection( Q ), b -> b^(-1)*a*a*b in LeftSection( Q ) ) );
    fi;
    return ForAll( Q, x -> ForAll( Q, y -> ForAll( Q, z -> (x*x)*(y*z) = ((x*x)*y)*z ) ) );
end );

# implies
# InstallTrueMethod( IsLeftPowerAlternative, IsLCLoop );
# InstallTrueMethod( IsLeftNuclearSquareLoop, IsLCLoop );
# InstallTrueMethod( IsMiddleNuclearSquareLoop, IsLCLoop );
# InstallTrueMethod( IsRCLoop, IsLCLoop and IsCommutative );

# IsLeftBolLoop
InstallMethod( IsLeftBolLoop, "for loop",
    [ IsLoop ],
function( Q )
    local sec;
    if HasLeftSection( Q ) then
        sec := LeftSection( Q );
        return ForAll( sec, a -> ForAll( sec, b -> a*b*a in sec ) );
    fi;
    return ForAll( Q, x -> ForAll( Q, y -> ForAll(Q, z -> (x*(y*x))*z = x*(y*(x*z)) ) ) );
end );

# implies
# InstallTrueMethod( IsRightBolLoop, IsLeftBolLoop and IsCommutative );
# InstallTrueMethod( IsLeftPowerAlternative, IsLeftBolLoop );

# IsLeftNuclearSquareLoop
InstallMethod( IsLeftNuclearSquareLoop, "for loop",
    [ IsLoop ],
    Q -> ForAll( Q, x -> x^2 in LeftNucleus( Q ) )
);

#implies
# InstallTrueMethod( IsRightNuclearSquareLoop, IsLeftNuclearSquareLoop and IsCommutative );

# IsMiddleNuclearSquareLoop
InstallMethod( IsMiddleNuclearSquareLoop, "for loop",
    [ IsLoop ],
    Q -> ForAll( Q, x -> x^2 in MiddleNucleus( Q ) )
);

# IsMoufangLoop
InstallMethod( IsMoufangLoop, "for loop",
    [ IsLoop ],
    Q -> IsLeftBolLoop( Q ) and HasRightInverseProperty( Q )
);

# implies
# InstallTrueMethod( IsLeftBolLoop, IsMoufangLoop );
# InstallTrueMethod( IsRightBolLoop, IsMoufangLoop );
# InstallTrueMethod( IsDiassociative, IsMoufangLoop );

# is implied by
# InstallTrueMethod( IsMoufangLoop, IsLeftBolLoop and IsRightBolLoop );

# IsNuclearSquareLoop
InstallMethod( IsNuclearSquareLoop, "for loop",
    [ IsLoop ],
    Q -> IsLeftNuclearSquareLoop( Q ) and IsRightNuclearSquareLoop( Q ) and IsMiddleNuclearSquareLoop( Q )
);

# implies
# InstallTrueMethod( IsLeftNuclearSquareLoop, IsNuclearSquareLoop );
# InstallTrueMethod( IsRightNuclearSquareLoop, IsNuclearSquareLoop );
# InstallTrueMethod( IsMiddleNuclearSquareLoop, IsNuclearSquareLoop );

# is implied by
# InstallTrueMethod( IsNuclearSquareLoop, IsLeftNuclearSquareLoop
#    and IsRightNuclearSquareLoop and IsMiddleNuclearSquareLoop );

# IsRCLoop
InstallMethod( IsRCLoop, "for loop",
    [ IsLoop ],
function( Q )
    if HasRightSection( Q ) and HasLeftSection( Q ) then
        return ForAll( LeftSection( Q ), a -> ForAll( RightSection( Q ), b -> a^(-1)*b*b*a in RightSection( Q ) ) );
    fi;
    return ForAll( Q, x -> ForAll( Q, y -> ForAll( Q, z -> x*((y*z)*z) = (x*y)*(z*z) ) ) );
end );

# implies
# InstallTrueMethod( IsRightPowerAlternative, IsRCLoop );
# InstallTrueMethod( IsRightNuclearSquareLoop, IsRCLoop );
# InstallTrueMethod( IsMiddleNuclearSquareLoop, IsRCLoop );
# InstallTrueMethod( IsLCLoop, IsRCLoop and IsCommutative );

# IsRightBolLoop
InstallMethod( IsRightBolLoop, "for loop",
    [ IsLoop ],
function( Q )
    local sec;
    if HasRightSection( Q ) then 
        sec := RightSection( Q );
        return ForAll( sec, a -> ForAll( sec, b -> a*b*a in sec ) );
    fi;
    return ForAll( Q, x -> ForAll( Q, y -> ForAll(Q, z -> ((x*y)*z)*y = x*((y*z)*y) ) ) );
end );

# implies
# InstallTrueMethod( IsLeftBolLoop, IsRightBolLoop and IsCommutative );
# InstallTrueMethod( IsRightPowerAlternative, IsRightBolLoop );

# IsRightNuclearSquareLoop
InstallMethod( IsRightNuclearSquareLoop, "for loop",
    [ IsLoop ],
    Q -> ForAll( Q, x -> x^2 in RightNucleus( Q ) )
);

# implies
# InstallTrueMethod( IsLeftNuclearSquareLoop, IsRightNuclearSquareLoop and IsCommutative );

# POWER ALTERNATIVE LOOPS
# _____________________________________________________________________________

# IsRightPowerAlternative
InstallMethod( IsRightPowerAlternative, "for loop",
    [ IsLoop ],
function( Q )
    local elms, S;
    if not IsPowerAssociative( Q ) then
        return false;
    fi;
    elms := Elements( Q );
    while not IsEmpty( elms ) do
        S := Subloop( Q, [ elms[1] ] );
        if not Size( RelativeRightMultiplicationGroup( Q, S ) ) = Size( S ) then
            return false;
        fi;
        elms := Difference( elms, S );
    od;
    return true;
end );

# implies
# InstallTrueMethod( IsRightAlternative, IsRightPowerAlternative );
# InstallTrueMethod( HasRightInverseProperty, IsRightPowerAlternative );
# InstallTrueMethod( IsPowerAssociative, IsRightPowerAlternative );

# IsLeftPowerAlternative
InstallMethod( IsLeftPowerAlternative, "for loop",
    [ IsLoop ],
function( Q )
    local elms, S;
    if not IsPowerAssociative( Q ) then
        return false;
    fi;
    elms := Elements( Q );
    while not IsEmpty( elms ) do
        S := Subloop( Q, [elms[1]] );
        if not Size( RelativeLeftMultiplicationGroup( Q, S ) ) = Size( S ) then
            return false;
        fi;
        elms := Difference( elms, S );
    od;
    return true;
end );

# implies
# InstallTrueMethod( IsLeftAlternative, IsLeftPowerAlternative );
# InstallTrueMethod( HasLeftInverseProperty, IsLeftPowerAlternative );
# InstallTrueMethod( IsPowerAssociative, IsLeftPowerAlternative );

# IsPowerAlternative
InstallMethod( IsPowerAlternative, "for loop",
    [ IsLoop ],
    Q -> IsLeftPowerAlternative( Q ) and IsRightPowerAlternative( Q )
);

# implies
# InstallTrueMethod( IsLeftPowerAlternative, IsPowerAlternative );
# InstallTrueMethod( IsRightPowerAlternative, IsPowerAlternative );

# CC-LOOPS AND OSBORN LOOPS
# _____________________________________________________________________________

# IsRightConjugacyClosedLoop
InstallMethod( IsRightConjugacyClosedLoop, "for loop",
    [ IsLoop ],
function( Q )
    local sec;
    if HasRightSection( Q ) then
        sec := RightSection( Q );
        return ForAll( sec, a -> ForAll( sec, b -> b*a*b^(-1) in sec ) );
    fi;
    return ForAll( Q, x -> ForAll( Q, y -> ForAll(Q, z -> y*LeftDivision(x,z*x) = ((y/x)*z)*x ) ) );
end );

# implies
# InstallTrueMethod( IsAssociative, IsRCCLoop and IsCommutative );
# InstallTrueMethod( IsExtraLoop, IsRCCLoop and IsMoufangLoop );

# IsLeftConjugacyClosedLoop
InstallMethod( IsLeftConjugacyClosedLoop, "for loop",
    [ IsLoop ],
function( Q )
    local sec;
    if HasLeftSection( Q ) then
        sec := LeftSection( Q );
        return ForAll( sec, a -> ForAll( sec, b -> b*a*b^(-1) in sec ) );
    fi;
    return ForAll( Q, x -> ForAll( Q, y -> ForAll(Q, z -> x*(y*LeftDivision(x,z)) = ((x*y)/x)*z ) ) );
end );

# implies
# InstallTrueMethod( IsAssociative, IsLCCLoop and IsCommutative );
# InstallTrueMethod( IsExtraLoop, IsLCCLoop and IsMoufangLoop );

# IsConjugacyClosedLoop
InstallMethod( IsConjugacyClosedLoop, "for loop",
    [ IsLoop ],
    Q -> IsLeftConjugacyClosedLoop( Q ) and IsRightConjugacyClosedLoop( Q )
);

# implies
# InstallTrueMethod( IsLCCLoop, IsCCLoop );
# InstallTrueMethod( IsRCCLoop, IsCCLoop );

# is implied by
# InstallTrueMethod( IsCCLoop, IsLCCLoop and IsRCCLoop );

# IsOsbornLoop
InstallMethod( IsOsbornLoop, "for loop",
    [ IsLoop ],
function( Q )
    # REVISIT: might redo later
    return ForAll(Q, x-> ForAll(Q, y-> ForAll(Q, z-> x*((y*z)*x) = LeftDivision(LeftInverse(x),y)*(z*x)) ) );
end );

# is implied by
# InstallTrueMethod( IsOsbornLoop, IsMoufangLoop );
# InstallTrueMethod( IsOsbornLoop, IsCCLoop );

# ADDITIONAL VARIETIES OF LOOPS
# _____________________________________________________________________________

# IsCodeLoop
InstallMethod( IsCodeLoop, "for loop",
    [ IsLoop ],
function( Q )
    # code loops are precisely Moufang 2-loops with Frattini subloop of order 1, 2
    return Set( Factors( Size( Q ) ) ) = [ 2 ]
        and IsMoufangLoop( Q )
        and Size( FrattiniSubloop( Q ) ) in [1, 2];
end );

# implies
# InstallTrueMethod( IsExtraLoop, IsCodeLoop );
# InstallTrueMethod( IsCCLoop, IsCodeLoop );

# IsSteinerLoop
InstallMethod( IsSteinerLoop, "for loop",
    [ IsLoop ],
function( Q )
    # Steiner loops are inverse property loops of exponent at most 2.
    return HasInverseProperty( Q ) and Exponent( Q )<=2;
end );

# implies
# InstallTrueMethod( IsCommutative, IsSteinerLoop );
# InstallTrueMethod( IsCLoop, IsSteinerLoop );

# IsRightBruckLoop
InstallMethod( IsRightBruckLoop, "for loop",
    [ IsLoop ],
    Q -> HasAutomorphicInverseProperty( Q ) and IsRightBolLoop( Q )
);

# implies
# InstallTrueMethod( HasAutomorphicInverseProperty, IsRightBruckLoop );
# InstallTrueMethod( IsRightBolLoop, IsRightBruckLoop );
# InstallTrueMethod( IsLeftBruckLoop, IsRightBruckLoop and IsCommutative );

# is implied by
# InstallTrueMethod( IsRightBruckLoop, IsRightBolLoop and HasAutomorphicInverseProperty );

# IsLeftBruckLoop
InstallMethod( IsLeftBruckLoop, "for loop",
    [ IsLoop ],
    Q -> HasAutomorphicInverseProperty( Q ) and IsLeftBolLoop( Q )
);

# implies
# InstallTrueMethod( HasAutomorphicInverseProperty, IsLeftBruckLoop );
# InstallTrueMethod( IsLeftBolLoop, IsLeftBruckLoop );
# InstallTrueMethod( IsRightBruckLoop, IsLeftBruckLoop and IsCommutative );

# is implied by
# InstallTrueMethod( IsLeftBruckLoop, IsLeftBolLoop and HasAutomorphicInverseProperty );

# IsRightAutomorphicLoop
InstallMethod( IsRightAutomorphicLoop, "for loop",
    [ IsLoop ],
function( Q )
    # faster code suggested my Michael Kinyon
    # Thm: Q is right A-loop iff for all generators h of RInn(Q) and all x in Q we have h^(-1)R_x h in right section.
    local sec, gens;
    sec := RightSection(Q);
    gens := GeneratorsOfGroup( RightInnerMappingGroup( Q ) );
    return ForAll( gens, h -> ForAll( sec, x -> h^(-1)*x*h in sec ) );
end );

# IsMiddleAutomorphicLoop
InstallMethod( IsMiddleAutomorphicLoop, "for loop",
    [ IsLoop ],
function( Q )
    local sec, gens;
    sec := RightSection(Q); # could take the left section, it does not matter
    gens := GeneratorsOfGroup( MiddleInnerMappingGroup( Q ) );
    return ForAll( gens, h -> ForAll( sec, x -> h^(-1)*x*h in sec ) );
end );

# IsLeftAutomorphicLoop
InstallMethod( IsLeftAutomorphicLoop, "for loop",
    [ IsLoop ],
function( Q )
    # Thm: Q is left A-loop iff for all generators h of LInn(Q) and all x in Q we have h^(-1)L_x h in left section.
    local sec, gens;
    sec := LeftSection(Q);
    gens := GeneratorsOfGroup( LeftInnerMappingGroup( Q ) );
    return ForAll( gens, h -> ForAll( sec, x -> h^(-1)*x*h in sec ) );
end );

# IsAutomorphicLoop( L )
InstallMethod( IsAutomorphicLoop, "for loop",
    [ IsLoop ],
    Q -> IsLeftAutomorphicLoop( Q ) and IsMiddleAutomorphicLoop( Q )
    # Theorem: rigth A-loop + middle A-loop implies left A-loop
);

# implies
# InstallTrueMethod( IsLeftALoop, IsALoop );
# InstallTrueMethod( IsRightALoop, IsALoop );
# InstallTrueMethod( IsMiddleALoop, IsALoop );
# InstallTrueMethod( IsLeftALoop, IsRightALoop and HasAntiautomorphicInverseProperty );
# InstallTrueMethod( IsRightALoop, IsLeftALoop and HasAntiautomorphicInverseProperty );
# InstallTrueMethod( IsFlexible, IsMiddleALoop );
# InstallTrueMethod( HasAntiautomorphicInverseProperty, IsFlexible and IsLeftALoop );
# InstallTrueMethod( HasAntiautomorphicInverseProperty, IsFlexible and IsRightALoop );
# InstallTrueMethod( IsMoufangLoop, IsALoop and IsLeftAlternative );
# InstallTrueMethod( IsMoufangLoop, IsALoop and IsRightAlternative );
# InstallTrueMethod( IsMoufangLoop, IsALoop and HasLeftInverseProperty );
# InstallTrueMethod( IsMoufangLoop, IsALoop and HasRightInverseProperty );
# InstallTrueMethod( IsMoufangLoop, IsALoop and HasWeakInverseProperty );

# is implied by
# InstallTrueMethod( IsMiddleALoop, IsCommutative and IsLoop);
# InstallTrueMethod( IsLeftALoop, IsLeftBruckLoop );
# InstallTrueMethod( IsLeftALoop, IsLCCLoop );
# InstallTrueMethod( IsRightALoop, IsRightBruckLoop );
# InstallTrueMethod( IsRightALoop, IsRCCLoop );
# InstallTrueMethod( IsALoop, IsCommutative and IsMoufangLoop );
# InstallTrueMethod( IsALoop, IsLeftALoop and IsMiddleALoop );
# InstallTrueMethod( IsALoop, IsRightALoop and IsMiddleALoop );
# InstallTrueMethod( IsALoop, IsAssociative and IsLoop);
