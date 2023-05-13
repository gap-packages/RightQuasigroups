# PermutationGroups.gi
# Permutation groups associated with right quasigroups
# =============================================================================

# TRANSLATIONS AND SECTIONS
# _____________________________________________________________________________

# RightTranslation
# LeftTranslation

InstallMethod( RightTranslation, "for right quasigroup and element",
    [ IsRightQuasigroup, IsObject ],
function( Q, x )
    local posx, source, F, target;
    if not x in Q then x := Q[x]; fi;
    if HasRightSection( Q ) then
        return RightSection( Q )[ PositionSorted( Elements( Q ), x ) ];
    fi;
    source := ParentInd( Q );
    F := FamilyObj( Q.1 );
    if IsBound( F!.multTable ) then 
        posx := ParentInd( x );
        target := List( source, i -> F!.multTable[ i, posx ] );
    elif HasMultiplicationTable( Q ) then # this might happen without F!.multTable being bound
        posx := PositionSorted( Elements( Q ), x );
        target := List( [1..Size(Q)], i -> MultiplicationTable( Q )[ i, posx ] );
    else
        target := ParentInd( List( Q, y -> y*x ) );
    fi;
    # source and target are now lists of indices of length |Q|
    return MappingPermListList( source, target );
end );

InstallMethod( LeftTranslation, "for quasigroup and element",
    [ IsRightQuasigroup, IsObject ],
function( Q, x )
    local posx, source, F, target, f, i;
    if not x in Q then x := Q[x]; fi;
    if HasLeftSection( Q ) then
        return LeftSection( Q )[ PositionSorted( Elements( Q ), x ) ];
    fi;
    source := ParentInd( Q );
    F := FamilyObj( Q.1 );
    if IsBound( F!.multTable ) then 
        posx := ParentInd( x );
        target := List( source, i -> F!.multTable[ posx, i ] );
    elif HasMultiplicationTable( Q ) then
        posx := PositionSorted( Elements( Q ), x );
        target := List( [1..Size(Q)], i -> MultiplicationTable( Q )[ posx, i ] );
    else
        target := ParentInd( List( Q, y -> x*y ) );
    fi;
    if IsQuasigroup( Q ) then
        return MappingPermListList( source, target );
    fi;
    # right quasigroup
    return TransformationListList( source, target );
end );

# RightSection
# LeftSection

InstallMethod( RightSection, "for right quasigroup",
    [ IsRightQuasigroup ],
    Q -> List( Q, x -> RightTranslation( Q, x ) )
);

InstallMethod( LeftSection, "for quasigroup",
    [ IsRightQuasigroup ],
    Q -> List( Q, x -> LeftTranslation( Q, x ) )
);

# MULTIPLICATION GROUPS AND RELATIVE MULTIPLICATION GROUPS
# _____________________________________________________________________________

# RightMultiplicationGroup
# LeftMultiplicationGroup
# MultiplicationGroup

InstallMethod( RightMultiplicationGroup, "for right quasigroup",
    [ IsRightQuasigroup ],
    Q -> RQ_GroupByGenerators( SmallGeneratingSet( Group( RightSection( Q ) ) ) )
);

InstallMethod( LeftMultiplicationGroup, "for quasigroup",
    [ IsQuasigroup ],
    Q -> RQ_GroupByGenerators( SmallGeneratingSet( Group( LeftSection( Q ) ) ) )
);

InstallMethod( MultiplicationGroup, "for quasigroup",
    [ IsQuasigroup ],
    Q -> RQ_GroupByGenerators( SmallGeneratingSet( Group( Union( RightSection( Q ), LeftSection( Q ) ) ) ) )
);

# LeftMultiplicationSemigroup
InstallMethod( LeftMultiplicationSemigroup, "for right quasigroup",
    [ IsRightQuasigroup ],
function( Q )
    if IsQuasigroup( Q ) then
        return LeftMultiplicationGroup( Q );
    fi;
    # right quasigroup
    return Semigroup( LeftSection( Q ) );
end );

# RelativeRightMultiplicationGroup
# RelativeLeftMultiplicationGroup
# RelativeMultiplicationGroup

InstallMethod( RelativeRightMultiplicationGroup, "for right quasigroup and its subrightquasigroup",
    [ IsRightQuasigroup, IsRightQuasigroup ],
function( Q, S )
    local perms;
    if not IsSubrightquasigroup( Q, S ) then
        Error("RQ: <2> must be a subrightquasigroup of <1>.");
    fi;
    perms := RightSection( Q ){ ParentInd( S ) };
    return RQ_GroupByGenerators( SmallGeneratingSet( Group( perms ) ) );
end );

InstallMethod( RelativeLeftMultiplicationGroup, "for quasigroup and its subquasigroup",
    [ IsQuasigroup, IsQuasigroup ],
function( Q, S )
    local perms;
    if not IsSubquasigroup( Q, S ) then
        Error("RQ: <2> must be a subquasigroup of <1>.");
    fi;
    perms := LeftSection( Q ){ ParentInd( S ) };
    return RQ_GroupByGenerators( SmallGeneratingSet( Group( perms ) ) );
end );

InstallMethod( RelativeMultiplicationGroup, "for quasigroup and its subquasigroups",
    [ IsQuasigroup, IsQuasigroup ],
function( Q, S )
    local perms;
    if not IsSubquasigroup( Q, S ) then
        Error("RQ: <2> must be a sub quasigroup of <1>.");
    fi;
    perms := Union( RightSection( Q ){ ParentInd( S ) }, LeftSection( Q ){ ParentInd( S ) } );
    return RQ_GroupByGenerators( SmallGeneratingSet( Group( perms ) ) );
end );

# INNER MAPPING GROUPS
# _____________________________________________________________________________

# RightInnerMapping
InstallMethod( RightInnerMapping, "for right quasigroup and two right quasigroup elements",
    [ IsRightQuasigroup, IsRightQuasigroupElement, IsRightQuasigroupElement ],
function( Q, x, y )
    if not x in Q then x := Q[x]; fi;
    if not y in Q then y := Q[y]; fi;
    return RightTranslation( Q, x ) * RightTranslation( Q, y ) * RightTranslation( Q, x*y )^(-1);
end );

# MiddleInnerMapping
InstallMethod( MiddleInnerMapping, "for right quasigroup and right quasigroup element",
    [ IsQuasigroup, IsQuasigroupElement ],
function( Q, x )
    if not x in Q then x := Q[x]; fi;
    return RightTranslation( Q, x ) * LeftTranslation( Q, x )^(-1);
end );

# LeftInnerMapping
InstallMethod( LeftInnerMapping, "for loop and two loop elements",
    [ IsQuasigroup, IsQuasigroupElement, IsQuasigroupElement ],
function( L, x, y )
    if not x in L then x := L[x]; fi;
    if not y in L then y := L[y]; fi;
    return LeftTranslation( L, x ) * LeftTranslation( L, y ) * LeftTranslation( L, y*x )^(-1);
end );

# RightInnerMappingGroup
InstallMethod( RightInnerMappingGroup, "for loop",
    [ IsLoop ], 1, # so that this methods gets called for loops
    Q -> Stabilizer( RightMultiplicationGroup( Q ), ParentInd( One( Q ) ) )
);

InstallOtherMethod( RightInnerMappingGroup, "for right quasigroup",
    [ IsRightQuasigroup ], 0,
function( Q )
    local gens;
    gens := List( Tuples(Q,2), t -> RightInnerMapping( Q, t[1], t[2] ) );
    return RQ_GroupByGenerators( SmallGeneratingSet( Group( gens ) ) );
end );

# MiddleInnerMappingGroup
InstallMethod( MiddleInnerMappingGroup, "for quasigroup",
   [ IsQuasigroup],
function( Q )
    local gens;
    gens := List( Q, x -> MiddleInnerMapping( Q, x ) );
    return RQ_GroupByGenerators( SmallGeneratingSet( Group( gens ) ) );
end );

# LeftInnerMappingGroup
InstallMethod( LeftInnerMappingGroup, "for loop",
    [ IsLoop ], 1, # so that this methods gets called for loops
    Q -> Stabilizer( LeftMultiplicationGroup( Q ), ParentInd( One( Q ) ) )
);

InstallOtherMethod( LeftInnerMappingGroup, "for quasigroup",
    [ IsQuasigroup ], 0,
function( Q )
    local gens;
    gens := List( Tuples(Q,2), t -> LeftInnerMapping( Q, t[1], t[2] ) );
    return RQ_GroupByGenerators( SmallGeneratingSet( Group( gens ) ) );
end );

# InnerMappingGroup
InstallMethod( InnerMappingGroup, "for loop",
    [ IsLoop ], 1, # so that this methods gets called for loops
    Q -> Stabilizer( MultiplicationGroup( Q ), ParentInd( One( Q ) ) )
);

InstallOtherMethod( InnerMappingGroup, "for quasigroup",
    [ IsQuasigroup ], 0,
function( Q )
    return Group( Union(
        GeneratorsOfGroup( RightInnerMappingGroup( Q ) ),
        GeneratorsOfGroup( MiddleInnerMappingGroup( Q ) ),
        GeneratorsOfGroup( LeftInnerMappingGroup( Q ) )
    ) );
end );

# DISPLACEMENT GROUPS
# ____________________________________________________________________________

# RightPosDisplacementGroup

InstallMethod( RightPosDisplacementGroup, "for right quasigroup",
    [ IsRightQuasigroup ],
function( Q )
    local g, gens;
    g := RightSection( Q )[1]^(-1);
    gens := List( RightSection( Q ), f -> g*f );
    return RQ_GroupByGenerators( SmallGeneratingSet( Group( gens ) ) );
end );

# RightNegDisplacementGroup

InstallMethod( RightNegDisplacementGroup, "for right quasigroup",
    [ IsRightQuasigroup ],
function( Q )
    local g, gens;
    g := RightSection( Q )[1]^(-1);
    gens := List( RightSection( Q ), f -> f*g );
    return RQ_GroupByGenerators( SmallGeneratingSet( Group( gens ) ) );
end );

# RightDisplacementGroup

InstallMethod( RightDisplacementGroup, "for right quasigroup",
    [ IsRightQuasigroup ],
function( Q )
    local g, gens;
    g:= RightSection( Q )[1]^(-1);
    gens := Union( List( RightSection( Q ), f -> g*f ), List( RightSection( Q ), f -> f*g ) );
    return RQ_GroupByGenerators( SmallGeneratingSet( Group( gens ) ) );
end );

# LeftPosDisplacementGroup

InstallMethod( LeftPosDisplacementGroup, "for quasigroup",
    [ IsQuasigroup ],
function( Q )
    local g, gens;
    g := LeftSection( Q )[1]^(-1);
    gens := List( LeftSection( Q ), f -> g*f );
    return RQ_GroupByGenerators( SmallGeneratingSet( Group( gens ) ) );
end );

# LeftNegDisplacementGroup

InstallMethod( LeftNegDisplacementGroup, "for quasigroup",
    [ IsQuasigroup ],
function( Q )
    local g, gens;
    g := LeftSection( Q )[1]^(-1);
    gens := List( LeftSection( Q ), f -> f*g );
    return RQ_GroupByGenerators( SmallGeneratingSet( Group( gens ) ) );
end );

# LeftDisplacementGroup

InstallMethod( LeftDisplacementGroup, "for quasigroup",
    [ IsQuasigroup ],
function( Q )
    local g, gens;
    g := LeftSection( Q )[1];
    gens := Union( List( LeftSection( Q ), f -> g*f ), List( LeftSection( Q ), f -> f*g ) );
    return RQ_GroupByGenerators( SmallGeneratingSet( Group( gens ) ) );
end );

# IsIsotopicToGroup

InstallMethod( IsIsotopicToGroup, "for quasigroup",
    [ IsQuasigroup ],
    Q -> Size( LeftPosDisplacementGroup( Q ) ) = Size( Q )
);

# IsIsotopicToAbelianGroup

InstallMethod( IsIsotopicToAbelianGroup, "for quasigroup",
    [ IsQuasigroup ],
function( Q )
    local D;
    D := LeftPosDisplacementGroup( Q );
    return Size( D ) = Size( Q ) and IsCommutative( D );
end );