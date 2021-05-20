# Mappings.gi
# Mappings, tranformations and permutations of right quasigroups
# =============================================================================

# RIGHT QUASIGROUP MAPPINGS
# _____________________________________________________________________________

# RQ_IsAlgebraMapping
InstallMethod( RQ_IsAlgebraMapping, "for category and mapping",
    [ IsObject, IsMapping ],
function( category, m )
    return category( Source( m ) ) and category( Range( m ) );
end );;

# IsRightQuasigroupMapping
InstallMethod( IsRightQuasigroupMapping, "for mapping",
    [ IsMapping ],
    m -> RQ_IsAlgebraMapping( IsRightQuasigroup, m )
);

# IsQuasigroupMapping
InstallMethod( IsQuasigroupMapping, "for mapping",
    [ IsMapping ],
    m -> RQ_IsAlgebraMapping( IsQuasigroup, m )
);

# IsLoopMapping
InstallMethod( IsLoopMapping, "for mapping",
    [ IsMapping ],
    m -> RQ_IsAlgebraMapping( IsLoop, m )
);

# AsRightQuasigroupMapping

InstallMethod( AsRightQuasigroupMapping, "for two right quasigroups and transformation",
    [ IsRightQuasigroup, IsRightQuasigroup, IsTransformation ],
function( Q1, Q2, f )
    return AsRightQuasigroupMapping( Q1, Q2, f, false );
end );

InstallOtherMethod( AsRightQuasigroupMapping, "for two right quasigroups, transformation and bool",
    [ IsRightQuasigroup, IsRightQuasigroup, IsTransformation, IsBool ],
function( Q1, Q2, f, isCanonical )
    # this is the main method for AsRightQuasigroupMapping
    local func;
    RQ_IsTransformation( Q1, Q2, f, isCanonical, true ); # true = reportErrors
    if isCanonical then
        func := function( x )
            return Elements( Q2 )[ Position( Elements( Q1 ), x )^f ];
        end;
    else
        func := function( x )
            return Q2.( ParentInd( x )^f );
        end;
    fi;
    return MappingByFunction( Q1, Q2, func );
end );

InstallOtherMethod( AsRightQuasigroupMapping, "for right quasigroup and permutation",
    [ IsRightQuasigroup, IsPerm ],
function( Q, f )
    return AsRightQuasigroupMapping( Q, Q, AsTransformation( f ), false );
end );

InstallOtherMethod( AsRightQuasigroupMapping, "for right quasigroup, permutation and bool",
    [ IsRightQuasigroup, IsPerm, IsBool ],
function( Q, f, isCanonical )
    return AsRightQuasigroupMapping( Q, Q, AsTransformation( f ), isCanonical );
end );

# REVISIT: Add AsQuasigroupMapping, AsLoopMapping? It's just busy work.

# CANONICAL PERMUTATIONS AND PARENT PERMUTATIONS
# _____________________________________________________________________________

# IsParentPerm
InstallMethod( IsParentPerm, "for right quasigroup and permuation",
    [ IsRightQuasigroup, IsPerm ],
function( Q, f )
    return RQ_IsTransformation( Q, Q, AsTransformation( f ), false, false ); # isCanonical, reportErrors
end );

# AsCanonicalPerm

InstallMethod( AsCanonicalPerm, "for right quasigroup mapping",
    [ IsMapping ],
function( m )
    local Q;
    Q := Source( m );
    if not ( IsRightQuasigroupMapping( m ) and Size( Q ) = Size( Range( m ) ) and IsBijective( m ) ) then
        Error( "RQ: <1> must be a bijective mapping between right quasigroups of the same size." );
    fi;
    return PermList( List( [1..Size(Q)], i -> Position( Elements( Range(m) ), Elements( Q )[i]^m ) ) );
end );

InstallOtherMethod( AsCanonicalPerm, "for right quasigroup and parent transformation",
    [ IsRightQuasigroup, IsTransformation ],
function( Q, t )
    local ind;
    RQ_IsTransformation( Q, Q, t, false, true ); # isCanonical, reportErrors
    # bijective?
    if Size( Set( ParentInd( Q ), i -> i^t ) ) < Size( Q ) then
        Error( "RQ: <2> must be a bijective transformation on the parent indices of <1>." );
    fi;
    ind := ParentInd( Q );
    return PermList( List( ind, i -> Position( ind, i^t ) ) );
end );

InstallOtherMethod( AsCanonicalPerm, "for right quasigroup and parent permutation",
    [ IsRightQuasigroup, IsPerm ],
function( Q, f )
    return AsCanonicalPerm( Q, AsTransformation( f ) );
end );

# AsParentPerm

InstallMethod( AsParentPerm, "for right quasigroup mapping",
    [ IsMapping ],
function( m )
    local Q;
    Q := Source( m );
    if not ( IsRightQuasigroupMapping( m ) and Q = Range( m ) and IsBijective( m ) ) then
        Error( "RQ: <1> must be a bijective mapping from a right quasigroup to itself." );
    fi;
    return MappingPermListList( ParentInd( Q ), ParentInd( List( Q, x -> x^m ) ) ); 
end );

InstallOtherMethod( AsParentPerm, "for right quasigroup and canonical transformation",
    [ IsRightQuasigroup, IsTransformation ],
function( Q, t )
    local ind;
    RQ_IsTransformation( Q, Q, t, true, true ); # isCanonical, reportErrors
    # bijective?
    if not Size( Set( [1..Size(Q)], i -> i^t ) ) = Size( Q ) then
        Error( "RQ: <3> must be a bijective transformation on <1>." );
    fi;
    ind := ParentInd( Q );
    return MappingPermListList( ParentInd( Q ), List( [1..Size(Q)], i -> ind[ i^t ] ) );
end );

InstallOtherMethod( AsParentPerm, "for right quasigroup and canonical permutation",
    [ IsRightQuasigroup, IsPerm ],
function( Q, f )
    return AsParentPerm( Q, AsTransformation( f ) );
end );

# CANONICAL TRANSFORMATIONS AND PARENT TRANSFORMATIONS
# _____________________________________________________________________________

# RQ_IsTransformation
InstallMethod( RQ_IsTransformation, "for two right quasigroups, transformation and two bools",
    [ IsRightQuasigroup, IsRightQuasigroup, IsTransformation, IsBool, IsBool ],
function( Q1, Q2, f, isCanonical, reportErrors )
    local ind1, ind2;
    if isCanonical then
        if not ForAll( [1..Size(Q1)], i -> i^f in [1..Size(Q2) ] ) then
            return RQ_OptionalError( reportErrors, "RQ: <3> is not a canonical transformation from <1> to <2>." );
        fi;
        return true;
    fi;
    # parent transformation test
    ind1 := ParentInd( Q1 );
    ind2 := ParentInd( Q2 );
    if not ForAll( ind1, i -> i^f in ind2 ) then
        return RQ_OptionalError( reportErrors, "RQ: <3> is not a parent transformation from <1> to <2>." );
    fi;
    return true;
end );

# IsCanonicalTransformation
InstallMethod( IsCanonicalTransformation, "for two right quasigroups and transformation",
    [ IsRightQuasigroup, IsRightQuasigroup, IsTransformation ],
function( Q1, Q2, t )
    return RQ_IsTransformation( Q1, Q2, t, true, false ); # isCanonical, reportErrors
end );

# IsParentTransformation
InstallMethod( IsParentTransformation, "for two right quasigroups and transformation",
    [ IsRightQuasigroup, IsRightQuasigroup, IsTransformation ],
function( Q1, Q2, t )
    return RQ_IsTransformation( Q1, Q2, t, false, false ); # isCanonical, reportErrors
end );

# AsCanonicalTransformation

InstallMethod( AsCanonicalTransformation, "for right quasigroup mapping",
    [ IsMapping ],
function( m )
    local Q, range;
    if not IsRightQuasigroupMapping( m ) then
        return Error( "RQ: <1> must be a right quasigroup mapping." );
    fi;
    Q := Source( m );
    range := Elements( Range( m ) );
    return Transformation( [1..Size(Q)], List( Q, x -> PositionSorted( range, x^m ) ) );
end );

InstallOtherMethod( AsCanonicalTransformation, "for two right quasigroups and parent transformation",
    [ IsRightQuasigroup, IsRightQuasigroup, IsTransformation ],
function( Q1, Q2, t )
    local ind2;
    if Parent( Q1 ) = Q1 and Parent( Q2 ) = Q2 then
        return t; # canonical = parent
    fi;
    RQ_IsTransformation( Q1, Q2, t, false, true ); # isCanonical, reportErrors
    ind2 := ParentInd( Q2 );
    return Transformation( [1..Size(Q1)], List( ParentInd( Q1 ), i -> PositionSorted( ind2, i^t ) ) );
end );

InstallOtherMethod( AsCanonicalTransformation, "for right quasigroup and parent permutation",
    [ IsRightQuasigroup, IsPerm ],
function( Q, f )
    return AsCanonicalTransformation( Q, Q, AsTransformation( f ) );
end );

# AsParentTransformation

InstallMethod( AsParentTransformation, "for right quasigroup mapping",
    [ IsMapping ],
function( m )
    local Q;
    if not IsRightQuasigroupMapping( m ) then
        return Error( "RQ: <1> must be a right quasigroup mapping." );
    fi;
    Q := Source( m );
    return Transformation( ParentInd( Q ), List( ParentInd( Q ), i -> ParentInd( (Q.(i))^m ) ) );
end );

InstallOtherMethod( AsParentTransformation, "for two right quasigroups and canonical transformation",
    [ IsRightQuasigroup, IsRightQuasigroup, IsTransformation ],
function( Q1, Q2, t )
    local ind2;
    RQ_IsTransformation( Q1, Q2, t, true, true ); # isCanonical, reportErrors
    if Parent( Q1 ) = Q1 and Parent( Q2 ) = Q2 then
        return t; # canonical = parent
    fi;
    ind2 := ParentInd( Q2 );
    return Transformation( ParentInd( Q1 ), List( [1..Size(Q1)], i -> ind2[ i^t ] ) );
end );
  
InstallOtherMethod( AsParentTransformation, "for right quasigroup and canonical permutation",
    [ IsRightQuasigroup, IsPerm ],
function( Q, f )
    return AsParentTransformation( Q, Q, AsTransformation( f ) );
end );

# IsCanonicalPerm
InstallMethod( IsCanonicalPerm, "for right quasigroup and permutation",
    [ IsRightQuasigroup, IsPerm ],
function( Q, f )
    return RQ_IsTransformation( Q, Q, AsTransformation( f ), true, false ); # isCanonical, reportErrors
end );
