# Racks.gi
# Racks and quandles
# =============================================================================

# TOWARD RACKS AND QUANDLES
# _____________________________________________________________________________

InstallOtherMethod( IsIdempotent, "for right quasigroup",
    [ IsRightQuasigroup ],
    Q -> ForAll( Q, x -> x = x*x )
);

InstallMethod( IsRack, "for right quasigroup",
    [ IsRightQuasigroup ],
    Q -> IsRightSelfDistributive( Q )
);

InstallMethod( IsQuandle, "for right quasigroup",
    [ IsRightQuasigroup ],
    Q -> IsRack( Q ) and IsIdempotent( Q )
);

InstallMethod( IsLatinRack, "for right quasigroup",
    [ IsRightQuasigroup ],
    Q -> IsRack( Q ) and IsLeftQuasigroupMagma( Q )
);

InstallMethod( IsLatinQuandle, "for right quasigroup",
    [ IsRightQuasigroup ],
    Q -> IsQuandle( Q ) and IsLeftQuasigroupMagma( Q )
);

# CONTRUCTORS FOR RACKS
# _____________________________________________________________________________

InstallMethod( PermutationalRack, "for collection and permutation",
    [ IsCollection, IsPerm ],
function( S, f )
    return PermutationalRack( S, f, RQ_defaultConstructorStyle );
end );

InstallOtherMethod( PermutationalRack, "for collection, permutation and record",
    [ IsCollection, IsPerm, IsRecord ],
function( S, f, style )
    local mult, Q;
    RQ_CompleteConstructorStyle( style );
    if style.checkArguments then
        if not ForAll( S, x -> x^f in S ) then
            Error( "RQ: <2> must be a permutation the restricts to <1>." );
        fi;
    fi;
    f := RestrictedPerm( f, S );
    mult := function( x, y )
        return x^f;
    end;
    Q := RightQuasigroupByFunction( S, mult, ConstructorStyle( style.indexBased, false ) );
    SetIsRack( Q, true );
    return Q;
end );

# CONTRUCTORS FOR QUANDLES
# _____________________________________________________________________________

# REVISIT: Are these constructors safe in the non-index based case? If so, can we use this approach elsewhere?

# AffineQuandle

InstallMethod( AffineQuandle, "for abelian group and automorphism",
    [ IsGroup, IsMapping ],
function( A, f )
    return AffineQuandle( A, f, RQ_defaultConstructorStyle );
end );

InstallOtherMethod( AffineQuandle, "for abelian group, automorphism and record",
    [ IsGroup, IsMapping, IsRecord ],
function( A, f, style )
    local mult, Q;
    RQ_CompleteConstructorStyle( style );
    if style.checkArguments then
        if not IsCommutative( A ) then
            Error( "RQ: <1> must be an abelian group.");
        fi;
        if not ( Source(f)=A and Range(f)=A and ForAll( A, x -> ForAll( A, y -> x^f*y^f = (x*y)^f ) ) ) then
            Error( "RQ: <2> must be an automorphism of <1>." );
        fi;
    fi;
    if IsAdditiveGroup( A ) then
        mult := function( x, y ) return x^f + y - y^f; end;
    else 
        mult := function( x, y ) return x^f*y*((y^f)^-1); end;
    fi;
    Q := RightQuasigroupByFunction( A, mult, ConstructorStyle( style.indexBased, false ) ); 
    SetIsQuandle( Q, true );
    return Q;
end );

# GalkinQuandle

InstallMethod( GalkinQuandle, "for group, subgroup and automorphism",
    [ IsGroup, IsGroup, IsMapping ],
function( G, H, f )
    return GalkinQuandle( G, H, f, RQ_defaultConstructorStyle);
end );

InstallOtherMethod( GalkinQuandle, "for group, subgroup, automorphism and record",
    [ IsGroup, IsGroup, IsMapping, IsRecord ],
function( G, H, f, style )
    local S, mult, Q;
    RQ_CompleteConstructorStyle( style );
    if style.checkArguments then # check arguments
        if not IsSubgroup( G, H ) then
            Error( "RQ: <2> must be a subgroup of <1>.");
        fi;
        if not ( Source(f)=G and Range(f)=G and ForAll( G, x -> ForAll( G, y -> x^f*y^f = (x*y)^f ) ) ) then
            Error( "RQ: <3> must be an automorphism of <1>." );
        fi;
        if not ForAll( H, x -> x=x^f ) then
            Error( "RQ: <2> must be fixed pointwise by <3>." );
        fi;
    fi; 
    S := Set( RightTransversal( G, H ) );
    mult := function( x, y )
        return First( S, z -> (x*y^-1)^f*y in H*z );
    end;
    Q := RightQuasigroupByFunction( S, mult, ConstructorStyle( style.indexBased, false ) );
    SetIsQuandle( Q, true );
    return Q;
end );

# ConjugationQuandle

InstallMethod( ConjugationQuandle, "for group",
    [ IsGroup ],
function( G )
    return ConjugationQuandle( G, RQ_defaultConstructorStyle);
end );

InstallOtherMethod( ConjugationQuandle, "for group and record",
    [ IsGroup, IsRecord ],
function( G, style )
    local mult, Q;
    RQ_CompleteConstructorStyle( style );
    mult := function( x, y )
        return y^-1*x*y;
    end;
    Q := RightQuasigroupByFunction( G, mult, ConstructorStyle( style.indexBased, false ) );
    SetIsQuandle( Q, true );
    return Q;
end );

# RACK AND QUANDLE ENVELOPES (JOYCE-BLACKBURN REPRESENTATION)
# _____________________________________________________________________________

# RQ_IsRackOrQuandleEnvelope
InstallMethod( RQ_IsRackOrQuandleEnvelope, "for category, group, two lists and bool",
    [ IsObject, IsGroup, IsList, IsList, IsBool ],
function( category, G, reps, perms, reportErrors )
    local set, orbs, m, pos_reps, conj_classes;
    if not IsPermGroup( G ) then
        return RQ_OptionalError( reportErrors, "RQ: <1> must be a permutation group." );
    fi;
    if not Length( reps ) = Length( perms ) then
        return RQ_OptionalError( reportErrors, "RQ: <2> and <3> must be of the same length." );
    fi;
    if not ForAll( reps, x -> IsPosInt( x ) ) then
        return RQ_OptionalError(reportErrors, "RQ: <2> must be a list of positive integers." );
    fi;
    set := Union( Orbits( G, reps ) );
    orbs := Orbits( G, set );
    m := Length( orbs );
    if not Length( reps ) = m then
        return RQ_OptionalError( reportErrors, "RQ: <2> is not a list of orbit representatives for <1>." );
    fi;
    pos_reps := List( reps, x -> First( [1..m],i -> x in orbs[i] ) ); # in which orbits reps are located
    if not Set( pos_reps ) = [1..m] then
        return RQ_OptionalError( reportErrors, "RQ: <2> is not a list of orbit representatives for <1>." );
    fi;
    if category = IsRack and not ForAll( [1..m], i -> perms[i] in Centralizer( G, Stabilizer( G, reps[i] ) ) ) then
        return RQ_OptionalError( reportErrors, "RQ: the translations do no lie in the centralizers of the stabilizers." );
    fi;
    if category = IsQuandle and not ForAll( [1..m], i -> perms[i] in Center( Stabilizer( G, reps[i] ) ) ) then
        return RQ_OptionalError( reportErrors, "RQ: the translations do no lie in the centers of the stabilizers." );
    fi;
    conj_classes := List( perms, f -> ConjugacyClass( G, f ) );
    if not Size( G ) = Size( Group( Union( conj_classes ) ) ) then
        return RQ_OptionalError( reportErrors, "RQ: the conjugacy classes of translations do not generate <1>." );
    fi;
    return true;
end );

# IsRackEnvelope
# IsQuandleEnvelope

InstallMethod( IsRackEnvelope, "for group and two lists",
    [ IsGroup, IsList, IsList ],
function( G, reps, perms )
    return RQ_IsRackOrQuandleEnvelope( IsRack, G, reps, perms, false );
end );

InstallMethod( IsQuandleEnvelope, "for group and two lists",
    [ IsGroup, IsList, IsList ],
function( G, reps, perms )
    return RQ_IsRackOrQuandleEnvelope( IsQuandle, G, reps, perms, false );
end );

# RackEnvelope
# QuandleEnvelope

InstallMethod( RackEnvelope, "for rack",
    [ IsRack ],
function( Q )
    # PROG: we allow non-canonical situations
    local set, gens, G, orbs, reps, perms;
    set := ParentInd( Q );
    gens := Set( Q, x -> RestrictedPerm( RightTranslation( Q, x ), set ) );
    G := Group( gens ); # right multiplication group restricted to ParentInd( Q )
    G := RQ_GroupByGenerators( SmallGeneratingSet( G ) ); # for efficiency
    orbs := Orbits( G, set );
    reps := List( orbs, O -> O[1] );
    perms := List( reps, i -> RestrictedPerm( RightTranslation( Q, Q.(i) ), set ) );
    return [ G, reps, perms ];
end );

InstallMethod( QuandleEnvelope, "for quandle",
    [ IsQuandle ],
    Q -> RackEnvelope( Q )
);

# RQ_RackOrQuandleByEnvelope

InstallMethod( RQ_RackOrQuandleByEnvelope, "for category, group, list, list and record",
    [ IsObject, IsGroup, IsList, IsList, IsRecord ],
function( category, G, reps, perms, style )
    local uSet, rsection, m, i, r, t, Q;
    RQ_CompleteConstructorStyle( style );
    if style.checkArguments then
        RQ_IsRackOrQuandleEnvelope( category, G, reps, perms, true ); # report errors
    fi;
    uSet := Union( Orbits( G, reps ) );
    rsection := [];
    m := Length( reps ); # the number of orbits
    for i in [1..m] do
        r := perms[ i ]; # right translation for orbit representative
        for t in RightTransversal( G, Stabilizer( G, reps[i] ) ) do
            rsection[ Position( uSet, reps[i]^t ) ] := RestrictedPerm( r^t, uSet );
        od;
    od;
    Q := RightQuasigroupByRightSection( uSet, rsection, ConstructorStyle( style.indexBased, false ) );
    if category = IsRack then
        SetIsRack( Q, true );
    elif category = IsQuandle then
        SetIsQuandle( Q, true );
    fi;
    return Q;
end );

# RackByRackEnvelope

InstallMethod( RackByRackEnvelope, "for group and two lists",
    [ IsGroup, IsList, IsList ],
function( G, reps, perms )
    return RackByRackEnvelope( G, reps, perms, RQ_defaultConstructorStyle );
end );

InstallOtherMethod( RackByRackEnvelope, "for group, two lists and record",
    [ IsGroup, IsList, IsList, IsRecord ],
function( G, reps, perms, style )
    return RQ_RackOrQuandleByEnvelope( IsRack, G, reps, perms, style );
end );

InstallOtherMethod( RackByRackEnvelope, "for list",
    [ IsList ],
    envelope -> RackByRackEnvelope( envelope, RQ_defaultConstructorStyle )
);

InstallOtherMethod( RackByRackEnvelope, "for list and record",
    [ IsList, IsRecord ],
function( envelope, style )
    if not Length( envelope ) = 3 then
        Error( "RQ: <1> must be a list of legth 3, a rack envelope." );
    fi;
    return RQ_RackOrQuandleByEnvelope( IsRack, envelope[1], envelope[2], envelope[3], style );
end );
 
# QuandleByQuandleEnvelope

InstallMethod( QuandleByQuandleEnvelope, "for group and two lists",
    [ IsGroup, IsList, IsList ],
function( G, reps, perms )
    return QuandleByQuandleEnvelope( G, reps, perms, RQ_defaultConstructorStyle );
end );

InstallOtherMethod( QuandleByQuandleEnvelope, "for group, two lists and record",
    [ IsGroup, IsList, IsList, IsRecord ],
function( G, reps, perms, style )
    return RQ_RackOrQuandleByEnvelope( IsQuandle, G, reps, perms, style );
end );

InstallOtherMethod( QuandleByQuandleEnvelope, "for list",
    [ IsList ],
    envelope -> QuandleByQuandleEnvelope( envelope, RQ_defaultConstructorStyle )
);

InstallOtherMethod( QuandleByQuandleEnvelope, "for list and record",
    [ IsList, IsRecord ],
function( envelope, style )
    if not Length( envelope ) = 3 then
        Error( "RQ: <1> must be a list of legth 3, a quandle envelope." );
    fi;
    return RQ_RackOrQuandleByEnvelope( IsQuandle, envelope[1], envelope[2], envelope[3], style );
end );

# SUBRACKS AND SUBQUANDLES
# _____________________________________________________________________________

# IsSubrack
InstallMethod( IsSubrack, "for two racks",
    [ IsRack, IsRack ],
function( Q, S )
    return IsSubrightquasigroup( Q, S );
end );

# IsSubquandle
InstallMethod( IsSubquandle, "for two quandles",
    [ IsQuandle, IsQuandle ],
function( Q, S )
    return IsSubrightquasigroup( Q, S );
end );

# Subrack
InstallMethod( Subrack, "for rack and collection of elements",
    [ IsRack, IsCollection ],
function( Q, gens )
    return RQ_Subalgebra( Q, gens );
end );

# Subquandle
InstallMethod( Subquandle, "for quandle and collection of elements",
    [ IsQuandle, IsCollection ],
function( Q, gens )
    return RQ_Subalgebra( Q, gens );
end );

# AllSubracks
InstallMethod( AllSubracks, "for rack",
    [ IsRack ],
    Q -> RQ_AllSubalgebras( Q )
);

# AllSubquandles
InstallMethod( AllSubquandles, "for quandle",
    [ IsQuandle ],
    Q -> RQ_AllSubalgebras( Q )
);

# Rack
# Quandle

InstallGlobalFunction( Rack,
function( gens... )
    return RQ_AlgebraByGenerators( IsRack, gens );
end );

InstallGlobalFunction( Quandle,
function( gens... )
    return RQ_AlgebraByGenerators( IsQuandle, gens ); 
end );

# RackByGenerators
# QuandleByGenerators

InstallGlobalFunction( RackByGenerators,
function( gens... )
     return RQ_AlgebraByGenerators( IsRack, gens ); 
end );

InstallGlobalFunction( QuandleByGenerators,
function( gens... )
     return RQ_AlgebraByGenerators( IsQuandle, gens ); 
end );

# RackWithGenerators
# QuandleWithGenerators

InstallGlobalFunction( RackWithGenerators,
function( gens... )
    return RQ_AlgebraWithGenerators( IsRack, gens );
end );

InstallGlobalFunction( QuandleWithGenerators,
function( gens... )
    return RQ_AlgebraWithGenerators( IsQuandle, gens );
end );

# ISOMORPHISMS AND ISOTOPISMS FOR RACKS AND QUANDLES
# _____________________________________________________________________________

# IsomorphismRacks
# IsomorphismQuandles

InstallMethod( IsomorphismRacks, "for two racks",
    [ IsRack, IsRack ],
function( Q1, Q2 )
    return IsomorphismRightQuasigroups( Q1, Q2 );
end );

InstallMethod( IsomorphismQuandles, "for two quandles",
    [ IsQuandle, IsQuandle ],
function( Q1, Q2 )
    return IsomorphismRightQuasigroups( Q1, Q2 );
end );

# RacksUpToIsomorphism
# QuandlesUpToIsomorphism

InstallMethod( RacksUpToIsomorphism, "for a list or racks",
    [ IsList ],
function( ls )
    if not ForAll( ls, Q -> HasIsRack( Q ) and IsRack( Q ) ) then
        Error( "RQ: The list must contain only racks." );
    fi;
    return RightQuasigroupsUpToIsomorphism( ls );
end );

InstallMethod( QuandlesUpToIsomorphism, "for a list or quandles",
    [ IsList ],
function( ls )
    if not ForAll( ls, Q -> HasIsQuandle( Q ) and IsQuandle( Q ) ) then
        Error( "RQ: The list must contain only quandles." );
    fi;
    return RightQuasigroupsUpToIsomorphism( ls );
end );