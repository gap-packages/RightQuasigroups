# Topisms.gi
# Homotopisms, isotopisms and autotopisms of right quasigroups
# =============================================================================

# HOMOTOPISMS OF RIGHT QUASIGROUPS
# _____________________________________________________________________________

# ISOTOPISMS OF RIGHT QUASIGROUPS
# _____________________________________________________________________________

# IsotopismRightQuasigroups

InstallMethod( IsotopismRightQuasigroups, "for two right quasigroups",
    [ IsRightQuasigroup, IsRightQuasigroup ],
function( Q1, Q2 )
    # REVISIT: Implement isotopism check for right quasigroups
    Error( "RQ: Not implemented yet." );
end);

# IsotopismQuasigroups

InstallMethod( IsotopismQuasigroups, "for two quasigroups",
    [ IsQuasigroup, IsQuasigroup ],
function( Q1, Q2 )
    # REVISIT: Implement isotopism check for quasigroups
    Error( "RQ: Not implemented yet." );
end);

# IsotopismLoops

InstallMethod( IsotopismLoops, "for two loops",
    [ IsLoop, IsLoop ],
function( Q1, Q2 )
    local T, f, g, Q, phi;
    # make all loops canonical
    if not IsCanonical(Q1) then Q1 := CanonicalCopy( Q1 ); fi;
    if not IsCanonical(Q2) then Q2 := CanonicalCopy( Q2 ); fi;
    # testing for isotopic invariants
    if not Size(Q1)=Size(Q2) then return fail; fi;
    if IsomorphismLoops( Center(Q1), Center(Q2) ) = fail then return fail; fi;
    if IsomorphismLoops( LeftNucleus(Q1), LeftNucleus(Q2) ) = fail then return fail; fi;
    if IsomorphismLoops( RightNucleus(Q1), RightNucleus(Q2) ) = fail then return fail; fi;
    if IsomorphismLoops( MiddleNucleus(Q1), MiddleNucleus(Q2) ) = fail then return fail; fi;
    # REVISIT: we could test for isomorphism among multiplication groups and inner mapping groups, too
    if not Size(MultiplicationGroup(Q1)) = Size(MultiplicationGroup(Q2)) then return fail; fi;
    if not Size(InnerMappingGroup(Q1)) = Size(InnerMappingGroup(Q2)) then return fail; fi;
    # construct all distinct principal loop isotopes of Q1 an check for isomorphism with Q2
    T := RightTransversal( Q1, MiddleNucleus( Q1 ) ); # just a subset 
    for f in Q1 do for g in T do 
        Q := PrincipalLoopIsotope( Q1, f, g );
        phi := IsomorphismLoops( Q, Q2 );
        if not phi = fail then 
            # reconstruct the isotopism (alpha, beta, gamma)
            return [ RightTranslation( Q1, f ) * phi, LeftTranslation( Q1, g ) * phi, phi ];
        fi; 
    od; od;
    return fail;
end);

# RQ_AlgebrasUpToIsotopism

InstallMethod( RQ_AlgebrasUpToIsotopism, "for category and list of algebras",
    [ IsObject, IsList ],
function( category, ls )
    local kept, positions, pos, Q, is_new, K;
    
    if IsEmpty( ls ) then
        return ls;
    fi;
    if not ForAll( ls, IsRightQuasigroup ) then
        Error( "RQ: <1> must be a list of right quasigroups" );
    fi;
    if Length( ls ) = 1 then
        return ls;
    fi;
    if not ForAll( ls, Q -> CategoryOfRightQuasigroup( Q ) = category ) then 
        Error("RQ: <1> must be a list of algebras of the same type");
    fi;        
       
    kept := []; # kept algebras 
    positions := []; # positions of kept algebras in the original list
    pos := 0;
    for Q in ls do
        pos := pos + 1;
        if not IsCanonical( Q ) then Q := CanonicalCopy( Q ); fi; # making canonical when seen for the first time
        is_new := true;
        for K in kept do
            if not RQ_IsotopismAlgebras( category, Q, K ) = fail then
                is_new := false;
                break;
            fi;
        od;
        if is_new then
            Add( kept, Q ); # storing discriminator, too
            Add( positions, pos );
        fi;
    od;
    # returning algebras from the original list
    return ls{positions};
end );

# RightQuasigroupsUpToIsotopism 
# QuasigroupsUpToIsotopism 
# LoopsUpToIsotopism

InstallMethod( RightQuasigroupsUpToIsotopism, "for list of right quasigroups",
    [ IsList ],
function( ls )
    return RQ_AlgebrasUpToIsotopism( IsRightQuasigroup, ls );
end );

InstallMethod( QuasigroupsUpToIsotopism, "for list of quasigroups",
    [ IsList ],
function( ls )
    return RQ_AlgebrasUpToIsotopism( IsQuasigroup, ls );
end );

InstallMethod( LoopsUpToIsotopism, "for list of loops",
    [ IsList ],
function( ls )
    return RQ_AlgebrasUpToIsotopism( IsLoop, ls );
end );

# ISOTOPES OF RIGHT QUASIGROUPS
# _____________________________________________________________________________

InstallMethod( RQ_AlgebraIsotopeByPerms, "for category, right quasigroup, three permutations and record",
    [ IsObject, IsRightQuasigroup, IsPerm, IsPerm, IsPerm, IsRecord ],
function( category, Q, f, g, h, style )
    local n, F, t, inv_f, inv_g, uSet, ct, copyQ;
    RQ_CompleteConstructorStyle( style );
    n := Size( Q );
    # checking arguments f, g, h
    if style.checkArguments and LargestMovedPoint( Group([f,g,h]) ) > n then
        Error( "RQ: The three permutations must act on [1..Size(Q)]");
    fi;
    # constructing Cayley table
    F := FamilyObj( Q.1 );
    t := MultiplicationTable( Q );
    inv_f := Inverse( f );
    inv_g := Inverse( g );
    uSet := UnderlyingSet( Q );
    ct := List( [1..n], i-> List( [1..n], j -> (t[ i^inv_f, j^inv_g ])^h ) ); # entries are in [1..n]
    ct := List( ct, row -> List( row, i -> uSet[ i ] ) );
    # constructing the algebra
    copyQ := RQ_AlgebraByCayleyTable( category, ct, style ); # always index based
    # REVISIT: inherit some properties?
    return copyQ;
end );

# RightQuasigroupIsotope
# QuasigroupIsotope
# LoopIsotope

InstallMethod( RightQuasigroupIsotope, "for right quasigroup and tree permutations",
    [ IsRightQuasigroup, IsPerm, IsPerm, IsPerm ],
function( Q, f, g, h )
    local style;
    style := rec( indexBased := IsIndexBased( Q ), checkArguments := RQ_defaultConstructorStyle.checkArguments );
    return RQ_AlgebraIsotopeByPerms( IsRightQuasigroup, Q, f, g, h, style );
end );

InstallOtherMethod( RightQuasigroupIsotope, "for right quasigroup, tree permutations and record",
    [ IsRightQuasigroup, IsPerm, IsPerm, IsPerm, IsRecord ],
function( Q, f, g, h, style )
    return RQ_AlgebraIsotopeByPerms( IsRightQuasigroup, Q, f, g, h, style );
end );

InstallMethod( QuasigroupIsotope, "for right quasigroup and tree permutations",
    [ IsQuasigroup, IsPerm, IsPerm, IsPerm ], # not a mistake in the 1st filder; see documentation
function( Q, f, g, h )
    local style;
    style := rec( indexBased := IsIndexBased( Q ), checkArguments := RQ_defaultConstructorStyle.checkArguments );
    return RQ_AlgebraIsotopeByPerms( IsQuasigroup, Q, f, g, h, style );
end );

InstallOtherMethod( QuasigroupIsotope, "for right quasigroup, tree permutations and record",
    [ IsQuasigroup, IsPerm, IsPerm, IsPerm, IsRecord ],
function( Q, f, g, h, style )
    return RQ_AlgebraIsotopeByPerms( IsQuasigroup, Q, f, g, h, style );
end );

InstallMethod( LoopIsotope, "for right quasigroup and tree permutations",
    [ IsQuasigroup, IsPerm, IsPerm, IsPerm ],
function( Q, f, g, h )
    local style;
    style := rec( indexBased := IsIndexBased( Q ), checkArguments := RQ_defaultConstructorStyle.checkArguments );
    return RQ_AlgebraIsotopeByPerms( IsLoop, Q, f, g, h, style );
end );

InstallOtherMethod( LoopIsotope, "for right quasigroup, tree permutations and record",
    [ IsQuasigroup, IsPerm, IsPerm, IsPerm, IsRecord ],
function( Q, f, g, h, style )
    return RQ_AlgebraIsotopeByPerms( IsLoop, Q, f, g, h, style );
end );

# PrincipalLoopIsotope

InstallMethod( PrincipalLoopIsotope, "for quasigroup and two quasigroup elements",
    [ IsQuasigroup, IsQuasigroupElement, IsQuasigroupElement ],
function( Q, a, b )
    local n, f, g;
    if not (a in Q and b in Q) then
        Error("RQ: <2> and <3> must be elements of quasigroup <1>.");
    fi;
    # PROG: cannot use RightTranslation( Q, a ) since it is a permutation on [1..Parent(Q)] and we need to operate on [1..n]
    f := AsCanonicalPerm( Q, RightTranslation( Q, a ) );
    g := AsCanonicalPerm( Q, LeftTranslation( Q, b ) );
    return RQ_AlgebraIsotopeByPerms( IsLoop, Q, f, g, (), rec( indexBased := IsIndexBased( Q ), checkArguments := false ) );
end );  

# AFFINE RIGHT QUASIGROUPS
# _____________________________________________________________________________

# RQ_IsAffineAlgebraArithmeticForm
InstallGlobalFunction( RQ_IsAffineAlgebraArithmeticForm,
function( category, Q, f, g, c, reportErrors )
    if not ( IsGroup( Q ) or IsAdditiveGroup( Q ) or IsLoop( Q ) or IsPosInt( Q ) ) then
        return RQ_OptionalError( reportErrors, "RQ: <1> must be a group, additive group, loop or a positive integer." );
    fi;
    # the case IsPosInt( Q )
    if IsPosInt( Q ) then
        if not ( IsInt( f ) and Gcd( Q, f ) = 1 ) then
            return RQ_OptionalError( reportErrors, "RQ: <2> must be relatively prime to <1>." );
        fi;
        if not ( IsInt( g ) and IsInt( c ) ) then
            return RQ_OptionalError( reportErrors, "RQ: <3> and <4> must be integers." );
        fi;
        if category = IsQuasigroup and Gcd( Q, g ) <> 1 then    
            return RQ_OptionalError( reportErrors, "RQ: <3> must be relatively prime to <1>." );
        fi;
        return true;
    fi;
    # the case IsLoop( Q )
    if IsLoop( Q ) then 
        if not ( IsLoopAutomorphism( f ) and IsLoopEndomorphism( g ) and Source( f ) = Q and Source( g ) = Q ) then
            return RQ_OptionalError( reportErrors, "RQ: <2> and <3> must be loop endomorphisms on <1>, and <2> must be bijective." );
        fi;       
    fi;
    # the case IsGroup( Q )
    if IsGroup( Q ) then 
        if not ( IsGroupHomomorphism( f ) and IsGroupHomomorphism( g ) and Source( f ) = Q
                    and Source( g ) = Q and Range( f ) = Q and Range( g ) = Q and IsBijective( f ) ) then
            return RQ_OptionalError( reportErrors, "RQ: <2> and <3> must be group endomorphisms on <1>, and <2> must be bijective." );
        fi;
    fi;
    # the case IsAdditiveGroup( Q )
    if IsAdditiveGroup( Q ) then 
        if not ( IsAdditiveGroupHomomorphism( f ) and IsAdditiveGroupHomomorphism( g ) and Source( f ) = Q
                    and Source( g ) = Q and Range( f ) = Q and Range( g ) = Q and IsBijective( f ) ) then
            return RQ_OptionalError( reportErrors, "RQ: <2> and <3> must be additive group endomorphisms on <1>, and <2> must be bijective." );
        fi;
    fi;
    # finishing all three cases IsLoop( Q ), IsGroup( Q ) and IsAdditiveGroup( Q )
    if not c in Center( Q ) then
        return RQ_OptionalError( reportErrors, "RQ: <4> must be a central element of <1>." );
    fi;
    if category = IsQuasigroup and not IsBijective( g ) then
        return RQ_OptionalError( reportErrors, "RQ: <3> must be bijecive." );
    fi;
    return true;
end );

# IsAffineRightQuasigroupArithmeticForm
InstallGlobalFunction( IsAffineRightQuasigroupArithmeticForm,
function( Q, f, g, c )
    return RQ_IsAffineAlgebraArithmeticForm( IsRightQuasigroup, Q, f, g, c );
end );

# IsAffineQuasigroupAlgebraArithmeticForm
InstallGlobalFunction( IsAffineQuasigroupArithmeticForm,
function( Q, f, g, c )
    return RQ_IsAffineAlgebraArithmeticForm( IsQuasigroup, Q, f, g, c );
end );

# RQ_AffineAlgebra
InstallGlobalFunction( RQ_AffineAlgebra,
function( category, Q, f, g, c, style )
    local mult;
    if style.checkArguments then
        RQ_IsAffineAlgebraArithmeticForm( category, Q, f, g, c, true ); # report errors
    fi;
    if IsPosInt( Q ) then
        mult := function( x, y ) return (f*x + g*y + c) mod Q; end;
        Q := [1..Q]; 
    elif IsAdditiveGroup( Q ) then
        mult := function( x, y ) return x^f + y^g + c; end;
    else # IsLoop( Q ) or IsGroup( Q )
        mult := function( x, y ) return x^f*y^g*c; end; # c is central
    fi;
    return RQ_AlgebraByFunction( category, Q, mult, fail, fail, fail, ConstructorStyle( style.indexBased, false ) );
    # REVISIT: calculate divisions
end );

# AffineRightQuasigroup
InstallGlobalFunction( AffineRightQuasigroup,
function( arg ) # expecting Q, f, g, c[, style]
    if not Length( arg ) in [4,5] then
        Error( "RQ: Arguments must be Q, f, g, c[, style]." );
    fi;
    if not IsBound( arg[5] ) then arg[5] := RQ_defaultConstructorStyle; fi;
    return RQ_AffineAlgebra( IsRightQuasigroup, arg[1], arg[2], arg[3], arg[4], arg[5] );
end );

# AffineQuasigroup
InstallGlobalFunction( AffineQuasigroup,
function( arg ) # expecting Q, f, g, c[, style]
    if not Length( arg ) in [4,5] then
        Error( "RQ: Arguments must be Q, f, g, c[, style]." );
    fi;
    if not IsBound( arg[5] ) then arg[5] := RQ_defaultConstructorStyle; fi;
    return RQ_AffineAlgebra( IsQuasigroup, arg[1], arg[2], arg[3], arg[4], arg[5] );
end );