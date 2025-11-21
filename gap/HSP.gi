
# HSP.gi
# Direct products, subalgebras and factor algebras
# =============================================================================

# DIRECT PRODUCT OF RIGHT QUASIGROUPS
# _____________________________________________________________________________

# RQ_DirectProduct
# PROG: constructor OK, builds function from elements and calls RQ_AlgebraByFunction
InstallGlobalFunction( RQ_DirectProduct,
function( list )
    local S, mult, rdiv, ldiv, rq_list, indexBased, category, prod;
    # assumes that list is nonempty and consists of groups and right quasigroups, with at least one non-group
    if Length( list ) = 1 then 
        return list[ 1 ];
    fi;
    # at least two algebras
    # underlying set
    S := Cartesian( list );
    # multiplication and right division function
    mult := function( x, y ) return List( [1..Length(x)], i -> x[i]*y[i] ); end;
    rdiv := function( x, y ) return List( [1..Length(x)], i -> x[i]/y[i] ); end;
    # selecting category and whether it is index based
    rq_list := Filtered( list, IsRightQuasigroup ); # list of right quasigroups
    indexBased := ForAll( rq_list, IsIndexBased );
    category := CategoryOfRightQuasigroup( rq_list );
    if category in [ IsQuasigroup, IsLoop ] then
        ldiv := function( x, y ) return List( [1..Length(x)], i -> LeftQuotient( x[i]/y[i] ) ); end;
    else
        ldiv := fail;
    fi;
    prod :=  RQ_AlgebraByFunction( category, S, mult, [ rdiv, ldiv, ConstructorStyle( indexBased, false ) ] );
    # REVISIT: set One for loops?
    RQ_InheritProperties( list, prod, false );
    return prod;
end );

# DirectProductOp( list, first )
# The following is necessary due to implementation of DirectProduct for groups in GAP.
# We want to calculate direct product of right quasigroups, quasigroups, loops and groups.
# If only groups are on the list, standard GAP DirectProduct will take care of it.
# If there are also some right quasigroups on the list, we must take care of it.
# However, we do not know if such a list will be processed with
# DirectProductOp( <IsList>, <IsGroup> ), or
# DirectProductOp( <IsList>, <IsRightQuasigroup> ),
# since this depends on which algebra is listed first. We therefore take care of both situations.

InstallOtherMethod( DirectProductOp, "for DirectProduct( <IsList>, <IsGroup> )",
    [ IsList, IsGroup],
function( list, first )
    # check the arguments
    if IsEmpty( list ) then Error( "RQ: <1> must be nonempty." ); fi;
    if not ForAny( list, IsRightQuasigroup ) then
        # there are no right quasigroups on the list
        TryNextMethod();
    fi;
    if ForAny( list, Q -> (not IsGroup( Q )) and (not IsRightQuasigroup( Q ) ) ) then
        # there are other objects beside groups and right quasigroups on the list
        TryNextMethod();
    fi;
    return RQ_DirectProduct( list );
end);

InstallOtherMethod( DirectProductOp, "for DirectProduct( <IsList>, <IsRightQuasigroup> )",
    [ IsList, IsRightQuasigroup ],
function( list, first )
    # check the arguments
    if IsEmpty( list ) then Error( "RQ: <1> must be nonempty." ); fi;
    if ForAny( list, Q -> (not IsGroup( Q )) and (not IsRightQuasigroup( Q ) ) ) then
        TryNextMethod();
    fi;
    return RQ_DirectProduct( list );
end );

# OPPOSITE QUASIGROUPS
# _____________________________________________________________________________

# RQ_OppositeAlgebra
# PROG: constructor OK
InstallGlobalFunction( RQ_OppositeAlgebra, 
function( category, Q )
    local old_mult, mult, oppQ;
    if IsIndexBased( Q ) then
        oppQ := RQ_AlgebraByCayleyTable( category, TransposedMat( CayleyTable( Q ) ), ConstructorStyle( true, false ) );
    else # not index based
        old_mult := MultiplicationFunction( Q );
        mult := function( x, y )
            return old_mult( y, x );
        end;
        oppQ := RQ_AlgebraByFunction( category, UnderlyingSet( Q ), mult, [ ConstructorStyle( false, false ) ] );
    fi;
    # inherit dual properties from Q
    RQ_InheritProperties( Q, oppQ, true );
    return oppQ;
end );

# OppositeQuasigroup
InstallMethod( OppositeQuasigroup, "for quasigroup",
    [ IsQuasigroup ],
    Q -> RQ_OppositeAlgebra( IsQuasigroup, Q )
);

# OppositeLoop
InstallMethod( OppositeLoop, "for loop",
    [ IsLoop ],
    Q -> RQ_OppositeAlgebra( IsLoop, Q )
);

# SUBALGEBRAS
# _____________________________________________________________________________

# IsSubrightquasigroup
# IsSubquasigroup
# IsSubloop

InstallMethod( IsSubrightquasigroup, "for two right quasigroups",
    [ IsRightQuasigroup, IsRightQuasigroup ],
function( Q, S )
    return Parent( Q ) = Parent( S ) and IsSubset( Elements( Q ), Elements( S ) );
end );

InstallMethod( IsSubquasigroup, "for two quasigroups",
    [ IsQuasigroup, IsQuasigroup ],
function( Q, S )
    return IsSubrightquasigroup( Q, S );
end );

InstallMethod( IsSubloop, "for two loops",
    [ IsLoop, IsLoop ],
function( Q, S )
    return IsSubrightquasigroup( Q, S );
end );

# RQ_InheritProperties
InstallGlobalFunction( RQ_InheritProperties,
function( P, Q, isDual )
    # PROG: Add more as new properties are implemented.
    local Ps, i, A, category;
    if IsList( P ) then 
        Ps := P;
    else 
        Ps := [ P ];
    fi;
    # PROG: replace groups with associative loops (possibly commutative).
    # That way implied properties can be detected.
    # This is safe only if no inherited property depends on the order and such.
    for i in [1..Length(Ps)] do
        A := Ps[i];
        if IsGroup( A ) then
            Ps[i] := LoopByCayleyTable([[1]]);
            SetIsAssociative(Ps[i], true );
            if HasIsCommutative( Ps[i] ) and IsCommutative( Ps[i] ) then
                SetIsCommutative( Ps[i], true );
            fi;
        fi;
    od;
    # inherit properties
    # UPDATE POINT: as more properties are added, update this list
    category := CategoryOfRightQuasigroup( Q );
    if ForAll( Ps, HasIsAssociative ) and ForAll( Ps, IsAssociative ) then SetIsAssociative( Q, true ); fi; # self-dual
    if ForAll( Ps, HasIsCommutative ) and ForAll( Ps, IsCommutative ) then SetIsCommutative( Q, true ); fi; # self-dual
    if ForAll( Ps, HasIsUnipotent ) and ForAll( Ps, IsUnipotent ) then SetIsUnipotent( Q, true ); fi; # self-dual
    if ForAll( Ps, HasIsIdempotent ) and ForAll( Ps, IsIdempotent ) then SetIsIdempotent( Q, true ); fi; # self-dual
    if ForAll( Ps, HasIsRightSelfDistributive ) and ForAll( Ps, IsRightSelfDistributive ) then 
        if not isDual then SetIsRightSelfDistributive( Q, true ); else SetIsLeftSelfDistributive( Q, true ); fi;
    fi;
    if ForAll( Ps, HasIsLeftSelfDistributive ) and ForAll( Ps, IsLeftSelfDistributive ) then 
        if not isDual then SetIsLeftSelfDistributive( Q, true ); else SetIsRightSelfDistributive( Q, true ); fi;
    fi;
    if ForAll( Ps, HasIsSelfDistributive ) and ForAll( Ps, IsSelfDistributive ) then SetIsSelfDistributive( Q, true ); fi; # self-dual
    if ForAll( Ps, HasIsFlexible ) and ForAll( Ps, IsFlexible ) then SetIsFlexible( Q, true ); fi; # self-dual
    if ForAll( Ps, HasIsRightAlternative ) and ForAll( Ps, IsRightAlternative ) then
        if not isDual then SetIsRightAlternative( Q, true ); else SetIsLeftAlternative( Q, true ); fi;
    fi;
    if ForAll( Ps, HasIsLeftAlternative ) and ForAll( Ps, IsLeftAlternative ) then
        if not isDual then SetIsLeftAlternative( Q, true ); else SetIsRightAlternative( Q, true ); fi;
    fi;
    if ForAll( Ps, HasIsAlternative ) and ForAll( Ps, IsAlternative ) then SetIsAlternative( Q, true ); fi; # self-dual
    if ForAll( Ps, HasIsRack ) and ForAll( Ps, IsRack ) and not isDual then SetIsRack( Q, true ); fi; # no inheritance for duals
    if ForAll( Ps, HasIsQuandle ) and ForAll( Ps, IsQuandle ) and not isDual then SetIsQuandle( Q, true ); fi; # no inheritance for duals
    if ForAll( Ps, HasIsLeftQuasigroupMagma ) and ForAll( Ps, IsLeftQuasigroupMagma ) then
        if not isDual then SetIsLeftQuasigroupMagma( Q, true ); else SetIsRightQuasigroupMagma( Q, true ); fi;
    fi;
    if ForAll( Ps, HasIsLatinRack ) and ForAll( Ps, IsLatinRack ) and not isDual then SetIsLatinRack( Q, true ); fi; # no inheritance for duals
    if ForAll( Ps, HasIsLatinQuandle ) and ForAll( Ps, IsLatinQuandle )and not isDual then SetIsLatinQuandle( Q, true ); fi; # no inheritance for duals

    if category = IsRightQuasigroup then return true; fi; 
    
    # category is now IsQuasigroup or IsLoop
    if ForAll( Ps, HasIsPowerAssociative ) and ForAll( Ps, IsPowerAssociative ) then SetIsPowerAssociative( Q, true ); fi; # self-dual
    if ForAll( Ps, HasIsDiassociative ) and ForAll( Ps, IsDiassociative ) then SetIsDiassociative( Q, true ); fi; # self-dual
    if ForAll( Ps, HasIsSemisymmetric ) and ForAll( Ps, IsSemisymmetric ) then SetIsSemisymmetric( Q, true ); fi; # self-dual
    if ForAll( Ps, HasIsTotallySymmetric ) and ForAll( Ps, IsTotallySymmetric ) then SetIsTotallySymmetric( Q, true ); fi; # self-dual
    if ForAll( Ps, HasIsEntropic ) and ForAll( Ps, IsEntropic ) then SetIsEntropic( Q, true ); fi; # self-dual
    if ForAll( Ps, HasIsSteinerQuasigroup ) and ForAll( Ps, IsSteinerQuasigroup ) then SetIsSteinerQuasigroup( Q, true ); fi; # self-dual
    
    if category = IsQuasigroup then return true; fi;

    # category is now IsLoop

    if ForAll( Ps, HasHasRightInverseProperty ) and ForAll( Ps, HasRightInverseProperty ) then
        if not isDual then SetHasRightInverseProperty( Q, true ); else SetHasLeftInverseProperty( Q, true ); fi;
    fi;
    if ForAll( Ps, HasHasLeftInverseProperty ) and ForAll( Ps, HasLeftInverseProperty ) then
        if not isDual then SetHasLeftInverseProperty( Q, true ); else SetHasRightInverseProperty( Q, true ); fi; 
    fi;
    if ForAll( Ps, HasHasInverseProperty ) and ForAll( Ps, HasInverseProperty ) then SetHasInverseProperty( Q, true ); fi; # self-dual
    if ForAll( Ps, HasHasWeakInverseProperty ) and ForAll( Ps, HasWeakInverseProperty ) then SetHasWeakInverseProperty( Q, true ); fi; # self-dual
    if ForAll( Ps, HasHasTwosidedInverses ) and ForAll( Ps, HasTwosidedInverses ) then SetHasTwosidedInverses( Q, true ); fi; # self-dual
    if ForAll( Ps, HasHasAutomorphicInverseProperty ) and ForAll( Ps, HasAutomorphicInverseProperty ) then SetHasAutomorphicInverseProperty( Q, true ); fi; # self-dual
    if ForAll( Ps, HasHasAntiautomorphicInverseProperty ) and ForAll( Ps, HasAntiautomorphicInverseProperty ) then SetHasAntiautomorphicInverseProperty( Q, true ); fi; # self-dual
    if ForAll( Ps, HasIsExtraLoop ) and ForAll( Ps, IsExtraLoop ) then SetIsExtraLoop( Q, true ); fi; # self-dual
    if ForAll( Ps, HasIsMoufangLoop ) and ForAll( Ps, IsMoufangLoop ) then SetIsMoufangLoop( Q, true ); fi; # self-dual
    if ForAll( Ps, HasIsCLoop ) and ForAll( Ps, IsCLoop ) then SetIsCLoop( Q, true ); fi; # self-dual
    if ForAll( Ps, HasIsRightBolLoop ) and ForAll( Ps, IsRightBolLoop ) then
        if not isDual then SetIsRightBolLoop( Q, true ); else SetIsLeftBolLoop( Q, true ); fi;
    fi;
    if ForAll( Ps, HasIsLeftBolLoop ) and ForAll( Ps, IsLeftBolLoop ) then
        if not isDual then SetIsLeftBolLoop( Q, true ); else SetIsRightBolLoop( Q, true ); fi;
    fi;
    if ForAll( Ps, HasIsRCLoop ) and ForAll( Ps, IsRCLoop ) then
        if not isDual then SetIsRCLoop( Q, true ); else SetIsLCLoop( Q, true ); fi;
    fi;
    if ForAll( Ps, HasIsLCLoop ) and ForAll( Ps, IsLCLoop ) then
        if not isDual then SetIsLCLoop( Q, true ); else SetIsRCLoop( Q, true ); fi;
    fi;
    if ForAll( Ps, HasIsRightNuclearSquareLoop ) and ForAll( Ps, IsRightNuclearSquareLoop ) then
        if not isDual then SetIsRightNuclearSquareLoop( Q, true ); else SetIsLeftNuclearSquareLoop( Q, true ); fi;
    fi;
    if ForAll( Ps, HasIsLeftNuclearSquareLoop ) and ForAll( Ps, IsLeftNuclearSquareLoop ) then
        if not isDual then SetIsLeftNuclearSquareLoop( Q, true ); else SetIsRightNuclearSquareLoop( Q, true ); fi;
    fi;
    if ForAll( Ps, HasIsMiddleNuclearSquareLoop ) and ForAll( Ps, IsMiddleNuclearSquareLoop ) then SetIsMiddleNuclearSquareLoop( Q, true ); fi; # self-dual
    if ForAll( Ps, HasIsNuclearSquareLoop ) and ForAll( Ps, IsNuclearSquareLoop ) then SetIsNuclearSquareLoop( Q, true ); fi; # self-dual
    if ForAll( Ps, HasIsLeftPowerAlternative ) and ForAll( Ps, IsLeftPowerAlternative ) then
        if not isDual then SetIsLeftPowerAlternative( Q, true ); else SetIsRightPowerAlternative( Q, true ); fi;
    fi;
    if ForAll( Ps, HasIsRightPowerAlternative ) and ForAll( Ps, IsRightPowerAlternative ) then
        if not isDual then SetIsRightPowerAlternative( Q, true ); else SetIsLeftPowerAlternative( Q, true ); fi;
    fi;
    if ForAll( Ps, HasIsPowerAlternative ) and ForAll( Ps, IsPowerAlternative ) then SetIsPowerAlternative( Q, true ); fi; #self-dual
    if ForAll( Ps, HasIsRCCLoop ) and ForAll( Ps, IsRCCLoop ) then
        if not isDual then SetIsRCCLoop( Q, true ); else SetIsLCCLoop( Q, true ); fi;
    fi;
    if ForAll( Ps, HasIsLCCLoop ) and ForAll( Ps, IsLCCLoop ) then
        if not isDual then SetIsLCCLoop( Q, true ); else SetIsRCCLoop( Q, true ); fi;
    fi;
    if ForAll( Ps, HasIsCCLoop ) and ForAll( Ps, IsCCLoop ) then SetIsCCLoop( Q, true ); fi; # self-dual
    if ForAll( Ps, HasIsOsbornLoop ) and ForAll( Ps, IsOsbornLoop ) then SetIsOsbornLoop( Q, true ); fi; # self-dual
    if ForAll( Ps, HasIsSteinerLoop ) and ForAll( Ps, IsSteinerLoop ) then SetIsSteinerLoop( Q, true ); fi; # self-dual
    if ForAll( Ps, HasIsRightBruckLoop ) and ForAll( Ps, IsRightBruckLoop ) then
        if not isDual then SetIsRightBruckLoop( Q, true ); else SetIsLeftBruckLoop( Q, true ); fi;
    fi;
    if ForAll( Ps, HasIsLeftBruckLoop ) and ForAll( Ps, IsLeftBruckLoop ) then
        if not isDual then SetIsLeftBruckLoop( Q, true ); else SetIsRightBruckLoop( Q, true ); fi;
    fi;
    if ForAll( Ps, HasIsRightALoop ) and ForAll( Ps, IsRightALoop ) then
        if not isDual then SetIsRightALoop( Q, true ); else SetIsLeftALoop( Q, true ); fi;
    fi;
    if ForAll( Ps, HasIsMiddleALoop ) and ForAll( Ps, IsMiddleALoop ) then SetIsMiddleALoop( Q, true ); fi; # self-dual
    if ForAll( Ps, HasIsLeftALoop ) and ForAll( Ps, IsLeftALoop ) then
        if not isDual then SetIsLeftALoop( Q, true ); else SetIsRightALoop( Q, true ); fi;
    fi;
    if ForAll( Ps, HasIsALoop ) and ForAll( Ps, IsALoop ) then SetIsALoop( Q, true ); fi; # self-dual       
end );

# RQ_Subalgebra
# PROG: constructor OK, the operations reside in the parent
InstallGlobalFunction( RQ_Subalgebra,
function( Q, gens ) 
    local category, initial_gens, x, elms, transl, relmultgr, Qtype, subqg, P;
    category := CategoryOfRightQuasigroup( Q );
    P := Parent( Q );
    gens := Set( gens, function(x) if x in Q then return x; else return Q[x]; fi; end );
    initial_gens := ShallowCopy( gens );
    if category = IsLoop then  # add identity element into generator set
        AddSet( gens, One( Q ) );
    fi;
    if IsEmpty( gens ) then # return empty set
        return [];
    fi;
    # generate the subalgebra
    if not IsIndexBased( Q ) then
        # REVISIT: Multiplicative closure implemented in GAP. How efficient is it? See alternative code for MutiplicativeClosure in a text file.
        # MATH: If a subset of a finite right quasigroup is closed under multiplication, it is also closed under right division.
        elms := Elements( Magma( gens ) ); 
    else # index based; let's work with permutation groups 
        gens := ParentInd( gens );
        elms := [];
        while gens<>elms do
            elms := ShallowCopy( gens );
            if category = IsRightQuasigroup then
                transl := RightSection( P ){ gens };
                # relmultgr := Subgroup( RightMultiplicationGroup( P ), transl ); # slow                  
            else 
                transl := Concatenation( RightSection( P ){ gens }, LeftSection( P ){ gens } );
                # relmultgr := Subgroup( MultiplicationGroup( P ), transl ); # slow
            fi;          
            relmultgr := Group( transl ); # might have a lot of generators
            gens := Union( Orbits( relmultgr, gens ) );
        od;
        elms := Immutable( List( elms, i -> P.(i) ) );
    fi;
    # create the subrightquasigroup 
    Qtype := NewType( FamilyObj( elms ), category and IsAttributeStoringRep );
    subqg := Objectify( Qtype, rec( ) );
    SetSize( subqg, Length( elms ) );
    SetAsSSortedList( subqg, elms );
    SetParent( subqg, P );
    SetGeneratorsOfMagma( subqg, initial_gens );
    RQ_InheritProperties( Q, subqg, false ); # subqg inherits certain properties from Q
    return subqg;
end );

# Subrightquasigroup
# Subquasigroup
# Subloop

InstallMethod( Subrightquasigroup, "for right quasigroup and collection of elements",
    [IsRightQuasigroup, IsCollection ],
function( Q, gens )
    return RQ_Subalgebra( Q, gens );
end );

InstallMethod( Subquasigroup, "for quasigroup and collection of elements",
    [IsQuasigroup, IsCollection ],
function( Q, gens )
    return RQ_Subalgebra( Q, gens );
end );

InstallMethod( Subloop, "for loop and collection of elements",
    [IsLoop, IsList ], # PROG: we use IsList to allow empty set as argument
function( Q, gens )
    return RQ_Subalgebra( Q, gens );
end );

# ALL SUBALGEBRAS
# _____________________________________________________________________________

# AllSubrightquasigroups
InstallMethod( AllSubrightquasigroups, "for right quasigroup",
    [ IsRightQuasigroup ],
function( Q )
    # MATH: Let S be a subquasigroup of a right quasigroup Q. Then:
    # S is among right cosests (but not necessarily among left cosets).
    # In the quasigroup case, is x is in Q\S, then the cosets S and xS are disjoint.
    # In the quasigroup case, if S is proper then |Q| is at least 2|S|.
    # In the quasigroup case, if S is proper and |S| > |Q|/4 then S is maximal in Q.
    # If S is a subloop of a power-associative loop Q and x is in Q\S then <S,x> = <S,x^m> whenever m is relatively prime to |x|.
    # If S is a subloop of a LIP loop Q and x is in Q\S then <S,zx> = <S,x> for every z in S.
    # If S is a subloop of a LIP power associative loop Q and x is in Q\S then <S,z(x^m)> = <S,x> for every z in S and every m relatively prime to |x|.

    local category, allSubs, lastSubs, newSubs, A, out, B, x, coprime_powers, n, m;

    category := CategoryOfRightQuasigroup( Q );
    # initialization
    allSubs := [];   #all subalgebras
    if category = IsLoop then
        newSubs := [ Subloop( Q, [ One( Q ) ] ) ]; # the trivial subloop
    else
        newSubs := Set( Elements( Q ), x -> RQ_Subalgebra( Q, [ x ] ) ); # all mono-generated subrightquasigroups 
    fi;

    # rounds
    repeat
        Append( allSubs, newSubs );
        if category <> IsRightQuasigroup then 
            lastSubs := Filtered( newSubs, A -> Size( A ) <= Size( Q )/4 ); # subalgebras found in the previous round that are possibly not maximal in Q
        else
            lastSubs := Filtered( newSubs, A -> Size( A ) < Size( Q ) ); 
        fi;
        newSubs := [];   # subalgebras generated in this round
        for A in lastSubs do
            out := Difference( Elements( Q ), Elements( A ) );
            while not IsEmpty(out) do
                x := out[ 1 ];
                out := out{[2..Length(out)]};
                B := RQ_Subalgebra( Q, Union( Elements( A ), [ x ] ) );
                if not B in newSubs and not B in allSubs then
                    Add( newSubs, B );   # new subalgebra found
                fi;
                # attempting to reduce the number of elements to be checked; this is critical for speed.
                if category <> IsRightQuasigroup and Size( B ) < 4*Size( A ) then # A is maximal in B, removing everything in B
                    out := Difference( out, Elements( B ) );
                elif category = IsLoop then # additional removal methods for loops in the non-maximal case
                    coprime_powers := [ 1 ];
                    if IsPowerAssociative( Q ) then
                        n := Order( x );
                        coprime_powers := Filtered( [1..n], m -> Gcd(m,n) = 1 );
                    fi;
                    out := Difference( out, List( coprime_powers, m -> x^m ) );
                    if HasLeftInverseProperty( Q ) then
                        for m in coprime_powers do
                            out := Difference( out, Elements(A)*(x^m) );
                        od;
                    fi;
                    if HasRightInverseProperty( Q ) then
                        for m in coprime_powers do
                            out := Difference( out, (x^m)*Elements(A) );
                        od;
                    fi;
                fi; # end of removal
            od; # end of cycle for x
        od; # end of cycle for A
    until IsEmpty( newSubs );

    # finishing
    if not Q in allSubs then Add( allSubs, Q ); fi;
    return allSubs;

end );

# AllSubquasigroups
InstallMethod( AllSubquasigroups, "for quasigroup",
    [ IsQuasigroup ],
    Q -> AllSubrightquasigroups( Q )
);

# AllSubloops
InstallMethod( AllSubloops, "for loop",
    [ IsLoop ],
    Q -> AllSubrightquasigroups( Q )
);

# MINIMAL SUBALGEBRAS
# _____________________________________________________________________________

# IsMinimalSubrightquasigroup
InstallMethod( IsMinimalSubrightquasigroup, "for right quasigroup",
    [ IsRightQuasigroup ],
function( S )
    local n;
    if CategoryOfRightQuasigroup( S ) <> IsLoop then
        return ForAll( S, x -> RQ_Subalgebra( S, [x] ) = S );
    fi;
    # loop, must also be nontrivial
    n := Size( S );
    return n > 1 and ForAll( S, x -> Size( Subloop( S, [x] ) ) in [1,n] );
end );

InstallOtherMethod( IsMinimalSubrightquasigroup, "for two right quasigroups",
    [ IsRightQuasigroup, IsRightQuasigroup ],
function( Q, S )
    return IsSubrightquasigroup( Q, S ) and IsMinimalSubrightquasigroup( S );
end ); 

# IsMinimalSubquasigroup
InstallMethod( IsMinimalSubquasigroup, "for quasigroup",
    [ IsQuasigroup ],
    S -> IsMinimalSubrightquasigroup( S )
); 

InstallOtherMethod( IsMinimalSubquasigroup, "for two quasigroups",
    [ IsQuasigroup, IsQuasigroup ],
function( Q, S )
    return IsMinimalSubrightquasigroup( Q, S );
end );

# IsMinimalSubloop
InstallMethod( IsMinimalSubloop, "for loop",
    [ IsLoop ],
    S -> IsMinimalSubrightquasigroup( S )
);

InstallOtherMethod( IsMinimalSubloop, "for two loops",
    [ IsLoop, IsLoop ],
function( Q, S )
    return IsMinimalSubrightquasigroup( Q, S );
end );

# AllMinimalSubrightquasigroups
InstallMethod( AllMinimalSubrightquasigroups, "for right quasigroup",
    [ IsRightQuasigroup ],
function( Q )
    return Filtered( Set( Q, x -> RQ_Subalgebra( Q, [x] ) ), IsMinimalSubrightquasigroup );
end );

# AllMinimalSubquasigroups
InstallMethod( AllMinimalSubquasigroups, "for quasigroup",
    [ IsQuasigroup ],
    Q -> AllMinimalSubrightquasigroups( Q )
);

# AllMinimalSubloops
InstallMethod( AllMinimalSubloops, "for loop",
    [ IsLoop ],
    Q -> AllMinimalSubrightquasigroups( Q )
);

# MAXIMAL SUBALGEBRAS
# _____________________________________________________________________________

# IsMaximalSubrightquasigroup
InstallMethod( IsMaximalSubrightquasigroup, "for right quasigroup and its subrightquasigroup",
    [ IsRightQuasigroup, IsRightQuasigroup ],
function( Q, S )
    local cosets, C, x, A;
    if not IsSubrightquasigroup( Q, S ) then
        Error( "RQ: <2> must be a subrightquasigroup of <1>." );
    fi;
    if S=Q then # not a proper subalgebra
        return false;
    fi;
    # MATH: If S is a subrightquasigroup of Q and x is an element of Q then <S,x> = <S,x*s> for every s in S. 
    # In case of quasigroups we also have <S,x> = <S,s*x> for every s in S, and we prefer
    # right cosets here since they have more support in GAP. 
    if CategoryOfRightQuasigroup( Q ) = IsRightQuasigroup then  
        cosets := LeftCosets( Q, S );
    else
        cosets := RightCosets( Q, S );
    fi;
    # testing <S,x>, one element x per coset
    for C in cosets do
        x := C[1];
        if not x in S then  
            A := Subrightquasigroup( Q, Concatenation( Elements( S ), [x] ) );
            if not A=Q then # S<A<Q
                return false;
            fi;
        fi;
    od;
    return true;
end );

# IsMaximalSubquasigroup
InstallMethod( IsMaximalSubquasigroup, "for quasigroup",
    [ IsQuasigroup, IsQuasigroup ],
function( Q, S )
    return IsMaximalSubrightquasigroup( Q, S );
end );

# IsMaximalSubloop
InstallMethod( IsMaximalSubloop, "for loop",
    [ IsLoop, IsLoop ],
function( Q, S )
    return IsMaximalSubrightquasigroup( Q, S );
end );

# AllMaximalSubrightquasigroups
InstallMethod( AllMaximalSubrightquasigroups, "for right quasigroup",
    [ IsRightQuasigroup ],
function( Q )
    # REVISIT: Is there a better method than calculating all subalgebras first?
    local subs, max;
    subs := AllSubrightquasigroups( Q );
    subs := Filtered( subs, S -> Size(S)<Size(Q) ); # keep only proper subalgebras
    max := Filtered( subs, S -> ForAll( subs, T -> S=T or not IsSubrightquasigroup( T, S ) ) );
    return max;
end );

# AllMaximalSubquasigroups
InstallMethod( AllMaximalSubquasigroups, "for quasigroup",
    [ IsQuasigroup ],
    Q -> AllMaximalSubrightquasigroups( Q )
);

# AllMaximalSubloops
InstallMethod( AllMaximalSubloops, "for loop",
    [ IsLoop ],
    Q -> AllMaximalSubrightquasigroups( Q )
);

# COSETS AND TRANSVERSALS
# _____________________________________________________________________________

# RightCosets
# RightCosetsNC
InstallOtherMethod( RightCosetsNC, "for two right quasigroups",
    [ IsRightQuasigroup, IsRightQuasigroup ],
function( Q, S )
    local cosets, x;
    if not IsSubrightquasigroup( Q, S ) then
        Error( "RQ: <2> must be a subalgebra of <1>" );
    fi;
    if IsIndexBased( Q ) then
        cosets := Set( Orbit( RightMultiplicationGroup( Q ), Elements( S ), OnSets ) );
    else
        cosets := [];
        for x in Q do
            AddSet( cosets, Set( Elements(S)*x ) );
        od;
    fi;
    return cosets;
end );

# RightTransversal
InstallOtherMethod( RightTransversal, "for two right quasigroups",
    [ IsRightQuasigroup, IsRightQuasigroup ],
function( Q, S )
    return List( RightCosetsNC( Q, S ), x -> x[1] );
end );

# LeftCosetsNC
InstallOtherMethod( LeftCosetsNC, "for two right quasigroups",
    [ IsRightQuasigroup, IsRightQuasigroup ],
function( Q, S )
    local cosets, x;
    if not IsSubrightquasigroup( Q, S ) then
        Error( "RQ: <2> must be a subalgebra of <1>" );
    fi;
    if IsIndexBased( Q ) and CategoryOfRightQuasigroup( Q ) <> IsRightQuasigroup then  
            cosets := Set( Orbit( LeftMultiplicationGroup( Q ), Elements( S ), OnSets ) );
    else
        cosets := [];
        for x in Q do
            AddSet( cosets, Set( x*Elements(S) ) );
        od;
    fi;
    return cosets;
end );

# LeftTransversal
InstallMethod( LeftTransversal, "for two right quasigroups",
    [ IsRightQuasigroup, IsRightQuasigroup ],
function( Q, S )
    return List( LeftCosetsNC( Q, S ), x -> x[1] );
end );

# RIGHT QUASIGROUP BY GENERATORS
# _____________________________________________________________________________

# RQ_AlgebraByGenerators
# PROG: constructor OK, calls RQ_Subalgebra
InstallMethod( RQ_AlgebraByGenerators, "for category/property and list",
    [ IsOperation, IsList ],
function( category, gens )
    # PROG: category is only used to check for arguments, not to construct the algebra
    local F;
    if IsEmpty( gens ) then
        Error( "RQ: <1> must be a nonempty list of right quasigroup elements" );
    fi;
    if Length( gens ) = 1 and IsList( gens[ 1 ] ) then # generators given in a list, flatten
        gens := gens[ 1 ];
    fi;
    if not ForAll( gens, IsRightQuasigroupElement ) then
        Error( "RQ: <1> must consists of right quasigroup elements." );
    fi;
    F := FamilyObj( gens[1] );
    if not ForAll( gens, x -> x in F!.set ) then
        Error( "RQ: <1> must consists of elements in the same parent algebra") ;
    fi;
    if category in [ IsRack, IsQuandle ] then
        if not category( F!.parent ) then
            Error( "RQ: The generators do not lie in a rack/quandle. ");
        fi;
    elif not category = CategoryOfRightQuasigroup( F!.parent ) then
        Error( "RQ: The algebra must be of the same type as the parent of the generators." );
    fi;
    return RQ_Subalgebra( F!.parent, gens ); # this is one of the reasons for the existence of F!.parent
end );

# RightQuasigroup
# Quasigroup
# Loop

InstallGlobalFunction( RightQuasigroup,
function( gens... )
    # watch for how argument is passed down
    return RQ_AlgebraByGenerators( IsRightQuasigroup, gens );
end );

InstallGlobalFunction(Quasigroup,
function( gens... )
    return RQ_AlgebraByGenerators( IsQuasigroup, gens );
end );

InstallGlobalFunction( Loop,
function( gens... )
    return RQ_AlgebraByGenerators( IsLoop, gens ); 
end );

# RightQuasigroupByGenerators
# QuasigroupByGenerators
# LoopByGenerators

InstallGlobalFunction( RightQuasigroupByGenerators,
function( gens... )
    return RQ_AlgebraByGenerators( IsRightQuasigroup, gens ); 
end );

InstallGlobalFunction(QuasigroupByGenerators,
function( gens... )
    return RQ_AlgebraByGenerators( IsQuasigroup, gens );
end );

InstallGlobalFunction( LoopByGenerators,
function( gens... )
    return RQ_AlgebraByGenerators( IsLoop, gens ); 
end );

# RQ_AlgebraWithGenerators
InstallMethod( RQ_AlgebraWithGenerators, "for category/property and list",
    [ IsOperation, IsList ],
function( category, gens )
    local Q;
    Q := RQ_AlgebraByGenerators( category, gens );
    if Length( gens ) = 1 and IsList( gens[ 1 ] ) then # generators given in a list, flatten
        gens := gens[ 1 ];
    fi;
    SetGeneratorsOfMagma( Q, gens ); # this line is the purpose of "WithGenerators"
    return Q;
end );

# RightQuasigroupWithGenerators
# QuasigroupWithGenerators
# LoopWithGenerators

InstallGlobalFunction( RightQuasigroupWithGenerators,
function( gens... )
    return RQ_AlgebraWithGenerators( IsRightQuasigroup, gens );
end );

InstallGlobalFunction(QuasigroupWithGenerators,
function( gens... )
    return RQ_AlgebraWithGenerators( IsQuasigroup, gens );
end );

InstallGlobalFunction( LoopWithGenerators,
function( gens... )
    return RQ_AlgebraWithGenerators( IsLoop, gens ); 
end );

# INTERSECTIONS AND JOINS
# _____________________________________________________________________________

# PROG: constructor OK, calls RQ_Subalgebra

# Intersection2
InstallOtherMethod( Intersection2, "for two right quasigroups",
    [ IsRightQuasigroup, IsRightQuasigroup ],
function( A, B )
    local S;
    if not ( Parent( A ) = Parent( B ) ) then
        TryNextMethod();
    fi;
    S := Intersection( Elements( A ), Elements( B ) );
    return RQ_Subalgebra( Parent( A ), S );
end );

InstallGlobalFunction( Join,
function( arg... )
    local Q;
    if IsEmpty( arg ) then
        TryNextMethod();
    fi;
    if Length( arg ) = 1 then # arguments given in a list, flatten
        arg := arg[ 1 ];
    fi;
    if Length( arg ) = 1 then
        return arg[1];
    fi;
    # the list has at least two items
    Q := Join2( arg[1], arg[2] ); # mimicking the funcionality for Intersection and Union
    if Length( arg ) = 2 then
        return Q;
    fi;
    return Join( Concatenation( [ Q ], arg{[3..Length(arg)]} ) ); #recursive
end);

InstallMethod( Join2, "for two right quasigroups",
    [ IsRightQuasigroup, IsRightQuasigroup ],
function( A, B )
    local S;
    if not ( Parent( A ) = Parent( B ) ) then
        TryNextMethod();
    fi;
    S := Union( GeneratorsOfMagma( A ), GeneratorsOfMagma( B ) );
    return RQ_Subalgebra( Parent( A ), S );
end );

# CONGRUENCES
# _____________________________________________________________________________

InstallGlobalFunction( RQ_IsAlgebraCongruence,
function( category, C, reportErrors )
    # PROG: Works correctly only for finite algebras.
    local Q, AreEquiv, classes, ok;
    Q := Source( C );
    if not category( Q ) then   # note the usage of category as a filter
        return RQ_OptionalError( reportErrors, "RQ: Types of algebra and congruence do not agree." );
    fi;
    AreEquiv := function( x, y ) # returns true if x and y are equivalent modulo E
        return y in ImagesElm( C, x );
    end;
    # for a (right) quasigroup congruence on a finite (right) quasigroup, it is enough to check that
    # for all x and for all u~v we have x*u~x*v and u*x~v*x
    # we can certainly also assume that u is different from v and the tuple (u,v) is ordered
    classes := List( EquivalenceClasses( C ), c -> Elements( c ));
    ok := ForAll( Q, x -> ForAll( classes, c ->
        ForAll( Combinations(c,2), t -> AreEquiv(x*t[1],x*t[2]) and AreEquiv(t[1]*x,t[2]*x) )
    ) );
    if not ok then
        return RQ_OptionalError( reportErrors, "RQ: <1> is not a congruence." );
    fi;
    return true;
end );

# IsRightQuasigroupCongruence
# IsQuasigroupCongruence
# IsLoopCongruence

InstallMethod( IsRightQuasigroupCongruence, "for equivalence relation on right quasigroup",
    [ IsEquivalenceRelation ],
function( C ) 
    return RQ_IsAlgebraCongruence( IsRightQuasigroup, C, false );
end );

InstallMethod( IsQuasigroupCongruence, "for equivalence relation on quasigroup",
    [ IsEquivalenceRelation ],
function( C )
    return RQ_IsAlgebraCongruence( IsQuasigroup, C, false );
end );

InstallMethod( IsLoopCongruence, "for equivalence relation on loop",
    [ IsEquivalenceRelation ],
function( C )
    return RQ_IsAlgebraCongruence( IsLoop, C, false );
end );

# RQ_AlgebraCongruenceByPartition 
# this is the main method for generating congruences
# REVISIT: For loops we could take advantage of NormalClosure, which is faster

InstallMethod( RQ_AlgebraCongruenceByPartition, "for right quasigroup and partition",
    [ IsRightQuasigroup, IsList ],
function( Q, parts )
    local category, n, classes, partition, merge2Parts, mergeParts, done, G, indQ,
        f, x, y, old_partition, sizes, biggest, blocks, B;
    category := CategoryOfRightQuasigroup( Q );
    # checking arguments
    if not ForAll( parts, x -> IsList( x ) and IsSubset( Q, x ) ) then
        Error( "RQ: <2> must be a list of subsets of <1>.");
    fi;
    # convert all to indices 
    parts := List( parts, ParentInd );
    n := Size( Parent( Q ) );
    classes := EquivalenceClasses( EquivalenceRelationByPartition( Domain([1..n]), parts ) );
    partition := List( [1..n], i -> First( [1..n], j -> i in classes[j] ) ); # partition[i]=j means that i is in part j
    merge2Parts := function( x, y ) # merges the 2 parts of partition that contain x and y
        local a, b, i;
        if partition[x] = partition[y] then return true; fi; # nothing to merge
        a := Minimum(partition[x],partition[y]);
        b := Maximum(partition[x],partition[y]);
        for i in [1..n] do
            if partition[ i ] = b then
                partition[ i ] := a;
            elif partition[ i ] > b then # relabeling so that there are no gaps in indices
                partition[ i ] := partition[ i ] - 1;
            fi;
        od;
        return true;
    end;
    mergeParts := function( ls ) # merges parts of partition so that all elements of <ls> are in the same part
        local x, y;
        if IsEmpty( ls ) then return true; fi;
        x := ls[1];
        for y in ls{[2..Length(ls)]} do
            merge2Parts(x,y);
        od;
        return true;
    end;
    # prepare variables
    done := false;
    if category = IsRightQuasigroup then 
        G := RightMultiplicationGroup( Q );
    else
        G := MultiplicationGroup( Q );
    fi;
    indQ := ParentInd( Q );
    # main loop
    while not done do
        old_partition := ShallowCopy( partition );
        # find biggest part
        sizes := List( [1..n], i -> Length( Positions( partition, i ) ) );
        biggest := Position( sizes, Maximum( sizes ) );
        # find blocks of G so that a block will contain the biggest part
        blocks := Blocks( G, [1..n], Filtered([1..n], i -> partition[i]=biggest) );
        # merge parts of partition across blocks
        for B in blocks do 
            mergeParts( B );
        od;
        if category = IsRightQuasigroup then # for right quasigroups, merge parts by left translations
            for f in LeftSection( Q ) do # left section consists of transformations only
                for x in indQ do for y in indQ do
                    if x<y and partition[x] = partition[y] then # x and y are related
                        merge2Parts( x^f, y^f );  # thus relate f(x) and f(y)
                    fi;
                od; od;
            od;
        fi;
        # recalculating the equivalence relation and partition
        parts := List([1..Maximum(partition)], i -> Filtered( [1..n], j -> partition[j]=i ) );
        classes := EquivalenceClasses( EquivalenceRelationByPartition( Domain([1..n]), parts ) );
        partition := List( [1..n], i -> First( [1..n], j -> i in classes[j] ) );
        done := partition = old_partition;
    od;
    # returning the congruence
    parts := Filtered( parts, A -> IsSubset( indQ, A ) ); # keep only the parts that form Q
    parts := List( parts, part -> List( part, i -> Q.(i) ) ); # convert to elements
    return EquivalenceRelationByPartition( Q, parts );
end );

# RightQuasigroupCongruenceByPartition
# QuasigroupCongruenceByPartition
# LoopCongruenceByPartition

InstallMethod( RightQuasigroupCongruenceByPartition, "for right quasigroup and partition",
    [ IsRightQuasigroup, IsList ],
function( Q, parts )
    return RQ_AlgebraCongruenceByPartition( Q, parts );
end );

InstallMethod( QuasigroupCongruenceByPartition, "for quasigroup and partition",
    [ IsQuasigroup, IsList ],
function( Q, parts )
    return RQ_AlgebraCongruenceByPartition( Q, parts );
end );

InstallMethod( LoopCongruenceByPartition, "for loop and partition",
    [ IsLoop, IsList ],
function( Q, parts )
    return RQ_AlgebraCongruenceByPartition( Q, parts );
end );

# RQ_AlgebraCongruenceByPairs

InstallMethod( RQ_AlgebraCongruenceByPairs, "for right quasigroup and list of pairs",
    [ IsRightQuasigroup, IsList ],
function( Q, gens )
    local classes;
    # checking parameters
    if not ForAll( gens, x -> IsList( x ) and Length( x ) = 2 and x[1] in Q and x[2] in Q ) then
        Error( "RQ: <2> must be a list of pairs of elements of <1>.");
    fi;
    classes := EquivalenceClasses( EquivalenceRelationByPairs( Q, gens ) );
    return RQ_AlgebraCongruenceByPartition( Q, List( classes, Elements ) );
end );

# RightQuasigroupCongruenceByPairs
# QuasigroupCongruenceByPairs
# LoopCongruenceByPairs

InstallMethod( RightQuasigroupCongruenceByPairs, "for right quasigroup and list of pairs",
    [ IsRightQuasigroup, IsList ],
function( Q, pairs )
    return RQ_AlgebraCongruenceByPairs( Q, pairs );
end );
 
InstallMethod( QuasigroupCongruenceByPairs, "for quasigroup and list of pairs",
    [ IsQuasigroup, IsList ],
function( Q, pairs )
    return RQ_AlgebraCongruenceByPairs( Q, pairs );
end );

InstallMethod( LoopCongruenceByPairs, "for loop and list of pairs",
    [ IsLoop, IsList ],
function( Q, pairs )
    return RQ_AlgebraCongruenceByPairs( Q, pairs );
end );  
 
# AllRightQuasigroupCongruences

InstallMethod( AllRightQuasigroupCongruences, "for right quasigroup with transitive right multiplication group",
    [ IsRightQuasigroup ],
function( Q )
    # PROG: Only implemented for finite right quasigroups with transitive right multiplication groups.
	local G, blocks, firstQ, T, systems, good_systems, S, good, x, B, C, D;
    G := RightMultiplicationGroup( Q ); 
    if not IsTransitive( G, ParentInd( Q ) ) then
        Error( "RQ: No method for right quasigroups with non-transitive right multiplication groups.");
    fi;
	blocks := AllBlocks( G );
	# add trivial blocks
    firstQ := ParentInd( Elements( Q )[ 1 ] ); # the smallest index of an element in Q
	blocks := Concatenation( [ [firstQ] ], blocks );
	if Size( Q ) > 1 then
		blocks := Concatenation( blocks, [ ParentInd( Q ) ] );
	fi;
	# complete blocks into systems
	T := RightTransversal( G, Stabilizer(G, firstQ ) );
	systems := List( blocks, B -> Set( T, f -> Set( B, i -> i^f ) ) );
	# convert to elements
	systems := List( systems, S -> List( S, B -> List( B, i -> Q.(i) ) ) );
	# verify that these are preserved by multiplication on the left, too
    # MATH: multiplying on the left might map a block onto a proper subset of another block
	good_systems := [];
	for S in systems do
		good := true;
		for x in Q do
			for B in S do
				C := Set( x*B );
                D := First( S, block -> C[1] in block );
                if not IsSubset( D, C ) then
				    good := false; break;
				fi;
			od;
			if not good then
                break;
            fi;
		od;
		if good then
			Add( good_systems, S );
		fi;
	od;
	# return GAP congruences
	return List( good_systems, S -> EquivalenceRelationByPartition( Q, S ) );
end );

# AllQuasigroupCongruences
# AllLoopCongruences

InstallMethod( AllQuasigroupCongruences, "for quasigroup",
    [ IsQuasigroup ],
function( Q )
	local G, blocks, firstQ, T, systems;
	G := MultiplicationGroup( Q ); # acts transitively, no need to check
	blocks := AllBlocks( G );
	# add trivial blocks
    firstQ := ParentInd( Elements( Q )[ 1 ] ); # the smallest index of an element in Q
	blocks := Concatenation( [ [firstQ] ], blocks );
	if Size( Q ) > 1 then
		blocks := Concatenation( blocks, [ ParentInd( Q ) ] );
	fi;
    # complete blocks into systems
    T := RightSection( Q );
    systems := List( blocks, B -> Set( T, f -> Set( B, i -> i^f ) ) );
	# convert to elements
	systems := List( systems, S -> List( S, B -> List( B, i -> Q.(i) ) ) );
	# return GAP congruences
	return List( systems, S -> EquivalenceRelationByPartition( Q, S ) );
end );

InstallMethod( AllLoopCongruences, "for loop",
    [ IsLoop ],
    Q -> AllQuasigroupCongruences( Q )
);

# NORMALITY AND SIMPLICITY
# _____________________________________________________________________________

# IsNormalOp
InstallOtherMethod( IsNormalOp, "for loop and subloop",  
    [ IsLoop, IsLoop ], 
function( Q, S )
    local pos;
    if not IsSubloop( Q, S ) then
        Error( "RQ: <2> must be a subloop of <1>." );
    fi;
    pos := ParentInd( S );
    return ForAll( GeneratorsOfGroup( InnerMappingGroup( Q ) ), g -> pos = OnSets( pos, g ) );  
    # PROG: The above would not work if left translations of Q were permutations of [1..Size(Q)]; we would need indexes of S relative to Q.
end );

# NormalClosureOp
InstallOtherMethod( NormalClosureOp, "for loop and subloop",
    [ IsLoop, IsLoop ],
function( Q, S )
    if not IsSubloop( Q, S ) then
        Error( "RQ: <2> must be a subloop of <1>." );
    fi;
    return NormalClosure( Q, Elements( S ) );
end );

# NormalClosure
# PROG: constructor OK, calls Subloop
InstallOtherMethod( NormalClosure, "for loop and collection of its elements",
    [ IsLoop, IsCollection ],
function( Q, gens )
    local transl_of_gens, nc_mltgr, subloop_indices;
    if ForAny( gens, x -> not x in Q ) then
        Error( "RQ: <2> must be a subset of <1>." );
    fi;
    transl_of_gens := List( gens, x -> LeftTranslation( Q, x ) );
    nc_mltgr := Group( Union( GeneratorsOfGroup( InnerMappingGroup( Q ) ), transl_of_gens ) );
    subloop_indices :=  Orbit( nc_mltgr, ParentInd( One(Q) ) );
    return Subloop( Q, List( subloop_indices, i -> Q.(i) ) );
    # PROG: this seems to be faster than adding 1 to gens and calculating Blocks( MultiplicationGroup(Q), [1..Size(Parent(Q))], gens )[ 1 ]
end );

# AllNormalSubloops
InstallMethod( AllNormalSubloops, "for loop",
    [ IsLoop ],
function( Q )
	local G, blocks, new_blocks, B, f, subs;
	G := MultiplicationGroup( Q ); # acts transitively on the points it moves, namely on ParentInd( Q )
	blocks := AllBlocks( G ); # returns a representative from each nontrivial block system
	# adding trivial representatives
	blocks := Concatenation( [[1]], blocks ); # it does not matter that 1 is not necessarily the neutral element
	if Size( Q ) > 1 then
		blocks := Concatenation( blocks, [ ParentInd( Q ) ] );
	fi;
	# making sure that blocks contain the neutral element
	new_blocks := [];
	for B in blocks do
		f := Inverse( RightTranslation( Q, Q.(B[1]) ) ); # sends B[1] to the index of the neutral element
		Add( new_blocks, List( B, i -> i^f ) );
	od;
	# creating subloops
	subs := List( new_blocks, B -> List(B, i -> Q.(i) ) );
	return List( subs, S -> Subloop( Q, S ) );
end );

# IsSimpleRightQuasigroup
InstallMethod( IsSimpleRightQuasigroup, "for right quasigroup",
    [ IsRightQuasigroup ],
function( Q )
    local G, pairs, orbits, reps, x, C;
    if Size( Q ) in [1,2] then # always simple
        return true;
    fi;
    # size > 2
    if not IsTransitive( RightMultiplicationGroup( Q ), ParentInd( Q ) ) then
        return false; # the orbits (or some unions of orbits) form a nontrivial right quasigroup congruence
    fi;
    # transitive right multiplication group
    # return Length( AllRightQuasigroupCongruences( Q ) ) = 2; # this is not bad but the following might be faster
    G := RightMultiplicationGroup( Q );
    pairs := Filtered( Tuples( ParentInd( Q ), 2 ), x -> x[1]<>x[2] ); # diagonal removed
    orbits := OrbitsDomain( G, pairs, OnPairs );;  # action of G on QxQ
    # MATH: If (x,y), (u,v) are in the same orbit, they generate the same congruence since right translations are invertible.
    reps := List( orbits, orbit -> orbit[1] );
    for x in reps do
	    C := RightQuasigroupCongruenceByPairs( Q, [ [ Q.(x[1]), Q.(x[2]) ] ] ); # congruence generated by (x[1],x[2])
	    if Length( EquivalenceClasses( C ) ) > 1 then
            return false;
        fi;
    od;
    return true;
end );

# IsSimpleQuasigroup
InstallMethod( IsSimpleQuasigroup, "for quasigroup",
    [ IsQuasigroup ],
function( Q )
    return IsPrimitive( MultiplicationGroup( Q ) );
end );

# IsSimpleLoop
InstallMethod( IsSimpleLoop, "for loop",
    [ IsLoop ],
function( Q )
    return IsPrimitive( MultiplicationGroup( Q ) );
end );

# IsSimple
# PROG: Note the ranking so that IsSimpleLoop rather than IsSimpleQuasigroup is called for loops, etc.
InstallOtherMethod( IsSimple, "for right quasigroup", [ IsRightQuasigroup ], 0, IsSimpleRightQuasigroup );
InstallOtherMethod( IsSimple, "for quasigroup", [ IsQuasigroup ], 1, IsSimpleQuasigroup );
InstallOtherMethod( IsSimple, "for loop", [ IsLoop ], 2, IsSimpleLoop );

# FACTOR ALGEBRAS
# _____________________________________________________________________________

# RQ_FactorAlgebra
# PROG: constructor OK, all needed info is in F!.uSet
InstallMethod( RQ_FactorAlgebra, "for equivalence relation and record",
    [ IsEquivalenceRelation, IsRecord ],
function( C, style )
    local category, classes, ct, factorQ, F;
    RQ_CompleteConstructorStyle( style );
    if style.checkArguments then 
        RQ_IsAlgebraCongruence( category, C, true );
    fi;
    category := CategoryOfRightQuasigroup( Source( C ) );
    classes := EquivalenceClasses( C );
    if style.indexBased then
        ct := List( classes, A -> List( classes, B -> EquivalenceClassOfElement(C,Elements(A)[1]*Elements(B)[1]) ) );
        factorQ := RQ_AlgebraByCayleyTable( category, ct, style );
    else # not index based
        factorQ := RQ_AlgebraShell( category, classes, ConstructorStyle( false, false ) );
        F := FamilyObj( factorQ.1 );
        F!.mult := function( A, B )
            return First( F!.uSet, class -> Elements(A)[1]*Elements(B)[1] in class ); # since the equivalence relation is not stored
        end;
        RQ_AddDefaultOperations( factorQ );
    fi;
    RQ_InheritProperties( Source( C ), factorQ, false );
    return factorQ;
end );

# FactorRightQuasigroup
InstallMethod( FactorRightQuasigroup, "for equivalence relation",
    [ IsEquivalenceRelation ],
    C -> RQ_FactorAlgebra( C, RQ_defaultConstructorStyle )
);

InstallOtherMethod( FactorRightQuasigroup, "for equivalence relation and record",
    [ IsEquivalenceRelation, IsRecord ],
function( C, style )
    return RQ_FactorAlgebra( C, style );
end );

# FactorQuasigroup
InstallMethod( FactorQuasigroup, "for equivalence relation",
    [ IsEquivalenceRelation ],
    C -> RQ_FactorAlgebra( C, RQ_defaultConstructorStyle )
);

InstallOtherMethod( FactorQuasigroup, "for equivalence relation and record",
    [ IsEquivalenceRelation, IsRecord ],
function( C, style )
    return RQ_FactorAlgebra( C, style );
end );

# FactorLoop
InstallMethod( FactorLoop, "for equivalence relation",
    [ IsEquivalenceRelation ],
    C -> RQ_FactorAlgebra( C, RQ_defaultConstructorStyle )
);

InstallOtherMethod( FactorLoop, "for equivalence relation and record",
    [ IsEquivalenceRelation, IsRecord ],
function( C, style )
    return RQ_FactorAlgebra( C, style );
end );

InstallOtherMethod( FactorLoop, "for loop and normal subloop",
    [ IsLoop, IsLoop ],
function( Q, N )
    return FactorLoop( Q, N, RQ_defaultConstructorStyle );
end );

# PROG: constructor OK, relies on F!.cosets
InstallOtherMethod( FactorLoop, "for loop, normal subloop and record",
    [ IsLoop, IsLoop, IsRecord ],
function( Q, N, style )
    local cosets, ct, factorQ, F;
    RQ_CompleteConstructorStyle( style );
    if style.checkArguments and not ( IsSubloop( Q, N ) and IsNormal( Q, N ) ) then
        Error("RQ: <2> must be a normal subloop of <1>.");
    fi;
    cosets := RightCosets( Q, N );
    if style.indexBased then
        ct :=  List( cosets, x -> List( cosets, y -> First( cosets, c->x[1]*y[1] in c ) ) );
        factorQ := RQ_AlgebraByCayleyTable( IsLoop, ct, ConstructorStyle( true, false ) );
    else # not index based
        factorQ := RQ_AlgebraShell( IsLoop, cosets, ConstructorStyle( false, false ) );
        F := FamilyObj( factorQ.1 );
        F!.cosets := ShallowCopy( cosets );
        F!.mult := function( x, y ) return First( F!.cosets, c -> x[1]*y[1] in c ); end;
        RQ_AddDefaultOperations( factorQ );
    fi;
    RQ_InheritProperties( Q, factorQ, false );
    return factorQ;
end );

# \/ 
InstallOtherMethod( \/, "for right quasigroup and equivalence relation",
    ReturnTrue, # families of arguments are not the same hence default IsIdenticalObj cannot be used 
    [ IsRightQuasigroup, IsEquivalenceRelation ], 0,
function( Q, C )
    # there seems to be no way to have optional arguments here, note usage of inheritance and defaults
    return RQ_FactorAlgebra( C, ConstructorStyle( IsIndexBased( Q ), RQ_defaultConstructorStyle.checkArguments ) );
end );

InstallOtherMethod( \/, "for loop and its normal subloop",
    IsIdenticalObj,
    [ IsLoop, IsLoop ], 0,
function( Q, N )
    return FactorLoop( Q, N, ConstructorStyle( IsIndexBased( Q ), RQ_defaultConstructorStyle.checkArguments ) );
end );
