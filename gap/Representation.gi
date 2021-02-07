# Representations.gi
# Representing right quasigroups in GAP
# =============================================================================

# INFO CLASS
# _____________________________________________________________________________

SetInfoLevel( InfoRightQuasigroups, 1 );

# GAP CATEGORIES AND REPRESENTATIONS
# _____________________________________________________________________________

# CategoryOfRightQuasigroup

InstallMethod( CategoryOfRightQuasigroup, "for right quasigroup",
    [ IsRightQuasigroup ],
function( Q )
    if IsLoop( Q ) then return IsLoop; fi;
    if IsQuasigroup( Q ) then return IsQuasigroup; fi;
    return IsRightQuasigroup;
end );

# DISPLAYING RIGHT QUASIGROUPS AND THEIR ELEMENTS
# _____________________________________________________________________________

# RQ_ViewObjString
InstallMethod( RQ_ViewObjPrintObjString, "for right quasigroup",
    [ IsRightQuasigroup ],
function( Q )
    local category, c, v, F, s;
    category := CategoryOfRightQuasigroup( Q );
    # type of algebra
    if category = IsRightQuasigroup then
        if HasIsQuandle( Q ) and IsQuandle( Q ) then c := "quandle";
        elif HasIsRack( Q ) and IsRack( Q ) then c := "rack";
        else c := "right quasigroup";
        fi;
    elif category = IsQuasigroup then
        if HasIsQuandle( Q ) and IsQuandle( Q ) then c := "latin quandle";
        elif HasIsRack( Q ) and IsRack( Q ) then c := "latin rack";
        else c := "quasigroup";
        fi;
    else c := "loop";
    fi;
    # variety or class
    v := "";
    if HasIsAssociative( Q ) and IsAssociative( Q ) then v := "associative "; 
    elif HasIsExtraLoop( Q ) and IsExtraLoop( Q ) then v := "extra "; 
    elif HasIsMoufangLoop( Q ) and IsMoufangLoop( Q ) then v := "Moufang "; 
    elif HasIsCLoop( Q ) and IsCLoop( Q ) then v := "C "; 
    elif HasIsLeftBruckLoop( Q ) and IsLeftBruckLoop( Q ) then v := "left Bruck ";
    elif HasIsRightBruckLoop( Q ) and IsRightBruckLoop( Q ) then v := "right Bruck ";
    elif HasIsLeftBolLoop( Q ) and IsLeftBolLoop( Q ) then v := "left Bol ";
    elif HasIsRightBolLoop( Q ) and IsRightBolLoop( Q ) then v := "right Bol ";
    elif HasIsAutomorphicLoop( Q ) and IsAutomorphicLoop( Q ) then v := "automorphic ";
    elif HasIsLeftAutomorphicLoop( Q ) and IsLeftAutomorphicLoop( Q ) then v := "left automorphic ";
    elif HasIsRightAutomorphicLoop( Q ) and IsRightAutomorphicLoop( Q ) then v := "right automorphic ";
    elif HasIsLCLoop( Q ) and IsLCLoop( Q ) then v := "LC ";
    elif HasIsRCLoop( Q ) and IsRCLoop( Q ) then v := "RC ";
    elif HasIsAlternative( Q ) and IsAlternative( Q ) then v := "alternative ";
    elif HasIsLeftAlternative( Q ) and IsLeftAlternative( Q ) then v := "left alternative ";
    elif HasIsRightAlternative( Q ) and IsRightAlternative( Q ) then v := "right alternative ";
    elif HasIsCommutative( Q ) and IsCommutative( Q ) then v := "commutative ";
    elif HasIsFlexible( Q ) and IsFlexible( Q ) then v := "flexible ";
    fi;
    # will add the word "shell" if not all operations are bound
    F := FamilyObj(Q.1);
    s := "";
    if F!.indexBased then # index based, checking for existence of mult table and functions
        if not ( IsBound( F!.multTable ) and IsBound( F!.rdiv )
                 and ( category = IsRightQuasigroup or IsBound( F!.ldiv ) )
                 and ( category <> IsLoop or IsBound( F!.one ) ) )
            then s := " shell";
        fi;
    else # not index based, checking for existence of functions
        if not ( IsBound( F!.mult ) and IsBound( F!.rdiv )
                 and ( category = IsRightQuasigroup or IsBound( F!.ldiv ) )
                 and ( category <> IsLoop or IsBound( F!.one ) ) )
            then s := " shell";
        fi;
    fi;
    return Concatenation( "<", v, c, s, " of size ", String( Size( Q ) ) );
end );

# ViewObj
InstallOtherMethod( ViewObj, "for right quasigroup",
    [ IsRightQuasigroup ],
function( Q )
    Print( RQ_ViewObjPrintObjString( Q ), ">" );
end );

# PrintObj
# PROG: rank is set high so that associative right quasigroups are not printed as semigroups
InstallOtherMethod( PrintObj, "for right quasigroup",
    [ IsRightQuasigroup ], 10, 
function( Q )
    local n, i;
    Print( RQ_ViewObjPrintObjString( Q ), " on " );
    n := Minimum( Size(Q), 5 );
    for i in [1..n] do
            Print( UnderlyingSetElm( Elements( Q )[ i ] ) );
            if i<n then Print(", "); fi;
    od;
    if Size( Q ) > 5 then
        Print( ", ...");
    fi;
    Print( ">" );
end );

# PrintObj

InstallMethod( PrintObj, "for a right quasigroup element",
    [ IsRightQuasigroupElement ],
function( obj )
    local F, s;
    F := FamilyObj( obj );
    Print( F!.names );
    # Print( ":" );
    Print( UnderlyingSetElm( obj ) );
    return true;
end );

# SetRightQuasigroupElementsName
# SetQuasigroupElementsName
# SetLoopElementsName

InstallMethod( SetRightQuasigroupElementsName, "for right quasigroup and string",
    [ IsRightQuasigroup, IsString ],
function( Q, name )
    local F;
    F := FamilyObj( Q.1 );
    F!.names := name;
    return true;
end );

InstallMethod( SetQuasigroupElementsName, "for quasigroup and string",
    [ IsQuasigroup, IsString ],
function( Q, name )
    return SetRightQuasigroupElementsName( Q, name );
end );

InstallMethod( SetLoopElementsName, "for loop and string",
    [ IsLoop, IsString ],
function( Q, name )
    return SetRightQuasigroupElementsName( Q, name );
end );

# THE UNDERLYING SET
# _____________________________________________________________________________

# UnderlyingSetElm

InstallMethod( UnderlyingSetElm, "for right quasigroup element",
    [ IsRightQuasigroupElement ],
function( x )
    local F;
    F := FamilyObj( x );
    if F!.indexBased then
        return F!.uSet[ x![1] ];
    fi;
    return x![1];
end );

InstallOtherMethod( UnderlyingSetElm, "for list of right quasigroup elements",
    [ IsList ],
    ls -> List( ls, UnderlyingSetElm )
);

InstallOtherMethod( UnderlyingSetElm, "for right quasigroup",
    [ IsRightQuasigroup ],
    Q -> UnderlyingSetElm( Elements( Q ) )
);

# UnderlyingSet

InstallMethod( UnderlyingSet, "for right quasigroup",
    [ IsRightQuasigroup ],
    Q -> UnderlyingSetElm( Q )
);

# ChangeUnderlyingSet

InstallMethod( ChangeUnderlyingSet, "for right quasigroup and collection",
    [ IsRightQuasigroup, IsCollection ],
function( Q, S )
    local F;
    if not ( IsIndexBased( Q ) and Parent( Q ) = Q ) then
        return Error( "RQ: <1> must be index based and its own parent." );
    fi;
    if not IsSet( S ) then S := Set( S ); fi;
    if Size( Q ) <> Size( S ) then
        return Error( "RQ: <1> and <2> must be of the same size." );
    fi;
    # change the underlying set
    F := FamilyObj( Q.1 );
    F!.uSet := ShallowCopy( S );
    # change Cayley table if present
    if IsBound( F!.cayleyTable ) then
        F!.cayleyTable := CayleyTable( Q );
    fi;
    return true;
end );

# ELEMENTWISE OPERATIONS
# _____________________________________________________________________________

# \=

InstallMethod( \=, "for two elements of a right quasigroup",
    IsIdenticalObj,
    [ IsRightQuasigroupElement, IsRightQuasigroupElement ],
function( x, y )
    return x![ 1 ] = y![ 1 ] and FamilyObj( x ) = FamilyObj( y );
end );

# \<

InstallMethod( \<, "for two elements of a right quasigroup",
    IsIdenticalObj,
    [ IsRightQuasigroupElement, IsRightQuasigroupElement ],
function( x, y )
    return x![ 1 ] < y![ 1 ] and FamilyObj( x ) = FamilyObj( y );
end );

# \[\]

InstallOtherMethod( \[\], "for right quasigroup and element of the underlying set",
	[ IsRightQuasigroup, IsObject ],
function( Q, x )
    # this allows Q[x] for x in the underlying set of the parent of Q (so Q[x] need not be in Q)
    local F;
    F := FamilyObj( Q.1 );
    if not x in F!.uSet then
        Error("RQ: <2> must be en element of the underlying set of <1>");
    fi; 
    if F!.indexBased then
        return Objectify( TypeObj( Q.1 ), [ PositionSorted( F!.uSet, x) ] );
    fi;
    return Objectify( TypeObj( Q.1 ), [ x ] ); 
end ); 

# \.

# PROG: rank is set high so that associative right quasigroups do not call \. for semigroups
InstallMethod( \., "for right quasigroup and positive integer",
	[ IsRightQuasigroup, IsPosInt ], 10, 
function( Q, i )
    # (PROG) the HasParent( Q ) test is necessary since Objectify in constructors
    # calls this for some reason and Parent( Q ) is not yet set at that point
    if HasParent( Q ) then 
        return Elements( Parent( Q ) )[  Int( NameRNam( i ) ) ];
    fi;
    return fail;
end );

# \* (multiplication)

InstallMethod( \*, "for two right quasigroup elements",
    IsIdenticalObj,
    [ IsRightQuasigroupElement, IsRightQuasigroupElement ],
function( x, y )
    local F;
    F := FamilyObj( x );
    # SPEEDUP: compare running times on 10 million products in an index based right quasigroup of size 100
    if F!.indexBased then 
        return F!.set[ F!.multTable[ x![ 1 ], y![ 1 ] ] ]; # 1.7s
        #return Objectify( TypeObj( x ), [ F!.multTable[ x![ 1 ], y![ 1 ] ] ] ); # 3.8s
        #return Objectify( TypeObj( x ), [ F!.mult( x![ 1 ], y![ 1 ] ) ] ); # 5.2s
    fi;
    return Objectify( TypeObj( x ), [ F!.mult( x![ 1 ], y![ 1 ] ) ] );
end );

InstallOtherMethod( \*, "for right quasigroup element and list of right quasigroup elements",
    [ IsRightQuasigroupElement, IsList ],
function( x, ly )
    return List( ly, y -> x*y );
end );

InstallOtherMethod( \*, "for list of right quasigroup elements and right quasigroup element",
    [ IsList, IsRightQuasigroupElement ],
function( lx, y )
    return List( lx, x -> x*y );
end );

# \/ (right quotient, right division)
# RightQuotient, RightDivision

InstallOtherMethod( \/, "for two right quasigroup elements",
    IsIdenticalObj,
    [ IsRightQuasigroupElement, IsRightQuasigroupElement ],
function( x, y )
    local F;
    F := FamilyObj( x );
    if F!.indexBased then
        if not IsBound( F!.rdivTable ) then # first usage of right division for index based
            F!.rdivTable := RightDivisionTableFromMultiplicationTable( F!.multTable );
            SetRightDivisionTable( F!.parent, F!.rdivTable );
        fi;
        return F!.set[ F!.rdivTable[ x![ 1 ], y![ 1 ] ] ]; 
    fi;
    return Objectify( TypeObj( x ), [ F!.rdiv( x![ 1 ], y![ 1 ] ) ] );
end );

InstallOtherMethod( RightQuotient, "for two right quasigroup elements",
    IsIdenticalObj,
    [ IsRightQuasigroupElement, IsRightQuasigroupElement ],
function( x, y )
    return x/y;
end );

InstallOtherMethod( \/, "for list of right quasigroup elements and right quasigroup element",
    [ IsList, IsRightQuasigroupElement ],
    0,
function( ls, y )
    return List( ls, x -> x/y );
end );

InstallOtherMethod( RightQuotient, "for list of right quasigroup elements and right quasigroup element",
    [ IsList, IsRightQuasigroupElement ],
    0,
function( ls, y )
    return ls/y;
end );

InstallOtherMethod( \/, "for right quasigroup element and list of right quasigroup elements",
    [ IsRightQuasigroupElement, IsList ],
    0,
function( x, ls )
    return List( ls, y -> x/y );
end );

InstallOtherMethod( RightQuotient, "for right quasigroup element and list of right quasigroup elements",
    [ IsRightQuasigroupElement, IsList ],
    0,
function( x, ls )
    return x/ls;
end );

# LeftQuotient, LeftDivision

InstallOtherMethod( LeftQuotient, "for two quasigroup elements",
    IsIdenticalObj,
    [ IsQuasigroupElement, IsQuasigroupElement ],
function( x, y )
    local F;
    F := FamilyObj( x );
	if F!.indexBased then
        if not IsBound( F!.ldivTable ) then # first usage of left division for index based
            F!.ldivTable := LeftDivisionTableFromMultiplicationTable( F!.multTable );
            SetLeftDivisionTable( F!.parent, F!.ldivTable );
        fi;
        return F!.set[ F!.ldivTable[ x![ 1 ], y![ 1 ] ] ]; 
    fi;
    return Objectify( TypeObj( x ), [ F!.ldiv( x![ 1 ], y![ 1 ] ) ] );
end );

InstallOtherMethod( LeftQuotient, "for list of quasigroup elements and quasigroup element",
    [ IsList, IsQuasigroupElement ],
    0,
function( ls, y )
    return List( ls, x -> LeftQuotient(x,y) );
end );

InstallOtherMethod( LeftQuotient, "for quasigroup element and list of quasigroup elements",
    [ IsRightQuasigroupElement, IsList ],
    0,
function( x, ls )
    return List( ls, y -> LeftQuotient(x,y) );
end );

# \^
# Note: This is called when a permutation acts on a right quasigroup element, for instance within OnPoints, OnSets, etc.

InstallMethod( \^, "for right quasigroup element and permutation",
    [ IsRightQuasigroupElement, IsPerm ],
function( x, p )
    local F, i;
    F := FamilyObj( x );
    return F!.set[ ParentInd( x )^p ];
end );

# OneOp
InstallMethod( OneOp, "for loop element",
    [ IsLoopElement ],
function( x )
    local F;
    F := FamilyObj( x );
    return F!.one;
end );

# LeftInverse
InstallMethod( LeftInverse, "for loop element",
    [ IsLoopElement ],
    x -> RightDivision( One( x ), x )
);

# RightInverse
InstallMethod( RightInverse, "for loop element",
    [ IsLoopElement ],
    x -> LeftDivision( x, One( x ) )
);

# InverseOp
InstallMethod( InverseOp, "for loop element",
    [ IsLoopElement ],
function( x )
    local y;
    y := RightInverse( x );
    if y = LeftInverse( x ) then return y; fi;
    return fail;
end );

# Exponent
InstallOtherMethod( Exponent, "for loop",
    [ IsLoop ],
    Q -> Lcm( List( Elements( Q ), Order ) )
);

# Commutator
InstallMethod( Commutator, "for two right quasigroup elements",
    [ IsRightQuasigroupElement, IsRightQuasigroupElement ],
function( x, y )
    return (x*y)/(y*x);
end);

# Comm
InstallOtherMethod( Comm, "for two quasigroup elements",
    [ IsQuasigroupElement, IsQuasigroupElement ],
function( x, y )
    return LeftDivision( y*x, x*y );
end );

# Associator
InstallMethod( Associator, "for three right quasigroup elements",
    [ IsRightQuasigroupElement, IsRightQuasigroupElement, IsRightQuasigroupElement ],
function( x, y, z )
    return (x*(y*z))/((x*y)*z);
end);

# INDEX BASED AND CANONICAL COPIES
# _____________________________________________________________________________

# ParentInd

InstallMethod( ParentInd, "for right quasigroup element",
    [ IsRightQuasigroupElement ],
function( x )
    local F;
    F := FamilyObj( x );
    if F!.indexBased then # fast
        return x![1];
    fi;
    return PositionSorted( F!.set, x );
end );

InstallOtherMethod( ParentInd, "for list of right quasigroup elements",
    [ IsList ],
    ls -> List( ls, ParentInd )
);

InstallOtherMethod( ParentInd, "for right quasigroup",
    [ IsRightQuasigroup ],
    Q -> ParentInd( Elements( Q ) )
);

# IsIndexBased

InstallMethod( IsIndexBased, "for right quasigroup",
    [ IsRightQuasigroup ],
function( Q )
    return FamilyObj( Q.1 )!.indexBased;
end );

# IndexBasedCopy

InstallMethod( IndexBasedCopy, "for right quasigroup",
    [ IsRightQuasigroup ],
function( Q )
    local style, copyQ;
    style := ConstructorStyle( true, false );
    if IsLoop( Q ) then copyQ := LoopByCayleyTable( CayleyTable( Q ), style ); 
    elif IsQuasigroup( Q ) then copyQ := QuasigroupByCayleyTable( CayleyTable( Q ), style ); 
    else copyQ := RightQuasigroupByCayleyTable( CayleyTable( Q ), style );
    fi;
    RQ_InheritProperties( Q, copyQ );
    return copyQ;
end );

# IsCanonical

InstallMethod( IsCanonical, "for right quasigroup",
    [ IsRightQuasigroup ],
function( Q )
    local F;
    F := FamilyObj( Q.1 );
    return Q = Parent( Q ) and F!.indexBased and F!.uSet = [1..Size(Q)];
end );

# CanonicalCopy

InstallMethod( CanonicalCopy, "for right quasigroup",
    [ IsRightQuasigroup ],
function( Q )
    local style, copyQ;
    style := ConstructorStyle( true, false );
    if IsLoop( Q ) then copyQ := LoopByCayleyTable( MultiplicationTable( Q ), style );
    elif IsQuasigroup( Q ) then copyQ :=  QuasigroupByCayleyTable( MultiplicationTable( Q ), style );
    else copyQ := RightQuasigroupByCayleyTable( MultiplicationTable( Q ), style );
    fi;
    RQ_InheritProperties( Q, copyQ );
    return copyQ;
end );

# GENERATORS AND COMPARISON OF RIGHT QUASIGROUPS
# _____________________________________________________________________________

# SmallGeneratingSet
InstallOtherMethod( SmallGeneratingSet, "for right quasigroup",
        [ IsRightQuasigroup ],
function( Q )
    local category, gens, sub, candidates, max, S, best_gen, best_S;
    category := CategoryOfRightQuasigroup( Q );
    gens := [];         # generating set to be returned
    sub := [];          # substructure generated so far
    while sub <> Q do
        # find an element not in sub that most enlarges sub
        candidates := Difference( Elements(Q), sub );
        max := 0;
        while not IsEmpty( candidates ) do
            S := RQ_Subalgebra( Q, Union( gens, [candidates[1]] ) );
            if Size(S) > max then
                max := Size( S );
                best_gen := candidates[1];
                best_S := S;
            fi;
            # discard elements of S since they cannot do better
            candidates := Difference( candidates, Elements( S ) );
        od;
        AddSet( gens, best_gen );
        sub := best_S;
    od;
    return gens;
end );

# GeneratorsSmallest
InstallOtherMethod( GeneratorsSmallest, "for right quasigroup",
        [ IsRightQuasigroup ],
function( Q )
    local gens, diff;
    gens := [ ];
    diff := Elements( Q );
    while diff <> [ ] do
        Add( gens, diff[ Length(diff) ] );
        diff := Difference( diff, RQ_Subalgebra( Q, gens ) );
    od;
    return Set( gens );
end );

# \< (comparing two right quasigroups with common parent)
InstallMethod(\<,
    "for right quasigroups by lexicographically ordered small generating sets",
    IsIdenticalObj,
    [IsRightQuasigroup, IsRightQuasigroup],
function(a,b)
    local ga,gb;
    ga:=GeneratorsSmallest(a);
    gb:=GeneratorsSmallest(b);
    # making sure generator lists have the same length by repeating
    # the last element of the shorter set
    if Length(ga)<Length(gb) then
        a:=Elements(a)[Size(a)];
        ga:=ShallowCopy(ga);
        while Length(ga)<Length(gb) do Add(ga,a); od;
    else
        b:=Elements(b)[Size(b)];
        gb:=ShallowCopy(gb);
        while Length(gb)<Length(ga) do Add(gb,b); od;
    fi;
    return ga<gb;
end );

# AUXILIARY PROGRAMMING METHODS
# _____________________________________________________________________________

# RQ_OptionalError
InstallGlobalFunction( RQ_OptionalError,
function( reportErrors, error )
    if reportErrors then 
        Error( error );
    fi;
    return false;
end ); 

# RQ_GroupByGenerators 
InstallGlobalFunction( RQ_GroupByGenerators, 
function( gens )
    if IsEmpty( gens ) then
        return Group(());
    fi;
    return Group( gens );
end );
