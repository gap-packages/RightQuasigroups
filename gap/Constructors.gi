# Constructors.gi
# Constructing right quasigroups
# =============================================================================

# SetDefaultConstructorStyle
InstallMethod( SetDefaultConstructorStyle, "for two bools",
    [ IsBool, IsBool ],
function( index_based, check_arguments )
    RQ_defaultConstructorStyle.indexBased := index_based;
    RQ_defaultConstructorStyle.checkArguments := check_arguments;
    return true;
end );

# ConstructorStyle
InstallMethod( ConstructorStyle, "for two bools",
    [ IsBool, IsBool ],
function( index_based, check_arguments )
    return rec( indexBased := index_based, checkArguments := check_arguments );
end );

# RQ_CompleteConstructorStyle
InstallMethod( RQ_CompleteConstructorStyle, "for record",
    [ IsRecord ],
function( style )
    local completed;
    completed := [];
    if not IsBound( style.indexBased ) then
        style.indexBased := RQ_defaultConstructorStyle.indexBased;
        Add( completed, "indexBased" );
    fi;
    if not IsBound( style.checkArguments ) then
        style.checkArguments := RQ_defaultConstructorStyle.checkArguments;
        Add( completed, "checkArguments" );
    fi;
    return completed;
end );

# MATHEMATICAL TESTS OF CATEGORIES
# _____________________________________________________________________________

# RQ_IsBijectiveFunction
InstallGlobalFunction( RQ_IsBijectiveFunction,
function( S, f )
    local pos, x, posx;
    if not IsSet(S) then S := Set(S); fi; # just in case
    pos := List([1..Size(S)], i -> false);
    for x in S do
        posx := PositionSorted( S, f(x) );
        if (not posx in [1..Size(S)]) or pos[ posx ] then
            return false;
        else
            pos[ posx ] := true;
        fi;
    od;
    return true;
end );

InstallMethod( IsRightQuasigroupMagma, "for magma",
    [ IsMagma ],
    M -> ForAll( M, x -> RQ_IsBijectiveFunction( Elements( M ), function( y ) return y*x; end ) )
);

InstallMethod( IsLeftQuasigroupMagma, "for magma",
    [ IsMagma ],
    M -> ForAll( M, x -> RQ_IsBijectiveFunction( Elements( M ), function( y ) return x*y; end ) )
);

# IsQuasigroupMagma
InstallOtherMethod( IsQuasigroupMagma, "for magma",
    [ IsMagma ],
    # we check left translations first because presumably this will often be called for right quasigroups
    M -> IsLeftQuasigroupMagma( M ) and IsRightQuasigroupMagma( M )
);

# IsLoopMagma
InstallMethod( IsLoopMagma, "for magma",
    [ IsMagma ],
    M -> (not MultiplicativeNeutralElement( M ) = fail) and IsQuasigroupMagma( M )
);

# CONVERSIONS OF MAGMAS TO RIGHT QUASIGROUPS, QUASIGROUPS, LOOPS
# _____________________________________________________________________________

# RQ_AsAlgebra
InstallMethod( RQ_AsAlgebra, "for category, domain and record", 
    [ IsObject, IsDomain, IsRecord ],
function( category, M, style )
    local Q;
    RQ_CompleteConstructorStyle( style );
    if IsAdditiveGroup( M ) then
        Q := RQ_AlgebraByFunction( category, M, \+, \-, function(x,y) return -x+y; end, Zero( M ), style );
        SetIsAssociative( Q, true );
        return Q;
    fi;
    if IsGroup( M ) then
        Q := RQ_AlgebraByFunction( category, M, \*, \/, function(x,y) return x^(-1)*y; end, One( M ), style );
        SetIsAssociative( Q, true );
        return Q;
    fi;
    if IsRightQuasigroup( M ) then
        # REVISIT: This is not well done. If M is not index based, might stall in creating Cayley table.
        # The difficulty is that the multiplication in M might depend on all kinds of data specified under F!.mult
        # or in Parent(M). We therefore resort to a Cayley table constructor.
        
        # The following is necessary to prevent undesirable nesting of elements.
        # For instance, if M is a declared right quasigroup and category = IsQuasigroup, then
        # the call RQ_AlgebraByFunction produces a quasigroup with elements q:r:<object> rather than q:<object>

        # REVISIT: Unnesting procedure in general?
        
        if style.checkArguments and not RQ_IsAlgebraCayleyTable( category, CayleyTable( M ), false ) then
            # we set the third argument to false so that errors are not reported
            return fail;
        fi;
        Q := RQ_AlgebraByCayleyTable( category, CayleyTable( M ), style ); 
        RQ_InheritProperties( M, Q ); # isomorphic copy inherits everything
        return Q;
    fi; 
    if IsMagma( M ) then 
        if style.checkArguments and not RQ_AreAlgebraFunctions( category, M, \*, fail, fail, fail, false ) then
            return fail;
        fi; 
        return RQ_AlgebraByFunction( category, M, \*, fail, fail, fail, style );
    fi;
    return fail;    
end );

# AsRightQuasigroup
# AsQuasigroup
# AsLoop

InstallMethod( AsRightQuasigroup, "for domain (magma or additive group)",
    [ IsDomain ],
    M -> RQ_AsAlgebra( IsRightQuasigroup, M, RQ_defaultConstructorStyle )
);

InstallOtherMethod( AsRightQuasigroup, "for domain (magma od additive group) and record",
    [ IsDomain, IsRecord ],
function( M, style )
    return RQ_AsAlgebra( IsRightQuasigroup, M, style );
end );

InstallMethod( AsQuasigroup, "for domain (magma od additive group)",
    [ IsDomain ],
    M -> RQ_AsAlgebra( IsQuasigroup, M, RQ_defaultConstructorStyle )
);

InstallOtherMethod( AsQuasigroup, "for domain (magma od additive group) and record",
    [ IsDomain, IsRecord ],
function( M, style )
    return RQ_AsAlgebra( IsQuasigroup, M, style );
end );

InstallMethod( AsLoop, "for domain (magma od additive group)",
    [ IsDomain ],
    M -> RQ_AsAlgebra( IsLoop, M, RQ_defaultConstructorStyle )
);

InstallOtherMethod( AsLoop, "for domain (magma od additive group) and record",
    [ IsDomain, IsRecord ],
function( M, style )
    return RQ_AsAlgebra( IsLoop, M, style );
end );

# CONSTRUCTOR OF SHELL ALGEBRA
# _____________________________________________________________________________

# RQ_AlgebraShell
# PROG: We support the optional argument here because the function appears in the documentation.
# In general, auxiliary RQ_ functions do not support optional arguments.
InstallMethod( RQ_AlgebraShell, "for category and collection",
    [ IsObject, IsCollection ],
function( category, S )
    return RQ_AlgebraShell( category, S, RQ_defaultConstructorStyle );
end );

InstallOtherMethod( RQ_AlgebraShell, "for category, collection and record",
    [ IsObject, IsCollection, IsRecord ],
function( category, S, style )
    local elmName, familyName, elementType, repType, F, n, Q;
    RQ_CompleteConstructorStyle( style );
    if category = IsLoop then
        elmName := "l"; familyName := "LoopFam";
        elementType := IsLoopElement; repType := IsLoopElmRep; 
    elif category = IsQuasigroup then
        elmName := "q"; familyName := "QuasigroupFam";
        elementType := IsQuasigroupElement; repType := IsQuasigroupElmRep; 
    else # IsRightQuasigroup
        elmName := "r"; familyName := "RightQuasigroupFam";
        elementType := IsRightQuasigroupElement; repType := IsRightQuasigroupElmRep;
    fi;
    # constructing the family
    F := NewFamily( familyName, elementType );
    # installing data for the family
    if not IsSet( S ) then S := Set(S); fi;
    n := Size( S );
    F!.size := n;
    F!.uSet := ShallowCopy( S );
    F!.indexBased := style.indexBased;
    # constructing elements
    if F!.indexBased then
        F!.set := Immutable( List( [1..n], i -> Objectify( NewType( F, elementType and repType ), [ i ] ) ) );
    else 
        F!.set := Immutable( List( F!.uSet, x -> Objectify( NewType( F, elementType and repType ), [ x ] ) ) );
    fi;        
    # default prefix of elements
    F!.names := elmName;
    # creating the algebra
    Q := Objectify( NewType( FamilyObj( F!.set ), category and IsAttributeStoringRep ), rec() );
    F!.parent := Q;
    # setting attributes for the algebra
    SetSize( Q, n );
    SetAsSSortedList( Q, F!.set );
    SetParent( Q, Q );
    SetGeneratorsOfMagma( Q, Elements( Q ) ); # default generators
    return Q;
end );

# RQ_AddDefaultOperations
InstallMethod( RQ_AddDefaultOperations, "for right quasigroup",
    [ IsRightQuasigroup ],
function( Q )
    local F, category;
    category := CategoryOfRightQuasigroup( Q );
    F := FamilyObj( Q.1 ); 
    if F!.indexBased then # index based, assumes that F!.multTable is bound
        # the operation functions will not be used in \*, \/ and LeftQuotient, but it does not hurt to have them
        F!.mult := function( i, j ) return F!.multTable[ i, j ]; end;
        SetMultiplicationTable( Q, F!.multTable );
        F!.rdiv := function( i, j ) return F!.rdivTable[ i, j ]; end;
        # F!.rdivTable := RightDivisionTableFromMultiplicationTable( F!.multTable );
        # SetRightDivisionTable( Q, F!.rdivTable );
        if category <> IsRightQuasigroup then
            F!.ldiv := function( i, j ) return F!.ldivTable[ i, j ]; end;
            # F!.ldivTable := LeftDivisionTableFromMultiplicationTable( F!.multTable );
            # SetLeftDivisionTable( Q, F!.ldivTable );
        fi;
    else # not index based
        F!.rdiv := function(x,y) return First( F!.uSet, z -> x = F!.mult(z,y) ); end;
        if category <> IsRightQuasigroup then
            F!.ldiv := function(x,y) return First( F!.uSet, z -> y = F!.mult(x,z)); end;
        fi;
    fi;
    # both cases
    if category = IsLoop and not IsBound( F!.one ) then
        F!.one := First( Elements( Q ), x -> x*x=x );
        SetOne( Q, F!.one );
    fi;
    return true;
end );

# CONSTRUCTORS BY CAYLEY TABLE
# _____________________________________________________________________________

# MultiplicationTable
# CayleyTable
# RightDivisionTable
# RightDivisionCayleyTable
# LeftDivisionTable
# LeftDivisionCayleyTable

InstallOtherMethod( MultiplicationTable, "for right quasigroup",
    [ IsRightQuasigroup ], 1, # PROG: higher rank to call it before method for magma
function( Q )
    local F, ind, uSet, elms;
    F := FamilyObj( Q.1 );
    ind := ParentInd( Q );
    if IsBound( F!.multTable ) then
        return List( F!.multTable{ ind }{ ind }, row -> List( row, i -> PositionSorted( ind, i ) ) );
    fi;
    if IsBound( F!.cayleyTable ) then
        uSet := UnderlyingSet( Q );
        return List( F!.cayleyTable{ ind }{ ind }, row -> List( row, x -> PositionSorted( uSet, x ) ) );
    fi;
    # nothing available
    elms := Elements( Q );
    return List( Q, x -> List( Q, y -> PositionSorted( elms, x*y ) ) );
end );

InstallMethod( CayleyTable, "for right quasigroup",
    [ IsRightQuasigroup ],
function( Q )
    local F, ind;
    F := FamilyObj( Q.1 );
    ind := ParentInd( Q );
    if IsBound( F!.cayleyTable ) then 
        return F!.cayleyTable{ ind }{ ind };
    fi;
    if IsBound( F!.multTable ) then
        return List( F!.multTable{ ind }{ ind }, row -> List( row, i -> F!.uSet[ i ] ) );
    fi;
    # nothing available
    return List( Q, x -> List( Q, y -> UnderlyingSetElm( x*y ) ) );
end );

InstallMethod( RightDivisionTable, "for right quasigroup",
    [ IsRightQuasigroup ],
function( Q )
    local F, ind, elms;
    F := FamilyObj( Q.1 );
    ind := ParentInd( Q );
    if IsIndexBased( Q ) and not IsBound( F!.rdivTable ) then # first usage of right division for index based
            F!.rdivTable := RightDivisionTableFromMultiplicationTable( F!.multTable );
            SetRightDivisionTable( Parent( Q ), F!.rdivTable );
    fi;
    if IsBound( F!.rdivTable ) then
        return List( F!.rdivTable{ ind }{ ind }, row -> List( row, i -> PositionSorted( ind, i ) ) );
    fi;
    # nothing available
    elms := Elements( Q );
    return List( Q, x -> List( Q, y -> PositionSorted( elms, x/y ) ) );
end );

InstallMethod( RightDivisionCayleyTable, "for right quasigroup",
    [ IsRightQuasigroup ],
function( Q )
    local F, ind;
    F := FamilyObj( Q.1 );
    ind := ParentInd( Q );
    if IsIndexBased( Q ) and not IsBound( F!.rdivTable ) then # first usage of right division for index based
            F!.rdivTable := RightDivisionTableFromMultiplicationTable( F!.multTable );
            SetRightDivisionTable( Parent( Q ), F!.rdivTable );
    fi;
    if IsBound( F!.rdivTable ) then 
        return List( F!.rdivTable{ ind }{ ind }, row -> List( row, i -> F!.uSet[ i ] ) );
    fi;
    # nothing available
    return List( Q, x -> List( Q, y -> UnderlyingSetElm( x/y ) ) );
end );

InstallMethod( LeftDivisionTable, "for quasigroup",
    [ IsQuasigroup ],
function( Q )
    local F, ind, elms;
    F := FamilyObj( Q.1 );
    ind := ParentInd( Q );
    if IsIndexBased( Q ) and not IsBound( F!.ldivTable ) then # first usage of left division for index based
        F!.ldivTable := LeftDivisionTableFromMultiplicationTable( F!.multTable );
        SetLeftDivisionTable( Parent( Q ), F!.ldivTable );
    fi;
    if IsBound( F!.ldivTable ) then
        return List( F!.ldivTable{ ind }{ ind }, row -> List( row, i -> PositionSorted( ind, i ) ) );
    fi;
    # nothing available
    elms := Elements( Q );
    return List( Q, x -> List( Q, y -> PositionSorted( elms, LeftQuotient( x, y ) ) ) );
end );

InstallMethod( LeftDivisionCayleyTable, "for quasigroup",
    [ IsQuasigroup ],
function( Q )
    local F, ind;
    F := FamilyObj( Q.1 );
    ind := ParentInd( Q );
    if IsIndexBased( Q ) and not IsBound( F!.ldivTable ) then # first usage of left division for index based
        F!.ldivTable := LeftDivisionTableFromMultiplicationTable( F!.multTable );
        SetLeftDivisionTable( Parent( Q ), F!.ldivTable );
    fi;
    if IsBound( F!.ldivTable ) then 
        return List( F!.ldivTable{ ind }{ ind }, row -> List( row, i -> F!.uSet[ i ] ) );
    fi;
    # nothing available
    return List( Q, x -> List( Q, y -> UnderlyingSetElm( LeftQuotient( x, y ) ) ) );
end );

# RightDivisionTableFromMultiplicationTable
# LeftDivisionTableFromMultiplicationTable

InstallMethod( RightDivisionTableFromMultiplicationTable, "for multiplication table",
    [ IsRectangularTable ],
function( t )
    local n, rdiv, i, j;
    n := Length( t );
    rdiv := List([1..n], i -> [1..n]);
    for i in [1..n] do
        for j in [1..n] do
            rdiv[ t[i,j], j ] := i;
        od;
    od;
    return rdiv;
end );

InstallMethod( LeftDivisionTableFromMultiplicationTable, "for multiplication table",
    [ IsRectangularTable ],
function( t )
    local n, ldiv, i, j;
    n := Length( t );
    ldiv := List([1..n], i -> [1..n]);
    for i in [1..n] do
        for j in [1..n] do
            ldiv[ i, t[i,j] ] := j;
        od;
    od;
    return ldiv;
end );

# RQ_IsAlgebraCayleyTable
InstallMethod( RQ_IsAlgebraCayleyTable, "for category, square table and bool",
    [ IsObject, IsRectangularTable, IsBool ],
function( category, ct, reportErrors )
    local n, S, e;
    n := Length( ct );
    S := Set(ct{[1..n]}[1]); # first column
    if not ForAll( [1..n], i -> Set( ct{[1..n]}[i] ) = S ) then
        return RQ_OptionalError( reportErrors, "RQ: The Cayley table does not give rise to right division." );
    fi;
    if category <> IsRightQuasigroup and not ForAll( [1..n], i -> Set( ct[i] ) = S ) then
        return RQ_OptionalError( reportErrors, "RQ: The Cayley table does not give rise to left division." );
    fi;
    if category = IsLoop then
        e := First( [1..n], i -> ct[i]=S );
        if e = fail or ct{[1..n]}[e]<>S then
            return RQ_OptionalError( reportErrors, "RQ: The Cayley table does not contain an identity element." );
        fi;
    fi;
    return true;
end );

# IsRightQuasigroupCayleyTable
# IsQuasigroupCayleyTable
# IsLoopCayleyTable

InstallMethod( IsRightQuasigroupCayleyTable, "for square table",
    [ IsRectangularTable ],
    ct -> RQ_IsAlgebraCayleyTable( IsRightQuasigroup, ct, false ) # do not want to report error
);

InstallMethod( IsQuasigroupCayleyTable, "for square table",
    [ IsRectangularTable ],
    ct -> RQ_IsAlgebraCayleyTable( IsQuasigroup, ct, false )
);

InstallMethod( IsLoopCayleyTable, "for square table",
    [ IsRectangularTable ],
    ct -> RQ_IsAlgebraCayleyTable( IsLoop, ct, false )
);

# NormalizedRightQuasigroupCayleyTable
InstallMethod( NormalizedRightQuasigroupCayleyTable, "for square table",
    [ IsRectangularTable ],
    Q -> Set( Q )
);

# NormalizedQuasigroupCayleyTable
InstallMethod( NormalizedQuasigroupCayleyTable, "for square table",
    [ IsRectangularTable ],
    Q -> TransposedMat( Set( TransposedMat( Set( Q ) ) ) )
);

# RQ_AlgebraByCayleyTable
InstallMethod( RQ_AlgebraByCayleyTable, "for category, Cayley table and record",
    [ IsObject, IsRectangularTable, IsRecord ],
function( category, ct, style ) 
    local n, S, Q, F;
    RQ_CompleteConstructorStyle( style );
    if style.checkArguments then
        RQ_IsAlgebraCayleyTable( category, ct, true );
    fi;
    n := Length( ct );
    S := Set(ct{[1..n]}[1]); # first column as underlying set
    Q := RQ_AlgebraShell( category, S, style );
    F := FamilyObj( Q.1 );
    if style.indexBased then
        if S = [1..n] then # multiplication table = Cayley table
            F!.multTable := ShallowCopy( ct );
        else # construct multiplication table from Cayley table
            F!.multTable := List( [1..n], i -> List( [1..n], j -> PositionSorted( F!.uSet, ct[ i, j ] ) ) );
        fi;
    else # not index based
        F!.cayleyTable := ShallowCopy( ct );
        F!.mult := function( x, y ) return F!.cayleyTable[ PositionSorted( F!.uSet, x ), PositionSorted( F!.uSet, y )]; end;
    fi;
    RQ_AddDefaultOperations( Q );
    return Q;
end );

# RightQuasigroupByCayleyTable
# QuasigroupByCayleyTable
# LoopByCayleyTable

InstallMethod( RightQuasigroupByCayleyTable, "for Cayley table",
    [ IsRectangularTable ],
    ct -> RQ_AlgebraByCayleyTable( IsRightQuasigroup, ct, RQ_defaultConstructorStyle  )
);

InstallOtherMethod( RightQuasigroupByCayleyTable, "for Cayley table and record",
    [ IsRectangularTable, IsRecord ],
function( ct, style )
    return RQ_AlgebraByCayleyTable( IsRightQuasigroup, ct, style ); 
end );

InstallMethod( QuasigroupByCayleyTable, "for Cayley table",
    [ IsRectangularTable ],
    ct -> RQ_AlgebraByCayleyTable( IsQuasigroup, ct, RQ_defaultConstructorStyle  )
);

InstallOtherMethod( QuasigroupByCayleyTable, "for Cayley table and record",
    [ IsRectangularTable, IsRecord ],
function( ct, style )
    return RQ_AlgebraByCayleyTable( IsQuasigroup, ct, style ); 
end );

InstallMethod( LoopByCayleyTable, "for Cayley table",
    [ IsRectangularTable ],
    ct -> RQ_AlgebraByCayleyTable( IsLoop, ct, RQ_defaultConstructorStyle  ) 
);

InstallOtherMethod( LoopByCayleyTable, "for Cayley table and record",
    [ IsRectangularTable, IsRecord ],
function( ct, style )
    return RQ_AlgebraByCayleyTable( IsLoop, ct, style ); 
end );

# CONSTRUCTORS BY FUNCTION
# _____________________________________________________________________________

# RQ_AreAlgebraFunctions
InstallGlobalFunction( RQ_AreAlgebraFunctions,
function( category, S, mult, rdiv, ldiv, one, reportErrors )
    local e;
    if not IsSet( S ) then S := Set(S); fi;
    # checking multiplication and divisions
    if rdiv = fail then # only mult given
        if not ForAll( S, y -> RQ_IsBijectiveFunction( S, function( x ) return mult(x,y); end ) ) then
            return RQ_OptionalError( reportErrors, "RQ: The multiplication does not give rise to right division." );
        fi;
        if category <> IsRightQuasigroup and not ForAll( S, x -> RQ_IsBijectiveFunction( S, function( y ) return mult(x,y); end ) ) then 
           return RQ_OptionalError( reportErrors, "RQ: The multiplication does not give rise to left division." );
        fi;
    else # either [mult,rdiv] or [mult,rdiv,ldiv] given
        if not ForAll( S, x -> ForAll( S, y -> mult(rdiv(x,y),y)=x and rdiv(mult(x,y),y)=x ) ) then
            return RQ_OptionalError( reportErrors, "RQ: The multiplication and right division are not compatible." );
        fi;
        if category <> IsRightQuasigroup and ldiv <> fail
            and not ForAll( S, x -> ForAll( S, y -> mult(x,ldiv(x,y))=y and ldiv(x,mult(x,y))=y ) ) then
            return RQ_OptionalError( reportErrors, "RQ: The multiplication and left division are not compatible." );
        fi;
    fi;
    if category = IsLoop then # check for identity element
        if one <> fail and not ForAll( S, x -> mult(x,one)=x and mult(one,x)=x ) then 
            return RQ_OptionalError( reportErrors, "RQ: The given element is not an identity element." );
        else
            e := First( S, x-> mult(x,x)=x );
            if e=fail or not ForAll( S, x -> mult(x,e)=x and mult(e,x)=x ) then # no identity element
                return RQ_OptionalError( reportErrors, "RQ: There is no identity element." );
            fi;
        fi;
    fi;
    return true;
end );

# IsRightQuasigroupFunction
# IsQuasigroupFunction
# IsLoopFunction

InstallGlobalFunction( IsRightQuasigroupFunction,
function( S, mult, arg... )
    local functions, rdiv;
    functions := Filtered( arg, IsFunction );   
    rdiv := First( functions ); # returns fail if functions is empty 
    return RQ_AreAlgebraFunctions( IsRightQuasigroup, S, mult, rdiv, fail, fail, false );
end );

InstallGlobalFunction( IsQuasigroupFunction,
function( S, mult, arg... )
    local functions, rdiv, ldiv;
    functions := Filtered( arg, IsFunction );    
    rdiv := First( functions );
    if Length( functions ) > 1 then ldiv := functions[ 2 ]; else ldiv := fail; fi;
    return RQ_AreAlgebraFunctions( IsQuasigroup, S, mult, rdiv, ldiv, fail, false );
end );

InstallGlobalFunction( IsLoopFunction,
function( S, mult, arg... )
    local functions, rdiv, ldiv, one;
    functions := Filtered( arg, IsFunction );    
    rdiv := First( functions );
    if Length( functions ) > 1 then ldiv := functions[ 2 ]; else ldiv := fail; fi;
    one := First( arg, x -> not IsFunction( x ) );
    return RQ_AreAlgebraFunctions( IsLoop, S, mult, rdiv, ldiv, one, false );
end );

# MultiplicationFunction

InstallMethod( MultiplicationFunction, "for right quasigroup",
    [ IsRightQuasigroup ],
function( Q )
    local F;
    F := FamilyObj( Q.1 );
    if not IsBound( F!.mult ) then
        return fail;
    fi;
    return F!.mult;
end );

# RightDivisionFunction

InstallMethod( RightDivisionFunction, "for right quasigroup",
    [ IsRightQuasigroup ],
function( Q )
    local F;
    F := FamilyObj( Q.1 );
    if not IsBound( F!.rdiv ) then
        return fail;
    fi;
    return F!.rdiv;
end );

# LefttDivisionFunction

InstallMethod( LeftDivisionFunction, "for right quasigroup",
    [ IsQuasigroup ],
function( Q )
    local F;
    F := FamilyObj( Q.1 );
    if not IsBound( F!.ldiv ) then
        return fail;
    fi;
    return F!.ldiv;
end );

# RQ_AlgebraByFunction
InstallGlobalFunction( RQ_AlgebraByFunction,
function( category, S, mult, rdiv, ldiv, one, style ) 
    local n, ct, Q, F;
    RQ_CompleteConstructorStyle( style );
    # checking functions
    if not IsSet( S ) then S := Set( S ); fi;
    if style.checkArguments then 
        RQ_AreAlgebraFunctions( category, S, mult, rdiv, ldiv, one, true ); # will halt with an error message if there is a problem
    fi;
    n := Size( S );
    if style.indexBased then # forget everything except multiplication
        ct := List( [1..n], i -> List( [1..n], j -> mult(S[i],S[j]) ) );
        return RQ_AlgebraByCayleyTable( category, ct, ConstructorStyle( true, false ) ); # do not check again
    fi;
    # not index based
    Q := RQ_AlgebraShell( category, S, style );
    F := FamilyObj( Q.1 );
    F!.mult := mult;
    if one<>fail and category = IsLoop then
        F!.one := Q[one];
        SetOne( Q, F!.one );
    fi;
    if rdiv = fail then 
        RQ_AddDefaultOperations( Q ); # "one" will not be added if it has already been set
    else         
        F!.rdiv := rdiv;
        if category <> IsRightQuasigroup then
            F!.ldiv := ldiv;
        fi;
    fi;
    return Q;
end );

# RightQuasigroupByFunction
# QuasigroupByFunction
# LoopByFunction

InstallGlobalFunction( RightQuasigroupByFunction,
function( S, mult, arg... )
    local functions, rdiv, style;
    functions := Filtered( arg, IsFunction );
    rdiv := First( functions );
    style := Last( arg ); # capturing constructorStyle
    if style = fail or not IsRecord( style ) then 
        style := RQ_defaultConstructorStyle;
    fi;
    return RQ_AlgebraByFunction( IsRightQuasigroup, S, mult, rdiv, fail, fail, style );
end );

InstallGlobalFunction( QuasigroupByFunction,
function( S, mult, arg... )
    local functions, rdiv, ldiv, style;
    functions := Filtered( arg, IsFunction );
    rdiv := First( functions ); # PROG: returns fail if functions is empty
    if Length( functions ) > 1 then ldiv := functions[ 2 ]; else ldiv := fail; fi;
    style := Last( arg ); 
    if style = fail or not IsRecord( style ) then 
        style := RQ_defaultConstructorStyle;
    fi;
    return RQ_AlgebraByFunction( IsQuasigroup, S, mult, rdiv, ldiv, fail, style );
end );

InstallGlobalFunction( LoopByFunction,
function( S, mult, arg... )
    local functions, rdiv, ldiv, one, style;
    functions := Filtered( arg, IsFunction );
    rdiv := First( functions );
    if Length( functions ) > 1 then ldiv := functions[ 2 ]; else ldiv := fail; fi;
    one := First( arg, x -> x in S );
    style := Last( arg ); 
    if style = fail or not IsRecord( style ) then 
        style := RQ_defaultConstructorStyle;
    fi;
    return RQ_AlgebraByFunction( IsLoop, S, mult, rdiv, ldiv, one, style );
end );

# CONSTRUCTORS BY RIGHT SECTION
# _____________________________________________________________________________

# RQ_IsAlgebraRightSection
InstallMethod( RQ_IsAlgebraRightSection, "for category, two collections and bool",
    [ IsObject, IsCollection, IsCollection, IsBool ],
function( category, S, section, reportErrors )
    local n, A, e;
    if not IsSet( S ) then S := Set( S ); fi;
    n := Size( S );
    A := ShallowCopy( S );
    if IsPerm( section[1] ) and not IsPosInt(S[1]) then # implicit permutation action
        A := [1..n];
    fi;
    if not ForAll( section, f -> RQ_IsBijectiveFunction( A, function(i) return i^f; end ) ) then   
        return RQ_OptionalError( reportErrors, "RQ: The section does not act on the underlying set." );
    fi;
    if category <> IsRightQuasigroup and not ForAll( A, i -> Set( section, f -> i^f ) = A ) then
        return RQ_OptionalError( reportErrors, "RQ: The section does not give rise to a quasigroup." );  
    fi;
    if category = IsLoop then
        e := First([1..n], i -> ForAll( [1..n], j -> PositionSorted( A, A[i]^section[j]) = j ) );
        if e=fail or not ForAll( A, x -> x^section[ e ] = x ) then
            return RQ_OptionalError( reportErrors, "RQ: The section does not give rise to a loop." );     
        fi;
    fi;
    return true;
end );

# IsRightSection
# IsQuasigroupRightSection
# IsLoopRightSection

InstallMethod( IsRightSection, "for underlying set and section",
    [ IsCollection, IsCollection ],
function( S, section )
    return RQ_IsAlgebraRightSection( IsRightQuasigroup, S, section, false );
end );

InstallOtherMethod( IsRightSection, "for collection of pemutations",
    [ IsCollection ],
function( section )
    return RQ_IsAlgebraRightSection( IsRightQuasigroup, [1..Length(section)], section, false );
end );

InstallMethod( IsQuasigroupRightSection, "for underlying set and section",
    [ IsCollection, IsCollection ],
function( S, section )
    return RQ_IsAlgebraRightSection( IsQuasigroup, S, section, false );
end );

InstallOtherMethod( IsQuasigroupRightSection, "for collection of pemutations",
    [ IsCollection ],
function( section )
    return RQ_IsAlgebraRightSection( IsQuasigroup, [1..Length(section)], section, false );
end );

InstallMethod( IsLoopRightSection, "for underlying set and section",
    [ IsCollection, IsCollection ],
function( S, section )
    return RQ_IsAlgebraRightSection( IsLoop, S, section, false );
end );

InstallOtherMethod( IsLoopRightSection, "for collection of pemutations",
    [ IsCollection ],
function( section )
    return RQ_IsAlgebraRightSection( IsLoop, [1..Length(section)], section, false );
end );

# RQ_AlgebraByRightSection
InstallMethod( RQ_AlgebraByRightSection, "for category, two collections and record",
    [ IsObject, IsCollection, IsCollection, IsRecord ],
function( category, S, section, style )
    local n, ct, Q, F;
    RQ_CompleteConstructorStyle( style );
    if not IsSet( S ) then S := Set( S ); fi;
    if style.checkArguments then
        RQ_IsAlgebraRightSection( category, S, section, true );
    fi;
    if style.indexBased then
        n := Size( S );
        if IsPerm( section[1] ) and not IsPosInt( S[1] ) then # implicit permutation action
            ct := List([1..n], i -> List([1..n], j -> S[ i^section[j]] ) );
        else
            ct := List(S, x -> List([1..n], j -> x^section[j] ) );
        fi;
        Q := RQ_AlgebraByCayleyTable( category, ct, ConstructorStyle( true, false ) ); # not checking again
        F := FamilyObj( Q.1 );
        F!.rSection := ShallowCopy( section ); # REVISIT: Should we store it?
     else # not index based
        Q := RQ_AlgebraShell( category, S, style );
        F := FamilyObj( Q.1 );
        F!.rSection := ShallowCopy( section ); # will refer to it in mult function
        if IsPerm( section[1] ) and (not IsPosInt(S[1])) then
            F!.mult := function(x,y) return F!.uSet[ PositionSorted(F!.uSet,x)^F!.rSection[ PositionSorted( F!.uSet, y )] ]; end;
        else # action on S
            F!.mult := function(x,y) return x^F!.rSection[ PositionSorted( F!.uSet, y ) ]; end;

        fi;
        RQ_AddDefaultOperations( Q );
    fi;
    SetRightSection( Q, F!.rSection );
    return Q;
end );

# RightQuasigroupByRightSection
# QuasigroupByRightSection
# LoopByRightSection

InstallMethod( RightQuasigroupByRightSection, "for two collections",
    [ IsCollection, IsCollection ],
function( S, section )
    return RQ_AlgebraByRightSection( IsRightQuasigroup, S, section, RQ_defaultConstructorStyle );
end );

InstallOtherMethod( RightQuasigroupByRightSection, "for two collections and record",
    [ IsCollection, IsCollection, IsRecord ],
function( S, section, style )
    return RQ_AlgebraByRightSection( IsRightQuasigroup, S, section, style );
end );

InstallOtherMethod( RightQuasigroupByRightSection, "for a collection",
    [ IsCollection ],
function( section )
    return RQ_AlgebraByRightSection( IsRightQuasigroup, [1..Length(section)], section, RQ_defaultConstructorStyle );
end );

InstallOtherMethod( RightQuasigroupByRightSection, "for a collection and record",
    [ IsCollection, IsRecord ],
function( section, style )
    return RQ_AlgebraByRightSection( IsRightQuasigroup, [1..Length(section)], section, style );
end );

InstallMethod( QuasigroupByRightSection, "for two collections",
    [ IsCollection, IsCollection ],
function( S, section )
    return RQ_AlgebraByRightSection( IsQuasigroup, S, section, RQ_defaultConstructorStyle );
end );

InstallOtherMethod( QuasigroupByRightSection, "for two collections and record",
    [ IsCollection, IsCollection, IsRecord ],
function( S, section, style )
    return RQ_AlgebraByRightSection( IsQuasigroup, S, section, style );
end );

InstallOtherMethod( QuasigroupByRightSection, "for a collection",
    [ IsCollection ],
function( section )
    return RQ_AlgebraByRightSection( IsQuasigroup, [1..Length(section)], section, RQ_defaultConstructorStyle );
end );

InstallOtherMethod( QuasigroupByRightSection, "for a collection and record",
    [ IsCollection, IsRecord ],
function( section, style )
    return RQ_AlgebraByRightSection( IsQuasigroup, [1..Length(section)], section, style );
end );

InstallMethod( LoopByRightSection, "for two collections",
    [ IsCollection, IsCollection ],
function( S, section )
    return RQ_AlgebraByRightSection( IsLoop, S, section, RQ_defaultConstructorStyle );
end );

InstallOtherMethod( LoopByRightSection, "for two collections and record",
    [ IsCollection, IsCollection, IsRecord ],
function( S, section, style )
    return RQ_AlgebraByRightSection( IsLoop, S, section, style );
end );

InstallOtherMethod( LoopByRightSection, "for a collection",
    [ IsCollection ],
function( section )
    return RQ_AlgebraByRightSection( IsLoop, [1..Length(section)], section, RQ_defaultConstructorStyle );
end );

InstallOtherMethod( LoopByRightSection, "for a collection and record",
    [ IsCollection, IsRecord ],
function( section, style )
    return RQ_AlgebraByRightSection( IsLoop, [1..Length(section)], section, style );
end );

# CONSTRUCTORS BY RIGHT FOLDER
# _____________________________________________________________________________

# RQ_RightSectionFromRightFolder
InstallMethod( RQ_RightSectionFromRightFolder, "for group, subgroup and sorted right tranversal",
    [ IsGroup, IsGroup, IsSet ],
function( G, H, T ) 
    local hom;
    hom := ActionHomomorphism( G, List( T, x -> H*x ), OnRight );
    return List( T, x -> x^hom );
end );

# RQ_IsAlgebraRightFolder
InstallMethod( RQ_IsAlgebraRightFolder, "for category, group, subgroup, right transversal and bool",
    [ IsObject, IsGroup, IsGroup, IsList, IsBool ],
function( category, G, H, T, reportErrors )
    local checked, cosets, section;
    checked := IsRightTransversal( T ); # whether to check if T is a right transversal to H in G
    if IsRightTransversal( T ) then
        T := Elements ( T );
    fi;
    T := Set( T );
    if not checked then # checking if T is a right transversal to H in G
        if not ( IsSubgroup( G, H ) and IsSubset( G, T ) ) then
            return RQ_OptionalError( reportErrors, "RQ: arguments must be given as G, H, T.");
        fi;
        cosets := List( T, x-> H*x );
        if not ( Size( G ) = Size( H )*Length( T ) and Size( G ) = Size( Union( cosets ) ) ) then # T is not a right transversal to H in G
            return false;
        fi;
    fi;
    section := RQ_RightSectionFromRightFolder( G, H, T );
    return RQ_IsAlgebraRightSection( category, [1..Length(T)], section, reportErrors ); # only the permutations matter
end );

# IsRightFolder
# IsQuasigroupRightFolder
# IsLoopRightFolder

InstallMethod( IsRightFolder, "for a right transversal",
    [ IsRightTransversal ],
    T -> RQ_IsAlgebraRightFolder( IsRightQuasigroup, T!.group, T!.subgroup, T, false )
);

InstallOtherMethod( IsRightFolder, "for a group, subgroup and right transversal",
    [ IsGroup, IsGroup, IsList ],
function( G, H, T )
    return RQ_IsAlgebraRightFolder( IsRightQuasigroup, G, H, T, false );
end );

InstallMethod( IsQuasigroupRightFolder, "for a right transversal",
    [ IsRightTransversal ],
    T -> RQ_IsAlgebraRightFolder( IsQuasigroup, T!.group, T!.subgroup, T, false )
);

InstallOtherMethod( IsQuasigroupRightFolder, "for a group, subgroup and right transversal",
    [ IsGroup, IsGroup, IsList ],
function( G, H, T )
    return RQ_IsAlgebraRightFolder( IsQuasigroup, G, H, T, false );
end );

InstallMethod( IsLoopRightFolder, "for a right transversal",
    [ IsRightTransversal ],
    T -> RQ_IsAlgebraRightFolder( IsLoop, T!.group, T!.subgroup, T, false )
);

InstallOtherMethod( IsLoopRightFolder, "for a group, subgroup and right transversal",
    [ IsGroup, IsGroup, IsList ],
function( G, H, T )
    return RQ_IsAlgebraRightFolder( IsLoop, G, H, T, false );
end );

# RQ_AlgebraByRightFolder
InstallMethod( RQ_AlgebraByRightFolder, "for category, group, subgroup, right transversal and record",
    [ IsObject, IsGroup, IsGroup, IsList, IsRecord ],
function( category, G, H, T, style ) 
    local section;
    RQ_CompleteConstructorStyle( style );
    if IsRightTransversal( T ) then
        T := Elements( T );
    fi;
    T := Set( T );
    if style.checkArguments then
        return RQ_IsAlgebraRightFolder( category, [G, H, T], true );
    fi;
    section := RQ_RightSectionFromRightFolder( G, H, T );
    return RQ_AlgebraByRightSection( category, T, section, style );
end );

# RightQuasigroupByRightFolder
InstallMethod( RightQuasigroupByRightFolder, "for right transversal",
    [ IsRightTransversal ],
    T -> RQ_AlgebraByRightFolder( IsRightQuasigroup, T!.group, T!.subgroup, T, RQ_defaultConstructorStyle )
);

InstallOtherMethod( RightQuasigroupByRightFolder, "for right transversal and record",
    [ IsRightTransversal, IsRecord ],
function( T, style )
    return RQ_AlgebraByRightFolder( IsRightQuasigroup, T!.group, T!.subgroup, T, style );
end );

InstallOtherMethod( RightQuasigroupByRightFolder, "for group, subgroup and right transversal",
    [ IsGroup, IsGroup, IsList ],
function( G, H, T )
    return RQ_AlgebraByRightFolder( IsRightQuasigroup, G, H, T, RQ_defaultConstructorStyle );
end );

InstallOtherMethod( RightQuasigroupByRightFolder, "for group, subgroup, right transversal and record",
    [ IsGroup, IsGroup, IsList, IsRecord ],
function( G, H, T, style )
    return RQ_AlgebraByRightFolder( IsRightQuasigroup, G, H, T, style );
end );

# QuasigroupByRightFolder
InstallMethod( QuasigroupByRightFolder, "for right transversal",
    [ IsRightTransversal ],
    T -> RQ_AlgebraByRightFolder( IsRightQuasigroup, T!.group, T!.subgroup, T, RQ_defaultConstructorStyle )
);

InstallOtherMethod( QuasigroupByRightFolder, "for right transversal and record",
    [ IsRightTransversal, IsRecord ],
function( T, style )
    return RQ_AlgebraByRightFolder( IsQuasigroup, T!.group, T!.subgroup, T, style );
end );

InstallOtherMethod( QuasigroupByRightFolder, "for group, subgroup and right transversal",
    [ IsGroup, IsGroup, IsList ],
function( G, H, T )
    return RQ_AlgebraByRightFolder( IsQuasigroup, G, H, T, RQ_defaultConstructorStyle );
end );

InstallOtherMethod( QuasigroupByRightFolder, "for group, subgroup, right transversal and record",
    [ IsGroup, IsGroup, IsList, IsRecord ],
function( G, H, T, style )
    return RQ_AlgebraByRightFolder( IsQuasigroup, G, H, T, style );
end );

# LoopByRightFolder
InstallMethod( LoopByRightFolder, "for right transversal",
    [ IsRightTransversal ],
    T -> RQ_AlgebraByRightFolder( IsLoop, T!.group, T!.subgroup, T, RQ_defaultConstructorStyle )
);

InstallOtherMethod( LoopByRightFolder, "for right transversal and record",
    [ IsRightTransversal, IsRecord ],
function( T, style )
    return RQ_AlgebraByRightFolder( IsLoop, T!.group, T!.subgroup, T, style );
end );

InstallOtherMethod( LoopByRightFolder, "for group, subgroup and right transversal",
    [ IsGroup, IsGroup, IsList ],
function( G, H, T )
    return RQ_AlgebraByRightFolder( IsLoop, G, H, T, RQ_defaultConstructorStyle );
end );

InstallOtherMethod( LoopByRightFolder, "for group, subgroup, right transversal and record",
    [ IsGroup, IsGroup, IsList, IsRecord ],
function( G, H, T, style )
    return RQ_AlgebraByRightFolder( IsLoop, G, H, T, style );
end );

# SPECIAL TYPES OF RIGHT QUASIGROUPS
# _____________________________________________________________________________

# ProjectionRightQuasigroup
InstallMethod( ProjectionRightQuasigroup, "for collection",
    [ IsCollection ],
    S -> ProjectionRightQuasigroup( S, RQ_defaultConstructorStyle )
);

InstallOtherMethod( ProjectionRightQuasigroup, "for collection and record",
    [ IsCollection, IsRecord ],
function( S, style )
    local mult, Q;
    RQ_CompleteConstructorStyle( style );
    if not IsSet( S ) then S := Set( S ); fi;
    mult := function(x,y) return x; end; # right projection
	# multiplication and right division are the same here, and there is nothing to check
    Q := RQ_AlgebraByFunction( IsRightQuasigroup, S, mult, mult, fail, fail, ConstructorStyle( style.indexBased, false ) );
    # setting implied properties
    SetIsProjectionRightQuasigroup( Q, true );
    SetIsQuandle( Q, true );
    return Q;
end );

InstallOtherMethod( ProjectionRightQuasigroup, "for positive integer",
    [ IsPosInt ],
    n -> ProjectionRightQuasigroup( [1..n] )
);

InstallOtherMethod( ProjectionRightQuasigroup, "for positive integer abd record",
    [ IsPosInt, IsRecord ],
function( n, style )
    return ProjectionRightQuasigroup( [1..n], style );
end );