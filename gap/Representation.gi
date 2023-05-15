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

InstallOtherMethod( CategoryOfRightQuasigroup, "for a list of right quasigroups",
    [ IsList ],
function( ls )
    if not ForAll( ls, IsRightQuasigroup ) then
        Error( "RQ: <1> must be a list of right quasigroups." );
    fi;
    if ForAll( ls, IsLoop ) then return IsLoop; fi;
    if ForAll( ls, IsQuasigroup ) then return IsQuasigroup; fi;
    return IsRightQuasigroup;
end );

# DISPLAYING RIGHT QUASIGROUPS AND THEIR ELEMENTS
# _____________________________________________________________________________

# PROG: View is supposed to call ViewObj, which is supposed to call ViewString.
# Similarly for Print, PrintObj and PrintString, and for Display and DisplayString (there is no DisplayObj).
# In reality, ViewString often provides a very generic description, such as "<object>",
# and ViewObj does not call it, prorividing something more descriptive for the main loop
# instead. 
# Print is supposed to give more details than View and it might be machine readable.
# Display is supposed to be nice, human readable.

# PROG: For right quasigroups, we want View, Print and Display to return results
# dynamically, depending on what is known about them. Since String is an attribute
# (which is calculated at first usage), while ViewString is not an attribute,
# we DO NOT delegate to String in ViewString etc.

# PROG: I do not understand how Name interacts with View, Print and Display.
# It appears that once HasName is true, View, ViewObj, Print and PrintObj do
# not call ViewString and PrintString but instead print the name. Display, however,
# ignores the name.

# String
# PROG: The rank is high to beat semigroups in case of associative right quasigroups.
# Also, GAP manual says that PrintString should approximate String. Since we struggle
# with PrintString, we will use ViewString for now.
InstallOtherMethod( String, "for right quasigroup",
    [ IsRightQuasigroup ], RQ_rank,
    Q -> ViewString( Q )
);

InstallOtherMethod( String, "for right quasigroup element",
    [ IsRightQuasigroupElement ], 
function( obj )
    local F;
    F := FamilyObj( obj );
    return Concatenation( F!.names, String( UnderlyingSetElm( obj ) ) );
end );

# ViewString
InstallMethod( ViewString, "for right quasigroup",
    [ IsRightQuasigroup ], RQ_rank,
function( Q )
    local category, c, v, F, s;
    # named objects return their name
    if HasName( Q ) then
        return Name( Q );
    fi;
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
    return Concatenation( "<", v, c, s, " of size ", String( Size( Q ) ), ">" );
end );

InstallOtherMethod( ViewString, "for right quasigroup element",
    [ IsRightQuasigroupElement ], 
    x -> String( x ) 
);    

# PrintString
InstallOtherMethod( PrintString, "for right quasigroup",
    [ IsRightQuasigroup ], RQ_rank, 
function( Q )
    return DisplayString( Q ); # same for now
end );

InstallOtherMethod( PrintString, "for a right quasigroup element",
    [ IsRightQuasigroupElement ],
function( obj )
    local F;
    F := FamilyObj( obj );
    return Concatenation( F!.names, PrintString( UnderlyingSetElm( obj ) ) );
end );
  
# PrintObj
# PROG: This calls PrintString by default, but it looks like the semigroups package
# changed the default behavior and hence our PrintString would never be called for
# associative right quasigroups.
InstallOtherMethod( PrintObj, "for right quasigroup",
    [ IsRightQuasigroup ], RQ_rank, 
function( Q )    
    Print( PrintString( Q ) );
    return true;
end );

# DisplayString
# PROG: It appears that GAP "Display" does not often call "DisplayString" and, in addition,
# "DisplayString" is often weird. For instance, DisplayString(0) returns "<object>/n".
# We therefore call ViewString for elements of the underlying set.
InstallOtherMethod( DisplayString, "for right quasigroup",
    [ IsRightQuasigroup], RQ_rank,
function( Q )
    local s, n, i;
    if HasName( Q ) then 
        return Name( Q );
    fi;
    s := ViewString( Q );
    s := s{[1..Length(s)-1]}; # remove '>' from ViewString
    s := Concatenation( s, " on " );
    n := Minimum( Size(Q), 5 );
    for i in [1..n] do
        s := Concatenation( s, String( UnderlyingSetElm( Elements( Q )[ i ] ) ) ); 
        if i<n then 
            s := Concatenation( s, ", ");
        fi;
    od;
    if Size( Q ) > 5 then
        s := Concatenation( s, ", ...");
    fi;
    s := Concatenation( s, ">" );
    # PROG: A linebreak should be included per GAP specifiations but it causes problems
    # since some GAP functions call DisplayString when they are supposed to call
    # ViewString, for instance, EquivalenceRelationByPartition.
    return s;
end );

InstallOtherMethod( DisplayString, "for right quasigroup element",
    [ IsRightQuasigroupElement ], 
function( obj )
    local F;
    F := FamilyObj( obj );
    return Concatenation( F!.names, DisplayString( UnderlyingSetElm( obj ) ) ); # REVISIT: This might display weird things, perhaps change later.
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
    IsIdenticalObj, # REVISIT: Why is this here? Does it only check that the families are the same?
    [ IsRightQuasigroupElement, IsRightQuasigroupElement ],
function( x, y )
    return x![ 1 ] = y![ 1 ] and FamilyObj( x ) = FamilyObj( y );
end );

# PROG: Faster method for checking equality of two right quasigroups, avoiding generating sets.
# Rank is higher to be called (setting it to 2 does not do the trick).
# It's not clear what is called otherwise if the rank is low.
InstallOtherMethod( \=, "for two right quasigroups",
	[ IsRightQuasigroup, IsRightQuasigroup ], 10,
function( Q1, Q2 )
	return FamilyObj( Q1.1 ) = FamilyObj( Q2.1 ) and Size( Q1 ) = Size( Q2 ) and ForAll( Q1, x -> x in Q2 );
end );

# \<

InstallMethod( \<, "for two elements of a right quasigroup",
    IsIdenticalObj,
    [ IsRightQuasigroupElement, IsRightQuasigroupElement ],
function( x, y )
    return x![ 1 ] < y![ 1 ] and FamilyObj( x ) = FamilyObj( y );
end );

# \[\]

# PROG: Speed test
# call Q.100 one million times on Q = ProjectionRightQuasigroup([1..1000], ConstructorStyle(false,false))

InstallOtherMethod( \[\], "for right quasigroup and element of the underlying set",
	[ IsRightQuasigroup, IsObject ],
function( Q, x )
    # this allows Q[x] for x in the underlying set of the parent of Q (so Q[x] need not be in Q)
    local F, pos;
    F := FamilyObj( Q.1 );
    pos := PositionSorted( F!.uSet, x );
    if F!.set[ pos ] <> x then
        Error("RQ: <2> must be en element of the underlying set of <1>");
    fi; 
    return F!.set[ pos ]; # 750ms
    #if F!.indexBased then
    #    #return Objectify( TypeObj( Q.1 ), [ PositionSorted( F!.uSet, x) ] ); # 1406ms
    #fi;
    #return Objectify( TypeObj( Q.1 ), [ x ] ); # 1172ms
end ); 

# \.

# PROG: rank is set high so that associative right quasigroups do not call \. for semigroups
InstallMethod( \., "for right quasigroup and positive integer",
	[ IsRightQuasigroup, IsPosInt ], RQ_rank, 
function( Q, i )
    # PROG: the HasParent( Q ) test is necessary since Objectify in constructors
    # calls this for some reason and Parent( Q ) is not yet set at that point.
    if HasParent( Q ) then  
        return AsList( Parent( Q ) )[ Int( NameRNam( i ) ) ]; # 359 ms 
        #return Elements( Parent( Q ) )[  Int( NameRNam( i ) ) ]; # 469ms 
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
    # Speed test: compare running times on 10 million products in an index based right quasigroup of size 100
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

InstallOtherMethod( \*, "for right quasigroup element and a right quasigroup",
    [ IsRightQuasigroupElement, IsRightQuasigroup ],
function( x, Q )
    if not x in Parent( Q ) then
        Error( "RQ: <1> must be an element of the parent of <2>.");
    fi;
    return x*Elements(Q);  
end );

InstallOtherMethod( \*, "for right quasigroup and a right quasigroup element",
    [ IsRightQuasigroup, IsRightQuasigroupElement ],
function( Q, x )
    if not x in Parent( Q ) then
        Error( "RQ: <1> must be an element of the parent of <2>.");
    fi;
    return Elements(Q)*x;
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

InstallOtherMethod( \/, "for right quasigroup element and a right quasigroup",
    [ IsRightQuasigroupElement, IsRightQuasigroup ],
    0,
function( x, Q )
    if not x in Parent( Q ) then
        Error( "RQ: <1> must be an element of the parent of <2>.");
    fi;
    return x/Elements(Q);  
end );

InstallOtherMethod( RightQuotient, "for right quasigroup element and a right quasigroup",
    [ IsRightQuasigroupElement, IsRightQuasigroup ],
    0,
function( x, Q )
    return x/Q;
end );

InstallOtherMethod( \/, "for right quasigroup and a right quasigroup element",
    [ IsRightQuasigroup, IsRightQuasigroupElement ],
    0,
function( Q, x )
    if not x in Parent( Q ) then
        Error( "RQ: <2> must be an element of the parent of <1>.");
    fi;
    return Elements(Q)/x;
end );

InstallOtherMethod( RightQuotient, "for right quasigroup and a right quasigroup element",
    [ IsRightQuasigroup, IsRightQuasigroupElement ],
    0,
function( Q, x )
    return Q/x;
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
    1, # PROG: else a more generic outside method is called
function( ls, y )
    return List( ls, x -> LeftQuotient(x,y) );
end );

InstallOtherMethod( LeftQuotient, "for quasigroup element and list of quasigroup elements",
    [ IsQuasigroupElement, IsList ],
    0,
function( x, ls )
    return List( ls, y -> LeftQuotient(x,y) );
end );

InstallOtherMethod( LeftQuotient, "for quasigroup element and a quasigroup",
    [ IsQuasigroupElement, IsQuasigroup ],
    0,
function( x, Q )
    return LeftQuotient( x, Elements(Q) );
end );

InstallOtherMethod( LeftQuotient, "for quasigroup and a quasigroup element",
    [ IsQuasigroup, IsQuasigroupElement ],
    1, # PROG: else a more generic outside method is called
function( Q, x )
    return LeftQuotient( Elements(Q), x );
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

InstallMethod( \^, "for right quasigroup element and transformation",
    [ IsRightQuasigroupElement, IsTransformation ],
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

InstallMethod( ParentInd, "for right quasigroup",
    [ IsRightQuasigroup ],
function( Q )
    # PROG: Attempting to make this fast.
    # For some reason it is slower than ParentInd( Elements( Q ) ), even without saving the attribute.
    # The test IsIdenticalObj( Q, Parent( Q ) ) is fast.
    # It looks like just calling a method for IsRightQuasigroup is slower than calling one for IsList.
    local F;
    if IsIdenticalObj( Q, Parent( Q ) ) then
        return [1..Size(Q)];
    fi;
    F := FamilyObj( Q.1 );
    if F!.indexBased then
        return List( Q, x -> x![1] );
    fi;
    return List( Q, x -> PositionSorted( F!.uSet, x![1] ) );
end );

InstallOtherMethod( ParentInd, "for right quasigroup element",
    [ IsRightQuasigroupElement ],
function( x )
    local F;
    F := FamilyObj( x );
    if F!.indexBased then # fast
        return x![1];
    fi;
    return PositionSorted( F!.uSet, x![1] ); # PROG: much faster than PositionSorted( F!.set, x )
end );

InstallOtherMethod( ParentInd, "for list of right quasigroup elements",
    [ IsList ],
    ls -> List( ls, ParentInd )
);

InstallOtherMethod( ParentInd, "for mapping",
    [ IsMapping ],
function( m )  
    if IsBijective( m ) and Source( m ) = Range( m ) then
        return AsParentPerm( m );
    fi;
    return AsParentTransformation( m );
end );

# IsIndexBased

InstallMethod( IsIndexBased, "for right quasigroup",
    [ IsRightQuasigroup ],
function( Q )
    return FamilyObj( Q.1 )!.indexBased;
end );

# IndexBasedCopy
# PROG: constructor OK, calls RQ_AlgebraByCayleyTable
InstallMethod( IndexBasedCopy, "for right quasigroup",
    [ IsRightQuasigroup ],
function( Q )
    local copyQ;
    copyQ := RQ_AlgebraByCayleyTable( CategoryOfRightQuasigroup( Q ), CayleyTable( Q ), ConstructorStyle( true, false ) );
    RQ_InheritProperties( Q, copyQ, false );
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
# PROG: constructor OK, calls RQ_AlgebraByCayleyTable
InstallMethod( CanonicalCopy, "for right quasigroup",
    [ IsRightQuasigroup ],
function( Q )
    local copyQ;
    copyQ := RQ_AlgebraByCayleyTable( CategoryOfRightQuasigroup( Q ), MultiplicationTable( Q ), ConstructorStyle( true, false ) );
    RQ_InheritProperties( Q, copyQ, false );
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
    if Length( gens ) < Length( GeneratorsOfRightQuasigroup( Q ) ) then
        # PROG: Value cannot be reset. Must unbind first.
        Unbind( Q!.GeneratorsOfMagma );
        Q!.GeneratorsOfMagma := ShallowCopy( gens );
    fi;
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
    gens := Set( gens );
    if Length( gens ) < Length( GeneratorsOfMagma( Q ) ) then
        Unbind( Q!.GeneratorsOfMagma );
        Q!.GeneratorsOfMagma := ShallowCopy( gens );
    fi;
    return gens;
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

# MAPPINGS, PARENT PERMUTATIONS AND CANONICAL PERMUTATIONS
# _____________________________________________________________________________

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
