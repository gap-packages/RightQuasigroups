#
# RightQuqsigroups: Computing with one-sided quasigroups in GAP.
#
# Implementations
#

# NOTE:
# F!.indexBased means the following:
# true: the multiplication and right divison functions are NxN->N and elements are [n], has mult and rdiv tables
# false: SxS->S and [x]

InstallMethod( ProjectionRightQuasigroup, "for a collection",
	[ IsCollection ],
function( S )
	return ProjectionRightQuasigroup( S, false );
end );

InstallOtherMethod( ProjectionRightQuasigroup, "for a collection and a boolean",
	[ IsCollection, IsBool ],
function( S, indexBased )
	local f;
	f := function(x,y) return x; end;
	return RightQuasigroupByFunctionsNC( S, f, f, indexBased );
end );

InstallMethod( RightQuasigroupByFunctionsNC, "for a collection, two functions and a boolean",
    [ IsCollection, IsFunction, IsFunction, IsBool ],
function( S, f, g, indexBased )
    local F, n, rdiv, i, j, k, Q;
    # constructing the family
    F := NewFamily( "RightQuasigroupByFunctionsFam", IsRightQuasigroupElement );
    # installing data for the family
    n := Size( S );
    F!.size := n;
    F!.uset := ShallowCopy( Set( S ) );
    F!.indexBased := indexBased;
    # ? make it automatically index based when S = [1..n] ?
    if indexBased then
        F!.set := Immutable( List( [1..n], i -> Objectify(
            NewType( F, IsRightQuasigroupElement and IsRightQuasigroupElmRep ), [ i ] ) ) );
    else 
       F!.set := Immutable( List( F!.uset, x -> Objectify(
            NewType( F, IsRightQuasigroupElement and IsRightQuasigroupElmRep ), [ x ] ) ) );
    fi;        
    if indexBased then # index based, precalculate data
        # *** this could be used for MakeIndexBased
        F!.multTable := List([1..n], i -> List([1..n],
            j -> PositionSorted( F!.uset, f( F!.uset[i], F!.uset[j] ) )
        ) );
        rdiv := List([1..n], i -> [1..n]);
        for i in [1..n] do for j in [1..n] do
            k := F!.multTable[i][j];
            rdiv[k][j] := i;
        od; od;
        F!.rdivTable := rdiv;
        F!.mult := function( i, j ) return F!.multTable[i][j]; end;
        F!.rdiv := function( i, j ) return F!.rdivTable[i][j]; end;
        # *** end MakeIndexBased
    else # not index based
        F!.mult := f;
	    F!.rdiv := g;
    fi;
    F!.names := "r"; # default prefix of elements
    # creating the quasigroup
    Q := Objectify( NewType( FamilyObj( F!.set ),
        IsRightQuasigroup and IsAttributeStoringRep ), rec() );
    # setting attributes for the quasigroup
    SetSize( Q, n );
    SetAsSSortedList( Q, F!.set );
    SetParent( Q, Q );
    if indexBased then
        SetMultiplicationTable( Q, F!.multTable );
        # TO DO: set right division table here
    fi;
    return Q;
end );

InstallMethod( RightQuasigroupByFunctions, "for a collection and two functions",
    [ IsCollection, IsFunction, IsFunction ],
function( S, f, g )
	return RightQuasigroupByFunctions( S, f, g, false );
end );

InstallOtherMethod( RightQuasigroupByFunctions, "for a collection, two functions and a boolean",
    [ IsCollection, IsFunction, IsFunction, IsBool ],
function( S, f, g, indexBased )
	if ForAll(S,x->ForAll(S,y->g(f(x,y),y)=x and f(g(x,y),y)=x)) then
		return RightQuasigroupByFunctionsNC(S,f,g,indexBased);
	else 
		Error( "RQ: the functions do not define a right quasigroup" );
	fi;
end );


InstallMethod( RightQuasigroupByFunction, "for a collection and a function",
    [ IsCollection, IsFunction ],
function( S, f )
    return RightQuasigroupByFunction( S, f, false );
end );


InstallOtherMethod( RightQuasigroupByFunction, "for a collection, a function and a boolean",
    [ IsCollection, IsFunction, IsBool ],
function( S, f, indexBased)
    local g;
    g := function(x,y) return First( S, z -> x = f(z,y) ); end;
    return RightQuasigroupByFunctions( S, f, g, indexBased );
end );

InstallMethod( RightQuasigroupBySection, "for a finite ordered collection and a collection of group elements",
    [ IsCollection, IsCollection ],
function( S, section )
    return RightQuasigroupBySection( S, section, false );
end );

InstallOtherMethod( RightQuasigroupBySection, "for a finite ordered collection, a collection of group elements and a boolean",
    [ IsCollection, IsCollection, IsBool ],
function( S, section, indexBased )
    # CHANGE THIS TO INDEX BASED REPRESENTATION ?
	local f, g, Q, F;
    S := Set(S); # just in case
    if indexBased then
        f := function(x,y) return x^section[ PositionSorted( S, y ) ]; end;
        g := function(x,y) return x^((section[ PositionSorted( S, y ) ])^(-1)); end;
        return RightQuasigroupByFunctionsNC( S, f, g, true );
    fi;
    # not index based (tricky, the mult and rdiv functions refer to F)
    Q := ProjectionRightQuasigroup( S, false );
    F := FamilyObj( Elements(Q)[1] );
    F!.sec := ShallowCopy( section );
    F!.mult := function(x,y) return x^(F!.sec[ PositionSorted( F!.uset, y ) ]); end;
    F!.rdiv := function(x,y) return x^((F!.sec[ PositionSorted( F!.uset, y ) ])^(-1)); end;
    return Q;
end );

InstallMethod( CayleyTable, "for a right quasigroup",
    [ IsRightQuasigroup ],
function( Q )
    return List(Q, x -> List(Q, y -> x*y) );
end );

InstallOtherMethod( MultiplicationTable, "for a right quasigroup",
    [ IsRightQuasigroup ],
function( Q )
    return List(Q, x -> List(Q, y -> PositionSorted( Elements(Q), x*y ) ) );
end);

InstallMethod( RightQuasigroupByCayleyTable, "for a matrix",
    [ IsRectangularTable ],
function( M )
    return RightQuasigroupByCayleyTable( M, false );
end );

InstallOtherMethod( RightQuasigroupByCayleyTable, "for a matrix and a boolean",
    [ IsRectangularTable, IsBool ],
function( M, indexBased )
    local n, S, f, g, Q, F;
    n := Length( M );
    S := Set( M{[1..n]}[1] ); # first column
    if indexBased then
        # change S to [1..n] and then change it back
        if S=[1..n] then
            f := function( i, j ) return M[i][j]; end; # quick for canonical situations
        else
            f := function( i, j ) return PositionSorted( S, M[i][j] ); end;
        fi;
        Q := RightQuasigroupByFunction( S, f, true ); # still slow rdiv for S=[1..n]
        F := FamilyObj( Elements(Q)[1] );
        F!.uset := S; 
        return Q;
    fi;
    # not index based
    Q := ProjectionRightQuasigroup( S );
    F := FamilyObj( Elements(Q)[1] );
    F!.cayleyTable := ShallowCopy( M );
    F!.mult := function( x, y )
        return F!.cayleyTable[ PositionSorted( F!.uset, x ) ][ PositionSorted( F!.uset, y )];
    end;
    F!.rdiv := function( x, y )
        return First( F!.uset,  z -> F!.cayleyTable[ PositionSorted( F!.uset, z ) ][ PositionSorted( F!.uset, y )] = x );
    end; 
    return Q;
end );

#############################################################################
##  
#O  SubrightQuasigroupNC( Q, pos_of_gens )
##    
##  This auxiliary function assumes that:
##    a) Q is a quasigroup with Q = Parent( Q )
##    b) pos_of_gens is a nonempty subset of [ 1..Size( Q ) ] (not a sublist)
##    c) pos_of_gens determines a subquasigroup of Q
##  It then returns the corresponding subquasigroup of Q.
 
 InstallMethod( SubrightQuasigroupNC, "for a right quasigroup and a collection of elements", 
    [ IsRightQuasigroup, IsCollection ],
function( Q, pos_of_gens )
    local subqg, Qtype, elms;
    if IsEmpty( pos_of_gens ) then
        Error( "RQ: <2> must be a nonempty subset.");
    fi;
    if not( Q = Parent( Q ) ) then
        Error( "RQ: <1> must be its own parent." );
    fi;
#    elms := Immutable( Elements( Q ){ pos_of_gens } );
    elms := Immutable( pos_of_gens );
    Qtype := NewType( FamilyObj( elms ), IsRightQuasigroup and IsAttributeStoringRep );
    subqg := Objectify( Qtype, rec( ) );
    SetSize( subqg, Length( pos_of_gens ) );
    SetAsSSortedList( subqg, elms );
    SetParent( subqg, Q );
    return subqg;
end );

#############################################################################
##  
#O  SubrightQuasigroup( Q, gens )
#O  SubrightQuasigroup( Q, gens, elms_of_uset )
##  
##   <A>Q</A> is a right quasigroup and <A>gens</A> is one of the following:
##  
##   1) list of elements of <A>Q</A>
##   2) list of integers between 1 and the size of the parent of <A>Q</A>
##   3) list of elements of the underlying set of <A>Q</A>
##  
##   Returns: the corresponding generated sub right quasigroup of <A>Q</A>.
##   With identical input, 2) and 3) may return different result when the 
##   underlying set of the parent of <A>Q</A> consists of non-consequtive 
##   positive integers. In this case, in order to obtain 3), one has to add
##   <C>true</C> as third argument.

InstallMethod( SubrightQuasigroup, "for a right quasigroup and a list of elements",
    [ IsRightQuasigroup, IsCollection ],
function( Q, gens )
    local F, gen_elms, new_elms, transl, relmultgr, subqg;
    if IsEmpty( gens ) then
        return []; # do something with the empty quasigroup
    fi;
    F := FamilyObj( Elements(Q)[1] );
    if ForAll( gens, g -> IsPosInt(g) and g<=Size(Parent(Q) ) ) then
        gen_elms:=List(gens,i->Parent(Q).(i));
    else
        gen_elms := gens;
    fi;
    if not ForAll( gen_elms, g -> g in Q ) then
        Error( "RQ: <2> must be a list of elements of quasigroup <1> or their indices" );
    fi;
    # calculating the subquasigroup
    new_elms := [];
    while gen_elms<>new_elms do
        gen_elms := Union(gen_elms,new_elms);
        new_elms := Union( List(gen_elms,x->List(gen_elms,y->x*y)) );
#        transl := Union( LeftSection( Parent( Q ) ){ pos_gens }, RightSection( Parent( Q ) ){ pos_gens } );
#        relmultgr := Subgroup( MultiplicationGroup( Parent( Q ) ), transl );
#        pos_gens := Set( Orbits( relmultgr, pos_gens )[ 1 ] );
    od;
    subqg := SubrightQuasigroupNC( Parent( Q ), gen_elms );
    SetGeneratorsOfMagma( subqg, gen_elms );
    return subqg;
end );

InstallMethod( SubrightQuasigroup, "for a right quasigroup and a list of elements",
    [ IsRightQuasigroup, IsCollection, IsBool ],
function( Q, gens, bubu )
    return fail;
end );

#############################################################################
##
#O  SetQuasigroupElmName( Q, name )
##
##  Changes the name of elements of quasigroup or loop <Q> to <name>

InstallMethod( SetRightQuasigroupElmName, "for right quasigroup and string",
    [ IsRightQuasigroup, IsString ],
function( Q, name )
    local F;
    F := FamilyObj( Elements( Q )[ 1 ] );
    F!.names := name;
end);
 
InstallMethod( ViewObj, "for right quasigroup",
    [ IsRightQuasigroup ],
function( Q )
    Print( "<right quasigroup of order ", Size( Q ), ">" );
end );

#############################################################################
# ELEMENTS

InstallMethod( PrintObj, "for a right quasigroup element",
    [ IsRightQuasigroupElement ],
function( obj )
    local F, s;
    F := FamilyObj( obj );
    if F!.indexBased then
        s := String( F!.uset[ obj![1] ] );
    else
        s := String( obj![1] );
    fi;
    Print( Concatenation( F!.names, "[", s, "]" ) );
end );

InstallMethod( \=, "for two elements of a right quasigroup",
    IsIdenticalObj,
    [ IsRightQuasigroupElement, IsRightQuasigroupElement ],
function( x, y )
    return FamilyObj( x ) = FamilyObj( y ) and x![ 1 ] = y![ 1 ];
end );

InstallMethod( \<, "for two elements of a right quasigroup",
    IsIdenticalObj,
    [ IsRightQuasigroupElement, IsRightQuasigroupElement ],
function( x, y )
    return FamilyObj( x ) = FamilyObj( y ) and x![ 1 ] < y![ 1 ];
end );

InstallOtherMethod( \[\], "for a right quasigroup and an element",
	[ IsRightQuasigroup, IsObject ],
function( Q, x )
    # this allows Q[x] for x in the underlying set of the parent of Q
    local F;
    F := FamilyObj( Elements(Q)[1] );
    if not x in F!.uset then
        Error("RQ: <2> must be en element of the underlying set of <1>");
    fi; 
    if F!.indexBased then
        return Objectify( TypeObj( Elements(Q)[1] ), [ PositionSorted( F!.uset, x) ] );
    fi;
    return Objectify( TypeObj( Elements(Q)[1] ), [ x ] ); 
end ); 

InstallMethod( \., "for a right quasigroup and an integer",
	[ IsRightQuasigroup, IsPosInt ],
function( Q, i )
	return Elements(Q)[ Int( NameRNam( i ) ) ];
end );

#############################################################################
# OPERATIONS

InstallMethod( \*, "for two right quasigroup elements",
    IsIdenticalObj,
    [ IsRightQuasigroupElement, IsRightQuasigroupElement ],
function( x, y )
    local F;
    F := FamilyObj( x );
   	return Objectify( TypeObj(x), [ F!.mult( x![ 1 ], y![ 1 ] ) ] );
end );

InstallOtherMethod( \/, "for two right quasigroup elements",
    IsIdenticalObj,
    [ IsRightQuasigroupElement, IsRightQuasigroupElement ],
function( x, y )
    local F;
    F := FamilyObj( x );
	return Objectify( TypeObj(x), [ F!.rdiv( x![ 1 ], y![ 1 ] ) ] );
end );

#############################################################################
# SECTIONS

InstallMethod( PosInParent, "for an element of a right quasigroup",
    [ IsRightQuasigroupElement ],
function( x )
    local F;
    F := FamilyObj( x );
    if F!.indexBased then
        return x![1];
    fi;
    return PositionSorted( F!.set, x );
end );

InstallOtherMethod( PosInParent, "for a collection of elements in a right quasigroup",
    [ IsCollection ],
function( ls )
    return List( ls, PosInParent );
end );

InstallOtherMethod( PosInParent, "for a right quasigroup",
    [ IsRightQuasigroup ],
function( Q )
    return PosInParent( Elements( Q ) );
end );

InstallMethod( RightTranslation, "for a right quasigroup and its element",
    [ IsRightQuasigroup, IsRightQuasigroupElement ],
function( Q, x )
    local P, n, pos, t, i;
    if not x in Q then
        Error("RQ: <2> must be an element of <1>");
    fi;
    P := Parent( Q );
    n := Size( P );
    pos := PosInParent( Q );
    t := [1..n];
    for i in [1..n] do
        if i in pos then
            t[i] := PosInParent( (P.(i))*x );
        fi;
    od;
    return PermList( t );
end );

InstallOtherMethod( RightTranslation, "for a right quasigroup and its element given by index",
    [ IsRightQuasigroup, IsPosInt ],
function( Q, i )
    return RightTranslation( Q, Elements(Parent(Q))[i] );
end );

InstallMethod( RightSection, "for a right quasigroup",
    [ IsRightQuasigroup ],
function( Q )
    return List( Elements( Q ), x -> RightTranslation( Q, x ) );
end );

#############################################################################
##  CONSTRUCTIONS

InstallMethod( RightCoreOfGroup, "for a group",
    [ IsGroup ], 
function(G)
    local f;
    f := function(x,y) return y*x^(-1)*y; end;
    return RightQuasigroupByFunctionsNC( G, f, f, false );
end );


###################
## TO DO:
# RightMultiplicationGroup
# Actions by permutations
# IntoRightQuasigroup
# Isomorphism checks
# Automorphism group
# Factoring by congruences
# TOWARD RACKS AND QUANDLES:
# displacement group
# checking basic properties (is affine?)
# affine representations
# better isom checks
# ID function
# library of small racks and quandles