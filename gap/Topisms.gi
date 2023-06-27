# Topisms.gi
# Homotopisms, isotopisms and autotopisms of right quasigroups
# =============================================================================

# HOMOTOPISMS OF RIGHT QUASIGROUPS
# _____________________________________________________________________________

# HomotopismRightQuasigroups
# PROG: We expect right quasigroups mappings, or transformations, or permutations
InstallOtherMethod( HomotopismRightQuasigroups, "for two right quasigroups, three mappings, and bool", 
    [ IsRightQuasigroup, IsRightQuasigroup, IsObject, IsObject, IsObject, IsBool ],
function( source, range, f, g, h, isCanonical )
    local ind, t;
    # check source and range if mappings are right quasigroup mappings
    if ForAny( [f,g,h], x -> IsMapping( x ) and Source( x )<>source and Range( x )<>range ) then
        return fail;
    fi;
    # convert to parent permutations (if source = range) or parent transformations
    if source = range then 
        if (not IsPerm( f )) or isCanonical then f := AsParentPerm( source, f ); fi;
        if (not IsPerm( g )) or isCanonical then g := AsParentPerm( source, g ); fi;
        if (not IsPerm( h )) or isCanonical then h := AsParentPerm( source, h ); fi;
    else 
        if (not IsTransformation( f )) or isCanonical then f := AsParentTransformation( source, range, f ); fi;
        if (not IsTransformation( g )) or isCanonical then g:= AsParentTransformation( source, range, g ); fi;
        if (not IsTransformation( h )) or isCanonical then h:= AsParentTransformation( source, range, h ); fi;
    fi;
    # the actual test of homotopic property f(x).g(y) = h(x*y)
    ind := ParentInd( source );
    if not ForAll( ind, i -> ForAll( ind, j -> range.(i^f)*range.(j^g) = range.( ParentInd( source.(i)*source.(j) )^h ) ) ) then
        return fail;
    fi; 
    t := Objectify( RQ_HomotopismType, rec( source := source, range := range, f := f, g := g, h := h ) );
    # PROG: We now set properties IsSingleValued and IsTotal to true since the property IsBijective in GAP is handled
    # as an immediate method "IsSingleValued and IsTotal and IsInjective and IsSurjective". This way we do not have to
    # implement the methods.
    if not t=fail then
        SetIsSingleValued( t, true );
        SetIsTotal( t, true );
    fi;
    # PROG: We perform a cheap test for injectivity and surjectivity.
    if IsPerm( t!.f ) then # automatically bijective
        SetIsBijective( t, true );
    fi;
    return t;
end );

InstallOtherMethod( HomotopismRightQuasigroups, "for two right quasigroups and three mappings",
    [ IsRightQuasigroup, IsRightQuasigroup, IsObject, IsObject, IsObject ],
function( source, range, f, g, h )
    return HomotopismRightQuasigroups( source, range, f, g, h, false );
end );

InstallOtherMethod( HomotopismRightQuasigroups, "for a right quasigroup, three mappings and bool",
    [ IsRightQuasigroup, IsObject, IsObject, IsObject, IsBool ],
function( Q, f, g, h, isCanonical )
    return HomotopismRightQuasigroups( Q, Q, f, g, h, isCanonical );
end );

InstallOtherMethod( HomotopismRightQuasigroups, "for a right quasigroup and three mappings",
    [ IsRightQuasigroup, IsObject, IsObject, IsObject ],
function( Q, f, g, h )
    return HomotopismRightQuasigroups( Q, Q, f, g, h, false );
end );
 
InstallMethod( HomotopismRightQuasigroups, "for three right quasigroup mappings",
    [ IsMapping, IsMapping, IsMapping ],
function( f, g, h )
    return HomotopismRightQuasigroups( Source( f ), Range( f ), f, g, h, false ); 
end );

InstallMethod( ViewString, "for right quasigroup homotopism",
    [ IsRightQuasigroupHomotopism ],
function( t )
    local props, s, category;
    if t!.source = t!.range and t!.f = () and t!.g = () and t!.h = () then
        return "<identity autotopism>";
    fi;
    s := "<";
    props := [ HasIsInjective( t ) and IsInjective( t ), HasIsSurjective( t ) and IsSurjective( t ), t!.source = t!.range ];
    if props = [true, true, true] then
        Append( s, "autotopism" );
    elif props = [true, true, false ] then
        Append( s, "isotopism" );
    elif props[1]=true then 
        Append( s, "injective homotopism" );
    elif props[2]=true then
        Append( s, "surjective homotopism" );
    else 
        Append( s, "homotopism" );
    fi;
    Append( s, " of ");
    category := CategoryOfRightQuasigroup( [ t!.source, t!.range ] );
    if category = IsLoop then
        Append( s, "loops" );
    elif category = IsQuasigroup then
        Append( s, "quasigroups" );
    else
        Append( s, "right quasigroups" );
    fi;
    Append( s, ">" );
    return s;
end );

InstallMethod( DisplayString, "for right quasigroup homotopism",
    [ IsRightQuasigroupHomotopism ],
function( t )
    local s;
    s := ViewString( t );
    Remove(s); # remove last character
    if s[3] = 'd' then # identity autotopism, special treatment
        Append( s, " on "); Append( s, String( t!.source ) ); Append( s, ">");
        return s;
    fi;
    if t!.source <> t!.range then
        Append( s, "\n   source = " ); Append( s, String( t!.source ) );
        Append( s, "\n   range = " ); Append( s, String( t!.range ) );
    else
        Append( s, "\n   source = range = "); Append( s, String( t!.source ) );
    fi;
    Append( s, "\n   f = " ); Append( s, String( t!.f ) );
    Append( s, "\n   g = " ); Append( s, String( t!.g ) );
    Append( s, "\n   h = " ); Append( s, String( t!.h ) );
    Append( s, "\n>" );
    return s;
end );

InstallMethod( PrintObj, "for right quasigroup homotopism",
	[ IsRightQuasigroupHomotopism ],
function( t )
	Print( "HomotopismRightQuasigroups( ", t!.source, ", ", t!.range, ", ", t!.f, ", ", t!.g, ", ", t!.h, ")" );
end );

# attributes and properties of homotopisms

InstallMethod( Source, "for right quasigroup homotopism",
    [ IsRightQuasigroupHomotopism ],
    t -> t!.source
);

InstallMethod( Range, "for right quasigroup homotopism",
    [ IsRightQuasigroupHomotopism ],
    t -> t!.range
);

InstallMethod( ComponentOfHomotopism, "for right quasigorup homotopism and integer",
    [ IsRightQuasigroupHomotopism, IsInt ],
function( t, i )
    if i=1 then
        return t!.f;
    elif i=2 then
        return t!.g;
    elif i=3 then 
        return t!.h;
    fi;
    return fail;
end );

# AutotopismRightQuasigroup 
InstallGlobalFunction( AutotopismRightQuasigroup,
function( arg )
    local t;
    t := CallFuncList( HomotopismRightQuasigroups, arg );
    # check for endo and bijectivity
    if t = fail then return fail; fi;
    if ( Source( t ) <> Range( t ) ) or ( not IsBijective( t ) ) then
        return fail;
    fi;
    return t;
end );

# operations for homotopisms

InstallMethod( \=, "for two right quasigroup homotopisms",
	IsIdenticalObj,
	[ IsRightQuasigroupHomotopism, IsRightQuasigroupHomotopism ],
function( u, v )
    return [ u!.source, u!.range, u!.f, u!.g, u!.h ] = [v!.source, v!.range, v!.f, v!.g, v!.h ];
end );

InstallMethod( \<, "for two right quasigroup homotopisms",
	IsIdenticalObj,
	[ IsRightQuasigroupHomotopism, IsRightQuasigroupHomotopism ],
function( u, v )
	return [ u!.source, u!.range, u!.f, u!.g, u!.h ] < [ v!.source, v!.range, v!.f, v!.g, v!.h ];
end );

InstallMethod( \*, "for two right quasigroup homotopisms",
    IsIdenticalObj,
    [ IsRightQuasigroupHomotopism, IsRightQuasigroupHomotopism ],
function( u, v )
    if not u!.range = v!.source then
        Error( "RQ: The two homotopisms cannot be composed. ");
    fi;
    return HomotopismRightQuasigroups( u!.source, v!.range, u!.f*v!.f, u!.g*v!.g, u!.h*v!.h, false );
end );

InstallOtherMethod( OneMutable, "for right quasigroup homotopism",
	[ IsRightQuasigroupHomotopism ],
function( t )
    if t!.source<>t!.range then
        return fail;
    fi;
    return HomotopismRightQuasigroups( t!.source, (), (), () );
end );

InstallMethod( IdentityAutotopism, "for right quasigroup",
    [ IsRightQuasigroup ],
function( Q )
    return HomotopismRightQuasigroups( Q, (), (), () );
end );

InstallOtherMethod( InverseMutable, "for a right quasigroup homotopism",
	[ IsRightQuasigroupHomotopism ],
function( t )
    if not IsBijective( t ) then 
        return fail;
    fi;
    return HomotopismRightQuasigroups( t!.range, t!.source, (t!.f)^-1, (t!.g)^-1, (t!.h)^-1, false );
end );

# IsHomotopismRightQuasigroups
InstallGlobalFunction( IsHomotopismRightQuasigroups, 
function( arg )
# PROG: The constructor checks the homotopic property and does all the needed conversions.
    return CallFuncList( HomotopismRightQuasigroups, arg ) <> fail;
end );

# IsHomotopismQuasigroups
InstallGlobalFunction( IsHomotopismQuasigroups,
function( arg )
    if not ForAll( Filtered( arg, IsRightQuasigroup ), IsQuasigroup ) then return false; fi;
    return CallFuncList( IsHomotopismRightQuasigroups ) <> fail;
end );

# IsHomotopismLoops
InstallGlobalFunction( IsHomotopismLoops,
function( arg )
    if not ForAll( Filtered( arg, IsRightQuasigroup ), IsLoop ) then return false; fi;
    return CallFuncList( IsHomotopismRightQuasigroups ) <> fail;
end );

# IsInjective
InstallOtherMethod( IsInjective, "for right quasigroup homotopism",
    [ IsRightQuasigroupHomotopism ],
function( t )
    local ind, func;
    if IsPerm( t!.f ) then # perms are injective by definition
        return true;
    fi; 
    ind := ParentInd( t!.source );
    func := IsInjectiveListTrans;
    return ( func( ind, t!.f ) and func( ind, t!.g ) and func( ind, t!.h ) );
end );

# IsSurjective
InstallOtherMethod( IsSurjective, "for right quasigroup homotopism",
    [ IsRightQuasigroupHomotopism ],
function( t )
    local sourceInd, rangeInd;
    if IsPerm( t!.f ) then # perms are surjective by definition
        return true;
    fi; 
    sourceInd := ParentInd( t!.source );
    rangeInd := ParentInd( t!.range );
    return ForAll( [t!.f, t!.g, t!.h ], p -> Set( sourceInd, i->i^p ) = rangeInd );
end );

# IsBijective
InstallOtherMethod( IsBijective, "for right quasigroup homotopism",
    [ IsRightQuasigroupHomotopism ],
function( t )
    return IsInjective( t ) and IsSurjective( t );
end );

# IsIsotopismRightQuasigroups
InstallGlobalFunction( IsIsotopismRightQuasigroups,
function( arg )
    local t;
    t := CallFuncList( HomotopismRightQuasigroups, arg );
    return t<>fail and IsBijective( t );
end );

# IsIsotopismQuasigroups
InstallGlobalFunction( IsIsotopismQuasigroups,
function( arg )
    local t;
    t := CallFuncList( HomotopismRightQuasigroups, arg );
    return t<>fail and IsBijective( t ) and CategoryOfRightQuasigroup( [t!.source, t!.range] ) in [ IsQuasigroup, IsLoop ];
end );

# IsIsotopismLoops
InstallGlobalFunction( IsIsotopismLoops,
function( arg )
    local t;
    t := CallFuncList( HomotopismRightQuasigroups, arg );
    return t<>fail and IsBijective( t ) and CategoryOfRightQuasigroup([t!.source, t!.range] ) in [ IsLoop ];
end );

# IsAutotopismRightQuasigroups
InstallGlobalFunction( IsAutotopismRightQuasigroups,
function( arg )
    local t;
    t := CallFuncList( IsIsotopismRightQuasigroups, arg );
    if t in [fail, false] then 
        return false;
    fi;
    return t!.source = t!.range;
end );

# IsAutotopismQuasigroups
InstallGlobalFunction( IsAutotopismQuasigroups,
function( arg )
    local t;
    t := CallFuncList( IsIsotopismQuasigroups, arg );
    if t in [fail, false] then 
        return false;
    fi;
    return t!.source = t!.range;
end );

# IsAutotopismLoops
InstallGlobalFunction( IsAutotopismLoops,
function( arg )
    local t;
    t := CallFuncList( IsIsotopismLoops, arg );
    if t in [fail, false] then 
        return false;
    fi;
    return t!.source = t!.range;
end );

InstallMethod( IsGeneratorsOfMagmaWithInverses,
    "for a collection of right quasigroup homotopisms",
    [ IsRightQuasigroupHomotopismCollection ],
    elms -> ForAll( elms, x -> 
        Range( x ) = Range( elms[1] ) 
        and Source( x ) = Range( elms[1] ) 
        and Inverse( x ) <> fail 
    ) 
);

# TWISTS OF RIGHT QUASIGROUPS
# _____________________________________________________________________________

# RQ_AlgebraTwistByParentTransformations
# PROG: constructor OK
# Many other constuctors are reduced to this one.
InstallMethod( RQ_AlgebraTwistByParentTransformations, "for category, right quasigroup, three transformations and record",
    [ IsOperation, IsRightQuasigroup, IsTransformation, IsTransformation, IsTransformation, IsRecord ],
function( category, Q, f, g, h, style )
    local n, ind, F, ct, ret, invf, invg, invh, e;  
    RQ_CompleteConstructorStyle( style );
    n := Size( Q );
    # checking arguments
    if style.checkArguments and not ForAll( [f,g,h], t -> IsParentTransformation( Q, Q, t ) ) then
        Error( "RQ: The transformations must all be parent transformations." );
    fi;
    if style.checkArguments then
        if Inverse( f ) = fail or Inverse( h ) = fail then
            Error( "RQ: The transformations <f> and <h> must be invertible." );
        fi;
        if category <> IsRightQuasigroup and Inverse( g ) = fail then
            Error( "RQ: The transformation <g> must be invertible." );
        fi;
        if category = IsLoop then # e = g^{-1}(f(x)\h^{-1}(x)) = f^{-1}(h^{-1}(x)/g(x)) must hold for all x
            invf := Inverse( f ); invg := Inverse( g ); invh := Inverse( h );
            e := ParentInd( LeftQuotient( Q.(1^f), Q.(1^invh) ) )^invg;
            if not ( ForAll( ParentInd(Q), i -> ParentInd( LeftQuotient( Q.(1^f), Q.(1^invh) ) )^invg = e )
                    and ForAll( ParentInd(Q), i -> ParentInd( Q.(i^invh)/Q.(i^g) )^invf = e ) ) then
                Error( "RQ: There is no identity element." );
            fi;
        fi;
    fi;
    # constructing the algebra
    if style.indexBased then # index based case
        ind := ParentInd( Q );
        F := FamilyObj( Q.1 );
        ct := List( ind, i -> List( ind, j -> F!.uSet[ ParentInd( Q.(i^f)*Q.(j^g) )^h ] ) );
        return RQ_AlgebraByCayleyTable( category, ct, ConstructorStyle( true, false ) );
    fi;
    # not index based, convert to canonical, which is useful here
    f := AsCanonicalTransformation( Q, Q, f );
    g := AsCanonicalTransformation( Q, Q, g );
    h := AsCanonicalTransformation( Q, Q, h );
    ret := RQ_AlgebraShell( category, UnderlyingSet( Q ), ConstructorStyle( false, false ) );
    F := FamilyObj( ret.1 );
    F!.f := ShallowCopy( f ); F!.g := ShallowCopy( g ); F!.h := ShallowCopy( h );
    F!.invf := ShallowCopy( Inverse( f ) ); F!.invh := ShallowCopy( Inverse( h ) );
    if category <> IsRightQuasigroup then
        F!.invg := ShallowCopy( Inverse( g ) );
    fi;
    F!.origElms := ShallowCopy( Elements( Q ) ); # we will need these to multiply in the pre-isotope
    F!.mult := function( x, y ) # x*y = h( f(x) g(y) )
        x := F!.origElms[ PositionSorted(F!.uSet,x)^F!.f ];
        y := F!.origElms[ PositionSorted(F!.uSet,y)^F!.g ];
        return F!.uSet[ PositionSorted( F!.origElms, x*y )^F!.h ]; 
    end;
    F!.rdiv := function( x, y ) # x/y = f^{-1}( h^{-1}(x)/g(y) )
        x := F!.origElms[ PositionSorted(F!.uSet,x)^F!.invh ];
        y := F!.origElms[ PositionSorted(F!.uSet,y)^F!.g ];
        return F!.uSet[ PositionSorted( F!.origElms, x/y )^F!.invf ]; 
    end;
    if category <> IsRightQuasigroup then
        F!.ldiv := function( x, y ) # x\y = g^{-1}( f(x)\h^{-1}(y) )
            x := F!.origElms[ PositionSorted(F!.uSet,x)^F!.f ];
            y := F!.origElms[ PositionSorted(F!.uSet,y)^F!.invh ];
            return F!.uSet[ PositionSorted( F!.origElms, LeftQuotient( x, y ) )^F!.invg ]; 
        end;
    fi;
    RQ_AddDefaultOperations( ret ); # might add One
    return ret;
end );

# RQ_AlgebraTwist
InstallMethod( RQ_AlgebraTwist, "for category and list of arguments",
    [ IsOperation, IsList ],
function( category, data )
    local Q, maps, isCanonical, i, style;
    # processing argument "data"
    # expects data = [f,g,h] or [Q,f,g,h] or [Q,f,g,h,isCanonical] with another optional argument for constructor style
    if IsMapping( data[1] ) then # looks like [f,g,h]
        Q := Source( data[1] ); maps := data{[1,2,3]}; 
    else
        Q := data[1]; maps := data{[2,3,4]}; 
    fi;
    # check category
    if ( category = IsLoop and not IsQuasigroup( Q ) ) or ( category <> IsLoop and not category( Q ) ) then
        Error( "RQ: <Q> must be a right quasigroup or quasigroup." );
    fi;
    # convert mappings to parent transformations
    isCanonical := First( data, x -> x=true ) <> fail;
    for i in [1..3] do
        if IsMapping( maps[i] ) then
            maps[i] := AsParentTransformation( maps[i] );
        fi;
        if IsPerm( maps[i] ) then 
            maps[i] := AsTransformation( maps[i] );
        fi;
        if IsTransformation( maps[i] ) and isCanonical then
            AsParentTransformation( Q, Q, maps[i] );
        fi;
    od;
    style := Last( data, IsRecord );
    if style = fail then # no style given
        style := RQ_defaultConstructorStyle;
    else
        RQ_CompleteConstructorStyle( style );
    fi;
    return RQ_AlgebraTwistByParentTransformations( category, Q, maps[1], maps[2], maps[3], style );    
end );

# RightQuasigroupTwist
InstallGlobalFunction( RightQuasigroupTwist,
function( arg )
    return RQ_AlgebraTwist( IsRightQuasigroup, arg );
end );

# QuasigroupTwist
InstallGlobalFunction( QuasigroupTwist,
function( arg )
    return RQ_AlgebraTwist( IsQuasigroup, arg );
end );

# LoopTwist
InstallGlobalFunction( LoopTwist,
function( arg )
    return RQ_AlgebraTwist( IsLoop, arg );
end );

# ISOTOPES OF RIGHT QUASIGROUPS
# _____________________________________________________________________________

# RQ_AlgebraIsotope
# PROG: constructor OK, calls RQ_AlgebraTwist

InstallMethod( RQ_AlgebraIsotope, "for category and list of arguments",
    [ IsOperation, IsList ],
function( category, data )
    local ls, pos;
    # expects data to be as in RQ_AlgebraTwist
    # PROG: the multiplication will be given by x*y = h(f^{-1}(x)g^{-1}(y)), so call twist (f^{-1},g^{-1},h)
    ls := ShallowCopy( data );
    # positions of maps
    pos := Filtered( [1..Length( data )], i -> IsMapping( data[i] ) or IsPerm( data[i] ) or IsTransformation( data[i] ) );
    ls[pos[1]] := ls[pos[1]]^-1;
    ls[pos[2]] := ls[pos[2]]^-1;
    return RQ_AlgebraTwist( category, ls );
end );

# RightQuasigroupIsotope
# QuasigroupIsotope
# LoopIsotope

InstallGlobalFunction( RightQuasigroupIsotope,
function( arg )
    return RQ_AlgebraIsotope( IsRightQuasigroup, arg );
end);

InstallGlobalFunction( QuasigroupIsotope,
function( arg )
    return RQ_AlgebraIsotope( IsQuasigroup, arg );
end);

InstallGlobalFunction( LoopIsotope,
function( arg )
    return RQ_AlgebraIsotope( IsLoop, arg );
end);

# PrincipalLoopIsotope
# PROG: constructor OK, calls RQ_AlgebraIsotope

InstallOtherMethod( PrincipalLoopIsotope, "for quasigroup and two quasigroup elements",
    [ IsQuasigroup, IsQuasigroupElement, IsQuasigroupElement ],
function( Q, a, b )
    return PrincipalLoopIsotope( Q, a, b, RQ_defaultConstructorStyle );
end );

InstallOtherMethod( PrincipalLoopIsotope, "for quasigroup, two quasigroup elements and record",
    [ IsQuasigroup, IsQuasigroupElement, IsQuasigroupElement, IsRecord ],
function( Q, a, b, style )
    if not (a in Q and b in Q) then
        Error("RQ: <2> and <3> must be elements of quasigroup <1>.");
    fi;   
    return LoopIsotope( Q, RightTranslation( Q, a ), LeftTranslation( Q, b ), (), style ); # parent permutations, default
end );  

# AFFINE RIGHT QUASIGROUPS
# _____________________________________________________________________________

# PROG: This is quite painful because we allow so many possible arithmetic forms.

# RQ_IsAffineAlgebraArithmeticForm
InstallGlobalFunction( RQ_IsAffineAlgebraArithmeticForm,
function( category, data, reportErrors )
    local Q, f, g, u, v;
    if not Length( data ) in [4,5] then
        return RQ_OptionalError( reportErrors, "RQ: There must be 4 or 5 arguments." );
    fi;
    Q := data[1];
    if not ( IsPosInt( Q ) or IsGroup( Q ) or IsAdditiveGroup( Q ) or IsLoop( Q ) ) then # IsField passes this
        return RQ_OptionalError( reportErrors, "RQ: <1> must be a positive integer, group, additive group or loop." );
    fi;
    if Length( data ) = 4 and IsPosInt( Q ) then 
        if not ( ForAll( data, IsInt ) and Gcd( Q, data[2] ) = 1 ) then
            return RQ_OptionalError( reportErrors, "RQ: All arguments must be integers and <1>, <2> must be relatively prime." );
        fi;
        if category = IsQuasigroup and Gcd( Q, data[3] )<> 1 then
            return RQ_OptionalError( reportErrors, "RQ: <3> must be relatively prime to <1>." );
        fi;
        return true; # done with IsPosInt case
    fi;
    if Length( data ) = 4 and IsField( Q ) then 
        if not ( ForAll( data{[2,3,4]}, x -> x in Q ) and data[2]<>Zero( Q ) ) then
            return RQ_OptionalError( reportErrors, "RQ: <2>, <3>, <4> must be field elements, <2> invertible." );
        fi;
        if category = IsQuasigroup and data[3]=Zero( Q ) then
            return RQ_OptionalError( reportErrors, "RQ: <3> must be invertible." );
        fi;
        return true; # done with IsField
    fi;
    if Length( data ) = 4 then # expecting abelian group
        if not ( ( IsGroup( Q ) or IsAdditiveGroup( Q ) ) and IsCommutative( Q ) ) then
            return RQ_OptionalError( reportErrors, "RQ: <1> must be an abelian group." );
        fi;
        f := data[2]; g := data[3]; v := data[4]; u := Q.1; # u does not matter
    else # 5 arguments
        if data[2] in Q then u := data[2]; f := data[3]; else u := data[3]; f := data[2]; fi;
        if data[4] in Q then v := data[4]; g := data[5]; else v := data[5]; g := data[4]; fi;
    fi;
    # testing f, g, u, v
    if not ForAll( [f,g], m -> IsMapping( m ) and Source( m ) = Q and Range( m ) = Q ) then
        return RQ_OptionalError( reportErrors, "RQ: Two of the arguments must be mappings on <1>." );
    fi;
    if not ( IsBijective( f ) and ( category = IsRightQuasigroup or IsBijective( g ) ) ) then
        return RQ_OptionalError( reportErrors, "RQ: The mappings must be bijective." );
    fi;
    if not ( ( IsLoop( Q ) and ForAll( [f,g], IsLoopHomomorphism ) )
        or ( IsGroup( Q ) and ForAll( [f,g], IsGroupHomomorphism ) )
        or ( IsAdditiveGroup( Q ) and ForAll( [f,g], IsAdditiveGroupHomomorphism ) ) ) then
        return RQ_OptionalError( reportErrors, "RQ: The mappings must be homomorphisms." );
    fi;
    if not ( u in Q and v in Q ) then
        return RQ_OptionalError( reportErrors, "RQ: The remaining arguments must be elements of <1>." );
    fi;
    return true;
end );

# IsAffineRightQuasigroupArithmeticForm
InstallGlobalFunction( IsAffineRightQuasigroupArithmeticForm,
function( arg )
    return RQ_IsAffineAlgebraArithmeticForm( IsRightQuasigroup, arg, false );
end );

# IsAffineQuasigroupAlgebraArithmeticForm
InstallGlobalFunction( IsAffineQuasigroupArithmeticForm,
function( arg )
    return RQ_IsAffineAlgebraArithmeticForm( IsQuasigroup, arg, false );
end );

# RQ_AffineAlgebra
# PROG: constructor OK, calls RQ_AlgebraTwist
InstallGlobalFunction( RQ_AffineAlgebra,
function( category, data, style )
    local Q, fu, gv, f, g, u, v, origQ, func, elms;
    RQ_CompleteConstructorStyle( style );
    if style.checkArguments then
        RQ_IsAffineAlgebraArithmeticForm( category, data, true ); # report errors
    fi;
    # processing arguments
    Q := data[1];
    fu := false; # fu keeps track of the arguments u, f, either ordered as u, f or as f, u
    gv := false; 
    if Length( data ) = 4 then # (n,f,g,c) or (Q,g,f,c), set u=0, v=c
        Q := data[1]; f := data[2]; g := data[3]; v := data[4];
        if IsPosInt( Q ) then u := 0;
        elif IsGroup( Q ) then u := One( Q );
        else u := Zero( Q );  # works for IsField, too   
        fi;
    else
        if data[2] in Q then u := data[2]; f := data[3]; else u := data[3]; f := data[2]; fu := true; fi;
        if data[4] in Q then v:= data[4]; g := data[5]; else v := data[5]; g := data[4]; gv := true; fi;
    fi;
    # Will return them as right quasigroup twists.
    # In all cases except IsLoop, we must first build the algebra from which the twist will be derived.
    if IsPosInt( Q ) then
        origQ := QuasigroupByFunction( [0..Q-1], 
            function(x,y) return (x+y) mod Q; end,
            function(x,y) return (x-y) mod Q; end,
            function(x,y) return (-x+y) mod Q; end,
            ConstructorStyle( false, false )
        );
        f := Transformation( List( [0..Q-1], x -> ((f*x) mod Q) + 1 ) ); # note the +1
        g := Transformation( List( [0..Q-1], x -> (((g*x)+v) mod Q) + 1 ) ); # c is represented as 0 + v
    fi;
    if IsField( Q ) then
        origQ := QuasigroupByFunction( Q, \+, \-, function(x,y) return (-x+y); end, ConstructorStyle( false, false ) );
        f := Transformation( List( Elements(Q), x -> Position( Elements( Q ), f*x ) ) );
        g := Transformation( List( Elements(Q), x -> Position( Elements( Q ), g*x+v ) ) ) ;
    fi;
    if IsLoop( Q ) then
        origQ := Q;
        if fu then func := x -> (x^f)*u; else func := x -> u*(x^f); fi;
        f := AsParentTransformation( MappingByFunction( Q, Q, func ) );
        if gv then func := x -> (x^g)*v; else func := x -> v*(x^g); fi;
        g := AsParentTransformation( MappingByFunction( Q, Q, func ) );
    fi;
    if IsGroup( Q ) or ( IsAdditiveGroup( Q ) and not IsField( Q ) ) then
        elms := Elements( Q );
        origQ := RQ_AsAlgebra( IsQuasigroup, Q, ConstructorStyle( false, false ) );
    fi;
    if IsGroup( Q ) then 
        if fu then func := i -> elms[i]^f*u; else func := i -> u*elms[i]^f; fi;
        f := Transformation( List( [1..Size(Q)], i -> Position( elms, func(i) ) ) );
        if gv then func := i -> elms[i]^g*v; else func := i -> v*elms[i]^g; fi;
        g := Transformation( List( [1..Size(Q)], i -> Position( elms, func(i) ) ) );
    fi;
    if IsAdditiveGroup( Q ) and not IsField( Q ) then
        if fu then func := i -> elms[i]^f+u; else func := i -> u+elms[i]^f; fi;
        f := Transformation( List( [1..Size(Q)], i -> Position( elms, func(i) ) ) );
        if gv then func := i -> elms[i]^g+v; else func := i -> v+elms[i]^g; fi;
        g := Transformation( List( [1..Size(Q)], i -> Position( elms, func(i) ) ) );
    fi;
    return RQ_AlgebraTwist( category, [ origQ, f, g, (), style ] ); # takes care of index based/non index based
end );

# AffineRightQuasigroup
InstallGlobalFunction( AffineRightQuasigroup,
function( arg ) # last argument <style> is optional
    local style;
    if not IsRecord( Last(arg) ) then
        style := RQ_defaultConstructorStyle;
    else
        style := Last( arg );
        Remove( arg );
    fi;
    return RQ_AffineAlgebra( IsRightQuasigroup, arg, style );
end );

# AffineQuasigroup
InstallGlobalFunction( AffineQuasigroup,
function( arg ) # last argument <style> is optional
    local style;
    if not IsRecord( Last(arg) ) then
        style := RQ_defaultConstructorStyle;
    else
        style := Last( arg );
        Remove( arg );
    fi;
    return RQ_AffineAlgebra( IsQuasigroup, arg, style );
end );

# SEARCHING FOR AN ISOTOPISM OF RIGHT QUASIGROUPS
# _____________________________________________________________________________

# IsotopismDiscriminator
InstallMethod( IsotopismDiscriminator, "for a right quasigroup",
    [ IsRightQuasigroup ],
function( Q )
    local n, v, i, j, inv, x;
    inv := [];
    n := Size( Q ); 
    for x in Q do
	    v := 0*[1..n];
	    for i in [1..n] do
		    j := ParentInd( x*Q.(i) );
		    v[j] := v[j]+1;
	    od;
        Add( inv, SortedList( v ) );
    od;
    return inv;
end );

#AreEqualIsotopismDiscriminators
InstallMethod( AreEqualIsotopismDiscriminators, "for two isotopism discriminators (lists) of right quasigroups",
    [ IsList, IsList ],
function( D1, D2 )
    return SortedList( D1 ) = SortedList( D2 );
end );

# ArePossiblyIsotopicRightQuasigroups
InstallMethod( ArePossiblyIsotopicRightQuasigroups, "for two right quasigroups",
    [ IsRightQuasigroup, IsRightQuasigroup ],
function( Q1, Q2 )
    local category, D1, D2;
    # REVISIT: WE NEED SOMETHING FOR QUASIGROUPS!
    if not ( Size( Q1 ) = Size( Q2 ) ) then
        return true;
    fi;
    category := CategoryOfRightQuasigroup( [Q1, Q2] );
    D1 := IsotopismDiscriminator( Q1 );
    D2 := IsotopismDiscriminator( Q2 );
    if not AreEqualIsotopismDiscriminators( D1, D2 ) then
        return false;
    fi;
    if category <> IsLoop then # no further invariants
        return true;
    fi;
    # loops
    if IsomorphismLoops( Center(Q1), Center(Q2) ) = fail then return false; fi;
    if IsomorphismLoops( LeftNucleus(Q1), LeftNucleus(Q2) ) = fail then return false; fi;
    if IsomorphismLoops( RightNucleus(Q1), RightNucleus(Q2) ) = fail then return false; fi;
    if IsomorphismLoops( MiddleNucleus(Q1), MiddleNucleus(Q2) ) = fail then return false; fi;
    if not Size(MultiplicationGroup(Q1)) = Size(MultiplicationGroup(Q2)) then return false; fi;
    if not Size(InnerMappingGroup(Q1)) = Size(InnerMappingGroup(Q2)) then return false; fi;
    return true;
end );

# isotopism via perfect matchings with invariants
# and via perfect matchings with automorphism group
# -----------------------------------------------

InstallMethod( RQ_PerfectBipartiteMatching, "for a matrix",
    [ IsMatrix ],
function( A )
    local n, edges, NV, NU, i, j, M, u, v, eM, eN, m, vnext, p, posv, posu;
    n := Length( A );
	edges := []; # edges of the graph
	NV := List([1..n],i->[]); # NV[i] = { j in V: [i,j] in edghes }
	NU := List([1..n],i->[]); # NU[j] = { i in U: [i,j] in edghes }
	for i in [1..n] do for j in [1..n] do
		if A[i,j]=1 then
			AddSet( NV[i], j );
			AddSet( NU[j], i );
			AddSet( edges, [i,j] );
		fi;
	od; od;
	if IsEmpty( edges ) then
		return fail;
	fi;
	M := [ edges[ 1 ] ];
	# partial matching to be enlarged
	while Length( M ) < n do
		# construct data for an alternating path
		u := []; v := []; eM := []; eN := [];
		# ...find a vertex of U not in M
		u[1] := First( [1..n], i -> not (i in List(M,e->e[1])) ); if u[1]=fail then return fail; fi;
		# ...find any edge adjacent to u[1]
		v[1] := First( NV[u[1]] ); if v[1]=fail then return fail; fi;
		eN := [[u[1],v[1]]];
		m := 1;
		while v[m] in List(M,e->e[2]) do # last vertex of v is in M, continue
			u[m+1] := First( NU[v[m]], i -> [i,v[m]] in M ); if u[m+1]=fail then return fail; fi;
			Add( eM, [u[m+1],v[m]] );
			vnext := First( [1..n], i -> (not (i in v)) and (not IsEmpty( Intersection( NU[i], u ) ) ) ); # has some edge to a vertex of u
			if vnext=fail then return fail; fi;
			v[m+1] := vnext;
			Add( eN, [ First( Intersection( NU[vnext], u ) ), vnext ] );
			m := m+1;
		od;
		# construct the alternating path
		p := [];
		posv := m;
		repeat 
			posu := First( [1..posv], i -> [u[i],v[posv]] in eN );
			Add( p, [u[posu],v[posv]] );
			if posu > 1 then
				posv := First( [1..posu], j -> [u[posu],v[j]] in eM );
				Add( p, [u[posu],v[posv]] );
			fi;
		until posu = 1;
		# replace M with a larger matching
		M := Union( Difference( M, p ), Difference( p, M ) );
	od;
	return M;
end );

# RQ_Selector_Increment
InstallMethod( RQ_Selector_Increment, "for selector (list of integers) and blocks (list of lists)",
    [ IsList, IsList ],
function( selector, blocks )
    local n, pos, i;
	n := Length( selector );
	if selector[1]=0 then # special provision for selector that did not start yet
		return List([1..n], i->1);
	fi;
	pos := First([1..n], i -> selector[n-i+1] < Length( blocks[n-i+1] ) );
	if pos=fail then
		return fail;
	fi;
	pos := n-pos+1;
	selector[pos] := selector[pos]+1;
	for i in [pos+1..n] do
		selector[i]:=1;
	od;
	return selector;
end );

# RQ_Selector_FirstConflictWithBijectivity
InstallMethod( RQ_Selector_FirstConflictWithBijectivity, "for selector (list of integers) and blocks (list of lists)",
    [ IsList, IsList ],
function( selector, blocks )
	local n, used, i, z;
	n := Length( selector );
	used := 0*[1..n];
	for i in [1..n] do
		z := blocks[i][selector[i]];
		if used[z]=1 then # conflict
			return i;
		fi;
		used[z]:=1;
	od;
	return fail;
end );

# RQ_Selector_NextBijective
InstallMethod( RQ_Selector_NextBijective, "for selector (list of integers) and blocks (list of lists)",
    [ IsList, IsList ],
function( selector, blocks )
	local n, m, i;
	n := Length( selector );
	repeat 
		selector := RQ_Selector_Increment( selector, blocks );
		if selector=fail then 
			return fail;
		fi;
		m := RQ_Selector_FirstConflictWithBijectivity( selector, blocks );
		if m = fail then # no conflict
			return selector;
		fi;
		for i in [m+1..n] do
			selector[i] := Length(blocks[i]);
		od;
	until 0<>0;
end );

# RQ_IsotopismRightQuasigroupsPM
InstallMethod( RQ_IsotopismRightQuasigroupsPM, "for two right quasigroups and method",
    [ IsRightQuasigroup, IsRightQuasigroup, IsString ],
function( Q1, Q2, method )
    local n, t1, t2, s1, s2, blocks, selector, R2, f, g, h, e, x, S, y, A, M;
	n := Size( Q1 );
	t1 := MultiplicationTable( Q1 );
	t2 := MultiplicationTable( Q2 );
	s1 := IsotopismDiscriminator( Q1 );
	s2 := IsotopismDiscriminator( Q2 );
	# x and f(x) must have the same frequency statistics (no matter what g and h do)
    if method = "via perfect matchings with invariants" then
	    blocks := List([1..n], i -> Filtered( [1..n], j -> s1[i] = s2[j] ) );
	    if ForAny( blocks, b -> IsEmpty(b) ) then
		    return fail;
	    fi;
	    selector := 0*[1..n]; # current selector from blocks
    else # via perfect matchings with automorphism group
        R2 := RightTransversal( SymmetricGroup( n ), AutomorphismGroup( Q2 ) );
        selector := 0;
    fi;
	repeat
        # select next f
		if method = "via perfect matchings with invariants" then
            selector := RQ_Selector_NextBijective( selector, blocks );
		    if selector = fail then
			    return fail;
		    fi;
		    f := PermList( List( [1..n], i->blocks[i][selector[i]] ) );
        else # via perfect matchings with automorphism group
            selector := selector + 1;
            if selector > Length( R2 ) then
                return fail;
            fi;
            f := R2[ selector ];
        fi;
        # initiate g, h
		g := 0*[1..n];
		h := 0*[1..n];	
		for e in [1..n] do # select the value g[1]
			g[1] := e;
			# build h
			for x in [1..n] do
				h[ t1[x][1] ] := t2[ x^f, e ];
			od;
			# try to complete g
			S := List([1..n], i -> [] ); # g(y) must lie in S[y];
			S[1] := [e];
			for y in [2..n] do
				S[y] := [1..n];
				for x in [1..n]	do
					S[y] := Filtered( S[y], z -> t2[x^f,z] = h[t1[x,y]] );
				od;
			od;
			# solve the marriage problem for g
			# build bipartite graph
			A := List([1..n], i->0*[1..n]);
			for x in [1..n] do for y in [1..n] do
				if y in S[x] then
					A[x][y] := 1;
				fi;	
			od; od;
			M := RQ_PerfectBipartiteMatching( A );
			if M<>fail then # done, report results
				# build g
				for x in M do # x is an edge of the matching M
					g[x[1]] := x[2];
				od;
				g := PermList( g );
				h := PermList( h );
				return [f,g,h]; # this will be further processed by IsotopismRightQuasigroups
			fi;		
		od;
	until 0<>0;
end );

# isotopism via via domain extension
# ----------------------------------

# RQ_ExtendIsotopismByClosingSource
InstallGlobalFunction( RQ_ExtendIsotopismByClosingSource, 
function( f, g, h, tables1, tables2 )
    local mult1, mult2, rdiv1, rdiv2, ldiv1, ldiv2,
        n, df, dg, dh, rf, rg, rh, newf, newg, newh, lastf, lastg, lasth,
        add, x, y;
    f := ShallowCopy( f ); g := ShallowCopy( g ); h := ShallowCopy( h ); # to protect backtracking
    # multiplication, right division and left division tables
    mult1 := tables1[1]; rdiv1 := tables1[2]; ldiv1 := tables1[3]; 
    mult2 := tables2[1]; rdiv2 := tables2[2]; ldiv2 := tables2[3];
    n := Length( f );
    # domains of f, g, h
    df := Filtered([1..n], i -> f[i]<>0); dg := Filtered([1..n], i -> g[i]<>0); dh := Filtered([1..n], i -> h[i]<>0);
    # ranges of f, g, h
    rf := Set( df, i -> f[i] ); rg := Set( dg, i -> g[i] ); rh := Set( dh, i -> h[i] );
    # need to be checked in this round of extensions
    newf := ShallowCopy( df ); newg := ShallowCopy( dg );  newh := ShallowCopy( dh );
    # have been added in this round of extensions
    lastf := []; lastg := []; lasth := [];
    # function that attempts to add z to the domain of m with value mz
    # returns true iff successful
    # as a side effect, changes m, dm and lastm accordingly
    add := function( m, dm, rm, lastm, z, mz ) 
        if m[z] <> 0 then
            return m[z] = mz; # not well defined if m[z] <> mz
        fi;
        if mz in rm then # not 1-1
            return false;
        fi;
        # add image
        m[z] := mz; AddSet( dm, z ); AddSet( rm, mz ); AddSet( lastm, z );
        return true;
    end;
    while not ( IsEmpty( newf ) and IsEmpty( newg ) and IsEmpty( newh ) ) do
        lastf := []; lastg := []; lasth := [];
        for x in newf do
            for y in dg do if not add( h, dh, rh, lasth, mult1[x,y], mult2[f[x], g[y]] ) then return fail; fi; od;
            for y in dh do if not add( g, dg, rg, lastg, ldiv1[x,y], ldiv2[f[x], h[y]] ) then return fail; fi; od;
        od;
        for x in newg do
            for y in df do if not add( h, dh, rh, lasth, mult1[y,x], mult2[f[y], g[x]] ) then return fail; fi; od;
            for y in dh do if not add( f, df, rf, lastf, rdiv1[y,x], rdiv2[h[y], g[x]] ) then return fail; fi; od;
        od;
        for x in newh do
            for y in df do if not add( g, dg, rg, lastg, ldiv1[y,x], ldiv2[f[y], h[x]] ) then return fail; fi; od;
            for y in dg do if not add( f, df, rf, lastf, rdiv1[x,y], rdiv2[h[x], g[y]] ) then return fail; fi; od;
        od;
        newf := ShallowCopy( lastf ); newg := ShallowCopy( lastg ); newh := ShallowCopy( lasth );
    od;
    return [ f, g, h ];
end );

# RQ_ExtendIsotopism
InstallGlobalFunction( RQ_ExtendIsotopism, 
function( f, g, h, tables1, tables2, gens1 )
    # g[1] is fixed here
    local fgh, n, z, possiblefz, fz, f2;
    fgh := RQ_ExtendIsotopismByClosingSource( f, g, h, tables1, tables2 );
    if fgh = fail then
        return fail;
    fi;
    f := fgh[1]; g := fgh[2]; h := fgh[3]; n := Length( f );
    z := First( gens1, i -> f[i] = 0);
    if z = fail then # isotopism found
        return fgh;
    fi;
    possiblefz := Difference( [1..n], f ); # not yet in f
    for fz in possiblefz do
        f2 := ShallowCopy( f );
        f2[ z ] := fz;
        fgh := RQ_ExtendIsotopism( f2, g, h, tables1, tables2, gens1 );
        if not fgh = fail then # isotopism found
            return fgh;
        fi; 
    od;
    return fail;    
end );

# RQ_IsotopismQuasigroupsDE
InstallMethod( RQ_IsotopismQuasigroupsDE, "for two right quasigroups", 
    [ IsRightQuasigroup, IsRightQuasigroup ],
function( Q1, Q2 ) 
    local category, tables1, tables2, n, gens1, i, f, g, h, itp;
    category := CategoryOfRightQuasigroup( [Q1, Q2] );
    if not ArePossiblyIsotopicRightQuasigroups( Q1, Q2 ) then
        return fail;
    fi;
    # tables of operations
    tables1 := [ MultiplicationTable( Q1 ), RightDivisionTable( Q1 ), LeftDivisionTable( Q1 ) ];
    tables2 := [ MultiplicationTable( Q2 ), RightDivisionTable( Q2 ), LeftDivisionTable( Q2 ) ];
    n := Size( Q1 );
    # for loops, it suffices to know f on a generating set containing One(Q1) and g on One(Q1)
    # for quasigroups, it suffices to know f on Q1 and g on any one element of Q1
    if category = IsLoop then
        gens1 := ParentInd( Union( SmallGeneratingSet( Q1 ), [ One(Q1) ] ) );
    else
        gens1 := [1..n];
    fi;
    # main cycle
    for i in [1..n] do # the one value of g
        f := 0*[1..n]; # empty map
        g := 0*[1..n];
        if category = IsLoop then
            g[ ParentInd( One(Q1) ) ] := i;
        else
            g[ 1 ] := i;
        fi;
        h := 0*[1..n];
        itp := RQ_ExtendIsotopism( f, g, h, tables1, tables2, gens1 );
        if not itp=fail then 
            return List( itp, PermList );
        fi;
    od;
    return fail;
end );

# isotopism via via principal loop isotopes
# -----------------------------------------

# RQ_IsotopismLoopsPLI
InstallMethod( RQ_IsotopismLoopsPLI, "for two loops", 
    [ IsLoop, IsLoop ],
function( Q1, Q2 ) 
    local T, a, b, Q, iso, f, g, h;
    # checks all principal loop isotopes of Q1, whether they are isomorphic to Q2
    T := RightTransversal( Q1, MiddleNucleus( Q1 ) ); # just a subset 
    for a in Q1 do for b in T do 
        Q := PrincipalLoopIsotope( Q1, a, b );
        iso := IsomorphismLoops( Q, Q2 ); # fail or a right quasigroup mapping
        if not iso = fail then 
            # reconstruct the isotopism (alpha, beta, gamma)
            iso := AsCanonicalPerm( iso );
            f := RightTranslation(Q1,a)*iso;
            g := LeftTranslation(Q1,b)*iso;
            h := iso;
            return [f,g,h]; # will be further processed
        fi;
    od; od;
    return fail; 
end );

# main function for isotopism search
# ----------------------------------

# IsotopismRightQuasigroups
InstallMethod( IsotopismRightQuasigroups, "for two right quasigroups and method selector (string)",
    [ IsRightQuasigroup, IsRightQuasigroup, IsString ],
function( Q1, Q2, method )
    local origQ1, origQ2, category, itp, t;
    origQ1 := ShallowCopy( Q1 );
    origQ2 := ShallowCopy( Q2 );
    if not IsCanonical(Q1) then Q1 := CanonicalCopy( Q1 ); fi;
    if not IsCanonical(Q2) then Q2 := CanonicalCopy( Q2 ); fi;
    category := CategoryOfRightQuasigroup( [ Q1, Q2 ] );
    if method = "via perfect matchings with invariants" or method = "via perfect matchings with automorphism group" then
        itp := RQ_IsotopismRightQuasigroupsPM( Q1, Q2, method );
    elif method = "via domain extension" and category <> IsRightQuasigroup then 
        itp := RQ_IsotopismQuasigroupsDE( Q1, Q2 );
    elif method = "via principal loop isotopes" and category = IsLoop then
        itp := RQ_IsotopismLoopsPLI( Q1, Q2 );
    else
        Error( "RQ: The selected method is not suported for the type of algebras.");
    fi;
    if itp=fail then 
        return fail;
    fi;
    # convert back to original right quasigroups
    itp := List( itp, p -> AsParentTransformation( origQ1, origQ2, AsTransformation(p) ) );
    t := HomotopismRightQuasigroups( origQ1, origQ2, itp[1], itp[2], itp[3] );
    SetIsBijective( t, true );
    return t;
end );

InstallOtherMethod( IsotopismRightQuasigroups, "for two right quasigroups",
    [ IsRightQuasigroup, IsRightQuasigroup ],
function( Q1, Q2 )
    return IsotopismRightQuasigroups( Q1, Q2, "via perfect matchings with invariants" );
end );

# IsotopismQuasigroups
InstallMethod( IsotopismQuasigroups, "for two quasigroups and method selector (string)",
    [ IsQuasigroup, IsQuasigroup, IsString ],
function( Q1, Q2, method )
    return IsotopismRightQuasigroups( Q1, Q2, method );
end );

InstallOtherMethod( IsotopismQuasigroups, "for two quasigroups",
    [ IsQuasigroup, IsQuasigroup ],
function( Q1, Q2 )
    return IsotopismRightQuasigroups( Q1, Q2, "via domain extension" );
end );

# IsotopismLoops
InstallMethod( IsotopismLoops, "for two loops and method selector (string)",
    [ IsLoop, IsLoop, IsString ],
function( Q1, Q2, method )
    return IsotopismRightQuasigroups( Q1, Q2, method );
end );

InstallOtherMethod( IsotopismLoops, "for two loops",
    [ IsLoop, IsLoop ],
function( Q1, Q2 )
    return IsotopismRightQuasigroups( Q1, Q2, "via domain extension" );
end );

# RIGHT QUASIGROUPS UP TO ISOTOPISM
# _____________________________________________________________________________

# RQ_AlgebrasUpToIsotopism
InstallMethod( RQ_AlgebrasUpToIsotopism, "for category, list of algebras and method selector (string)",
    [ IsOperation, IsList, IsString ],
function( category, ls, method )
    local kept, positions, pos, Q, is_new, K, isequiv;
    # check arguments
    if IsEmpty( ls ) then return ls; fi;
    if not ForAll( ls, IsRightQuasigroup ) then
        Error( "RQ: <1> must be a list of right quasigroups" );
    fi;
    if Length( ls ) = 1 then return ls; fi;
    if not ForAll( ls, Q -> CategoryOfRightQuasigroup( Q ) = category ) then 
        Error("RQ: <1> must be a list of algebras of the same type");
    fi;        
    # main cycle   
    kept := []; # kept algebras 
    positions := []; # positions of kept algebras in the original list
    pos := 0;
    for Q in ls do
        pos := pos + 1;
        if not IsCanonical( Q ) then Q := CanonicalCopy( Q ); fi; # making canonical when seen for the first time
        is_new := true;
        for K in kept do
            if method = "via default method" then
                isequiv := ( IsotopismRightQuasigroups( Q, K ) <> fail );
            else 
                isequiv := ( IsotopismRightQuasigroups( Q, K, method ) <> fail );
            fi;
            if isequiv then
                is_new := false;
                break;
            fi;
        od;
        if is_new then
            Add( kept, Q ); 
            Add( positions, pos );
        fi;
    od;
    # returning algebras from the original list
    return ls{positions};
end );

# RightQuasigroupsUpToIsotopism 
# QuasigroupsUpToIsotopism 
# LoopsUpToIsotopism

InstallMethod( RightQuasigroupsUpToIsotopism, "for list of right quasigroups and method selector (string)",
    [ IsList, IsString ],
function( ls, method )
    return RQ_AlgebrasUpToIsotopism( IsRightQuasigroup, ls, method );
end );

InstallOtherMethod( RightQuasigroupsUpToIsotopism, "for list of right quasigroups",
    [ IsList ],
function( ls )
    return RQ_AlgebrasUpToIsotopism( IsRightQuasigroup, ls, "via default method" );
end );

InstallMethod( QuasigroupsUpToIsotopism, "for list of quasigroups and method selector (string)",
    [ IsList, IsString ],
function( ls, method )
    return RQ_AlgebrasUpToIsotopism( IsQuasigroup, ls, method );
end );

InstallOtherMethod( QuasigroupsUpToIsotopism, "for list of right quasigroups",
    [ IsList ],
function( ls )
    return RQ_AlgebrasUpToIsotopism( IsQuasigroup, ls, "via default method" );
end );

InstallMethod( LoopsUpToIsotopism, "for list of loops and method selector (string)",
    [ IsList, IsString ],
function( ls, method )
    return RQ_AlgebrasUpToIsotopism( IsLoop, ls, method );
end );

InstallOtherMethod( LoopsUpToIsotopism, "for list of loops",
    [ IsList ],
function( ls )
    return RQ_AlgebrasUpToIsotopism( IsLoop, ls, "via default method" );
end );

# AUTOTOPISM GROUPS OF RIGHT QUASIGROUPS
# _____________________________________________________________________________

# RQ_HtpOnPairs
InstallMethod( RQ_HtpOnPairs, "for a 2-tuple and a right quasigroups homotopism",
    [ IsList, IsRightQuasigroupHomotopism ],
function( ls, t )
    local Q1, Q2, i, j, a, b;
    Q1 := t!.source;
    Q2 := t!.range;
    i := ParentInd( ls[1] );
    j := ParentInd( ls[2] );
    a := Q2.( i^(t!.f) ); # f acting on the first coordinate
    b := Q2.( j^(t!.g) ); # g acting on the second coordinate
    return [a,b];
end );

InstallGlobalFunction( RQ_NiceMonomorphism, 
function( htop )
    local n;
    n := Size( Parent( Source( htop ) ) );
    return PermList( 
        Concatenation( List( [1,2,3], i -> (i-1)*n + ListPerm( ComponentOfHomotopism( htop, i), n ) ) ) 
    );
end);

InstallGlobalFunction( RQ_NiceMonomorphismInverse, 
function( rq )
    local n, fun;
    n := Size( Parent( rq ) );
    fun := function( perm )
        local li;
        li := List( [0,1,2], i -> PermList( OnTuples( i*n + [1..n], perm ) - i*n ) );
        return HomotopismRightQuasigroups( rq, rq, li[1], li[2], li[3] );
    end;
    return fun;
end);

InstallMethod( RQ_AutotopismGroupByGeneratorsNC, "for a right quasigroup and a list of autotopisms",
    [ IsRightQuasigroup, IsRightQuasigroupHomotopismCollection ],
function( rq, gens )
    local atpgr, ag, nice;
    if gens = [ ] then gens := [ IdentityAutotopism( rq ) ]; fi;
    atpgr := MakeGroupyObj( FamilyObj( gens ), IsGroup, gens, false );
    ag := Group( List( gens, RQ_NiceMonomorphism ) );
    nice := GroupHomomorphismByFunction( 
        atpgr, 
        ag, 
        RQ_NiceMonomorphism, 
        RQ_NiceMonomorphismInverse( rq )
    );
    SetNiceMonomorphism( atpgr, nice );
    SetIsHandledByNiceMonomorphism( atpgr, true );
    return atpgr;
end );

InstallMethod( GroupByGenerators, "for a list of autotopisms", 
    [ IsRightQuasigroupHomotopismCollection ],
function( gens )
    return RQ_AutotopismGroupByGeneratorsNC( Source( gens[1] ), AsList( gens ) );
end );

# AutotopismGroup
# MATH: Every autotopism of Q decomposes uniquely as a principal loop isotopism Q --> Q_{a,b}
# followed by an isomorphism Q_{a,b} --> Q. The principal loop isotope Q_{a,b}
# is isomorphic to Q iff (a,b) lies in the orbit of (1,1)
# under the action of the autotopism group Atp( Q ) on tuples: (f,g,h)(x,y) = (f(x),g(y)).
InstallMethod( AutotopismGroup, "for a loop",
    [ IsLoop ],
function( Q )  
    local origQ, aut, gens, isomorphic, nonisomorphic, open, a, b, f, atp, orbits;
    origQ := Q;
    Q := CanonicalCopy( Q ); 
    # start with the embedding of Aut( Q ) to Atp( Q )
    gens := Set( GeneratorsOfGroup( AutomorphismGroup( Q ) ), f -> AutotopismRightQuasigroup( Q, f, f, f ) );
    # keep track of pairs (a,b) for which Q_{a,b} is isomorphic to Q
    isomorphic := []; # reps [a,b] of orbits of atp such that Q_{b,a} is isomorphic to Q
    nonisomorphic := []; # reps [a,b] of orbits of atp such that Q_{b,a} is not isomorphic to Q
    open := Difference( Cartesian( Q, Q ), [[One(Q),One(Q)]] ); # unresolved 
    while not IsEmpty( open ) do
        a := open[1,1];
        b := open[1,2];
        f := IsomorphismLoops( PrincipalLoopIsotope( Q, b, a ), Q ); # note the reversal
        if f = fail then # not isomorphic
            Add( nonisomorphic, [a,b] );       
            open := open{[2..Length(open)]};          
        else # isomorphic, add the induced autotopism (R_b, L_a, id )*(f,f,f)
            Add( isomorphic, [a,b] );
            f := AsCanonicalPerm( f );
            AddSet( gens, AutotopismRightQuasigroup( Q, RightTranslation( Q, b )*f, LeftTranslation( Q, a )*f, f ) );
            atp := Group( gens );
            orbits := List( Concatenation( isomorphic, nonisomorphic ), x -> Orbit( atp, x, RQ_HtpOnPairs ) );
            open := Difference( Cartesian( Q, Q ), Union( orbits ) );
        fi;
    od;
    # convert back to original algebra
    gens := List( gens, t -> AutotopismRightQuasigroup( origQ, t!.f, t!.g, t!.h, true ) ); # data given as canonical
    return RQ_AutotopismGroupByGeneratorsNC( Q, gens ); #Group( gens );
end );

