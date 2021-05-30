# Topisms.gi
# Homotopisms, isotopisms and autotopisms of right quasigroups
# =============================================================================

# HOMOTOPISMS OF RIGHT QUASIGROUPS
# _____________________________________________________________________________

# RQ_IsAlgebraHomotopism
InstallMethod( RQ_IsAlgebraHomotopism, "for category and three mappings",
    [ IsObject, IsMapping, IsMapping, IsMapping ],
function( category, f, g, h )
    local Q1, Q2;
    Q1 := Source( f ); Q2 := Range( f );
    if not ( category( Q1 ) and category( Q2 ) ) then return false; fi;
    if not ( Source( g ) = Q1 and Source( h ) = Q1 ) then return false; fi;
    if not ( Range( g ) = Q2 and Range( h ) = Q2 ) then return false; fi;
    return ForAll( Q1, x -> ForAll( Q1, y -> (x^f)*(y^g) = (x*y)^h ) );
end );

# IsRightQuasigroupHomotopism
# IsQuasigroupHomotopism
# IsLoopHomotopism

InstallMethod( IsRightQuasigroupHomotopism, "for three mappings",
    [ IsMapping, IsMapping, IsMapping ],
function( f, g, h )
    return RQ_IsAlgebraHomotopism( IsRightQuasigroup, f, g, h );
end );

InstallOtherMethod( IsRightQuasigroupHomotopism, "for list of three mappings",
    [ IsList ],
function( fgh )
    return RQ_IsAlgebraHomotopism( IsRightQuasigroup, fgh[1], fgh[2], fgh[3] );
end );

InstallMethod( IsQuasigroupHomotopism, "for three mappings",
    [ IsMapping, IsMapping, IsMapping ],
function( f, g, h )
    return RQ_IsAlgebraHomotopism( IsQuasigroup, f, g, h );
end );

InstallOtherMethod( IsQuasigroupHomotopism, "for list of three mappings",
    [ IsList ],
function( fgh )
    return RQ_IsAlgebraHomotopism( IsQuasigroup, fgh[1], fgh[2], fgh[3] );
end );

InstallMethod( IsLoopHomotopism, "for three mappings",
    [ IsMapping, IsMapping, IsMapping ],
function( f, g, h )
    return RQ_IsAlgebraHomotopism( IsLoop, f, g, h );
end );

InstallOtherMethod( IsLoopHomotopism, "for list of three mappings",
    [ IsList ],
function( fgh )
    return RQ_IsAlgebraHomotopism( IsLoop, fgh[1], fgh[2], fgh[3] );
end );

# RQ_IsAlgebraIsotopism
InstallMethod( RQ_IsAlgebraIsotopism, "for category and three mappings",
    [ IsObject, IsMapping, IsMapping, IsMapping ],
function( category, f, g, h )
    return ForAll( [f,g,h], IsBijective ) and RQ_IsAlgebraHomotopism( category, f, g, h );
end );

# IsRightQuasigroupIsotopism
# IsQuasigroupIsotopism
# IsLoopIsotopism

InstallMethod( IsRightQuasigroupIsotopism, "for three mappings",
    [ IsMapping, IsMapping, IsMapping ],
function( f, g, h )
    return RQ_IsAlgebraIsotopism( IsRightQuasigroup, f, g, h );
end );

InstallOtherMethod( IsRightQuasigroupIsotopism, "for list of three mappings",
    [ IsList ],
function( fgh )
    return RQ_IsAlgebraIsotopism( IsRightQuasigroup, fgh[1], fgh[2], fgh[3] );
end );

InstallMethod( IsQuasigroupIsotopism, "for three mappings",
    [ IsMapping, IsMapping, IsMapping ],
function( f, g, h )
    return RQ_IsAlgebraIsotopism( IsQuasigroup, f, g, h );
end );

InstallOtherMethod( IsQuasigroupIsotopism, "for list of three mappings",
    [ IsList ],
function( fgh )
    return RQ_IsAlgebraIsotopism( IsQuasigroup, fgh[1], fgh[2], fgh[3] );
end );

InstallMethod( IsLoopIsotopism, "for three mappings",
    [ IsMapping, IsMapping, IsMapping ],
function( f, g, h )
    return RQ_IsAlgebraIsotopism( IsLoop, f, g, h );
end );

InstallOtherMethod( IsLoopIsotopism, "for list of three mappings",
    [ IsList ],
function( fgh )
    return RQ_IsAlgebraIsotopism( IsLoop, fgh[1], fgh[2], fgh[3] );
end );

# RQ_IsAlgebraAutotopism
InstallMethod( RQ_IsAlgebraAutotopism, "for category and three mappings",
    [ IsObject, IsMapping, IsMapping, IsMapping ],
function( category, f, g, h )
    return Source( f ) = Range( f ) and RQ_IsAlgebraIsotopism( category, f, g, h );
end );

# IsRightQuasigroupAutotopism
# IsQuasigroupAutotopism
# IsLoopAutotopism

InstallMethod( IsRightQuasigroupAutotopism, "for three mappings",
    [ IsMapping, IsMapping, IsMapping ],
function( f, g, h )
    return RQ_IsAlgebraAutotopism( IsRightQuasigroup, f, g, h );
end );

InstallOtherMethod( IsRightQuasigroupAutotopism, "for list of three mappings",
    [ IsList ],
function( fgh )
    return RQ_IsAlgebraAutotopism( IsRightQuasigroup, fgh[1], fgh[2], fgh[3] );
end );

InstallMethod( IsQuasigroupAutotopism, "for three mappings",
    [ IsMapping, IsMapping, IsMapping ],
function( f, g, h )
    return RQ_IsAlgebraAutotopism( IsQuasigroup, f, g, h );
end );

InstallOtherMethod( IsQuasigroupAutotopism, "for list of three mappings",
    [ IsList ],
function( fgh )
    return RQ_IsAlgebraAutotopism( IsQuasigroup, fgh[1], fgh[2], fgh[3] );
end );

InstallMethod( IsLoopAutotopism, "for three mappings",
    [ IsMapping, IsMapping, IsMapping ],
function( f, g, h )
    return RQ_IsAlgebraAutotopism( IsLoop, f, g, h );
end );

InstallOtherMethod( IsLoopAutotopism, "for list of three mappings",
    [ IsList ],
function( fgh )
    return RQ_IsAlgebraAutotopism( IsLoop, fgh[1], fgh[2], fgh[3] );
end );

# TWISTS OF RIGHT QUASIGROUPS
# _____________________________________________________________________________

# RQ_AlgebraTwistByParentTransformations
# PROG: constructor OK
# Many other constuctors are reduced to this one.
InstallMethod( RQ_AlgebraTwistByParentTransformations, "for category, right quasigroup, three transformations and record",
    [ IsObject, IsRightQuasigroup, IsTransformation, IsTransformation, IsTransformation, IsRecord ],
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
    [ IsObject, IsList ],
function( category, data )
    local Q, f, g, h, isCanonical, style, ConvertMapping;
    # processing arguments
    # expects Q, f, g, h, isCanonical, style, OR Q, [f,g,h], isCanonical, style, with the last two arguments optional 
    Q := data[1];
    if IsList( data[2] ) then
        f := data[2][1]; g := data[2][2]; h := data[2][3];
    else
        f := data[2]; g := data[3]; h := data[4];
    fi;
    if ( category = IsLoop and not IsQuasigroup( Q ) ) or ( category <> IsLoop and not category( Q ) ) then
        Error( "RQ: <Q> must be a right quasigroup or quasigroup." );
    fi;
    if not ForAll( [f,g,h], m -> ( IsMapping( m ) and Source( m ) = Q and Range( m ) = Q ) or IsPerm( m ) or IsTransformation( m ) ) then
        Error( "RQ: Each of <f>, <g>, <h> must be a mapping from <Q> to <Q>, or a permutation on <Q> or a transformation on <Q>." );
    fi;
    isCanonical := First( data, IsBool );
    if isCanonical = fail then
        isCanonical := false;
    fi;
    style := Last( data, IsRecord );
    if style = fail then # no style given
        style := RQ_defaultConstructorStyle;
    else
        RQ_CompleteConstructorStyle( style );
    fi;
    # converting arguments
    ConvertMapping := function( m )
        if IsMapping( m ) then
            return AsParentTransformation( m );
        fi;
        if IsPerm( m ) then # first convert to a transformation
            m := AsTransformation( m );
        fi;
        if IsTransformation( m ) and isCanonical then
            return AsParentTransformation( Q, Q, m );
        fi;
        return m; # no change to parent transformation
    end;
    f := ConvertMapping( f );
    g := ConvertMapping( g );
    h := ConvertMapping( h );
    return RQ_AlgebraTwistByParentTransformations( category, Q, f, g, h, style );    
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
    [ IsObject, IsList ],
function( category, data )
    local ls;
    # expects data to be Q, f, g, h, ... or Q, [f,g,h], ...
    # PROG: the multiplication will be given by x*y = h(f^{-1}(x)g^{-1}(y)), so call twist (f^{-1},g^{-1},h)
    ls := ShallowCopy( data );
    if IsList( ls[2] ) then 
        ls[2] := [ ls[2,1]^-1, ls[2,2]^-1, ls[2,3] ];
    else  
        ls[2] := ls[2]^-1; ls[3] := ls[3]^-1; ls[4] := ls[4];
    fi;
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
# PROG: constructor OK, calls RQ_AlgebraIsotopeByPerms

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
# PROG: constructor OK, calls RQ_AlgebraIsotopeByPerms
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

# RIGHT QUASIGROUPS UP TO ISOTOPISM
# _____________________________________________________________________________

# RQ_ArePossiblyIsotopicLoops

InstallMethod( RQ_ArePossiblyIsotopicLoops, "for two loops",
    [ IsLoop, IsLoop ],
function( Q1, Q2 )
    # testing a few properties of loops preserved by isotopisms
    if not Size(Q1)=Size(Q2) then return false; fi;
    if IsomorphismLoops( Center(Q1), Center(Q2) ) = fail then return false; fi;
    if IsomorphismLoops( LeftNucleus(Q1), LeftNucleus(Q2) ) = fail then return false; fi;
    if IsomorphismLoops( RightNucleus(Q1), RightNucleus(Q2) ) = fail then return false; fi;
    if IsomorphismLoops( MiddleNucleus(Q1), MiddleNucleus(Q2) ) = fail then return false; fi;
    # REVISIT: we could test for isomorphism among multiplication groups and inner mapping groups, too
    if not Size(MultiplicationGroup(Q1)) = Size(MultiplicationGroup(Q2)) then return false; fi;
    if not Size(InnerMappingGroup(Q1)) = Size(InnerMappingGroup(Q2)) then return false; fi;
    return true;
end );

# RQ_ExtendIsotopismByClosingSource

InstallGlobalFunction( RQ_ExtendIsotopismByClosingSource, 
function( f, g, h, tables1, tables2 )
    local mult1, mult2, rdiv1, rdiv2, ldiv1, ldiv2,
        n, df, dg, dh, rf, rg, rh, newf, newg, newh, lastf, lastg, lasth,
        add, x, y;
    f := ShallowCopy( f ); g := ShallowCopy( g ); h := ShallowCopy( h ); # to protect backtracking
    # multiplication, dight division and left division tables
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

# RQ_IsotopismAlgebras

InstallMethod( RQ_IsotopismAlgebras, "for category, two right quasigroups and bool",
    [ IsObject, IsRightQuasigroup, IsRightQuasigroup, IsBool ],
function( category, Q1, Q2, viaPrincipalLoopIsotopes ) 
    local origQ1, origQ2, tables1, tables2, n, gens1, i, f, g, h, fgh,
        T, Q, phi, alpha, beta, gamma;
    # making sure the quasigroups are canonical
    origQ1 := ShallowCopy( Q1 );
    origQ2 := ShallowCopy( Q2 );
    if not IsCanonical(Q1) then Q1 := CanonicalCopy( Q1 ); fi;
    if not IsCanonical(Q2) then Q2 := CanonicalCopy( Q2 ); fi;
    # check for isotopism invariants in case of loops
    if category = IsLoop and not RQ_ArePossiblyIsotopicLoops( Q1, Q2 ) then
        return fail;
    fi;
    if not viaPrincipalLoopIsotopes then # generic method
        # tables of operations
        tables1 := [ MultiplicationTable( Q1 ), RightDivisionTable( Q1 ), LeftDivisionTable( Q1 ) ];
        tables2 := [ MultiplicationTable( Q2 ), RightDivisionTable( Q2 ), LeftDivisionTable( Q2 ) ];
        n := Size( Q1 );
        if category = IsLoop then # for loops, it suffices to know f on a generating set containing 1
            gens1 := ParentInd( Union( SmallGeneratingSet( Q1 ), [ One(Q1) ] ) );
        else
            gens1 := [1..n];
        fi;
        # main cycle of generic method
        for i in [1..n] do # value of g[1]
            f := 0*[1..n]; # empty map
            g := 0*[1..n]; g[1] := i;
            h := 0*[1..n];
            fgh := RQ_ExtendIsotopism( f, g, h, tables1, tables2, gens1 );
            if not fgh = fail then # isotopism found
                return List( fgh, m -> AsRightQuasigroupMapping( origQ1, origQ2, Transformation( m ), true ) );
            fi;
        od;
        return fail;
    fi;
    # method via principal loop isotopes
    # construct all distinct principal loop isotopes of Q1 an check for isomorphism with Q2
    T := RightTransversal( Q1, MiddleNucleus( Q1 ) ); # just a subset 
    for f in Q1 do for g in T do 
        Q := PrincipalLoopIsotope( Q1, f, g );
        phi := IsomorphismLoops( Q, Q2 ); # fail or right quasigroup mapping
        if not phi = fail then 
            # reconstruct the isotopism (alpha, beta, gamma)
            phi := AsCanonicalPerm( phi );
            alpha := AsRightQuasigroupMapping( origQ1, origQ2, AsTransformation( RightTranslation(Q1,f)*phi ), true );
            beta := AsRightQuasigroupMapping( origQ1, origQ2, AsTransformation( LeftTranslation(Q1,g)*phi ), true );
            gamma := AsRightQuasigroupMapping( origQ1, origQ2, AsTransformation( phi ), true );
            return [ alpha, beta, gamma ];
        fi; 
    od; od;
    return fail; 
end );

# IsotopismRightQuasigroups

InstallMethod( IsotopismRightQuasigroups, "for two right quasigroups",
    [ IsRightQuasigroup, IsRightQuasigroup ],
function( Q1, Q2 )
    # REVISIT: Implement isotopism check for right quasigroups
    Error( "RQ: Not implemented yet." );
end );

# IsotopismQuasigroups

InstallMethod( IsotopismQuasigroups, "for two quasigroups",
    [ IsQuasigroup, IsQuasigroup ],
function( Q1, Q2 )
    return RQ_IsotopismAlgebras( IsQuasigroup, Q1, Q2, false ); # do not use principal loop isotopes
end );

# IsotopismLoops

InstallMethod( IsotopismLoops, "for two loops",
    [ IsLoop, IsLoop ],
function( Q1, Q2 )
    return RQ_IsotopismAlgebras( IsLoop, Q1, Q2, false );
end );

InstallOtherMethod( IsotopismLoops, "for two loops and bool",
    [ IsLoop, IsLoop, IsBool ],
function( Q1, Q2, viaPrincipalLoopIsotopes )
    return RQ_IsotopismAlgebras( IsLoop, Q1, Q2, viaPrincipalLoopIsotopes );    
end);

# RQ_AlgebrasUpToIsotopism

InstallMethod( RQ_AlgebrasUpToIsotopism, "for category, list of algebras and bool",
    [ IsObject, IsList, IsBool ],
function( category, ls, viaPrincipalLoopIsotopes )
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
            if not RQ_IsotopismAlgebras( category, Q, K, viaPrincipalLoopIsotopes ) = fail then
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
    return RQ_AlgebrasUpToIsotopism( IsRightQuasigroup, ls, false );
end );

InstallMethod( QuasigroupsUpToIsotopism, "for list of quasigroups",
    [ IsList ],
function( ls )
    return RQ_AlgebrasUpToIsotopism( IsQuasigroup, ls, false );
end );

InstallMethod( LoopsUpToIsotopism, "for list of loops",
    [ IsList ],
function( ls )
    return RQ_AlgebrasUpToIsotopism( IsLoop, ls, false );
end );

InstallOtherMethod( LoopsUpToIsotopism, "for list of loops and bool",
    [ IsList, IsBool ],
function( ls, viaPrincipalLoopIsotopes )
    return RQ_AlgebrasUpToIsotopism( IsLoop, ls, viaPrincipalLoopIsotopes );
end );

# AUTOTOPISM GROUPS OF RIGHT QUASIGROUPS
# _____________________________________________________________________________

#############################################################################
##  CONSTRUCTORS
##  -------------------------------------------------------------------------

InstallGlobalFunction( AutotopismObject@,
function( Q, f, g, h ) 
    local alpha;
    if ( IsMapping( f ) and ( Range( f ) <> Q or Source( f ) <> Q ) )
        or ( IsMapping( g ) and ( Range( g ) <> Q or Source( g ) <> Q ) )
        or ( IsMapping( h ) and ( Range( h ) <> Q or Source( h ) <> Q ) )
    then 
        Error( "RQ: wrong mappings given.");
    fi; 
    if ( IsTransformation(f) and not IsParentTransformation( Q, Q, f ) ) 
        or ( IsTransformation(g) and not IsParentTransformation( Q, Q, g ) ) 
        or ( IsTransformation(h) and not IsParentTransformation( Q, Q, h ) ) 
    then 
        Error( "RQ: wrong transformations given.");
    fi;
    if ( IsPerm(f) and not IsParentPerm( Q, f ) ) 
        or ( IsPerm(g) and not IsParentPerm( Q, g ) ) 
        or ( IsPerm(h) and not IsParentPerm( Q, h ) ) 
    then 
        Error( "RQ: wrong parent permutations given.");
    fi;
	alpha := Objectify(
		NewType(
            IsRightQuasigroupAutotopismObjectFamily, 
            IsRightQuasigroupAutotopismObject and IsRightQuasigroupAutotopismObjectRep
        ), [ f, g, h ] ); 
    SetAmbientRightQuasigroup( alpha, Q );
    return alpha;
end);

#############################################################################
##  DISPLAYING AND COMPARING ELEMENTS
##  -------------------------------------------------------------------------

InstallMethod( ViewObj, "for an autotopism object",
	[ IsRightQuasigroupAutotopismObject ],
function( obj )
	Print( "IsRightQuasigroupAutotopismObject(", obj![1], ", ", obj![2], ", ", obj![3], ")" );
end );

InstallMethod( Display, "for an autotopism object",
	[ IsRightQuasigroupAutotopismObject ],
function( obj )
	Print( "Autotopism object on a right quasigroup of order ", Size( AmbientRightQuasigroup( obj ) ) );
end );

InstallMethod( PrintObj, "for an autotopism object",
	[ IsRightQuasigroupAutotopismObject ],
function( obj )
	Print( "IsRightQuasigroupAutotopismObject(", obj![1], ", ", obj![2], ", ", obj![3], ")" );
end );

InstallMethod( \=, "for two autotopism objects",
	IsIdenticalObj,
	[ IsRightQuasigroupAutotopismObject, IsRightQuasigroupAutotopismObject ],
function( atop1, atop2 )
	return AmbientRightQuasigroup( atop1 ) = AmbientRightQuasigroup( atop2 ) and 
        [ atop1![1], atop1![2], atop1![3] ] = [ atop2![1], atop2![2], atop2![3] ];
end );

InstallMethod( \<, "for two autotopism objects",
	IsIdenticalObj,
	[ IsRightQuasigroupAutotopismObject, IsRightQuasigroupAutotopismObject ],
function( atop1, atop2 )
	return [ atop1![1], atop1![2], atop1![3] ] < [ atop2![1], atop2![2], atop2![3] ];
end );


#############################################################################
##  MULTIPLICATION
##  -------------------------------------------------------------------------

InstallMethod( \*, "for two autotopism objects",
	IsIdenticalObj,
	[ IsRightQuasigroupAutotopismObject, IsRightQuasigroupAutotopismObject ],
function( atop1, atop2 )
	if AmbientRightQuasigroup( atop1 ) = AmbientRightQuasigroup( atop2 ) then 
		return AutotopismObject@( 
            AmbientRightQuasigroup( atop1 ),
            atop1![1] * atop2![1],
            atop1![2] * atop2![2],
            atop1![3] * atop2![3]
        );
	else
		Error("RQ: Two autotopism objects must have the same ambient right quasigroup.");
	fi;
end );

InstallMethod( OneMutable, "for an autotopism object",
	[ IsRightQuasigroupAutotopismObject ],
function( atop )
	return AutotopismObject@( 
            AmbientRightQuasigroup( atop ),
            One( atop![1] ),
            One( atop![2] ),
            One( atop![3] )
        );
end );

#############################################################################
##  ACTIONS
##  -------------------------------------------------------------------------

# InstallMethod( \^, "for an autotopism object and ...",
# 	[ ..., IsRightQuasigroupAutotopismObject ],
# function( x, atop )
# end );

