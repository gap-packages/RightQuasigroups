# Extensions.gi
# Nuclear and central extensions
# =============================================================================

# NUCLEAR AND CENTRAL EXTENSIONS OF LOOPS
# _____________________________________________________________________________

# IsLoopCocycle

InstallMethod( IsLoopCocycle, "for two loops and square table",
    [ IsLoop, IsLoop, IsRectangularTable ],
function( K, F, theta )
    return IsLoopCocycle( K, F, theta, false ); 
end );

InstallOtherMethod( IsLoopCocycle, "for two loops, square table and bool",
    [ IsLoop, IsLoop, IsRectangularTable, IsBool ],
function( K, F, theta, reportErrors )
    local nK, nF, oneK, oneF;
    nK := Size( K );
    nF := Size( F );
    oneK := Position( Elements( K ), One( K ) );
    oneF := Position( Elements( F ), One( F ) );
    if not Length( theta ) = nF and ForAll( theta, row -> Length( row ) = nF ) then
        return RQ_OptionalError( "RQ: The cocycle must be an |F|x|F| table.", reportErrors );
    fi;
    if not ForAll( theta, row -> ForAll( row, x -> x in [1..nK] ) ) then
        return RQ_OptionalError( "RQ: The cocycle must have entires in [1..|K|].", reportErrors );
    fi;
    if not Set( theta[ oneF ] ) = [ oneK ] and Set( theta{[1..nF]}[ oneF ] ) = [ oneK ] then 
        return RQ_OptionalError( "RQ: The cocycle must be normalized.", reportErrors );
    fi;
    return true;
end );

# LoopByNuclearExtension

InstallMethod( LoopByNuclearExtension, "for a commutative associative loop, loop, automorhisms and cocycle",
    [ IsLoop, IsLoop, IsList, IsRectangularTable ],
function( K, F, phi, theta )
    return LoopByNuclearExtension( K, F, phi, theta, RQ_defaultConstructorStyle );
end);

InstallOtherMethod( LoopByNuclearExtension, "for a commutative associative loop, loop, automorhisms, cocycle and record",
    [ IsLoop, IsLoop, IsList, IsRectangularTable, IsRecord ],
function( K, F, phi, theta, style )                              
    local nK, nF, tK, tF, uSet, t, a, b, x, y, c, z, Q;
    RQ_CompleteConstructorStyle( style );
    nK := Size( K );
    nF := Size( F );
    tK := MultiplicationTable( K );
    tF := MultiplicationTable( F );
    if style.checkArguments then
        if not ( IsCommutative( K ) and IsAssociative( K ) ) then
            Error( "RQ: <1> must be a commutative, associative loop." );
        fi;
        # check phi
        if not ForAll( phi, f -> IsPerm( f ) and IsSubset( [1..nK], MovedPoints( f ) ) ) then
            Error( "RQ: <3> must be a list of permutations on [1..|K|].");
        fi;
        if not ForAll( [1..nF], i -> ForAll( [1..nF], j -> phi[i]*phi[j] = phi[ tF[i,j] ] ) ) then
            Error( "RQ: <3> must represent a homomorphism from F to K.");
        fi;
        # check theta
        IsLoopCocycle( K, F, theta, true );
    fi;
    uSet := Concatenation( List( UnderlyingSet( K ), a -> List( UnderlyingSet( F ), x -> [a,x] ) ) );
    # the ordering of K x F is [1,1], [1,2], ..., [1,|F|], [2,1], [2,2], ...

    # PROG: If F x K is preferred, uncomment the lines below
    #uSet := Concatenation( List( UnderlyingSet( F ), x -> List( UnderlyingSet( K ), a -> [x,a] ) ) );

    t := List( [1..nK*nF], i -> 0 * [1..nK*nF] );
    # constructing the multiplication table
    for a in [1..nK] do for b in [1..nK] do for x in [1..nF] do for y in [1..nF] do
        c := tK[ tK[ a, b^phi[x] ], theta[ x, y ] ]; # a*phi_x(b)*theta(x,y)
        z := tF[ x, y ];
        #t[ nK*(x-1) + a, nK*(y-1) + b ] := nK*(z-1) + c;
        t[ nF*(a-1) + x, nF*(b-1) + y ] := nF*(c-1) + z;
    od; od; od; od;
    Q := LoopByCayleyTable( t, rec( indexBased := true, checkArguments := false ) );
    ChangeUnderlyingSet( Q, uSet );
    return Q;
end);

InstallOtherMethod( LoopByNuclearExtension, "for an extension (list) and record",
    [ IsList, IsRecord ],
function( ext, style )
    if not Length( ext ) = 4 then
        Error(" RQ: <1> must be a list of length 4.");
    fi;
    return LoopByNuclearExtension( ext[1], ext[2], ext[3], ext[4], style );
end); 

InstallOtherMethod( LoopByNuclearExtension, "for an extension (list)",
    [ IsList ],
function( ext )
    LoopByNuclearExtension( ext, RQ_defaultConstructorStyle );
end); 

# LoopByCentralExtension

InstallMethod( LoopByCentralExtension, "for a commutative associative loop, loop and cocycle",
    [ IsLoop, IsLoop, IsRectangularTable ],
function( K, F, theta )
    return LoopByCentralExtension( K, F, theta, RQ_defaultConstructorStyle );
end);

InstallOtherMethod( LoopByCentralExtension, "for a commutative associative loop, loop, cocycle and record",
    [ IsLoop, IsLoop, IsRectangularTable, IsRecord ],
function( K, F, theta, style )
    local phi;
    phi := List( [1..Size(F) ], i -> () );
    return LoopByNuclearExtension( K, F, phi, theta, style );
end);

InstallOtherMethod( LoopByCentralExtension, "for an extension (list) and record",
    [ IsList, IsRecord ],
function( ext, style )
    if not Length( ext ) = 3 then
        Error(" RQ: <1> must be a list of length 3.");
    fi;
    return LoopByNuclearExtension( ext[1], ext[2], ext[3], style );
end); 

InstallOtherMethod( LoopByCentralExtension, "for an extension (list)",
    [ IsList ],
function( ext )
    LoopByNuclearExtension( ext, RQ_defaultConstructorStyle );
end); 

# AsSquareTable

InstallMethod( AsSquareTable, "for list of square length",
    [ IsList ],
function( ls )
    local n;
    n := Sqrt( Length( ls ) );
    if not IsInt( n ) then  
        Error( "RQ: <1> must be a list of length n^2 for some n." );
    fi;
    return List( [0..n-1], i -> ls{[i*n+1..(i+1)*n]} );
end);

# NuclearExtensionByNormalSubloop

InstallMethod( NuclearExtensionByNormalSubloop, "for loop and normal commutative nuclear subloop",
    [ IsLoop, IsLoop ],
function( Q, K )                
    local cosets, f, copyK, F, mtF, t, phi, theta;
    # checking arguments
    if not IsNormal( Q, K ) then Error("RQ: <2> must be a normal subloop of <1>."); fi;
    if not IsCommutative( K ) then Error("RQ: <2> must be a commutative loop."); fi;
    if not IsSubloop( Nuc( Q ), K ) then Error("RQ: <2> must be contained in the nucleus of <1>."); fi;
    # reordering Q nicely into cosets of K (not needed but it aids in visualization)
    cosets := Concatenation( List( RightCosets( Q, K ), ParentInd ) );
    f := PermList( cosets );
    Q := LoopIsomorph( Q, f^-1 );
    copyK := Subloop( Q, Elements(Q){[1..Size(K)]} );
    F := Q/copyK;
    mtF := MultiplicationTable( F );
    t := RightTransversal( Q, copyK );
    # action
    phi := List( t, x -> RestrictedPerm( MiddleInnerMapping( Q, x ), [1..Size(K)] ) );
    # cocycle
    theta := List( [1..Size(F)], i -> List( [1..Size(F)], j ->
        Position( Elements( copyK ), (t[i]*t[j])/t[mtF[i,j]] )
    ) );
    return [ K, F, phi, theta ]; # PROG: could also return copyK instead of K
end );

# CentralExtensionByNormalSubloop
InstallMethod( CentralExtensionByNormalSubloop, "for loop and central subloop",
    [ IsLoop, IsLoop ],
function( Q, K )
    # checking arguments
    if not IsSubloop( Center( Q ), K ) then Error( "RQ: <2> must be a central subloop of <1>." ); fi;
    return NuclearExtensionByNormalSubloop( Q, K ){[1,2,4]};
end );

# CENTRAL EXTENSIONS IN A VARIETY
# _____________________________________________________________________________

# LoopCocyclesInVariety

InstallMethod( LoopCocyclesInVariety, "for loop, prime and equational basis",
    [ IsLoop, IsPosInt, IsList ],
function( Q, p, equationalBasis )

    local ct, ld, rd, n, one,
        empty_vec, ret, row,
        addCoeff, addCoeffsFromTerm,
        id, pos, varNames, varPos, nVars, lhs, rhs, odometer, i;

    # checking parameters
    if not IsPrime( p ) then
        Error( "RQ: <2> must be a prime number." );
    fi;
    if not ForAll( equationalBasis, id -> LoopSatisfiesIdentity( AsLoop( CyclicGroup( p ) ), id )=true ) then
        Error( "RQ: One of the identities of <3> does not hold in the cyclic group of order <2>.");
    fi;
    if not ForAll( equationalBasis, id -> LoopSatisfiesIdentity( Q, id ) = true ) then
        Error( "RQ: One of the identities of <3> does not hold in the loop <1>.");
    fi;

    # making everything canonical
    if not IsCanonical( Q ) then Q := CanonicalCopy( Q ); fi;
    ct := CayleyTable( Q );
    ld := LeftDivisionCayleyTable( Q );
    rd := RightDivisionCayleyTable( Q );
    n := Size( Q );
    one := Position( Elements( Q ), One( Q ) );
       
    # add normalizing identities
    equationalBasis := Concatenation( [ "1*x=x", "x*1=x" ], equationalBasis );
    
    # preparing the vector representing the cocycle
    empty_vec := 0*Z(p)*[1..n^2];
    ConvertToVectorRep( empty_vec, p );
    ret := [ ShallowCopy( empty_vec ) ]; # system of linear equations that determines the vector space of cocycles

    # function that sets the coefficient corresponding to f(x,y) to v
    addCoeff := function( x, y, v ) # assumes that global variables row and n has been initiated
        AddCoeffs( row, [ (x-1)*n + y ], [v], [1], 1 );
    end;
    
    # function that adds coefficents according to a term
    # and returns the value of what has just been evaluated 
    # PROG: depth first recursion
    addCoeffsFromTerm := function( t, multiplier ) # use multiplier=1 for lhs, multiplier=-1 for rhs
        local x, y;
        if IsInt( t ) then return RQ_parserVars[ t ]; fi; # variable, no coeff to add
        if t = '1' then return one; fi; # one, no coeff to add
        # binary operation
        x := addCoeffsFromTerm( t[1], multiplier );
        y := addCoeffsFromTerm( t[3], multiplier );
        if t[2] = '*' then                  # (x,a)*(y,b) = (x*y, a+b+f(x,y) )
            addCoeff( x, y, multiplier ); 
            return ct[ x, y ];
        elif t[2] = '/' then                # (x,a)/(y,b) = (x/y, a-b-f(x/y,y) ) 
            addCoeff( rd[ x, y ], y, -multiplier ); 
            return rd[ x, y ];
        else # left division                # (x,a)\(y,b) = (x\y, b-a-f(x,x\y) )
            addCoeff( x, ld[ x, y ], - multiplier );
            return ld[ x, y ];
        fi;
    end;

    # adding linear equations for each identity
    for id in equationalBasis do
        varNames := Intersection( RQ_parserVarNames, id ); # variables present in the identity
        varPos := List( varNames, x -> Position( RQ_parserVarNames, x ) ); # their positions among all potential variables
        nVars := Length( varNames );
        pos := Position( id, '=' ); # position of the equality sign
        lhs := LoopTermFromString( id{[1..pos-1]} );
        rhs := LoopTermFromString( id{[pos+1..Length(id)]} );
        # iterate over all values of variables
        for odometer in IteratorOfTuples( [1..n], nVars ) do 
            for i in [1..nVars] do
                RQ_parserVars[ varPos[ i ] ] := odometer[ i ];
            od;
            row := ShallowCopy( empty_vec );    # new linear equation
            addCoeffsFromTerm( lhs, 1 );        # left hand side with a plus sign
            addCoeffsFromTerm( rhs, -1 );       # right hand side with a minus sign
            Add( ret, row );
        od;
        # REVISIT: Reduce system periodically to reduce memory usage.
    od;

    # basis of the vector space of cocycles
    ret := MutableCopyMat( BaseMat( ret ) );
     
    # returning the solution of the system, i.e., the A-loop cocycles
    return NullspaceMat( TransposedMat( ret ) );

end );

        # REVISIT
        #  the following is probably faster for the "odometer" for-cycle above
        #odometer := List([1..nVars], i -> 1); # least significant value on the right, as usual
        #for i in [1..nVars] do # init variable values
        #    RQ_parserVars[ varPos[ i ] ] := 1;
        #od;
        #done := false;  
        #while not done do
        #    row := ShallowCopy( empty_vec );    # new linear equation
        #    addCoeffsFromTerm( lhs, 1 );        # left hand side with a plus sign
        #    addCoeffsFromTerm( rhs, -1 );       # right hand side with a minus sign
        #    Add( ret, row );
        #    i := nVars;
        #    while i>0 and odometer[ i ] = n do
        #        odometer[ i ] := 1;
        #        RQ_parserVars[ varPos[ i ] ] := 1;
        #        i := i - 1;
        #    od;
        #    if i=0 then # done
        #        done := true;
        #    else # increment odometer in ith digit
        #        odometer[ i ] := odometer[ i ] + 1;
        #        RQ_parserVars[ varPos[ i ] ] := odometer[ i ];
        #    fi;

# LoopCoboundaries

InstallMethod( LoopCoboundaries, "for loop and prime", 
    [ IsLoop, IsPosInt ],
function( Q, p )
    local ret, n, ct, z, tau, f, x, y, row;

    ret:=[];
    if not IsCanonical( Q ) then Q := CanonicalCopy( Q ); fi;
    n := Size( Q );
    ct := CayleyTable( Q );
    for z in Difference( [1..n], [ Position( Elements( Q ), One( Q ) ) ] ) do
        tau := 0*Z(p)*[1..n];
        tau[z] := Z(p);
        f := List([1..n],i->0*[1..n]);
        for x in [1..n] do for y in [1..n] do
            f[x,y] := tau[ct[x,y]] - tau[x] - tau[y];
        od; od;
        row := Concatenation( f );
        ConvertToVectorRep( row );
        Add(ret, row );
    od;
    return BaseMat( ret );
end );

# LoopCocyclesModAction

InstallMethod( LoopCocyclesModAction, "for loop, prime, cocycles and coboundaries",
    [ IsLoop, IsPosInt, IsList, IsList ],
function( Q, p, coc, cob )
    local factorsets, exts, on_fs, autgr, ret, space;

    factorsets := BaseMat( Concatenation( cob, coc ) );
    # basis for the complement of coboundaries in the v.s. of cocycles
    exts := factorsets{[Length(cob)+1..Length(factorsets)]};
   
    # defining the action of the automorphism group on cocycles
    on_fs := function( fs, perm )
        local n, ret, a, b;
        n := Sqrt( Length( fs ) );
        ret := [];
        for a in [1..n] do
            for b in [1..n] do
                ret[ (a-1)*n+b ] := fs[ (a^(perm^-1)-1)*n + b^(perm^-1) ];
            od;
        od;
        ret := SolutionMat( factorsets, ret ){[Length(cob)+1..Length(factorsets)]};
        return ret*exts;
    end;
    # calculating the group
    autgr := List( SmallGeneratingSet( AutomorphismGroup( Q ) ), g->
                           List( exts, v->SolutionMat( exts, on_fs(v,g) ) )
                   );
    autgr := Group( autgr );

    # calculating representatives of orbits of the group on coc/cob
    ret:=[];
    space:=Elements( GF(p)^Length( exts ) );
    while space<>[] do
        # adding the first surviving element of the complement (in fact an isomorphic copy of the complement)
        Add( ret, space[1] );
        # removing the orbit of the elements from the vector space
        space := Difference( space, Orbit( autgr, space[1] ) );
    od;

    # returning the survivors with respect to the original basis
    return List( ret, x->x*exts );
end );

# AllLoopCocyclesInVariety
InstallMethod( AllLoopCocyclesInVariety, "for loop, prime and list of loop identities",
    [ IsLoop, IsPosInt, IsList ],
function( Q, p, equationalBasis )
    local cob, coc; 
    if not IsCanonical( Q ) then Q := CanonicalCopy( Q ); fi;
    Info( InfoRightQuasigroups, 1, "RQ: Calculating coboundaries");
    cob := LoopCoboundaries( Q, p );
    Info( InfoRightQuasigroups, 1, "RQ: Coboundaries have dimension ", Length(cob) );
    Info( InfoRightQuasigroups, 1, "RQ: Calculating cocycles");
    coc := LoopCocyclesInVariety( Q, p, equationalBasis );	
    Info( InfoRightQuasigroups, 1, "RQ: Cocycles have dimension ", Length(coc) );
    return LoopCocyclesModAction( Q, p, coc, cob );
end );

# AllLoopCentralExtensionsInVariety
InstallMethod( AllLoopCentralExtensionsInVariety, "for loop, prime and list of loop identities",
    [ IsLoop, IsPosInt, IsList ],
function( Q, p, equationalBasis )
    local coc, Zp;
    coc := AllLoopCocyclesInVariety( Q, p, equationalBasis );
    # convert cocycle entries to integers 1,...,p
    coc := List( coc, f -> List( f, x -> IntFFE(x)+1 ) );
    # convert cocycles to square tables
    coc := List( coc, AsSquareTable );
    # the normal subloop is a multiplicative group of order p
    Zp := LoopByFunction( [0..p-1], function(x,y) return (x+y) mod p; end ); 
    return List( coc, f -> LoopByCentralExtension( Zp, Q, f ) );
end );