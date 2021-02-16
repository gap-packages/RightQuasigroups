# Morphisms.gi
# Homomorphisms, isomorphisms and automorphisms of right quasigroups
# =============================================================================

# HOMOMORPHISMS OF RIGHT QUASIGROUPS
# _____________________________________________________________________________

# IsRightQuasigroupHomomorphism
# IsQuasigroupHomomorphism
# IsLoopHomomorphism

InstallMethod( IsRightQuasigroupHomomorphism, "for mapping",
    [ IsMapping ],
    f -> IsRightQuasigroupMapping( f ) and RespectsMultiplication( f )
);

InstallMethod( IsQuasigroupHomomorphism, "for mapping",
    [ IsMapping ],
    f -> IsQuasigroupMapping( f ) and RespectsMultiplication( f )
);

InstallMethod( IsLoopHomomorphism, "for mapping",
    [ IsMapping ],
    f -> IsLoopMapping( f ) and RespectsMultiplication( f )
);

# IsRightQuasigroupIsomorphism
# IsQuasigroupIsomorphism
# IsLoopIsomorphism

InstallMethod( IsRightQuasigroupIsomorphism, "for mapping",
    [ IsMapping ],
    f -> IsBijective( f ) and IsRightQuasigroupHomomorphism( f )
);

InstallMethod( IsQuasigroupIsomorphism, "for mapping",
    [ IsMapping ],
    f -> IsBijective( f ) and IsQuasigroupHomomorphism( f )
);

InstallMethod( IsLoopIsomorphism, "for mapping",
    [ IsMapping ],
    f -> IsBijective( f ) and IsLoopHomomorphism( f )
);

# IsRightQuasigroupEndomorphism
# IsQuasigroupEndomorphism
# IsLoopEndomorphism

InstallMethod( IsRightQuasigroupEndomorphism, "for mapping",
    [ IsMapping ],
    f -> Range( f ) = Source( f ) and IsRightQuasigroupHomomorphism( f )
);

InstallMethod( IsQuasigroupEndomorphism, "for mapping",
    [ IsMapping ],
    f -> Range( f ) = Source( f ) and IsQuasigroupHomomorphism( f )
);

InstallMethod( IsLoopEndomorphism, "for mapping",
    [ IsMapping ],
    f -> Range( f ) = Source( f ) and IsLoopHomomorphism( f )
);

# IsRightQuasigroupAutomorphism
# IsQuasigroupAutomorphism
# IsLoopAutomorphism

InstallMethod( IsRightQuasigroupAutomorphism, "for mapping",
    [ IsMapping ],
    f -> IsBijective( f ) and IsRightQuasigroupEndomorphism( f )
);

InstallMethod( IsQuasigroupAutomorphism, "for mapping",
    [ IsMapping ],
    f -> IsBijective( f ) and IsQuasigroupEndomorphism( f )
);

InstallMethod( IsLoopAutomorphism, "for mapping",
    [ IsMapping ],
    f -> IsBijective( f ) and IsLoopEndomorphism( f )
);

# RightQuasigroupHomomorphismByImages

InstallMethod( RightQuasigroupHomomorphismByImages, "for two right quasigroups",
    [ IsRightQuasigroup, IsRightQuasigroup ],
function( Q1, Q2 )
    return RightQuasigroupHomomorphismByImages( Q1, Q2, GeneratorsOfRightQuasigroup( Q1 ), GeneratorsOfRightQuasigroup( Q2 ) );
end );

InstallOtherMethod( RightQuasigroupHomomorphismByImages, "for two right quasigroups and list of images",
    [ IsRightQuasigroup, IsRightQuasigroup, IsList ],
function( Q1, Q2, imgs )
    return RightQuasigroupHomomorphismByImages( Q1, Q2, GeneratorsOfRightQuasigroup( Q1 ), imgs );
end );

InstallOtherMethod( RightQuasigroupHomomorphismByImages, "for two right quasigroups, list of generators and list of images",
    [ IsRightQuasigroup, IsRightQuasigroup, IsList, IsList ],
function( Q1, Q2, gens, imgs )
    local oldS, newS, f, domain, image, pairs, x, y, newNow, p, z, fz;
    # main method for RightQuasigroupHomomorphismByImages
    # checking parameters
    if not ForAll( gens, x -> x in Q1 ) then
        Error( "RQ: <3> must be a list of elements of <1>." );
    fi;
    if not ForAll( imgs, x -> x in Q2 ) then
        Error( "RQ: <4> must be a list of elements of <2>." );
    fi;
    if not Length( gens ) = Length( imgs ) then
        Error( "RQ: <3> and <4>must be lists of the same length." );
    fi;
    if not Subrightquasigroup( Q1, gens ) = Q1 then
        return fail; # same convention as for GroupHomomorphismByImages
    fi;
    # constructing homomorphism
    oldS := [ ];
    newS := ShallowCopy( gens );
    f := Set( [1..Length(gens)], i -> [gens[i],imgs[i]] ); 
    domain := Set( gens );
    image := function( x ) # x^f
        return f[ PositionSorted( domain, x ) ][ 2 ];
    end;

    repeat  
        pairs := [];
        for x in oldS do for y in newS do 
            Add( pairs, [ x, y ] ); 
            Add( pairs, [ y, x ] );
        od; od;
        for x in newS do for y in newS do
            Add( pairs, [ x, y ] );
        od; od;
        newNow := [];
        for p in pairs do
            x := p[ 1 ];
            y := p[ 2 ];
            z := x*y;
            fz := image(x)*image(y);
            if not z in domain then # new domain element
                AddSet( domain, z );
                AddSet( f, [z,fz] );
                Add( newNow, z );
            else # existing domain element, check for collision
                if not image( z ) = fz then
                    return fail; # same convention as for GroupHomomorphismByImages
                fi;
            fi;
        od;
        oldS := Union( oldS, newS );
        newS := ShallowCopy( newNow );
    until IsEmpty( newS );

    return MappingByFunction( Q1, Q2, image );
end );

# QuasigroupHomomorphismByImages

InstallMethod( QuasigroupHomomorphismByImages, "for two quasigroups",
    [ IsQuasigroup, IsQuasigroup ],
function( Q1, Q2 )
    return RightQuasigroupHomomorphismByImages( Q1, Q2, GeneratorsOfRightQuasigroup( Q1 ), GeneratorsOfRightQuasigroup( Q2 ) );
end );

InstallOtherMethod( QuasigroupHomomorphismByImages, "for two quasigroups and list of images",
    [ IsQuasigroup, IsQuasigroup, IsList ],
function( Q1, Q2, imgs )
    return RightQuasigroupHomomorphismByImages( Q1, Q2, GeneratorsOfRightQuasigroup( Q1 ), imgs );
end );

InstallOtherMethod( QuasigroupHomomorphismByImages, "for two quasigroups, list of generators and list of images",
    [ IsQuasigroup, IsQuasigroup, IsList, IsList ],
function( Q1, Q2, gens, imgs )
    return RightQuasigroupHomomorphismByImages( Q1, Q2, gens, imgs );
end );

# LoopHomomorphismByImages

InstallMethod( LoopHomomorphismByImages, "for two loops",
    [ IsLoop, IsLoop ],
function( Q1, Q2 )
    return LoopHomomorphismByImages( Q1, Q2, GeneratorsOfRightQuasigroup( Q1 ), GeneratorsOfRightQuasigroup( Q2 ) );
end );

InstallOtherMethod( LoopHomomorphismByImages, "for two loops and list of images",
    [ IsLoop, IsLoop, IsList ],
function( Q1, Q2, imgs )
    return RightQuasigroupHomomorphismByImages( Q1, Q2, GeneratorsOfRightQuasigroup( Q1 ), imgs );
end );

InstallOtherMethod( LoopHomomorphismByImages, "for two loops, list of generators and list of images",
    [ IsLoop, IsLoop, IsList, IsList ],
function( Q1, Q2, gens, imgs )
    return RightQuasigroupHomomorphismByImages( Q1, Q2, gens, imgs );
end );

# KernelRelationOfMapping

InstallMethod( KernelRelationOfMapping, "for mapping",
    [ IsMapping ],
function( f )
    local partition, elms, x;
    partition := List([1..Size(Range(f))], i -> [] );
    elms := Elements( Range( f ) );
    for x in Source(f) do
        Add( partition[ Position( elms, x^f ) ], x );
    od;
    return EquivalenceRelationByPartition( Source(f), partition );
end );

# KernelOfLoopHomomorphism

InstallMethod( KernelOfLoopHomomorphism, "for loop homomorphism",
    [ IsMapping ],
function( f )
    if not IsLoopHomomorphism( f ) then
        return Error( "RQ: <1> must be a loop homomorphism." );
    fi;
    return KernelOfLoopHomomorphismNC( f );
end );

InstallMethod( KernelOfLoopHomomorphismNC, "for loop homomorphism",
    [ IsMapping ],
function( f ) 
    local gens;
    gens := Filtered( Source( f ), x -> x^f = One( Range(f) ) );
    return Subloop( Source( f ), gens );
end );

# NaturalHomomorphismByCongruence

InstallMethod( NaturalHomomorphismByCongruence, "for right quasigroup congruence",
    [ IsEquivalenceRelation ],
function( C )
    if not IsRightQuasigroupCongruence( C ) then
        TryNextMethod();
    fi;
    return NaturalHomomorphismByCongruenceNC( C );
end );

InstallMethod( NaturalHomomorphismByCongruenceNC, "for right quasigroup congruence",
    [ IsEquivalenceRelation ],
function( C )
    local Q, F, gens, imgs, x, y;
    Q := Source( C );
    F := Q/C;
    gens := GeneratorsOfRightQuasigroup( Q );
    imgs := [];
    for x in gens do
        y := First( F, z -> x in UnderlyingSetElm( z ) ); # the underlying set of F concists of equivalence classes of C
        Add( imgs, y );
    od;
    return RightQuasigroupHomomorphismByImages( Q, F, gens, imgs );
end );

# NaturalHomomorphismByNormalSubloop

InstallMethod( NaturalHomomorphismByNormalSubloop, "for two loops",
    [ IsLoop, IsLoop ],
function( Q, S )
    if not ( IsSubloop( Q, S ) and IsNormal( Q, S ) ) then
        Error( "RQ: <2> must be a normal subloop of <1>." );
    fi;
    return NaturalHomomorphismByNormalSubloopNC( Q, S );
end );

InstallMethod( NaturalHomomorphismByNormalSubloopNC, "for two loops",
    [ IsLoop, IsLoop ],
function( Q, S )
    local F, gens, imgs, x, y;
    F := Q/S;
    gens := GeneratorsOfLoop( Q );
    imgs := [];
    for x in gens do
        y := First( F, z -> x in UnderlyingSetElm( z ) ); # the underlying set of F concists of cosets of S in Q
        Add( imgs, y );
    od;
    return RightQuasigroupHomomorphismByImages( Q, F, gens, imgs );
end );

# ISOMORPHISMS OF RIGHT QUASIGROUPS
# _____________________________________________________________________________

# IsomorphicCopyByPerm

InstallMethod( IsomorphicCopyByPerm, "for right quasigroup an permutation",
    [ IsRightQuasigroup, IsPerm ],
function( Q, f )
    local style;
    style := rec( indexBased := IsIndexBased( Q ), checkArguments := RQ_defaultConstructorStyle.checkArguments );
    return IsomorphicCopyByPerm( Q, f, style );
end );

InstallOtherMethod( IsomorphicCopyByPerm, "for right quasigroup, permutation and record",
    [ IsRightQuasigroup, IsPerm, IsRecord ],
function( Q, f, style )
    local copyQ;
    copyQ := RQ_AlgebraIsotopeByPerms( CategoryOfRightQuasigroup( Q ), Q, f, f, f, style );
    RQ_InheritProperties( Parent( Q ), copyQ ); 
    return copyQ;
end );

# RightQuasigroupIsomorph 
# QuasigroupIsomorph
# LoopIsomorph

InstallMethod( RightQuasigroupIsomorph, "for right quasigroup and permutation",
    [ IsRightQuasigroup, IsPerm ],
function( Q, f )
    local style;
    style := rec( indexBased := IsIndexBased( Q ), checkArguments := RQ_defaultConstructorStyle.checkArguments );
    return IsomorphicCopyByPerm( Q, f, style );
end );

InstallOtherMethod( RightQuasigroupIsomorph, "for right quasigroup, permutation and record",
    [ IsRightQuasigroup, IsPerm, IsRecord ],
function( Q, f, style )
    return IsomorphicCopyByPerm( Q, f, style );
end );

InstallMethod( QuasigroupIsomorph, "for quasigroup and permutation",
    [ IsQuasigroup, IsPerm ],
function( Q, f )
    local style;
    style := rec( indexBased := IsIndexBased( Q ), checkArguments := RQ_defaultConstructorStyle.checkArguments );
    return IsomorphicCopyByPerm( Q, f, style );
end );

InstallOtherMethod( QuasigroupIsomorph, "for quasigroup, permutation and record",
    [ IsQuasigroup, IsPerm, IsRecord ],
function( Q, f, style )
    return IsomorphicCopyByPerm( Q, f, style );
end );

InstallMethod( LoopIsomorph, "for loop and permutation",
    [ IsLoop, IsPerm ],
function( Q, f )
    local style;
    style := rec( indexBased := IsIndexBased( Q ), checkArguments := RQ_defaultConstructorStyle.checkArguments );
    return IsomorphicCopyByPerm( Q, f, style );
end );

InstallOtherMethod( LoopIsomorph, "for loop, permutation and record",
    [ IsLoop, IsPerm, IsRecord ],
function( Q, f, style )
    return IsomorphicCopyByPerm( Q, f, style );
end );

# IsomorphismDiscriminator

InstallMethod( IsomorphismDiscriminator, "for right quasigroup",
    [ IsRightQuasigroup ],
function( Q )
    local n, T, I, case, i, j, k, js, ks, count1, count2, ebo, A, P, B;
    # making sure the quasigroup is canonical
    if not IsCanonical( Q ) then
        Q := CanonicalCopy( Q );
    fi;
    n := Size( Q );
    T := MultiplicationTable( Q );

    # Calculating 9 invariants for four cases: right quasigroup, quasigroup, loop, power associative loop
    # I[i] will contain the invariant vector for ith element of Q
    I := List( [1..n], i -> 0*[1..9] );

    # invariant 1
    # to distinguish the 4 cases
    if not IsQuasigroup( Q ) then case := 0;
    elif not IsLoop( Q ) then case := 1;
    elif not IsPowerAssociative( Q ) then case := 2;
    else case := 3;
    fi;
    for i in [1..n] do I[i,1] := case; od;
    
    # invariant 2
    # for given x, cycle structure of R_x (, L_x)
    for i in [1..n] do
        I[i,2] := CycleStructurePerm( RightTranslation( Q, Q.(i) ) );
        if case > 0 then
            I[i,2] := [ I[i,2], CycleStructurePerm( LeftTranslation( Q, Q.(i) ) ) ];
        fi;
    od;

    # invariant 3
    if case < 2 then # am I an idempotent?
        for i in [1..n] do I[i,3] := T[i,i]=i; od;
    elif case = 2 then # am I an involution?
        for i in [1..n] do I[i,3] := T[i,i]=1; od;
    else # what's my order?
        for i in [1..n] do I[i,3] := Order( Q.(i) ); od;
    fi;

    # invariant 4
    if case <> 3 then # how many times am I a square ?
        for i in [1..n] do j := T[i,i]; I[j,4] := I[j,4] + 1; od;
    else # how many times am I a square, third power, fourth power?
        for i in [1..n] do I[i,4] := [0,0,0]; od;
        for i in [1..n] do
            j := T[i,i]; I[j][4][1] := I[j][4][1] + 1;
            j := T[i,j]; I[j][4][2] := I[j][4][2] + 1;
            j := T[i,j]; I[j][4][3] := I[j][4][3] + 1;
        od;
    fi;
    
    # invariant 5
    if case <> 3 then #  with how many elements do I commute?
        for i in [1..n] do
            I[i,5] := Length( Filtered( [1..n], j -> T[i,j] = T[j,i] ) );
        od;
    else # with how many elements of given order do I commute?
        ebo := List( [1..n], i -> Filtered( [1..n], j -> I[j,3]=i ) ); # elements by order. PROG: must point to order invariant
        ebo := Filtered( ebo, x -> not IsEmpty( x ) );
        for i in [1..n] do
            I[i,5] := List( ebo, J -> Length( Filtered( J, j -> T[i,j] = T[j,i] ) ) );
        od;
    fi;

    # invariant 6
    # is it true that (x*x)*x = x*(x*x)?
    for i in [1..n] do
        I[i,6] :=  T[T[i,i],i] = T[i,T[i,i]];
    od;

    # invariant 7
    if case <> 3 then # for how many elements y is (x*x)*y = x*(x*y)?
        for i in [1..n] do
            I[i,7] := Length( Filtered( [1..n], j -> T[T[i,i],j] = T[i,T[i,j]] ) );
        od;
    else # for how many elements y of given order is (x*x)*y=x*(x*y)
        for i in [1..n] do
            I[i,7] := List( ebo, J -> Length( Filtered( J, j -> T[T[i,i],j] = T[i,T[j,i]] ) ) );
        od;
    fi;

    # invariants 8 and 9 (these take longer)
    if case <> 3 then # with how many pairs of elements do I associate in the first, second position?
        for i in [1..n] do
            for j in [1..n] do for k in [1..n] do
                if T[i,T[j,k]] = T[T[i,j],k] then I[i,8] := I[i,8] + 1; fi;
                if T[j,T[i,k]] = T[T[j,i],k] then I[i,9] := I[i,9] + 1; fi;
            od; od;
        od;
    else # for how many pairs of elements of given orders do I associate in the first, second position?
        for i in [1..n] do
            I[i,8] := []; I[i,9] := [];
            for js in ebo do for ks in ebo do
                count1 := 0; count2 := 0;
                for j in js do for k in ks do
                    if T[i,T[j,k]] = T[T[i,j],k] then count1 := count1 + 1; fi;
                    if T[j,T[i,k]] = T[T[j,i],k] then count2 := count2 + 1; fi;
                od; od;
                Add( I[i,8], count1 ); Add( I[i,9], count2 );
            od; od;
        od;   
    fi;

    # all invariants have now been calculated
    
    # setting up the first part of the discriminator (invariants with the number of occurence)
    A := Collected( I );
    P := Sortex( List( A, x -> x[2] ) ); # rare invariants will be listed first, but the set ordering of A is otherwise not disrupted
    A := Permuted( A, P );

    # setting up the second part of the discriminator (blocks of elements invariant under isomorphisms)
    B := List( [1..Length(A)], j -> Filtered( [1..n], i -> I[i] = A[j,1] ) );

    return [ A, B ];
end );

# RQ_EfficientGenerators( Q, dis ) 
# Revisit: slow for larger projection right quasigroups

InstallMethod( RQ_EfficientGenerators, "for a right quasigroup",
    [ IsRightQuasigroup, IsList ],
function( Q, dis ) 
    local gens, sub, elements, candidates, max, S, best_gen, best_S;

    gens := [];                             # generating set to be returned
    sub := [];                              # substructure generated so far
    elements := Concatenation( dis[2] );    # all elements ordered by block size
    candidates := ShallowCopy( elements );  # candidates for next generator
    while sub <> Q do
        # find an element not in sub that most enlarges sub
        max := 0;
        while not IsEmpty( candidates ) do
            S := Subrightquasigroup( Q, Union( gens, [candidates[1]] ) );
            if Size(S) > max then
                max := Size( S );
                best_gen := candidates[1];
                best_S := S;
            fi;
            # discard elements of S since they cannot do better
            candidates := Filtered( candidates, x -> not Elements(Q)[x] in S );
        od;
        Add( gens, best_gen );
        sub := best_S;
        # reset candidates for next round
        candidates := Filtered( elements, x -> not Elements(Q)[x] in sub );
    od;
    return gens;
end );

# AreEqualIsomorphismDiscriminators
  
InstallMethod( AreEqualIsomorphismDiscriminators, "for two lists (discrimninators)",
    [ IsList, IsList ],
function( dis1, dis2 )
      return dis1[ 1 ] = dis2[ 1 ];
end );

# EXTENDING MAPPINGS (AUXILIARY)

# RQ_ExtendHomomorphismByClosingSource

InstallMethod( RQ_ExtendHomomorphismByClosingSource, "for a list and two tables",
    [ IsList, IsRectangularTable, IsRectangularTable ],
function( f, t1, t2 )
    local oldS, newS, pairs, x, y, newNow, p, z, fz;    
    oldS := [ ];
    newS := f[ 2 ];

    repeat  
        pairs := [];
        for x in oldS do for y in newS do 
            Add( pairs, [ x, y ] ); 
            Add( pairs, [ y, x ] );
        od; od;
        for x in newS do for y in newS do
            Add( pairs, [ x, y ] );
        od; od;
        newNow := [];
        for p in pairs do
            x := p[ 1 ];
            y := p[ 2 ];
            z := t1[ x, y ];
            fz := t2[ f[ 1, x ], f[ 1, y ] ];
            if f[ 1, z ] = 0 then
                f[ 1, z ] := fz; AddSet( f[ 2 ], z ); AddSet( f[ 3 ], fz );
                Add( newNow, z );
            else 
                if not f[ 1, z ] = fz then return fail; fi;
            fi;
        od;
        oldS := Union( oldS, newS );
        newS := ShallowCopy( newNow );
    until IsEmpty( newS );
    return f;           
end );

# RQ_SublistPosition

InstallMethod( RQ_SublistPosition, "for a list and an element",
    [ IsList, IsObject ],
function( S, x )
    local i;
    for i in [ 1..Length( S ) ] do if x in S[ i ] then return i; fi; od;
    return fail;
end);

# RQ_ExtendIsomorphism

InstallMethod( RQ_ExtendIsomorphism, "for a list, rectangular table, two lists, rectangular table and a list",
    [ IsList, IsRectangularTable, IsList, IsList, IsRectangularTable, IsList ],
function( f, Q1, gen1, dis1, Q2, dis2 )
    local x, possible_images, y, g;
    f := RQ_ExtendHomomorphismByClosingSource( f, Q1, Q2 );
    if f = fail or Length( f[ 2 ] ) > Length( f[ 3 ] ) then return fail; fi;
    if Length( f[ 2 ] ) = Length( Q1 ) then return f; fi; #isomorphism found
    
    x := gen1[ 1 ];
    gen1 := gen1{[2..Length(gen1)]}; 
    possible_images := Filtered( dis2[ RQ_SublistPosition( dis1, x ) ], y -> not y in f[ 3 ] );    
    for y in possible_images do
        g := StructuralCopy( f );
        g[ 1, x ] := y; AddSet( g[ 2 ], x ); AddSet( g[ 3 ], y );
        g := RQ_ExtendIsomorphism( g, Q1, gen1, dis1, Q2, dis2 );
        if not g = fail then return g; fi; #isomorphism found
    od;
    return fail;    
end );

# RQ_IsomorphismAlgebrasWithPrecalculatedData

InstallMethod( RQ_IsomorphismAlgebrasWithPrecalculatedData,
    "for category, right quasigroups, generators, discriminator, right quasigroup and discriminator",
    [ IsObject, IsRightQuasigroup, IsList, IsList, IsRightQuasigroup, IsList ],
function( category, Q1, gen1, dis1, Q2, dis2 ) # PROG: category is never used
    local map, iso;
    map := 0 * [ 1.. Size( Q1 ) ]; # empty mapping
    iso := RQ_ExtendIsomorphism( [ map, [ ], [ ] ], MultiplicationTable( Q1 ), gen1, dis1[2], MultiplicationTable( Q2 ), dis2[2] );
    if not iso = fail then
        return SortingPerm( iso[ 1 ] );
    fi;
    return fail;
end );

# RQ_IsomorphismAlgebras

InstallMethod( RQ_IsomorphismAlgebras, "for category and two right quasigroups",
    [ IsObject, IsRightQuasigroup, IsRightQuasigroup ],
function( category, Q1, Q2 ) # PROG: category is never used
    local origQ1, origQ2, dis1, dis2, gen1, iso;
   
    # making sure the quasigroups have canonical Cayley tables
    origQ1 := ShallowCopy( Q1 );
    origQ2 := ShallowCopy( Q2 );
    if not IsCanonical(Q1) then Q1 := CanonicalCopy( Q1 ); fi;
    if not IsCanonical(Q2) then Q2 := CanonicalCopy( Q2 ); fi;
    
    # precalculating data
    dis1 := IsomorphismDiscriminator( Q1 );
    dis2 := IsomorphismDiscriminator( Q2 );
    if not AreEqualIsomorphismDiscriminators( dis1, dis2 ) then
        return fail;
    fi;
    gen1 := RQ_EfficientGenerators( Q1, dis1 ); 
    iso := RQ_IsomorphismAlgebrasWithPrecalculatedData( category, Q1, gen1, dis1, Q2, dis2 );    
    if not iso = fail then
        iso := AsRightQuasigroupMapping( origQ1, origQ2, AsTransformation( iso ), true ); # true because iso is canonical
    fi;
    return iso;
end );

# IsomorphismRightQuasigroups
# IsomorphismQuasigroups
# IsomorphismLoops

InstallMethod( IsomorphismRightQuasigroups, "for two right quasigroups",
    [ IsRightQuasigroup, IsRightQuasigroup ],
function( Q1, Q2 )
    return RQ_IsomorphismAlgebras( IsRightQuasigroup, Q1, Q2 );
end );

InstallMethod( IsomorphismQuasigroups, "for two quasigroups",
    [ IsQuasigroup, IsQuasigroup ],
function( Q1, Q2 )
    return RQ_IsomorphismAlgebras( IsQuasigroup, Q1, Q2 );
end );

InstallMethod( IsomorphismLoops, "for two loops",
    [ IsLoop, IsLoop ],
function( Q1, Q2 )
    return RQ_IsomorphismAlgebras( IsLoop, Q1, Q2 );
end );

# RQ_AlgebrasUpToIsomorphism

InstallMethod( RQ_AlgebrasUpToIsomorphism, "for category and list of algebras",
    [ IsObject, IsList ],
function( category, ls )
    local kept, positions, pos, Q, dis, gen, with_same_D, is_new, K;
    
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
       
    kept := []; # kept algebras an their discriminators
    positions := []; # positions of kept algebras in the original list
    pos := 0;
    for Q in ls do
        pos := pos + 1;
        if not IsCanonical( Q ) then Q := CanonicalCopy( Q ); fi; # making canonical when seen for the first time
        dis := IsomorphismDiscriminator( Q );
        gen := RQ_EfficientGenerators( Q, dis );
        # testing against kept algebras with the same discriminator
        with_same_D := Filtered( kept, K -> AreEqualIsomorphismDiscriminators( K[2], dis ) );
        is_new := true;
        for K in with_same_D do
            if not RQ_IsomorphismAlgebrasWithPrecalculatedData( category, Q, gen, dis, K[1], K[2] ) = fail then
                is_new := false;
                break;
            fi;
        od;
        if is_new then
            Add( kept, [ Q, dis ] ); # storing discriminator, too
            Add( positions, pos );
        fi;
    od;
    # returning algebras from the original list
    return ls{positions};
end );

# RightQuasigroupsUpToIsomorphism
# QuasigroupsUpToIsomorphism
# LoopsUpToIsomorphism

InstallMethod( RightQuasigroupsUpToIsomorphism, "for list of right quasigroups",
    [ IsList ],
    ls -> RQ_AlgebrasUpToIsomorphism( IsRightQuasigroup, ls )
);

InstallMethod( QuasigroupsUpToIsomorphism, "for list of right quasigroups",
    [ IsList ],
    ls -> RQ_AlgebrasUpToIsomorphism( IsQuasigroup, ls )
);

InstallMethod( LoopsUpToIsomorphism, "for list of right quasigroups",
    [ IsList ],
    ls -> RQ_AlgebrasUpToIsomorphism( IsLoop, ls )
);

# AUTOMORPHISM GROUPS OF RIGHT QUASIGROUPS
# _____________________________________________________________________________

# RQ_AutomorphismsFixingSet

InstallMethod( RQ_AutomorphismsFixingSet, "for a list, right quasigroup and two lists",
    [ IsList, IsRightQuasigroup, IsList, IsList ],
function( S, Q, GenQ, DisQ )
    local n, x, A, possible_images, y, i, map, g;
    
    # this is faster than extending a map
    n := Size( Q );
    S := RQ_Subalgebra( Q, S );             # can be empty if the argument S is empty
    if Size( S ) = n then return []; fi;    # identity, no need to return
    S := ParentInd( S );
    
    # pruning blocks
    DisQ := List( DisQ, B -> Filtered( B, x -> not x in S ) );
    
    # first unmapped generator
    x := GenQ[ 1 ]; 
    GenQ := GenQ{[2..Length(GenQ)]};
    
    A := [];
    
    possible_images := Filtered( DisQ[ RQ_SublistPosition( DisQ, x ) ], y -> y <> x );   
    for y in possible_images do
        # constructing map
        map := 0*[1..n];
        for i in [1..n] do if i in S then map[ i ] := i; fi; od;
        map[ x ] := y;
        g := [ map, Union( S, [ x ] ), Union( S, [ y ] ) ];
        # extending map
        g := RQ_ExtendIsomorphism( g, MultiplicationTable( Q ), GenQ, DisQ, MultiplicationTable( Q ), DisQ );
        if not g = fail then AddSet( A, g[ 1 ] ); fi;
    od;
    
    S := Union( S, [ x ] );
    return Union( A, RQ_AutomorphismsFixingSet( S, Q, GenQ, DisQ ) );  
end );

# AutomorphismGroup

InstallOtherMethod( AutomorphismGroup, "for right quasigroup",
    [ IsRightQuasigroup ],
function( Q )
    local origQ, DisQ, GenQ, gens;
    # will work with canonical copy
    origQ := ShallowCopy( Q );
    if not IsCanonical( Q ) then  Q := CanonicalCopy( Q ); fi;
    DisQ := IsomorphismDiscriminator( Q );
    GenQ := RQ_EfficientGenerators( Q, DisQ );
    gens := RQ_AutomorphismsFixingSet( [ ], Q, GenQ, DisQ[2] ); # using only the disciminator blocks 
    if IsEmpty( gens ) then return Group( () ); fi; # no notrivial automorphism
    gens := List( gens, p -> SortingPerm( p ) ); # canonical perms
    gens := List( gens, g -> AsParentPerm( origQ, g ) ); # convert to parent perms
    return RQ_GroupByGenerators( SmallGeneratingSet( Group( gens ) ) );
end );