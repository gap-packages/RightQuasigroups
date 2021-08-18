# this file was used to redo the library of Moufang loops while transitioning from LOOPS to RQ

# the ordering of loops in the library is the same

# activation of loops is much faster

# there is no need reorder according to Goodaire et al (the data is already reordered)

# the data file is 2.5x bigger than in LOOPS

lib := RQ_moufang_data;;
newlib := [1,2,3];;
newlib[1] := ShallowCopy( lib[1] );;
newlib[2] := ShallowCopy( lib[2] );;
newlib[3] := List([1..Length(lib[3])], i -> List([1..Length(lib[3,i])], j -> []));;

mloops := List([1..243], i->[]);;

groups := List([1..81], n -> AllGroups( n ) );;
groups := List(groups, gps -> List( gps, AsLoop ) );;

#auxiliary
CheckMoufangLoops := function()
    local n, p, m, m_inv;
    for n in Filtered( lib[1], m -> m<64 ) do
        p := Position( lib[1], n );
        for m in [1..lib[2,p]] do
            m_inv := m^Inverse(PG[n]);
            if lib[3][p][m_inv][2] <> ['G'] then
                if Size(Center(MoufangLoop(n,m)))=1 then
                    return [n,m];
                fi;
            fi;
        od;
    od;
    return true;
end;

MovePerm := function( f, d )
# moves the domain of f by d
    local ls;
    ls := ListPerm( f );
    ls := Concatenation( [1..d], ls+d );
    return PermList( ls );
end;

FattenPerm := function( f, d )
# fatten f by the factor of d
    local ls;
    ls := ListPerm( f );
    ls := Concatenation( List( ls, x -> [1..d]+(x-1)*d ) );
    return PermList( ls );
end;

ReconstructLoop := function( Q )
    local n, K, nK, cosets, f, i, fK, posK, F, nF, fF, posF, tF, T, j, theta, mu, g, coc, ret;
    n := Size( Q );
    if not IsCanonical( Q ) then Q := CanonicalCopy( Q ); fi;
    # reorder Q by cosets of K
    K := Center( Q );
    nK := Size( K );
    if nK=1 then Error("Centerless!"); fi;
    cosets := Concatenation( List( RightCosets( Q, K ), ParentInd ) );
    f := PermList( cosets );
    Q := LoopIsomorph( Q, f^-1 );
    # make K agree with the GAP group
    K := Center( Q );
    i := 0;
    repeat
        i := i + 1;
        fK := IsomorphismLoops( K, groups[nK,i] );
    until fK <> fail;
    posK := i;
    f := fK;
    for i in [1..n/nK-1] do 
        f := f*MovePerm(fK,i*nK);
    od;
    Q := LoopIsomorph( Q, f );
    # identify factor
    K := Center( Q );
    F := Q/K;
    nF := Size( F );
    i := 0;
    repeat
        i := i + 1;
        if IsAssociative( F ) then
            fF := IsomorphismLoops( F, groups[nF,i] );
        else
            fF := IsomorphismLoops( F, mloops[nF,i] );
        fi;
    until fF <> fail;
    posF := i;
    f := fF;
    # build cocycle (with a twist)
    tF := MultiplicationTable( F );
    T := RightTransversal( Q, K );
    theta := List( [1..nF], i -> List( [1..nF], j ->
        Position( Elements( K ), (T[i]*T[j])/T[tF[i,j]] )
    ) );
    g := Inverse( f );
    mu := List([1..nF], i -> List( [1..nF], j -> theta[ i^g, j^g ] ) ); # (a,x) --> (a,f(x)) is an isomorphism
    # return the result
    ret := [ nK, posK ];
    if IsAssociative( F ) then
        Add( ret, "g" );
    else
        Add( ret, "m" );
    fi;
    coc := RQ_EncodeCocycle( mu, [1..nK] );
    return Concatenation( ret, [ nF, posF, coc[2], coc[3] ] ); # no need to store coc[1] = nF
end;

PopulateLibrary := function()
    local n, p, m, m_inv, d, G, pos, dat, F, K, cocycle;
    for n in lib[1] do
        p := Position( lib[1], n );
        for m in [1..lib[2,p]] do
            Info(InfoWarning,1,Concatenation(String(n),":",String(m)));
            m_inv := m^Inverse(PG[n]);
            d := lib[3][p][m_inv];
            if d[2] = ['G'] then
                G := AllGroups( d[ 3 ], IsCommutative, false)[ d[ 4 ] ];
                # must locate G among all groups, not just among commutative ones
                pos := First( groups[d[3]], H -> IsomorphismLoops( H, AsLoop( G ) ) <> fail );
                pos := Position( groups[d[3]], pos );
                newlib[3][p][m] := [d[3],pos]; # group id for the chein loop
                mloops[n,m] := CheinLoop( SmallGroup( d[3], pos ) );
                #if IsAssociative( mloops[n,m] ) then Error("Associative Chein!"); fi;
                #if LoopSatisfiesIdentity( mloops[n,m], "x*(y*(x*z)) = ((x*y)*x)*z" ) <>true then Error("non Moufang Chein!"); fi;
                #if IsomorphismLoops( mloops[n,m], MoufangLoop( n, m ) ) = fail then Error("not isomorphic Chein"); fi;
            else # not a Chein loop
                dat := ReconstructLoop( MoufangLoop( n, m ) );
                newlib[3][p][m] := ShallowCopy( dat );
                if dat[3] = "g" then
                    F := AsLoop( SmallGroup( dat[4], dat[5] ) );
                else
                    F := mloops[ dat[4], dat[5] ];
                fi;
                K := AsLoop( SmallGroup( dat[1], dat[2] ) );
                cocycle := RQ_DecodeCocycle( [ dat[4], dat[6], dat[7] ], [1..Size(K)] );
                #cocycle := dat[6];
                mloops[n,m] := LoopByCentralExtension( K, F, cocycle );
                #if IsAssociative( mloops[n,m] ) then Error("Associative!"); fi;
                #if LoopSatisfiesIdentity( mloops[n,m], "x*(y*(x*z)) = ((x*y)*x)*z" )<>true then Error("non Moufang!"); fi;
                #if IsomorphismLoops( mloops[n,m], MoufangLoop( n, m ) ) = fail then Error("not isomorphic"); fi;
            fi;
        od;
    od;
    return true;
end;

SaveLibrary := function()
    local f, i, n, j, s, x, k;
    f := "newmoufang.tbl";
    # header
    PrintTo( f, "# moufang.tbl\n# Library of Moufang loops\n" );
    AppendTo( f, "# =============================================================================\n\n" );
    # [1] and [2]
    AppendTo( f, "RQ_moufang_data :=\n[\n") ;
    AppendTo( f, "# implemented orders\n", newlib[1], ",\n" );
    AppendTo( f, "# number of loops of given order\n", newlib[2], ",\n" );
    # [3]
    AppendTo( f, "[\n" );
    for i in [1..Length(newlib[1])] do
        n := newlib[1,i];
        AppendTo( f, "# loops of order ", n, "\n[\n" );
        for j in [1..newlib[2,i]] do
            s := "[";
            for k in [1..Length(newlib[3][i][j])] do
                x :=  newlib[3][i][j][k];
                if IsString( x ) then
                    Add( s, '"' ); s := Concatenation( s, x ); Add( s, '"' );
                else 
                    s := Concatenation( s, String(x) );
                fi;
                if k < Length(newlib[3][i][j]) then
                    s := Concatenation(s,",");
                else 
                    s := Concatenation(s,"]");
                fi;
            od;
            AppendTo( f, s );
            if j < newlib[2,i] then
                AppendTo( f, ",\n" );
            else
                AppendTo( f, "\n]");
            fi;
        od;
        if i < Length( newlib[1]) then
            AppendTo( f, ",\n" );
        else
            AppendTo( f, "\n]" );
        fi;
    od;
    AppendTo( f, "\n];");
    return true;
end;