# Enumeration of racks and quandles
# Petr Vojtechovsky, 2018

# Main idea: Fix a subgroup G of S_n up to conjugacy. Find all quandles/racks Q with <L_x:x> = G.
# Memory management and indexing must be done carefully.

# CONSTRUCTING QUANDLES UP TO ISOMORPHISM
# Note: The main methods.
# -------------------------------------------------------------------

# indexing functions IntToCounter and CounterToInt

IntToCounter := function( i, ls )
# Example: Suppose that ls = [3,2]. Then possible counter states are
# [1,1], [1,2], [2,1], [2,2], [3,1], [3,2], in this order.
# This method returns the ith element on this list.
    local m, counter, j;
    m := Length( ls );
    i := i-1;
    counter := 0*[1..m];
    for j in [1..m] do
        counter[ m-j+1 ] := i mod ls[ m-j+1 ];
        i := (i - counter[ m-j+1 ])/ls[ m-j+1 ];
    od;
    counter := counter + 1; # adds 1 in all slots
    return counter;
end;

CounterToInt := function( counter, ls )
# the inverse function to IntToCounter
    local m, sum, prod, j;
    m := Length( ls );
    counter := counter - 1; # subtracts 1 from all slots
    sum := 0;
    prod := 1;
    for j in [1..m] do
        sum := sum + prod*counter[m-j+1];
        prod := prod*ls[m-j+1];
    od;
    sum := sum + 1;
    return sum;
end;

# global variables

GROUPS := [];
RACKS := [];
KEEP := [ true ];

# main functions

AllRacksOrQuandlesForGroup := function( n, group_index, reductive, racks_or_quandles )

        local G, orbs, reps, m, stabs, cands, cands_lengths, upper_bound, trans, gens, orb_locators,
            movers, i, isoms, inv_isoms, I, a, f, invf, k, s, c, j, index, counter, q, L0, t, qf, pos;

        # initializing orbits, etc
        G := Group( GROUPS[ group_index ] );
        Info(InfoWarning,1,"processing group ", group_index, "/", Length(GROUPS), " of order ", Size(G) );
        orbs := Orbits( G, [1..n] );
        reps := List( orbs, O -> O[1] );
        m := Length( reps ); # number of orbits

        # candidate sets for left translations at each orbit representative
        Info(InfoWarning,1," - calculating candidates for left translations");
        stabs := List( [1..m], i -> Stabilizer( G, reps[i] ) );
        if racks_or_quandles = "quandles" then
            cands := List( [1..m], i -> Set( Elements( Center( stabs[i] ) ) ) );
        else # racks
            cands := List( [1..m], i -> Set( Elements( Centralizer( G, stabs[i] ) ) ) );
        fi;
        cands_lengths := List(cands, Length);
        upper_bound := Product( cands_lengths ); # upper bound on the number of quandles/racks constructed for G
        Info(InfoWarning,1," - - there will be at most ", upper_bound, " ", reductive, " ", racks_or_quandles );

        # a test for admissibility of G as LMlt(Q); this disqualifies "large" subgroups G
        Info(InfoWarning,1," - checking if G is admissible" );
        trans := List( [1..m], i -> Set( Elements( RightTransversal( G, stabs[i] ) ) ) ); # transversals to stabilizers
        gens := List( [1..m], i -> Union( List( trans[i], f -> f^(-1)*cands[i]*f ) ) ); # note that L_y = f^(-1)*L_x*f, where xf=y
        if not Group( Union( gens ) ) = G then
            Info(InfoWarning,1," - G is not admissible");
            return true;
        else
            Info(InfoWarning,1," - G might be admissible");
        fi;

        # group might be admissible, precalculate additional data
        orb_locators := List([1..n], i -> First( [1..m], j -> i in orbs[j] ) ); # orb_locators[i] is the index of the orbit in which i resides
        movers := List( [1..n], i -> First( Union( trans ), g -> i^(g^(-1)) in reps ) ); # movers[i] is an element of g that maps the orbit representative for i onto i
        for i in [1..upper_bound] do
            KEEP[i] := true;
        od;
        if Size(G)=1 then # special handling of the trivial group to save time
            isoms := Group(());
        else
            isoms := Normalizer( SymmetricGroup( n ), G );
        fi;
        isoms := Elements( isoms );
        inv_isoms := List(isoms, Inverse); # to save time later

        # precalculating indices
        I := List([1..Length(isoms)], a -> List([1..m], k -> [1..Length(cands[k])]));
        for a in [1..Length(isoms)] do
            f := isoms[a];
            invf := inv_isoms[a];
            for k in [1..m] do
                for s in [1..Length(cands[k])] do
                    c := cands[k][s];
                    j := orb_locators[ reps[k]^f ];
                    I[a][k][s] := PositionSet( cands[ j ], c^( movers[ reps[j]^invf ] * f ) );
                od;
            od;
        od;

        # processing quandles/racks
        for index in [1..upper_bound] do
            if index mod 10000=1 then
                Info(InfoWarning,1," - - processing ", racks_or_quandles, " ", index, "/", upper_bound);
            fi;
            if KEEP[index] = true then # look at this quandle/rack

                # construct quandle/rack
                counter := IntToCounter( index, cands_lengths );
                q := []; # the quandle/rack, represented as a list of n left translations
                for i in [1..m] do
                    L0 := cands[i][ counter[i] ]; # left translation for orbit representative
                    for t in trans[ i ] do
                        q[ reps[i]^t ] := L0^t;
                    od;
                od;
                if Group( q ) = G then # keep this quandle/rack (resp. its coordinates)
                    Add( RACKS[ group_index ], index );
                fi;

                # remove isomorphic copies (even in the case when Group( q ) < G)
                qf := List([1..m], i -> ());
                for a in [1..Length(isoms)] do
                    invf := inv_isoms[a];
                    for i in [1..m] do
                        k := orb_locators[ reps[i]^invf ];
                        qf[i] := I[a][k][counter[k]];
                    od;
                    pos := CounterToInt( qf, cands_lengths );
                    if pos > index then
                        KEEP[pos] := false;
                    fi;
                od;
            fi;
        od;

        # all quandles/racks have now been constructed and filtered up to isomorphism
        Info(InfoWarning,1," - found ", Length( RACKS[group_index] ), " ", reductive, " ", racks_or_quandles, " up to isomorphism for this group");

        return true;
end;

# auxiliary function
SafeGeneratorsOfGroup := function( G )
    if Size(G)=1 then
        return [()];
    fi;
    return GeneratorsOfGroup( G );
end;

AllRacksOrQuandles := function( n, racks_or_quandles, reductive, save_results )
# n is the order
# racks_or_quandles should be "racks" or "quandles"
# reductive should be "non-2-reductive" or ""
# save_results should be true or false
# usage: AllRacksOrQuandles( 9, "quandles", "non-2-reductive", false );
#           calculates non-2-reductive quandles of order 9 and does not save the results
# returns: true, the info is saved in global variable RACKS

    local start_time, group_index, filename;

    start_time := Runtime();

    # subgroups of S_n up to conjugacy, to be interpreted as LMlt(Q)
    #GROUPS := List( ConjugacyClassesSubgroups( SymmetricGroup( n ) ), Representative );
    Info(InfoWarning,1,"reading in subgroups of Sym(",n,") up to conjugacy");
    filename := Concatenation( "subgroups_of_S_", String(n), "_up_to_conjugacy.txt" );
    Read(filename);
    Info(InfoWarning,1," - there are ", Length( GROUPS ), " groups");
    GROUPS := List( GROUPS, Group );
    if reductive = "non-2-reductive" then
        GROUPS := Filtered(GROUPS, G -> not IsAbelian(G));
        Info(InfoWarning,1," - there are ", Length(GROUPS), " nonabelian groups");
    fi;
    GROUPS := List( GROUPS, SafeGeneratorsOfGroup );
    RACKS := List( [1..Length(GROUPS)], i -> [] );

    # processing groups one at a time (main loop)
    for group_index in [1..Length(GROUPS)] do
        AllRacksOrQuandlesForGroup( n, group_index, reductive, racks_or_quandles );
    od;

    Info(InfoWarning,1,"found ", Sum( List(RACKS,Length) ), " ", reductive, " ", racks_or_quandles, " of order ", n);
    Info(InfoWarning,1,"running time = ", Runtime()-start_time, " ms" );

    if save_results then
        if reductive <> "" then
            reductive := "non-2-reductive_";
        fi;
        filename := Concatenation(reductive, racks_or_quandles, "_of_order_", String(n), "_up_to_isomorphism.txt");
        PrintTo( filename, "# ", filename, "\n\n" );
        AppendTo( filename, "# groups\n\n" );
        AppendTo( filename, "groups := ", GROUPS, ";\n\n" );
        AppendTo( filename, "# ", racks_or_quandles, "\n\n" );
        AppendTo( filename, racks_or_quandles, " := ", RACKS, ";\n\n" );
        AppendTo( filename, "# number of algebras in this file = ", Sum( List(RACKS,Length) ), "\n\n" );
        AppendTo( filename, "# running time = ", Runtime()-start_time, " ms\n");
    fi;

    return true;

end;

IsMedial := function( q )
# assumes that q is a quandle
# returns true if q is a medial quandle
# Note: This is never used in the code.
    local gens, a, b;
    gens := [];
    for a in q do for b in q do
        Add( gens, a*b^(-1));
    od; od;
    return IsAbelian( Group( gens ) );
end;


# POST-PROCESSING OF DATA
# Note: Reformatting data for the library or racks and quandles.
# -------------------------------------------------------------------

racks := [];
quandles := [];
groups := [];

ListOfListsToCompactString := function( data )
#produces a compact string from a list of lists (removes some spaces)
    local s, i, d, j, x;
    s := "[";
    for i in [1..Length(data)] do
        d := data[i];
        Append(s,"[");
        for j in [1..Length(d)] do
            if IsBound( d[j] ) then
                x := d[j];
                Append(s,String(x));
            fi;
            if j<Length(d) then
                Append(s,",");
            fi;
        od;
        Append(s,"]");
        if i<Length(data) then
            Append(s,",");
        fi;
    od;
    Append(s, "]");
    return s;
end;

ProcessData := function( racks_or_quandles, n )
# racks_or_quandles should be "racks" or "quandles"
# processes data in the corresponding data file produced by AllRacksOrQuandles
    local filename, data, not_empty, i, d, new_d, j;
    filename := Concatenation(racks_or_quandles,"_of_order_",String(n),"_up_to_isomorphism.txt");
    if n=13 or (n=12 and racks_or_quandles="racks") then # only have non-2-reductive
        filename := Concatenation("non-2-reductive_",filename);
    fi;
    Info( InfoWarning, 1, "reading file");
    Read( filename );
    if racks_or_quandles = "racks" then
        data := racks;
    else
        data := quandles;
    fi;

    # remove groups with no racks/quandles associated
    Info( InfoWarning, 1, "removing groups with no racks/quandles associated");
    not_empty := Filtered([1..Length(groups)], i -> not IsEmpty(data[i]));
    groups := groups{not_empty};
    data := data{not_empty};

    # compactify ids
    Info( InfoWarning, 1, "compactifying ids" );
    for i in [1..Length(data)] do
        d := data[i];
        new_d := [];
        for j in [1..Length(d)] do
            if j mod 100 = 1 then
                new_d[j] := d[j];
            else
                if d[j]-d[j-1]>1 then
                    new_d[j] := d[j]-d[j-1];
                elif j=Length(d) then # leave blank unless it is the last entry in this list
                    new_d[j] := 1;
                fi;
            fi;
        od;
        data[i] := new_d;
    od;

    # saving results
    Info( InfoWarning, 1, "saving results" );
    filename := Concatenation(racks_or_quandles,"_of_order_",String(n),".txt");
    Info( InfoWarning, 1, filename );
    PrintTo( filename, "# ", filename, "\n\n" );
    AppendTo( filename, "# groups\n\n" );
    if racks_or_quandles = "racks" then
        AppendTo( filename, "LRQ.rack_groups[", String(n), "] := ", groups, ";\n\n" );
    else
        AppendTo( filename, "LRQ.quandle_groups[", String(n), "] := ", groups, ";\n\n" );
    fi;
    AppendTo( filename, "# ", racks_or_quandles, "\n\n" );
    if racks_or_quandles = "racks" then
        AppendTo( filename, "LRQ.rack_ids[", String(n), "] := ", ListOfListsToCompactString( data ), ";\n\n" );
    else
        AppendTo( filename, "LRQ.quandle_ids[", String(n), "] := ", ListOfListsToCompactString( data ), ";\n\n" );
    fi;
    AppendTo( filename, "# number of ", racks_or_quandles, " in this file = ", Sum( List(data,Length) ), "\n\n" );

    return true;

end;
