# various routines that could be useful in futher development of the package

# ZipString( s )
# Note: There is no Unzip yet
ZipString := function( s )
    # applies Huffman coding to a string and returns the characters, their codes and the encoded string as 0,1-string
    local chars, freqs, c, pos, n, m, mergers, merge2, codes, merged, ret;
    # initial character frequencies
    chars := "";
    freqs := 0*[1..256];
    for c in s do
        if c in chars then
            pos := Position( chars, c );
            freqs[pos] := freqs[pos]+1;
        else
            Add( chars, c );
            freqs[ Length(chars) ] := 1;
        fi;
    od;
    n := Length( chars );
    freqs := freqs{[1..n]};
    # merge
    m := n;
    mergers := [];
    merge2 := function( )
        local pos, p1, v1, p2, v2;
        pos := Positions( freqs, Minimum( freqs ) );
        if Length( pos ) > 1 then # tie
            p1 := pos[1]; p2 := pos[2];
            v1 := freqs[p1]; v2 := freqs[p2];           
        else # single smallest
            p1 := pos[1]; v1 := freqs[p1];
            freqs[ p1 ] := infinity;
            p2 := Position( freqs, Minimum( freqs ) );
            v2 := freqs[p2];
        fi;
        freqs[p1] := v1+v2; 
        freqs[p2] := infinity;
        Add( mergers, [p1,p2] );
    end;
    while m>1 do
        merge2();
        m := m - 1;
    od;
    # split
    codes := List([1..n],i->"");
    m := 1;
    sets := List([1..n],i->[i]);
    while m < n do
        merged := mergers[m];
        for i in sets[ merged[1] ] do codes[i] := Concatenation( "0", codes[i] ); od; 
        for i in sets[ merged[2] ] do codes[i] := Concatenation( "1", codes[i] ); od;
        u := Union( sets[ merged[1] ], sets[ merged[ 2 ] ]);
        for i in u do sets[i] := u; od;
        m := m + 1;
    od;
    # encode s
    ret := [];
    for c in s do
        pos := Position( chars, c );
        Append( ret, codes[ pos ] );
    od;
    return [ chars, codes, ret ];
end;

PackListWithRepetitions := function( ls )
# assumes that ls contains no negative numbers
# it might contain unbind entries
# replaces repeated entries x,x,x,...,x (n times) with -n,x

    local ret, same, pos_ls, pos_ret, j;
    if IsEmpty( ls ) then
        return [];
    fi;
	ret := []; # list to be returned
    same := function( i, j )
        return (not IsBound(ls[i]) and not IsBound(ls[j])) or (IsBound(ls[i]) and IsBound(ls[j]) and ls[i]=ls[j]);
    end;
    pos_ls := 1;
    pos_ret := 1;
    repeat
        j := First( [pos_ls+1..Length(ls)], k -> not same(pos_ls,k) );
        if j = fail then
            j := Length( ls );
        else
            j := j-1;
        fi;
        if j>pos_ls then # more than one consecutive occurrence
            ret[ pos_ret ] := -(j-pos_ls+1); # how many times to repeat
            if IsBound( ls[pos_ls] ) then 
                ret[ pos_ret+1 ] := ls[ pos_ls ];
            fi;
            pos_ret := pos_ret + 2;
        else
            if IsBound( ls[pos_ls] ) then 
                ret[ pos_ret ] := ls[ pos_ls ];
            fi;
            pos_ret := pos_ret + 1;
        fi;
        pos_ls := j + 1;
    until pos_ls > Length( ls );
    return ret;
end;    

UnpackListWithRepetitions := function( ls )
# reverses PackListWithRepetitions
    local ret, pos_ls, pos_ret, i;
    if IsEmpty( ls ) then 
        return [];
    fi;
    ret := [];
    pos_ls := 1;
    pos_ret := 1;
    repeat
        if not IsBound( ls[ pos_ls ] ) then
            pos_ls := pos_ls + 1;
            pos_ret := pos_ret + 1;
        elif IsNegInt( ls[ pos_ls ] ) then # repeated entry
            if IsBound( ls[ pos_ls+1 ] ) then
                for i in [1..-ls[ pos_ls ]] do
                    ret[ pos_ret + i - 1 ] := ls[ pos_ls + 1];
                od;
            fi;
            pos_ret := pos_ret - ls[ pos_ls ];
            pos_ls := pos_ls + 2;   
        else
            ret[ pos_ret ] := ls[ pos_ls ];
            pos_ls := pos_ls + 1;
            pos_ret := pos_ret + 1;
        fi;
    until pos_ls > Length( ls );
    return ret;
end;

ListOfListsToCompactString := function( data )
# produces a compact string from a list of lists (removes some spaces)
# useful for saving in a file
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