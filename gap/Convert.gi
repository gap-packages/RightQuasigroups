# Convert.gd
# Converting between numerical bases
# =============================================================================

# RQ_DigitToChar( d )

InstallMethod( RQ_DigitToChar, "for integer",
    [ IsInt ],
function( d )
    return RQ_conversionString[ d + 1 ];
end );


# RQ_CharToDigit( c )

InstallMethod( RQ_CharToDigit, "for character",
    [ IsChar ],
function( c )
    return Position( RQ_conversionString, c ) - 1;
end );

# RQ_EncodeCayleyTable( ct ) 

InstallMethod( RQ_EncodeCayleyTable, "for category and list",
    [ IsObject, IsList ],
function( category, ct )
    local n, ret, start, i, j;
    n := Length( ct );
    ret := "";
    start := 1;
    if category = IsLoop and n>=3 then
        start := 2;
    fi;
    if n>91 then
        Error("RQ: Encoding of Cayley tables is supported only for order less than 92.");
    fi;
    ret := "";
    if ct <> TransposedMat( ct ) then # general case
        for i in [start..n] do for j in [start..n] do
            Add(ret, RQ_DigitToChar( ct[i,j]-1 ) );
        od; od;
    else # commutative case
        for i in [start..n] do for j in [i..n] do
            Add(ret, RQ_DigitToChar( ct[i,j]-1 ) );
        od; od;
    fi;
    return ret;
end );

# RQ_DecodeCayleyTable( str )

InstallMethod( RQ_DecodeCayleyTable, "for category and string",
    [ IsObject, IsString ],
function( category, str )
    local symbols, n, start, pos, ret, i, j;
    symbols := Set( Set( str ), c -> RQ_CharToDigit(c) + 1 );
    if Length(str)=1 then
        return [[RQ_CharToDigit(str[1])+1]];
    fi;
    if Length(str)=3 and Size(symbols)=2 then # n=2 (automatically commutative)
        return [
            [RQ_CharToDigit(str[1])+1, RQ_CharToDigit(str[2])+1],
            [RQ_CharToDigit(str[2])+1, RQ_CharToDigit(str[3])+1]
        ];
    fi;
    # n>2 or not IsLoop
    n := Size( symbols ); # number of distinct symbols
    start := 1;
    if category = IsLoop then
        start := 2;
    fi;
    if n>91 then
        Error("RQ: Decoding of Cayley tables is supported only for order less than 92.");
    fi;
    ret := List([1..n], i -> List( [1..n], j -> -1 ) );
    # the table except for the first row and first column
    pos := 1;
    if Length(str) in [n^2, (n-1)^2] then # noncommutative case
        for i in [start..n] do for j in [start..n] do
            ret[i,j] := RQ_CharToDigit( str[pos] ) + 1;
            pos := pos+1;
        od; od;
    else # commutative case
        for i in [start..n] do for j in [i..n] do
            ret[i,j] := RQ_CharToDigit( str[pos] ) + 1;
            ret[j,i] := ret[i,j];
            pos := pos + 1;
        od; od;
    fi;
    # determining the first row and first column
    if start>1 then 
        for i in [2..n] do
            ret[i,1] := Difference( symbols, ret[i] )[1];
            ret[1,i] := Difference( symbols, List( [2..n], j->ret[j,i] ) )[1];
        od;
        ret[1,1] := Difference( symbols, ret[1] )[1];
    fi;
    return ret;
end );

# RQ_ConvertToDecimal( s, n )

InstallMethod( RQ_ConvertToDecimal, "for string and integer",
    [ IsString, IsInt ],
function( s, n )
    local ls, d, i;
    ls := Length( s );
    d := 0;
    for i in [1..ls] do
        d := d + RQ_CharToDigit(s[i])*(n^(ls-i));
    od;
    return d;
end );


# RQ_ConvertFromDecimal( arg )

InstallGlobalFunction( RQ_ConvertFromDecimal, function( arg )
    local d, m, s, r, prefix_s;
    d := arg[1];
    m := arg[2];
    s := "";
    while d>0 do
        r := d mod m;
        Add( s, RQ_DigitToChar( r ) );
        d := (d-r)/m;
    od;
    s := Reversed( s );
    if Length( arg ) > 2 then
        prefix_s := List( [1..arg[3]-Length(s)], i -> RQ_DigitToChar( 0 ) );
        s := Concatenation( prefix_s, s );
    fi;
    return s;
end );


# RQ_ConvertBase( arg )

InstallGlobalFunction( RQ_ConvertBase, function( arg )
    local d;
    d := RQ_ConvertToDecimal( arg[1], arg[2] );
    if Length(arg)>3 then
        return RQ_ConvertFromDecimal( d, arg[3], arg[4] );
    fi;
    return RQ_ConvertFromDecimal( d, arg[3] );
end );

# RQ_EncodeCocycle( coc, values ) 

InstallMethod( RQ_EncodeCocycle, "for two lists",
    [ IsList, IsList ],
function( coc, values )
    local b, n, is_commutative, ret, i, start, j;
    b := Length( values );
    if not b < 92 then
        Error("LOOPS: Encoding of cocycles is supported only for loops of order less than 92.");
    fi;
    n := Length(coc);
    is_commutative := coc = TransposedMat(coc);
    ret := [ n, is_commutative, "" ];
    for i in [1..n] do
        start := 1;
        if is_commutative then start := i; fi;
        for j in [start..n] do
            Add( ret[3], RQ_DigitToChar( Position( values, coc[i,j] ) - 1 ) );
        od;
    od;
    ret[3] := RQ_ConvertBase( ret[3], b, 91 );
    return ret;
end);

# RQ_DecodeCocycle( ecoc, values ) 

InstallMethod( RQ_DecodeCocycle, "for two lists",
    [ IsList, IsList ],
function( ecoc, values )
    local n, is_commutative, b, s, coc, pos, i, j;
    n := ecoc[1];
    is_commutative := ecoc[2];
    b := Length( values );
    if is_commutative then
        s := RQ_ConvertBase( ecoc[3], 91, b, n*(n+1)/2 );
    else
        s := RQ_ConvertBase( ecoc[3], 91, b, n^2 );
    fi;
    coc := List([1..n], i -> [1..n]);
    pos := 1;
    if is_commutative then
        for i in [1..n] do for j in [i..n] do
            coc[i,j] := values[ RQ_CharToDigit( s[pos] ) + 1 ];
            coc[j,i] := coc[i,j];
            pos := pos + 1;
        od; od;
    else
        for i in [1..n] do for j in [1..n] do
            coc[i,j] := values[ RQ_CharToDigit( s[pos] ) + 1 ];
            pos := pos + 1;
        od; od;
    fi;
    return coc;
end);