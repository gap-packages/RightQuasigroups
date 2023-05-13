# Parser.gi
# Parser for evaluating right quasigroup terms and identities
# =============================================================================

# RQ_TermFromString
# assumes that s has been sanitized and type checked
InstallGlobalFunction( RQ_TermFromString,
function( s )
    local depths, depth, x, n, pos;
    # create depth map of the term
    depths := [];
    depth := 0;
    for x in s do 
        if x='(' then
            depth := depth + 1;
            Add( depths, depth );
        elif x=')' then
            Add( depths, depth );
            depth := depth - 1;
            if depth < 0 then 
                Error( "RQ: Incorrect parenthetisation in <1>.");
            fi;
        else
            Add( depths, depth );
        fi;
    od;
    if not depth = 0 then
        Error( "RQ: Incorrect parenthetisation in <1>.");
    fi;
    if not 0 in depths then # (...)
        return RQ_TermFromString( s{[2..Length(s)-1]} );
    fi;
    # locate the last binary operation at depth 0 (recall that in GAP x*y*z is interpreted as (x*y)*z )
    n := Length( s );
    pos := Last([1..n], i-> depths[i]=0 and s[i] in "*/|");
    if not pos=fail then # binary operation
        return [ RQ_TermFromString( s{[1..pos-1]} ), s[pos], RQ_TermFromString( s{[pos+1..n]} ) ]; # infix
    fi;
    # no binary operation
    if n>1 then return
        Error( "RQ: <1> contains a leaf that is neither a variable nor 1.");
    fi;
    if s[1]='1' then # neutral element
        return '1';
    fi;
    return Position( RQ_parserVarNames, s[1]);
end );

# RQ_AlgebraTermFromString

InstallGlobalFunction( RQ_AlgebraTermFromString,
function( category, w )
    local allowed_chars, s, x;
    allowed_chars := Concatenation( RQ_parserVarNames, "()[]{}*/|1 " ); # | is used for left division
    if not (IsString( w ) and ForAll( w, x -> x in allowed_chars )) then
        Error( Concatenation( "RQ: <2> must be a string with characters in ", allowed_chars, "." ) );
    fi;
    # checking for correct operations
    if '|' in w and category = IsRightQuasigroup then 
        Error( "RQ: <2> must not contain left division." );
    elif '1' in w and category <> IsLoop then
        Error( "RQ: <2> must not contain neutral element." );
    fi;
    # sanitize string
    s := "";
    for x in w do
        if x in "[{" then Add( s, '(' );    # replace [,{ with (
        elif x in "]}" then Add( s, ')' );  # replace ],} with )
        elif x=' ' then Append( s, "" );    # ignore spaces
        else Add( s, x );
        fi;
    od;
    return  RQ_TermFromString( s );
end );

# RightQuasigroupTermFromString
# QuasigroupTermFromString
# LoopTermFromString

InstallMethod( RightQuasigroupTermFromString, "for string", 
    [ IsString ],
    w -> RQ_AlgebraTermFromString( IsRightQuasigroup, w )
);

InstallMethod( QuasigroupTermFromString, "for string", 
    [ IsString ],
    w -> RQ_AlgebraTermFromString( IsQuasigroup, w )
);

InstallMethod( LoopTermFromString, "for string", 
    [ IsString ],
    w -> RQ_AlgebraTermFromString( IsLoop, w )
);

# RQ_EvaluateTerm

InstallGlobalFunction( RQ_EvaluateTerm,
function( t ) 
    local i1, i3;
    if IsInt( t ) then # variable index
        return RQ_parserVars[ t ];
    fi;
    if t = '1' then # neutral element
        return RQ_parserOne;
    fi;
    # binary operation
    # PROG: recursive calls are expensive so we will take a shortcut if at least one branch is a leaf
    i1 := IsInt(t[1]);
    i3 := IsInt(t[3]);
    if t[2] = '*' then # multiplication
        if i1 and i3 then return RQ_parserVars[t[1]]*RQ_parserVars[t[3]]; fi;
        if i1 then return RQ_parserVars[t[1]]*RQ_EvaluateTerm(t[3]); fi;
        if i3 then return RQ_EvaluateTerm(t[1])*RQ_parserVars[t[3]]; fi;
        return RQ_EvaluateTerm(t[1])*RQ_EvaluateTerm(t[3]);
    elif t[2] = '/' then # right division
        if i1 and i3 then return RQ_parserVars[t[1]]/RQ_parserVars[t[3]]; fi;
        if i1 then return RQ_parserVars[t[1]]/RQ_EvaluateTerm(t[3]); fi;
        if i3 then return RQ_EvaluateTerm(t[1])/RQ_parserVars[t[3]]; fi;
        return RQ_EvaluateTerm(t[1])/RQ_EvaluateTerm(t[3]);
    else  # left division (by default)
        if i1 and i3 then return LeftQuotient( RQ_parserVars[t[1]], RQ_parserVars[t[3]] ); fi;
        if i1 then return LeftQuotient( RQ_parserVars[t[1]], RQ_EvaluateTerm(t[3]) ); fi;
        if i3 then return LeftQuotient( RQ_EvaluateTerm(t[1]), RQ_parserVars[t[3]] ); fi;
        return LeftQuotient( RQ_EvaluateTerm(t[1]), RQ_EvaluateTerm(t[3]) );
    fi;
end);

# RQ_AlgebraSatisfiesIdentity

InstallGlobalFunction( RQ_AlgebraSatisfiesIdentity,
function( Q, w )
    local elms, category, pos, lhs, rhs, varNames, varPos, n, odometer, i, done;
    elms := Elements( Q );
    category := CategoryOfRightQuasigroup( Q );
    if category = IsLoop then 
        RQ_parserOne := One( Q );
    fi;
    pos := Position(w,'=');
    if pos=fail then
        Error("RQ: <2> must contain the equality sign.");
    fi;
    lhs := RQ_AlgebraTermFromString( category, w{[1..pos-1]} );
    rhs := RQ_AlgebraTermFromString( category, w{[pos+1..Length(w)]} );
    varNames := Intersection( RQ_parserVarNames, w ); # variables present in w
    varPos := List( varNames, x -> Position( RQ_parserVarNames, x ) ); # their positions among all potential variables
    n := Length( varNames );
    # iterate over all tuples of values for variables (fast approach)
    # IteratorOfTuples is an alternative
    odometer := List([1..n], i -> 1); # least significant value on the right, as usual
    for i in [1..n] do # init variable values
        RQ_parserVars[ varPos[ i ] ] := elms[ 1 ];
    od;
    done := false;  
    while not done do
        if RQ_EvaluateTerm( lhs ) <> RQ_EvaluateTerm( rhs ) then # counterexample
            return List( varPos, i -> [ RQ_parserVarNames[ i ], RQ_parserVars[ i ] ] ); 
        fi; 
        i := n;
        while i>0 and odometer[ i ] = Size( Q ) do
            odometer[ i ] := 1;
            RQ_parserVars[ varPos[ i ] ] := elms[ 1 ];
            i := i - 1;
        od;
        if i=0 then # done
            done := true;
        else # increment odometer in ith digit
            odometer[ i ] := odometer[ i ] + 1;
            RQ_parserVars[ varPos[ i ] ] := elms[ odometer[ i ] ];
        fi;
    od;
    return true;
end);

# RightQuasigroupSatisfiesIdentity
# QuasigroupSatisfiesIdentity
# LoopSatisfiesIdentity

InstallMethod( RightQuasigroupSatisfiesIdentity, "for right quasigroup and string",
    [ IsRightQuasigroup, IsString ],
function( Q, w )
    return RQ_AlgebraSatisfiesIdentity( Q, w );
end);

InstallMethod( QuasigroupSatisfiesIdentity, "for quasigroup and string",
    [ IsQuasigroup, IsString ],
function( Q, w )
    return RQ_AlgebraSatisfiesIdentity( Q, w );
end);

InstallMethod( LoopSatisfiesIdentity, "for loop and string",
    [ IsLoop, IsString ],
function( Q, w )
    return RQ_AlgebraSatisfiesIdentity( Q, w );
end);