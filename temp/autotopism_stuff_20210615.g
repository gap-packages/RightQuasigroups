LoadPackage("right");

Q := RightBolLoop(8,1);
gens := [];
for a in Q do 
    for b in Q do
        iso := AutotopismFromPrincipalLoopIsotope( Q, a, b );
        Print( "## ", CheckAtopInvariant@RightQuasigroups( Q, Q, a, b ), "\n" );
        if iso <> fail then
            Print( a, " ", b, " ", iso, "\n" );
            Add( gens, iso );
        fi;
    od;
od;
time;

Q := RightBolLoop(8,1);
gens := [];
for a in Q do 
    for b in Q do
        if CheckAtopInvariant@RightQuasigroups( Q, Q, a, b ) then
            iso := AutotopismFromPrincipalLoopIsotope( Q, a, b );
            if iso <> fail then
                Print( a, " ", b, " ", iso, "\n" );
                Add( gens, iso );
            fi;
        fi;
    od;
od;
time;


Q := SteinerLoop( 16, 50 );
Q2 := PrincipalLoopIsotope( Q, Q.10, Q.15 );
S := SteinerLoop( 16, 51 );
IsotopismLoops( Q, S ); time;
IsotopismLoops( S, Q2 ); time;
# IsotopismLoops( Q2, S ); # gives error!! # fixed by Petr June 16, 2021
IsotopismLoops( Q, Q2 ); time;

find_isotopism := function( Q, S )
    local at, orep, pt, S0, iso;
    at := AutotopismGroup( S );
    orep := List( Orbits( at, Cartesian(S,S), AtopOnnSquareElms@RightQuasigroups ), o -> o[1] );
    for pt in orep do
        if CheckAtopInvariant@RightQuasigroups( Q, S, pt[2], pt[1] ) then
            S0 := PrincipalLoopIsotope( S, pt[2], pt[1] );
            iso := IsomorphismLoops( Q, S0 );
            if iso <> fail then 
                return [ iso, pt ];
            fi;
        fi;
    od;
    return fail;
end;

find_isotopism( Q, S ); time;
find_isotopism( S, Q2 ); time;
find_isotopism( Q2,S ); time;
find_isotopism( Q, Q2 ); time;

find_isotopism( Q, S ); time;
find_isotopism( S, Q2 ); time;
find_isotopism( Q2,S ); time;
find_isotopism( Q, Q2 ); time;

