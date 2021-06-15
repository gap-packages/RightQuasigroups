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










g:=AutotopismGroupByGenerators(gens);
Size(g);
g;

elm:=Elements(g);;
ForAll(elm,x->ForAll(elm,y->x^NiceMonomorphism(g)*y^NiceMonomorphism(g)=(x*y)^NiceMonomorphism(g)));

TraceMethods( [ Center,Centralizer,CentralizerOp,CentralizerInParent ] );
c := Center( g );
UntraceAllMethods();

Size(c);
c;
TrivialSubgroup(g);

AtopGr_allinone := function( Q )
    local ag, gens, green, yellow, red, g, pt, newgen;
    ag := AutomorphismGroup(Q);
    gens := List( GeneratorsOfGroup( ag ), u -> AutotopismObject@RightQuasigroups( Q, u, u, u ) );
    green := []; red := []; yellow := Cartesian( Q, Q );
    while yellow <> [] do
        pt := yellow[1];
        newgen := AutotopismFromPrincipalLoopIsotope( Q, pt[2], pt[1] );
        if newgen <> fail then
            Add( gens, newgen );
            Add( green, pt );
        else
            Add( red, pt );
        fi;
        g := AutotopismGroupByGenerators( gens );
        yellow := Difference( Cartesian( Q, Q ),
            Union( List( Concatenation( green, red ), x -> Orbit( g, x, AtopOnnSquare@RightQuasigroups) ) ) 
        );
    od;
    return g;
end;

at := AutotopismGroup( Q );
at = g;
