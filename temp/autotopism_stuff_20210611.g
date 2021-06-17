# LoadPackage("right");
# Q := AsLoop( DihedralGroup(6) );
# a := Q.2; b:= Q.3;
# Q2 := PrincipalLoopIsotope( Q, a, b );                                
# iso := IsomorphismLoops( Q, Q2 );                             
# h := PermList( List( Elements(Q), x -> Position( Elements(Q2), x^iso ) ) );
# f := h / RightTranslation( Q, a );
# g := h / LeftTranslation( Q, b );
# ForAll( Q, x -> ForAll( Q, y -> x^f * y^g = (x * y)^u ) );

# atop := AutotopismObject@( Q, f, g, h );

# LoadPackage("right");
# Q := AsLoop( DihedralGroup(6) );
# atop := AutotopismFromPrincipalLoopIsotope( Q, Q.2, Q.3 );

LoadPackage("right");

Q := RightBolLoop(8,1);
gens := [];
for a in Q do 
    for b in Q do
        iso := AutotopismFromPrincipalLoopIsotope( Q, a, b );
        if iso <> fail then
            Print( a, " ", b, " ", iso, "\n" );
            Add( gens, iso );
        fi;
    od;
od;

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
            Union( List( Concatenation( green, red ), x -> Orbit( g, x, AtopOnnSquareElms@RightQuasigroups) ) ) 
        );
    od;
    return g;
end;

at := AutotopismGroup( Q );
at = g;
