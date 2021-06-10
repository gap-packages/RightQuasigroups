Q := AsLoop( DihedralGroup(6) );
a := Q.2; b:= Q.3;
Q2 := PrincipalLoopIsotope( Q, a, b );                                
iso := IsomorphismLoops( Q, Q2 );                             
h := PermList( List( Elements(Q), x -> Position( Elements(Q2), x^iso ) ) );
f := h / RightTranslation( Q, a );
g := h / LeftTranslation( Q, b );
ForAll( Q, x -> ForAll( Q, y -> x^f * y^g = (x * y)^u ) );

atop := AutotopismObject@( Q, f, g, h );

LoadPackage("right");
Q := AsLoop( DihedralGroup(6) );
AutotopismFromPrincipalLoopIsotope( Q, Q.2, Q.3 );

LoadPackage("right");
Q := RightBolLoop(8,1);
for a in Q do 
    for b in Q do
        iso := AutotopismFromPrincipalLoopIsotope( Q, a, b );
        if iso <> fail then
            Print( a, " ", b, " ", iso, "\n" );
        fi;
    od;
od;
