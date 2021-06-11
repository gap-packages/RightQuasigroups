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

AtopOnnSquare := function( x, atop )
    if Length( x ) = 2 then
        return [ x[1]^atop![1], x[2]^atop![2] ];
    elif Length( x ) = 3 then 
        return [ x[1]^atop![1], x[2]^atop![2], x[3]^atop![3] ];
    else
        Error( "RQ: <1> must have length 2 or 3." );
    fi;
end;

ExtendAtopGrp := function( Q, gens, green, yellow, red )
    local g;
    g := 0;
end;

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

