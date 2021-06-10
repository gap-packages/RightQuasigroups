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

atop_on_3n := function( i, atop )
    local n;
    n := Size( atop![4] );
    if i <= n then 
        return i^atop![1];
    elif i <= 2*n then 
        return n+(i-n)^atop![2];
    else 
        return 2*n+(i-2*n)^atop![3];
    fi;
end;

g := GroupWithGenerators(gens);
nice := ActionHomomorphism( g, [1 .. 3*Size( Q )], atop_on_3n );
SetIsInjective( nice, true );
SetNiceMonomorphism( g, nice );
SetIsHandledByNiceMonomorphism( g, true );
SetCanEasilyCompareElements( g, true );
SetCanEasilySortElements( g, true );

elm:=Elements(g);;
ForAll(elm,x->ForAll(elm,y->x^NiceMonomorphism(g)*y^NiceMonomorphism(g)=(x*y)^NiceMonomorphism(g)));

IsSolvableGroup(g);
IsPerfectGroup(g);    
IsPolycyclicGroup(g);
IsGeneratorsOfMagmaWithInverses(g);;
KnowsHowToDecompose(g);
Parent(g);

Difference( KnownPropertiesOfObject(NiceObject(g)), KnownPropertiesOfObject(g) );
Difference( KnownPropertiesOfObject(g), KnownPropertiesOfObject(NiceObject(g)) );
Difference( KnownAttributesOfObject(NiceObject(g)), KnownAttributesOfObject(g) );
Difference( KnownAttributesOfObject(g), KnownAttributesOfObject(NiceObject(g)) );

TraceMethods( [ Center,Centralizer,CentralizerOp,CentralizerInParent ] );
c := Center( g );
UntraceAllMethods();

Size(c);

