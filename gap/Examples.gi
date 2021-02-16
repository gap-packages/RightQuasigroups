# Examples.gi
# Libraries of loops, racks and quandles
# =============================================================================

#############################################################################
##  Binding global variable RQ_aux 
##
##  The variable is used for temporary storage throughout the package. 
##  We therefore do not want the variable to be read only.

RQ_aux := [];

#############################################################################
##  READING DATA
##  -------------------------------------------------------------------------

# PROG: reading only *.tbl files that contain several global variables referred in the code below.
# The remaining *.tbl files will be read when first needed.
# The files read here are typically small.

# up to isomorphism
#ReadPackage("RightQuasigroups", "data/left_Bol_loops.tbl");     # left Bol loops
#ReadPackage("RightQuasigroups", "data/Moufang_loops.tbl");      # Moufang loops
#ReadPackage("RightQuasigroups", "data/Paige_loops.tbl");        # Paige loops
#ReadPackage("RightQuasigroups", "data/code_loops.tbl");         # code loops
#ReadPackage("RightQuasigroups", "data/Steiner_loops.tbl");      # Steiner loops
ReadPackage("RightQuasigroups", "data/CC_loops.tbl");           # CC loops
ReadPackage("RightQuasigroups", "data/RCC_loops.tbl");          # RCC loops (more is read upon calling RCCLoop(n,m) for the first time)
#ReadPackage("RightQuasigroups", "data/small_loops.tbl");        # small loops
#ReadPackage("RightQuasigroups", "data/interesting_loops.tbl");  # interesting loops
#ReadPackage("RightQuasigroups", "data/nilpotent_loops.tbl");    # nilpotent loops
#ReadPackage("RightQuasigroups", "data/automorphic_loops.tbl");  # automorphic loops
ReadPackage("RightQuasigroups", "data/right_Bruck_loops.tbl");  # right Bruck loops
ReadPackage("RightQuasigroups", "data/small_racks.tbl");        # small racks (more is read upon calling SmallRack(n,m) for the first time)
ReadPackage("RightQuasigroups", "data/small_quandles.tbl");     # small quandles (more is read upon calling SmallQuandle(n,m) for the first time)
#ReadPackage("RightQuasigroups", "data/connected_quandles.tbl"); # connected quandles

# up to isotopism
ReadPackage("RightQuasigroups", "data/itp_small_loops.tbl");     # small loops up to isotopism

# DISPLAYING INFORMATION ABOUT A LIBRARY
# _____________________________________________________________________________

#############################################################################
##  
#F  RQ_LibraryByName( name ) 
##    
##  Auxiliary. Returns the library corresponding to <name>.

InstallGlobalFunction( RQ_LibraryByName, 
function( name )
    local varName, fileName;
    # reconstruct the name of the library
    name := List( name, function( c ) if c <> ' ' then return c; else return '_'; fi; end ); # replace spaces by underscores
    varName := Concatenation( "RQ_", name ); # add prefix "RQ_"
    # check that the variable (library) has been loaded
    if not IsBoundGlobal( varName ) then # load relevant file
        fileName := Concatenation( "data/", name, ".tbl" );
        Info( InfoRightQuasigroups, 2, "RQ: reading library file ", fileName );
        ReadPackage( "RightQuasigroups", fileName );
    fi;
    # return the variable (library)
    return ValueGlobal( varName );
end);

# DisplayLibraryInfo( name ) 

InstallMethod( DisplayLibraryInfo, "for string",
    [ IsString ],
function( name )
    local s, lib, k;
    # up to isomorphism
    if name = "left Bol loops" or name = "right Bol loops" then
        s := Concatenation( "The library contains all nonassociative ", name, " of order less than 17\nand all nonassociative ", name, " of order p*q, where p>q>2 are primes." );
    elif name = "Moufang loops" then
        s := "The library contains all nonassociative Moufang loops of order less than 65,\nand all nonassociative Moufang loops of order 81 and 243.";
    elif name = "Paige loops" then
        s := "The library contains the smallest nonassociative finite simple Moufang loop.";
    elif name = "code loops" then
        s := "The library contains all nonassociative even code loops of order less than 65.";
    elif name = "Steiner loops" then
        s := "The library contains all nonassociative Steiner loops of order less or equal to 16.\nIt also contains the associative Steiner loops of order 4 and 8.";
    elif name = "CC loops" then
        s := "The library contains all CC loops of order\n2<=2^k<=64, 3<=3^k<=81, 5<=5^k<=125, 7<=7^k<=343,\nall nonassociative CC loops of order less than 28,\nand all nonassociative CC loops of order p^2 and 2*p for any odd prime p.";
    elif name = "RCC loops" or name = "LCC loops" then
        s := Concatenation( "The library contains all nonassociative ", name, " loops of order less than 28." );
    elif name = "small loops" then
        s := "The library contains all nonassociative loops of order less than 7.";
    elif name = "interesting loops" then
        s := "The library contains a few interesting loops.";
    elif name = "nilpotent loops" then
        s := "The library contains all nonassociative nilpotent loops of order less than 12.";
    elif name = "automorphic loops" then
        s := "The library contains:\n";
        s := Concatenation(s," - all nonassociative automorphic loops of order less than 16,\n");
        s := Concatenation(s," - all commutative automorphic loops of order 3, 9, 27, 81.");
    elif name = "left Bruck loops" or name = "right Bruck loops" then
        s := Concatenation( "The library contains all ", name, " of orders 3, 9, 27 and 81." );
    elif name = "small racks" then
        s := "The library contains all racks of order less than 11.";
    elif name = "small quandles" then
        s := "The library contains all quandles of order less than 12.";
    elif name = "connected quandles" then   
        s := "The library contains all connected quandles of order less than 48.";
    # up to isotopism
    elif name = "itp small loops" then
        s := "The library contains all nonassociative loops of order less than 7 up to isotopism.";
    else
        Info( InfoRightQuasigroups, 1, Concatenation(
            "The admissible names for libraries are: \n",
            "\"automorphic loops\", ", "\"CC loops\", ", "\"connected quandles\, ", "\"code loops\", ",  "\"interesting loops\", ", "\"itp small loops\", ",
            "\"LCC loops\", ", "\"left Bol loops\", ",  "\"left Bruck loops\", ", "\"Moufang loops\", ", "\"nilpotent loops\", ",
            "\"Paige loops\", ", "\"right Bol loops\", ", "\"right Bruck loops\", ", "\"RCC loops\", ", "\"small loops\", ", "\"small quandles\", ", 
            "\"small racks\", ", "and \"Steiner loops\"."
        ) );
        return fail;
    fi;
    
    s := Concatenation( s, "\n------\nExtent of the library:" );
    
    # renaming for data access
    if name = "right Bol loops" then name := "left Bol loops"; fi;
    if name = "LCC loops" then name := "RCC loops"; fi;
    if name = "left Bruck loops" then name := "right Bruck loops"; fi;
    
    lib := RQ_LibraryByName( name );
    for k in [1..Length( lib[ 1 ] ) ] do
        if lib[ 2, k ] = 1 then
            s := Concatenation( s, "\n   ", String( lib[ 2, k ] ), " algebra of order ", String( lib[ 1, k ] ) );
        else
            s := Concatenation( s, "\n   ", String( lib[ 2, k ] ), " algebras of order ", String( lib[ 1, k ] ) );
        fi;
    od;
    if name = "left Bol loops" then
        s := Concatenation( s, "\n   (p-q)/2 loops of order p*q for primes p>q>2 such that q divides p-1");
        s := Concatenation( s, "\n   (p-q+2)/2 loops of order p*q for primes p>q>2 such that q divides p+1" );
    fi;
    if name = "CC loops" then
        s := Concatenation( s, "\n   3 loops of order p^2 for every prime p>7,\n   1 loop of order 2*p for every odd prime p" );
    fi;
    s := Concatenation( s, "\n" );
    Print( s );
    return true;
end);

#############################################################################
##  AUXILIARY FUNCTIONS
##  -------------------------------------------------------------------------
##  When the data in the database is encoded in some way, we usually
##  retrieve the loop via function RQ_ActivateXLoop, where X is the name 
##  of the class.

#############################################################################
##  
#F  RQ_SmallestNonsquare( p ) 
##    
##  Auxiliary function. 
##  Returns the smallest nonsquare modulo p.

InstallGlobalFunction( RQ_SmallestNonsquare,
function( p )
    local squares, i;
    squares := Set( [1..p-1], i->i^2 mod p );
    for i in [2..p-1] do 
        if not i in squares then return i; fi;
    od;
    return fail; # will never happen if p>2 is prime
end);

#############################################################################
##  
#F  RQ_ActivateLeftBolLoopPQ( p, q, m ) 
##    
##  Auxiliary function for activating left Bol loop of order p*q.
##  See paper by Kinyon, Nagy and Vojtechovsky.
##  p>q>2 are primes such that q divides p^2-1, m is an integer in the range [1..(p-q)/2] or [1..(p-q+2)/2]

InstallGlobalFunction( RQ_ActivateLeftBolLoopPQ,
function( p, q, m )
    local F, omega, lambda, ev, inv_ev, params, t, sqrt_t, final_params, alpha, theta, GFp, ct, i, j, k, l, u, v, w, x, y;
    F := GF(p^2);
    omega := PrimitiveRoot( F )^((p^2-1)/q);
    lambda := omega + omega^(-1);
    ev := List([0..q-1], j -> omega^j);
    inv_ev := List([0..q-1], j -> omega^(-j));
    if IsInt((p-1)/q) then
        params := List([2..p-1], j->j*One(F)); # GF(p); 0 and 1 correspond to isomorphic nonabelian groups
        params := Filtered( params, x -> not ((One(F) - x^(-1)) in ev) );
    else # q divides p+1
        t := RQ_SmallestNonsquare( p );
        sqrt_t := RootsOfPolynomial( F, X(F,"x")^2 - t )[ 1 ]; # a squre root of t in GF(p^2) 
        params := List([0..p-1], j -> (1/2)*One(F) + j*One(F)*sqrt_t );  # 1/2 + GF(p)\sqrt{t} 
        params := Filtered( params, x -> not ((One(F) - x^(-1)) in ev) );
    fi;
    final_params := [];
    for x in params do
        if not ( (One(F)-x) in final_params ) then
            Add( final_params, x );
        fi;
    od;
    alpha := final_params[ m ]; 
    theta := alpha*ev + (One(F)-alpha)*inv_ev; 
    theta := List( theta, x -> x^(-1) );
    GFp := List([0..p-1], j -> j*One(F));
    theta := List( theta, x -> Position( GFp, x )-1 );
    # construct the Cayley table according to (a^i b^j)*(a^k b^l) = a^{i+k} b^{ w + (l+w)*theta[k]^{-1}*theta[i+k], where w+w*theta[i] = j    
    ct := List([1..p*q], i -> 0*[1..p*q]);
    for i in [0..q-1] do for j in [0..p-1] do for k in [0..q-1] do for l in [0..p-1] do
        u := (i+k) mod q;
        w := ( j/(1+theta[i+1]) ) mod p;
        v := ( w + (l+w)*theta[ ((i+k) mod q) + 1 ]/theta[k+1] ) mod p;
        x := i*p+j+1;
        y := k*p+l+1;
        ct[x,y] := u*p+v+1;
    od; od; od; od; 
    # return the loop
    return LoopByCayleyTable( ct ); 
end);

#############################################################################
##  
#F  RQ_ActivateLeftBolLoop( pos_n, m, case ) 
##    
##  Auxiliary function for activating left Bol loops from the database.
##  case = [p,q] if it is a left Bol loop of order p*q, with p>q>2 primes such that q divides p^2-1
##  case = false otherwise
##  pos_n is meaningless when case = [p,q]

InstallGlobalFunction( RQ_ActivateLeftBolLoop,
function( pos_n, m, case )
    local lib, rep_m, ct, perm;
    lib := RQ_LibraryByName( "left Bol loops" );
    if not case=false then # p*q
        return RQ_ActivateLeftBolLoopPQ( case[1], case[2], m );
    fi;    
    # in database 
    rep_m := m;
    # searching for a Cayley table on which the loop is based
    while not IsString( lib[ 3 ][ pos_n ][ rep_m ] ) do 
        rep_m := rep_m - 1;
    od;
    if rep_m = m then # loop given by encoded Cayley table
        ct := RQ_DecodeCayleyTable( IsLoop, lib[ 3 ][ pos_n ][ m ] );
    else # loop given as an isotope of another loop
        ct := RQ_DecodeCayleyTable( IsLoop, lib[ 3 ][ pos_n ][ rep_m ] );
        perm := PermList( ct[ lib[ 3 ][ pos_n ][ m ] ] );
        ct := Set( List( ct, row -> OnTuples( row, perm^-1 ) ) );
    fi;
    return LoopByCayleyTable( ct );
end);

#############################################################################
##  
#F  RQ_ActivateMoufangLoop( n, pos_n, m ) 
##    
##  Auxiliary function for activating Moufang loops from the database.

InstallGlobalFunction( RQ_ActivateMoufangLoop,
function( n, pos_n, m )
    local lib, d, K, F, cocycle;
    # every loop in the library is either a Chein loop or a central extension
    lib := RQ_LibraryByName( "Moufang loops" );
    d := lib[ 3 ][ pos_n ][ m ]; # data
    if Length( d ) = 2 then # Chein loop
        return CheinLoop( SmallGroup( d[1], d[2] ) );
    fi;
    # central extension
    K := AsLoop( SmallGroup( d[1], d[2] ) );
    if d[3]="g" then 
        F := AsLoop( SmallGroup( d[4], d[5] ) );
    else # Moufang factor
        F := MoufangLoop( d[4], d[5] );
    fi;
    cocycle := [ d[4], d[6], d[7] ]; # still encoded
    cocycle := RQ_DecodeCocycle( cocycle, [1..d[1]] ); 
    return LoopByCentralExtension( K, F, cocycle );
end);

#############################################################################
##  
#F  RQ_ActivateSteinerLoop( n, pos_n, m ) 
##    
##  Auxiliary function activating Steiner loops from the database.
##
##  The database RQ_Steiner_loops contains blocks of steiner triple systems.
##  If the system is on k points, the poitns are labelled 0,...,k-1.
##  The constructed Steiner loop has elements labelled 1,...,k+1=n

InstallGlobalFunction( RQ_ActivateSteinerLoop,
function( n, pos_n, m )
    local lib, d, blocks, i, T, i_in, ij_in, j, MyInt;

    #############################################################################
    ##  
    #F  MyInt( s ) 
    ##    
    ##  Auxiliary function. 
    ##  Given a digit or a lower case letter, returns the numerical value, where
    ##  a = 10, f=15

    MyInt := function( s )
        return Position( "0123456789abcdef", s ) - 1;
    end;
    
    lib := RQ_LibraryByName( "Steiner loops" );
    d := lib[ 3 ][ pos_n ][ m ]; # data for the loop = three strings 
    # creating the blocks
    blocks := []; 
    for i in [1..Length( d[ 1 ] )] do
        Add( blocks, [ MyInt( d[1,i] ), MyInt( d[2,i] ), MyInt( d[3,i] ) ] );
    od;
    
    #creating the multiplication table
    T := List( [1..n], i->[1..n] );
    for i in [1..n] do T[i,1] := i; od;
    for i in [0..n-2] do 
        i_in := Filtered( blocks, B->i in B);
        for j in [0..n-2] do
            if j=i then T[i+2,j+2] := 1; fi;
            if not j=i then
                ij_in := Filtered( i_in, B->j in B )[1]; #unique block;
                T[i+2,j+2] := Difference( ij_in, [i,j])[1] + 2;
            fi;
        od;
    od;
    
    return LoopByCayleyTable( T );
end);

#############################################################################
##  
#F  RQ_ActivateRCCLoop( n, pos_n, m ) 
##    
##  Activates an RCC loop from the library.
##  See manual for complete discussion concerning this library.

InstallGlobalFunction( RQ_ActivateRCCLoop,
function( n, pos_n, m )
    local lib, pos_m, g, nr_conj_classes, data, data2, next_compactified, x, i, rel_m, G, section, pos_conjugacy_classes;

    if IsEmpty( RQ_RCC_transitive_groups ) then # read additional data
        Info( InfoRightQuasigroups, 2, "RQ: reading data file with transitive groups for RCC loops" );
        ReadPackage( "RightQuasigroups", "data/RCC/RCC_transitive_groups.tbl" );
    fi;
    # determining the transitive group corresponding to pos_n, m
    pos_m := Length( RQ_RCC_loops[ 3 ][ pos_n ][ 1 ] ); # nr of transitive groups associated with order n
    while RQ_RCC_loops[ 3 ][ pos_n ][ 2 ][ pos_m ] > m do 
        pos_m := pos_m - 1;
    od;
    g := RQ_RCC_loops[ 3 ][ pos_n ][ 1 ][ pos_m ]; # index of transitive group (of degree n) in GAP library
    # activating data for the group, if needed
    if not IsBound( RQ_RCC_sections[ pos_n ][ pos_m ] ) then 
        # data must be read from file and decoded
        Info( InfoRightQuasigroups, 2, "RQ: reading data file with sections for RCC loops" );
        ReadPackage( "RightQuasigroups", Concatenation( "data/RCC/sections", String(n), ".", String(g), ".tbl" ) );
        # variable RQ_aux is now read and ready to be processed
        nr_conj_classes := Length( RQ_RCC_transitive_groups[ pos_n ][ pos_m ][ 2 ] );
        data := SplitString( RQ_aux, " " );
        data2 := [];
        next_compactified := false;
        for x in data do 
            if x="" then # the next entry is compactified, eg. "a$X" means "a", "$", "X"
                next_compactified := true;
            elif not next_compactified then
                Add( data2, x );
            else # compactified string
                next_compactified := false;
                for i in [1..Length(x)] do
                    Add( data2, x{[i..i]} );    # Add( data2, x[i] ) is not safe when data2 is empty
                od;
            fi;
        od;
        data := List( data2, x -> RQ_ConvertToDecimal( x, 91 ) ); 
        # reconstructing the sequence from the difference sequence
        for i in [2..Length( data )] do
            data[ i ] := data[ i-1 ] - data[ i ];
        od;
        RQ_RCC_sections[ pos_n, pos_m ] := data; 
    fi;
    # data is now loaded
    G := TransitiveGroup(n, g);
    rel_m := m - RQ_RCC_loops[ 3 ][ pos_n ][ 2 ][ pos_m ] + 1;  # relative position of the loop in the file for G
    section := [];
    nr_conj_classes := Length( RQ_RCC_transitive_groups[ pos_n ][ pos_m ][ 2 ] );
    if not RQ_RCC_conjugacy_classes[ 1 ] = [ n, g ] then # must calculate conjugacy classes, so let's reset old data
        RQ_RCC_conjugacy_classes[ 1 ] := [ n, g ];
		RQ_RCC_conjugacy_classes[ 2 ] := List( [1..nr_conj_classes], x->[] );
    fi;
	x := RQ_RCC_sections[ pos_n ][ pos_m ][ rel_m ];
	x := RQ_ConvertFromDecimal( x, 2, nr_conj_classes ); # convert to a binary string of prescribed length
	pos_conjugacy_classes := Positions( x, '1' );
    for i in [1..nr_conj_classes] do
        if RQ_RCC_conjugacy_classes[ 2, i ] = [] then
            RQ_RCC_conjugacy_classes[ 2, i ] := Elements( ConjugacyClass( G, RQ_RCC_transitive_groups[ pos_n ][ pos_m ][ 2 ][ i ] ) );
        fi;
    od;
	section := Concatenation( RQ_RCC_conjugacy_classes[ 2 ]{pos_conjugacy_classes} );
    Add( section, One( G ) ); # the trivial class is contained in all loops and never stored
    Sort( section );
    return LoopByRightSection( [1..n], section );
end);

#############################################################################
##  
#F  RQ_ActivateCCLoop( n, pos_n, m, case ) 
##    
##  Activates a CC-loop from the library.
##  The argument p_case is set to [p,"p^2"] if n = p^2, to [p,"2*p"] if n=2*p, and false otherwise.
##  See manual for complete discussion concerning this library.

InstallGlobalFunction( RQ_ActivateCCLoop,
function( n, pos_n, m, case )
    local powers, p, i, k, F, basis, coords, coc, T, a, b, x, y;
    powers := [,[4,8,16,32,64],[9,27,81],,[25,125],,[49,343]];
    if n in Union( powers ) then # use cocycles
        # determine p and position of n in database
        p := Filtered([2,3,5,7], x -> n in powers[x])[1];
        pos_n := Position( powers[p], n );
        if not IsBound( RQ_CC_cocycles[p] ) then
            # data not read yet, activate once
            Info( InfoRightQuasigroups, 2, "RQ: reading data file with cocycle for CC loops" );
            ReadPackage( "RightQuasigroups", Concatenation( "data/CC/CC_cocycles_", String(p), ".tbl" ) );
            # decode cocycles and separate coordinates from a long string
            for i in [1..Length(powers[p])] do
                RQ_CC_cocycles[ p, i ] := List( RQ_CC_cocycles[ p, i ],
                    c -> RQ_DecodeCocycle( [ p^i, c[1], c[2] ], [0..p-1] )
                );
                RQ_CC_coordinates[ p, i ] := List( RQ_CC_coordinates[ p, i ],
                    c -> SplitString( c, " " )
                );
            od;
        fi;
        # data is now read
        # determine position of loop in the database
        k := 1;
        while m > Length( RQ_CC_coordinates[ p ][ pos_n ][ k ] ) do
            m := m - Length( RQ_CC_coordinates[ p ][ pos_n ][ k ] );
            k := k + 1;
        od;
        # factor loop
        F := CCLoop( n/p, RQ_CC_used_factors[ p ][ pos_n ][ k ] );
        # basis
        basis := List( RQ_CC_bases[ p ][ pos_n ][ k ],
            i -> RQ_CC_cocycles[ p ][ pos_n ][ i ]
        );
        # coordinates 
        coords := RQ_CC_coordinates[ p ][ pos_n ][ k ][ m ];
        coords := RQ_ConvertBase( coords, 91, p, Length( basis ) );
        coords := List( coords, RQ_CharToDigit );
        # cocycle
        coc := (coords*basis) mod p;
        coc := List( coc, i -> i+1 ); 
        # return extension of Z_p by F using cocycle and trivial action
        return LoopByCentralExtension( CCLoop(p,1), F, coc );
    fi;
    
    if case=false then # use library of RCC loops, must recalculate pos_n
        return RQ_ActivateRCCLoop( n, Position(RQ_RCC_loops[ 1 ], n), RQ_CC_loops[ 3 ][ pos_n ][ m ] ); 
    fi;

    # parameters n, m are already checked to be permissible
    p := case[ 1 ];
    if case[ 2 ] = "2*p" then # 2*p
        T := List( [1..n], i -> [1..n ] );
        for a in [0..p-1] do for b in [0..p-1] do
            T[a+1,b+1]:= ((a+b) mod p) + 1;
            T[a+1,p+b+1] := p + ((-a+b) mod p ) + 1;
            T[p+a+1,b+1] := p + ((a+b) mod p) + 1;
            T[p+a+1,p+b+1] := ((1-a+b) mod p) + 1;
        od; od;
        return LoopByCayleyTable( T );
    fi;

    # p^2
    T := List([1..n], i->[1..n]);
    if m = 1 then
        for x in [0..n-1] do for y in [0..n-1] do
            T[ x+1, y+1 ] := ((x + y + p*(x^2)*y) mod n) + 1;
        od; od;
    elif m = 2 then
        k := RQ_SmallestNonsquare( p );
        for x in [0..n-1] do for y in [0..n-1] do
            T[ x+1, y+1 ] := ((x + y + k*p*(x^2)*y) mod n) + 1;
        od; od;
    elif m = 3 then
        for x in [0..p-1] do for y in [0..p-1] do for a in [0..p-1] do for b in [0..p-1] do
            T[ x*p+a+1, y*p+b+1 ] := ((x+y) mod p)*p + ((a+b+(x^2)*y) mod p) + 1;
        od; od; od; od; 
    fi;
    return LoopByCayleyTable( T );
end);

#############################################################################
##  
#F  RQ_ActivateNilpotentLoop( data ) 
##    
##  Activates the nilpotent loop based on data = [ K, F, t ], where
##  K determines a central (normal) subloop,
##  F determines the factor loop, 
##  t determines the cocycle.
##  Understanding K and F:
##      If the value of K or F is in [2,3,4,5], it is the cyclic group of order V.
##      If the value of K or F is 0, it is the Klein group.
##  Understanding t:
##      The cocycle is mapping from F x F to K. Let f = |F|. Let k = |K|.
##      The value t corresponds to a (f-1)x(f-1) array of values in [0..k-1].
##      It is represented by a single integer in base k, with the least 
##      significant digit in the first row and first column, then following
##      the rows.
##      Once t is decoded into a (f-1)x(f-1) array, 1 is added to all entries.
##      Then a first row and forst column of all ones is added to t,
##      resulting in a f x f array.
##  The loop is then obtained via LoopByCentralExtension( K, F, t), where
##  phi is trivial.

InstallGlobalFunction( RQ_ActivateNilpotentLoop,
function( data )
    local K, F, f, k, t, theta, i, j;
    
    # preparing normal subloop and factor loop
    if data[ 1 ] = 0 then 
        K := AsLoop( Group( (1,2),(3,4) ) ); 
    else
        K := AsLoop( CyclicGroup( data[ 1 ] ) );
    fi;
    if data[ 2 ] = 0 then 
        F := AsLoop( Group( (1,2),(3,4) ) );
    else
        F := AsLoop( CyclicGroup( data[ 2 ] ) );    
    fi;
    
    # preparing cocycle
    f := Size( F );
    k := Size( K );
    t := data[ 3 ];
    theta := List( [1..f], i->[1..f] );
    for i in [2..f] do
        theta[ 1, i ] := 1;
    od;
    for i in [2..f] do for j in [2..f] do
        theta[ i, j ] := t mod k;
        t := (t - theta[i,j])/k;
        theta[ i, j ] := theta[ i, j ] + 1;
    od; od;
    
    # constructing the loop
    return LoopByCentralExtension( K, F, theta );

end);

#############################################################################
##  
#F  RQ_ActivateAutomorphicLoop( n, m ) 
##    
##  Activates an automorphic loop from the library.

InstallGlobalFunction( RQ_ActivateAutomorphicLoop,
function( n, m )
    # returns the associated Gamma loop (which here always happens to be automorphic)
    # improve later
    local P, L, s, Ls, ct, i, j, pos, f;
    P := LeftBruckLoop( n, m );
    L := LeftMultiplicationGroup( P );;
    s := List(Elements(L), x -> x^2 );;
    Ls := List([1..n], i -> LeftTranslation( P, Elements(P)[i] ) );;
    ct := List([1..n],i->0*[1..n]);;
    for i in [1..n] do for j in [1..n] do
	   pos := Position( s, Ls[i]*Ls[j]*Ls[i]^(-1)*Ls[j]^(-1) );
	   f := Elements(L)[pos];
	   ct[i,j] := 1^(f*Ls[j]*Ls[i]);
    od; od;
    return LoopByCayleyTable(ct);
end);

#############################################################################
##  
#F  RQ_ActivateRightBruckLoop( n, m ) 
##    
##  Activates a right Bruck loop from the library.

InstallGlobalFunction( RQ_ActivateRightBruckLoop,
function( n, m )
    local pos_n, factor_id, F, basis, coords, coc;
    # factor loop
    pos_n := Position( [27,81], n );
    factor_id := RQ_CharToDigit( RQ_right_Bruck_coordinates[ pos_n ][ m ][ 1 ] );
    F := RightBruckLoop( n/3, factor_id );
    # basis (only decode cocycles at first usage)
    if IsString( RQ_right_Bruck_cocycles[ pos_n ][ 1 ][ 3 ] ) then # not converted yet
        RQ_right_Bruck_cocycles[ pos_n ] := List( RQ_right_Bruck_cocycles[ pos_n ],
            coc -> RQ_DecodeCocycle( coc, [0,1,2] )
        );
    fi;
    basis := RQ_right_Bruck_cocycles[ pos_n ];
    # coordinates determining the cocycle
    coords := RQ_right_Bruck_coordinates[ pos_n ][ m ];
    coords := coords{[2..Length(coords)]}; # remove the character that determines factor id
    coords := RQ_ConvertBase( coords, 91, 3, Length( basis ) );
    coords := List( coords, RQ_CharToDigit );
    # calculate cocycle
    coc := (coords*basis) mod 3;
    coc := coc + 1;
    # return extension of Z_3 by F using cocycle and trivial action
    return LoopByCentralExtension( RightBruckLoop(3,1), F, coc );
end);  

# RQ_IntToCounter(i,ls)
# auxiliary function
# Example: Suppose that ls = [3,2]. Then possible counter states are
# [1,1], [1,2], [2,1], [2,2], [3,1], [3,2], in this order.
# This method returns the ith element on this list.

RQ_IntToCounter := function( i, ls )
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

#############################################################################
##  
#F  RQ_ActivateRackOrQuandle( category, n, m ) 
##    
##  Activates a small rack or quandle. Category is IsRack or IsQuandle.

InstallGlobalFunction( RQ_ActivateRackOrQuandle,
function( category, n, m )

    local groups, ids, previous, group_id, sum, G, id, i, lsection, L0, t, ct;

    if category = IsRack then
        groups := RQ_rack_groups;
        ids := RQ_rack_ids;
        previous := RQ_rack_previous;
    else
        groups := RQ_quandle_groups;
        ids := RQ_quandle_ids;
        previous := RQ_quandle_previous;
    fi;

    # load data file if needed
    if not IsBound( groups[ n ] ) then
        if category = IsRack then
            Info( InfoRightQuasigroups, 2, "RQ: reading data file with racks of size ", n );
            ReadPackage( "RightQuasigroups", Concatenation( "data/racks/racks_of_order_", String(n), ".txt" ) );
        else 
            Info( InfoRightQuasigroups, 2, "RQ: reading data file with quandles of size ", n );
            ReadPackage( "RightQuasigroups", Concatenation( "data/quandles/quandles_of_order_", String(n), ".txt" ) );
        fi;
    fi;

    # construct relevant group
    group_id := 1;
    sum := 0;
    while sum + Length( ids[n][group_id] ) < m do
        sum := sum + Length( ids[n][group_id] );
        group_id := group_id + 1;
        
    od;
    G := Group( groups[ n ][ group_id ] );

    # construct relevant id
    m := m - sum; # relative m
    id := ids[n][group_id][ m-1 - ((m-1) mod 100) + 1 ];
    for i in [m-1 - ((m-1) mod 100) + 2 .. m] do
        if not IsBound( ids[n][group_id][i] ) then
            id := id + 1;
        else
            id := id + ids[n][group_id][i];
        fi;
    od;

    if previous.G <> G or previous.m <> m then # new parameters, initialize
        # PROG: it's not enough to test previous.G <> G since trivial G does not keep track of m and the orbits
        previous.G := G;
        previous.orbs := Orbits( G, [1..n] );
        previous.reps := List( previous.orbs, O -> O[1] );
        previous.m := Length( previous.reps ); # number of orbits
        previous.stabs := List( [1..previous.m], i -> Stabilizer( G, previous.reps[i] ) );
        if category = IsRack then
            previous.folder := List( [1..previous.m], i -> Set( Elements( Centralizer( G, previous.stabs[i] ) ) ) );
        else
            previous.folder := List( [1..previous.m], i -> Set( Elements( Center( previous.stabs[i] ) ) ) );
        fi;
        previous.trans := List( [1..previous.m], i -> Set( Elements( RightTransversal( G, previous.stabs[i] ) ) ) );
    fi;

    # construct and return the algebra
    # PROG: we do not call RackByRackEnvelope etc. since lots of things are precalculated
    # the original library was for left racks/quandles, hence we build the left section
    id := RQ_IntToCounter( id, List( previous.folder, Length ) );
    lsection := []; # the quandle/rack, represented as a list of n left translations
    for i in [1..previous.m] do
        L0 := previous.folder[i][ id[i] ]; # left translation for orbit representative
        for t in previous.trans[ i ] do
            lsection[ previous.reps[i]^t ] := L0^t;
        od;
    od;
    ct := List( [1..n], i -> List( [1..n], j -> i^lsection[j] ) ); # PROG: note how this accomplishes matrix transpose
    return RightQuasigroupByCayleyTable( ct );

end );

# ACCESSING LOOPS IN THE LIBRARY
# _____________________________________________________________________________

# LibraryAlgebra( name, n, m ) 

InstallMethod( LibraryAlgebra, "for string and two positive integers",
    [ IsString, IsPosInt, IsPosInt ],
function( name, n, m )

    local lib, implementedOrders, NOA, algebra, pos_n, p, q, divs, root, half, case, g, h;

    # selecting data library
    lib := RQ_LibraryByName( Concatenation( name, "s" ) ); # adding plural 's' for the name of the library

    # extent of the library
    implementedOrders := lib[ 1 ];
    
    # number of algebras of given order in the library
    NOA := lib[ 2 ];
    
    # CHECKING ARGUMENTS

    if (not n in Integers) or (not m in Integers) or not (n>0) or not (m>0) then
        Error("RQ: Both arguments must be positive integers.");
    fi;

    # parameters for handling systematic cases, such as CCLoop( p^2, 1 )
    pos_n := fail;
    case := false; 
    if name="left Bol loop" then
        divs := DivisorsInt( n );
        if Length( divs ) = 4 and not IsInt( divs[3]/divs[2] ) then # case n = p*q
            q := divs[ 2 ];
            p := divs[ 3 ];
            case := [p,q];
            if not (IsOddInt( q ) and IsInt((p^2-1)/q)) then
                Error("RQ: Nonassociative ", name, " of order p*q exist only for primes p>q>2 such that q divides p^2-1.");
            fi;
            if IsInt((p-1)/q) and (not m in [1..(p-q)/2]) then
                Error("RQ: There are only ", (p-q)/2, " nonassociative ", name, "s of order ", n, ".");
            fi;
            if IsInt((p+1)/q) and (not m in [1..(p-q+2)/2]) then
                Error("RQ: There are only ", (p-q+2)/2, " nonassociative ", name, "s of order ", n, ".");
            fi;
        fi;
    fi;
    if name="CC loop" then 
        divs := DivisorsInt( n );
        if Length( divs ) = 3 and divs[ 2 ] > 7 then # case p^2, p>7
            p := divs[ 2 ];
            case := [p,"p^2"];
            if not m in [1..3] then
                Error("RQ: There are only 3 nonassociative CC loops of order p^2 for an odd prime p.");
            fi;
        elif Length( divs ) = 4 and not IsInt( divs[3]/divs[2] ) and not n=21 then # p*q
            p := divs[ 3 ];
            case := [p,"2*p"];
            if not divs[2] = 2 then
                Error("RQ: Order ", n, " not implemented.");
            fi;
            if not m=1 then 
                Error("RQ: There is only 1 nonassociative CC loop of order 2*p for an odd prime p.");
            fi;
        fi;
    fi;
    if case=false then
        if not n in implementedOrders then
            Error("RQ: Order ", n, " not implemented.");
        fi;
        pos_n := Position( implementedOrders, n );
        if NOA[ pos_n ] < m then 
            if NOA[ pos_n ] = 1 then
                Error("RQ: There is only ", NOA[ pos_n ], " ", name, " of order ", n, " in the library."); 
            else
                Error("RQ: There are only ", NOA[ pos_n ], " ", name, "s of order ", n, " in the library."); 
            fi;
        fi; 
    fi;                                     
       
    # ACTIVATING THE DESIRED ALGEBRA (treat cases separately below)
    
    # up to isomorphism
    if name = "left Bol loop" then 
        algebra := RQ_ActivateLeftBolLoop( pos_n, m, case );
        SetIsLeftBolLoop( algebra, true );
    elif name = "Moufang loop" then
        algebra := RQ_ActivateMoufangLoop( n, pos_n, m );
        SetIsMoufangLoop( algebra, true );
    elif name = "Paige loop" then
        algebra := LoopByCayleyTable( lib[ 3 ][ 1 ][ 1 ] ); #only one loop there at this point
        SetIsMoufangLoop( algebra, true );
    elif name = "code loop" then
        algebra := LibraryAlgebra( "Moufang", n, lib[ 3 ][ pos_n ][ m ] ); 
        SetIsCodeLoop( algebra, true );
    elif name = "Steiner loop" then
        algebra := RQ_ActivateSteinerLoop( n, pos_n, m );
        SetIsSteinerLoop( algebra, true );
    elif name = "CC loop" then
        if n in [2,3,5,7] then # use Cayley table for canonical cyclic group
            algebra := LoopByCayleyTable( RQ_DecodeCayleyTable( IsLoop, lib[ 3 ][ pos_n ][ m ] ) );
        else
            algebra := RQ_ActivateCCLoop( n, pos_n, m, case );
        fi;
        SetIsCCLoop( algebra, true );
    elif name = "RCC loop" then
        algebra := RQ_ActivateRCCLoop( n, pos_n, m );
        SetIsRCCLoop( algebra, true );
    elif name = "small loop" then
        algebra := LoopByCayleyTable( RQ_DecodeCayleyTable( IsLoop, lib[ 3 ][ pos_n ][ m ] ) );
    elif name = "interesting loop" then
        if [n,m] = [96,1] then # simple Bol loop of order 96
            g := Group((1,4)(2,9)(3,10)(6,11)(7,12)(13,21)(14,22)(15,24)(16,23)(17,30)(18,29)(19,31)(20,32)(33,35)(38,40), 
                (1,2,4,6,8,7,5,3)(9,13,25,18,10,14,26,17)(11,15,27,20,12,16,28,19)(21,30,38,34,23,31,40,35)(22,32,39,36,24,29,37,33));
            h := Normalizer( g, SylowSubgroup( g, 5) );
            g := Action( g, RightCosets( g, h ), OnRight );
            algebra := LoopByRightSection([1..n],Union(Filtered(ConjugacyClasses(g),c->Size(c) in [1,15,80])));
        else             
            algebra := LoopByCayleyTable( RQ_DecodeCayleyTable( IsLoop, lib[ 3 ][ pos_n ][ m ][ 1 ] ) );
        fi;
        SetName( algebra, lib[ 3 ][ pos_n ][ m ][ 2 ] ); # special naming for interesting loops
    elif name = "nilpotent loop" then
        algebra := RQ_ActivateNilpotentLoop( lib[ 3 ][ pos_n ][ m ] );
    elif name = "automorphic loop" then
        if not n in [3, 9, 27, 81] then # use Cayley table
            algebra := LoopByCayleyTable( RQ_DecodeCayleyTable( IsLoop, lib[ 3 ][ pos_n ][ m ] ) );
        else # use associated left Bruck loop
            algebra := RQ_ActivateAutomorphicLoop( n, m );
        fi;
        SetIsAutomorphicLoop( algebra, true );
    elif name = "right Bruck loop" then
        if not n in [27,81] then # use Cayley table
            algebra := LoopByCayleyTable( RQ_DecodeCayleyTable( IsLoop, lib[ 3 ][ pos_n ][ m ] ) );
        else # use cocycles
            algebra := RQ_ActivateRightBruckLoop( n, m );
        fi;
        SetIsRightBruckLoop( algebra, true );
    elif name = "small rack" then
        algebra := RQ_ActivateRackOrQuandle( IsRack, n, m );
        SetIsRack( algebra, true );
    elif name = "small quandle" then
        algebra := RQ_ActivateRackOrQuandle( IsQuandle, n, m );
        SetIsQuandle( algebra, true );
    elif name = "connected quandle" then
        # lib[3][pos_n][m] = [ generators_of_group, [ translation ] ]
        algebra := QuandleByQuandleEnvelope( Group( lib[3][pos_n][m][1] ), [1], lib[3][pos_n][m][2], ConstructorStyle( true, false ) ); 
    # up to isotopism     
    elif name = "itp small loop" then
        return LibraryAlgebra( "small loops", n, lib[ 3 ][ pos_n ][ m ] );
    fi;
    
    # setting the name
    SetName( algebra, Concatenation( "<", name, " ", String( n ), "/", String( m ), ">" ) ); # PROG: SetName will not rename an already named object

    # returning the algebra
    return algebra;
end);

#############################################################################
##  READING LOOPS FROM THE LIBRARY - SPECIFIC CALLS
##  -------------------------------------------------------------------------

# LeftBolLoop( n, m ) 
# RightBolLoop( n, m )
# MoufangLoop( n, m ) 
# PaigeLoop( q )
# CodeLoop( n, m ) 
# SteinerLoop( n, m ) 
# CCLoop( n, m ) 
# SmallLoop( n, m ) 
# InterestingLoop( n, m ) 
# NilpotentLoop( n, m ) 
# AutomorphicLoop( n, m )
# LeftBruckLoop( n, m )
# RightBruckLoop( n, m )
# SmallRack( n, m )
# SmallQuandle( n, m )
# ConnectedQuandle( n, m )
# ItpSmallLoop( n, m )   

InstallMethod( LeftBolLoop, "for two positive integers",
    [ IsPosInt, IsPosInt ],
function( n, m )
    return LibraryAlgebra( "left Bol loop", n, m );
end);

InstallMethod( RightBolLoop, "for two positive integers",
    [ IsPosInt, IsPosInt ],
function( n, m )
    local loop;
    loop := OppositeLoop( LeftBolLoop( n, m ) );
    SetIsRightBolLoop( loop, true );
    SetName( loop, Concatenation( "<right Bol loop ", String( n ), "/", String( m ), ">" ) );
    return loop;
end);

InstallMethod( LeftBruckLoop, "for two positive integers",
    [ IsPosInt, IsPosInt ],
function( n, m )
    local loop;
    loop := OppositeLoop( RightBruckLoop( n, m ) );
    SetIsLeftBruckLoop( loop, true );
    SetName( loop, Concatenation( "<left Bruck loop ", String( n ), "/", String( m ), ">" ) );
    return loop;
end);

InstallMethod( RightBruckLoop, "for two positive integers",
    [ IsPosInt, IsPosInt ],
function( n, m )
    return LibraryAlgebra( "right Bruck loop", n, m );
end);

InstallMethod( MoufangLoop, "for two positive integers",
    [ IsPosInt, IsPosInt ],
function( n, m )
    return LibraryAlgebra( "Moufang loop", n, m );
end);

InstallMethod( PaigeLoop, "for prime power",
    [ IsPosInt ],
function( q )
    # Paige loop over GF(q)
    if not q=2 then return Error("RQ: Only q=2 is implemented."); fi;
    return LibraryAlgebra( "Paige loop", 120, 1 );
end);

InstallMethod( CodeLoop, "for two positive integers",
    [ IsPosInt, IsPosInt ],
function( n, m )
    return LibraryAlgebra( "code loop", n, m );
end);

InstallMethod( SteinerLoop, "for two positive integers",
    [ IsPosInt, IsPosInt ],
function( n, m )
    return LibraryAlgebra( "Steiner loop", n, m );
end);

InstallMethod( CCLoop, "for two positive integers",
    [ IsPosInt, IsPosInt ],
function( n, m )
    return LibraryAlgebra( "CC loop", n, m );
end);

InstallMethod( ConjugacyClosedLoop, "for two positive integers",
    [ IsPosInt, IsPosInt ],
function( n, m )
    return LibraryAlgebra( "CC loop", n, m );
end);

InstallMethod( RCCLoop, "for two positive integers",
    [ IsPosInt, IsPosInt ],
function( n, m )
    return LibraryAlgebra( "RCC loop", n, m );
end);

InstallMethod( RightConjugacyClosedLoop, "for two positive integers",
    [ IsPosInt, IsPosInt ],
function( n, m )
    return LibraryAlgebra( "RCC loop", n, m );
end);

InstallMethod( LCCLoop, "for two positive integers",
    [ IsPosInt, IsPosInt ],
function( n, m )
    local loop;
    loop := OppositeLoop( RCCLoop( n, m ) );
    SetIsLCCLoop( loop, true );
    SetName( loop, Concatenation( "<LCC loop ", String( n ), "/", String( m ), ">" ) );
    return loop;
end);

InstallMethod( LeftConjugacyClosedLoop, "for two positive integers",
    [ IsPosInt, IsPosInt ],
function( n, m )
    return LCCLoop( n, m );
end);

InstallMethod( SmallLoop, "for two positive integers",
    [ IsPosInt, IsPosInt ],
function( n, m )
    return LibraryAlgebra( "small loop", n, m );
end);

InstallMethod( InterestingLoop, "for two positive integers",
    [ IsPosInt, IsPosInt ],
function( n, m )
    return LibraryAlgebra( "interesting loop", n, m );
end);

InstallMethod( NilpotentLoop, "for two positive integers",
    [ IsPosInt, IsPosInt ],
function( n, m )
    return LibraryAlgebra( "nilpotent loop", n, m );
end);

InstallMethod( AutomorphicLoop, "for two positive integers",
    [ IsPosInt, IsPosInt ],
function( n, m )
    return LibraryAlgebra( "automorphic loop", n, m );
end);

InstallMethod( SmallRack, "for two positive integers",
    [ IsPosInt, IsPosInt ],
function( n, m )
    return LibraryAlgebra( "small rack", n, m );
end );

InstallMethod( SmallQuandle, "for two positive integers",
    [ IsPosInt, IsPosInt ],
function( n, m )
    return LibraryAlgebra( "small quandle", n, m );
end );

InstallMethod( ConnectedQuandle, "for two positive integers",
    [ IsPosInt, IsPosInt ],
function( n, m )
    return LibraryAlgebra( "connected quandle", n, m );
end );

InstallMethod( ItpSmallLoop, "for two positive integers",
    [ IsPosInt, IsPosInt ],
function( n, m )
    return LibraryAlgebra( "itp small loop", n, m );
end);