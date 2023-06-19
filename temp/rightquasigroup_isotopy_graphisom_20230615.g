LoadPackage( "RightQuasigroups" ); 

###########################
###        TESTS        ###
###########################

TraceMethods([AutotopismGroup,AutomorphismGroup,IsomorphismRightQuasigroups,IsotopismRightQuasigroups]);

AutotopismGroup(RightBolLoop(8,2));

if not IsBound(n) then n:=36; fi;

rq:=ConnectedQuandle(n,5);
ig:=AutotopismGroup(rq); Print( "#time = ", time, "\n"); Size(ig);

###

rq:=ConnectedQuandle(n,5);

ag2:=AutomorphismGroup(rq); Print( "#time = ", time, "\n");
ag2=AutomorphismGroup(rq);

###

n2:=32;
rq:=RandomNilpotentLoop([ElementaryAbelianGroup(2),ElementaryAbelianGroup(n2/2)]);

ag2:=AutomorphismGroup(rq); Print( "#time = ", time, "\n");
AutomorphismGroup(rq); Print( "#time = ", time, "\n");
ag2=AutomorphismGroup(rq);

ig:=AutotopismGroup(rq); Print( "#time = ", time, "\n");
Size(ig);
Exponent(ig);

li:=List([1..5],i->Random(ig));
h:=Group(li);
Size(h);
KnownAttributesOfObject(h);

###

Q1 := RightQuasigroupByFunction( [0..9], function(x,y) return (x+2*y) mod 10; end );
Q2 := RightQuasigroupIsomorph( Q1, (3,4,5) );
IsomorphismRightQuasigroups( Q1, Q2 );

Q1:=MoufangLoop(64,222);
f:=List([1,2,3],i->Random(SymmetricGroup([2..64])));
Q2:=LoopIsotope(Q1,f[1],f[2],f[3]);
iso:=IsotopismRightQuasigroups(Q1,Q2); Print( "#time = ", time, "\n");
# IsotopismRightQuasigroups(Q1,Q2); time; # this takes very very long
ff:=List([1,2,3],i->AsPermutation(ComponentOfHomotopism(iso,i)));
AutotopismRightQuasigroup(Q1,f[1]/ff[1],f[2]/ff[2],f[3]/ff[3]);

UntraceMethods([AutotopismGroup,AutomorphismGroup,IsomorphismRightQuasigroups,IsotopismRightQuasigroups]);

#####################################
###        TESTS for UPTOs        ###
#####################################

LoadPackage( "RightQuasigroups" );
TraceMethods([AutotopismGroup,AutomorphismGroup,IsomorphismRightQuasigroups,IsotopismRightQuasigroups]);
AutomorphismGroup( MoufangLoop( 12, 1) );
UntraceMethods([AutotopismGroup,AutomorphismGroup,IsomorphismRightQuasigroups,IsotopismRightQuasigroups]);

g := TransitiveGroup( 8, 31 );
lps := AllLoopsWithMltInGroup( g, 2, 0 );; Size( lps );
Apply( lps, LoopByRightSection );
Size( LoopsUpToIsomorphism( lps ) ); time;

g := TransitiveGroup( 8, 31 );
lps := AllLoopsWithMltInGroup( g, 2, 0 );; Size( lps );
Apply( lps, LoopByRightSection );
Size( LoopsUpToIsomorphism( lps : UseDiscriminator ) ); time;

# with Digraphs: 3917 vs 1397
# without Digraphs: 1523 vs 1629

############################
############################

LoadPackage( "RightQuasigroups" );
TraceMethods([AutotopismGroup,AutomorphismGroup,IsomorphismRightQuasigroups,IsotopismRightQuasigroups]);

g := TransitiveGroup( 8, 31 );
lps := AllLoopsWithMltInGroup( g, 2, 0 );; Size( lps );
Apply( lps, LoopByRightSection );
Size( LoopsUpToIsotopism( lps{[1..5]} ) ); time;

UntraceMethods([AutotopismGroup,AutomorphismGroup,IsomorphismRightQuasigroups,IsotopismRightQuasigroups]);

# with Digraphs: 9ms
# without Digraphs: 88433ms

############################
############################

ls := AllLoopsWithMltInGroup( SymmetricGroup( 6 ), 2, 0 );; Size( ls );
Apply( ls, LoopByRightSection );
ls1 := LoopsUpToIsomorphism( ls ); time; Size( ls1 );
#ls2 := LoopsUpToIsotopism( ls1 ); time; Size( ls2 );


############################
############################


rq:=RandomNilpotentLoop([ElementaryAbelianGroup(2),ElementaryAbelianGroup(n2/2)]);

ts:=[];; 
for n in [10..50] do 
    t1:=Runtime(); 
    a:=RandomLoop(n); 
    t2:=Runtime(); 
    b:=IsomorphismDiscriminator(a); 
    t3:=Runtime();
    Add(ts,[Size(a),t2-t1,t3-t2]); 
    Print(Size(a),"\t",t2-t1,"\t",t3-t2,"\n"); 
od;

ts:=[];; 
for n in [5..25] do 
    t1:=Runtime(); 
    a:=RandomNilpotentLoop([ElementaryAbelianGroup(2),CyclicGroup(n)]); 
    t2:=Runtime(); 
    b:=IsomorphismDiscriminator(a); 
    t3:=Runtime();
    Add(ts,[Size(a),t2-t1,t3-t2]); 
    Print(Size(a),"\t",t2-t1,"\t",t3-t2,"\n"); 
od;

ls:=List([1..100],i->RandomNilpotentLoop([ElementaryAbelianGroup(2),ElementaryAbelianGroup(4)]));;
ls1 := LoopsUpToIsomorphism( ls ); time; Size( ls1 );
