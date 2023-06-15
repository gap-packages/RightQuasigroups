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
