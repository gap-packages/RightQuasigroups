#LoadPackage("loops");
LoadPackage("RightQuasigroups");

R:=RightQuasigroupByFunctions(List([1..6],i->Z(7)^i),\*,\/);
S:=RightQuasigroupByFunctions(GF(5),\+,\-);

r:=Elements(R)[1];
s:=Elements(S)[1];

r=s;

r in R;
r in S;

SetRightQuasigroupElmName(S,"s");
Elements(S);

S[Z(5)^2];
S.2;

SortedList(S);

# RightQuasigroupByFunctions(GF(5),\+,\*);

sec:=[(1,2,3,4),(1,2,3,4),(),(1,3)(2,4)];
u:=RightQuasigroupBySection([1..4],sec);
List(u,x->List(u,y->x*y));
List(u,x->List(u,y->x/y));

sec[4]:=();
List(u,x->List(u,y->x*y));

###

S:=GF(2)^2;
id:=Z(2)*[[1,0],[0,1]];
ma:=Z(2)*[[1,1],[0,1]];
sec:=[ma,ma,id,id];
v:=RightQuasigroupBySection(S,sec);
List(v,x->List(v,y->x*y));

v.2;
v[[Z(2),0*Z(2)]];
CayleyTable(v);
MultiplicationTable(v);

###

ct:=[["b","a","a"],["c","c","b"],["a","b","c"]];
w:=RightQuasigroupByCayleyTable(ct);
w.1;
w["b"];

###

ct:=[[1,3,5],[3,5,1],[5,1,3]];
w:=RightQuasigroupByCayleyTable(ct);
w.2;
w[3];

###

q:=ProjectionRightQuasigroup([1,3,5,7,9]);
Elements(q);
CayleyTable(q);
MultiplicationTable(q);
s:=SubrightQuasigroup(q,[1,3]);
Elements(s);
CayleyTable(s);
MultiplicationTable(s);

###

RightSection(s);
RightSection(w);
RightSection(v);

###

s:=RightCoreOfGroup(SymmetricGroup(5));
StructureDescription(Group(RightSection(s)));

###

S:=GF(2)^10;
sec:=List(S,x->Random(GL(10,2)));;
uu0:=RightQuasigroupBySection(S,sec); time;
uu1:=RightQuasigroupBySection(S,sec,true); time;
RightSection(uu0);; time;
RightSection(uu1);; time;
RightSection(uu1)=RightSection(uu0);


