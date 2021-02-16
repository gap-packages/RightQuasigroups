# MltSearch.gi
# Realizing permutation groups as multiplication groups of loops
# =============================================================================

# REVISIT: Declare auxiliary global functions?

# Debugging
RQ_MltSearchRuntime:=function(tstart)
	return Concatenation("[ ", StringTime(Runtime()-tstart)," ]");
end;

# RQ_TableSearchNC(G, depth, infolevel, task)
# 	restiction in ["", "all", "all proper", "one", "one proper", "all exact", "one exact"]
# 	if task="" then task:="all";

# auxiliary function
RQ_TableSearchNC:=function(g, ldepth, infolevel, task)
	local 
	# task variables
	only_one, only_proper, only_exact, takeit,
	# local external variables
	results, V, hash, row, pi, col_chunks, degree, level, depth,
	# local scanner functions
	move_right, next_node, possible_next_rows,
	# main search loop variables
	fpf_classes, reps, x0, x1, k, i, j, ct, ls, rs, 
	# debugging
	old_infolevel,time_start;

	# setting the local external variables
	results:=[];
	V:=[];
	hash:=[];
	row:=[];
	pi:=[];
	col_chunks:=[];
	degree:=0;
	level:=0;
	depth:=0;

	# functions
	move_right:=function()
		local i;
		if level=0 then 
			return false;
		fi;
		if pi[level]<>[] then 
			row[level]:=Remove(pi[level],1);
			for i in [1..degree] do
				col_chunks[i,level]:=i^V[row[level]];
			od;
			return true;
		else
			Unbind(row[level]);
			Unbind(pi[level]);
			for i in [1..degree] do
				Unbind(col_chunks[i][level]);
			od;
			level:=level-1;
			return move_right();
		fi;
	end;
	
	# next_node()
	# we call it when 
	# a) row[level+1] and pi[level+1] are not defined ---> MOVE RIGHT
	# b) row[level+1] is not defined and pi[level+1] is defined
	#    and non empty ---> MOVE DOWN
	next_node:=function()
		local i;
		if IsBound(pi[level+1]) 
			and (not IsBound(row[level+1])) 
			and (pi[level+1]<>[])
		then 
			level:=level+1;
			row[level]:=Remove(pi[level],1);
			for i in [1..degree] do
				col_chunks[i,level]:=i^V[row[level]];
			od;
			return true;
		elif (not IsBound(pi[level+1])) and (not IsBound(row[level+1])) then
			return move_right();
		else 
			return fail;
		fi;
	end;
	
	possible_next_rows:=function()
		local np,vp,dp;
		dp:=Minimum(level, depth);
		np:=List([2..degree],i->Sum([1..dp],j->(col_chunks[i,j]-1)*degree^(dp-j))+1);
		if ForAny(np,i->not(IsBound(hash[dp][i]))) then 
			pi[level+1]:=[];
			return false;
		fi;
		np:=List(np,i->[hash[dp][i][1]..hash[dp][i][1]+hash[dp][i][2]]);
		np:=List(np,x->Set(V{x},y->(level+1)^y));
		vp:=List(Cartesian(np{[1..depth-1]}),x->Concatenation([level+1],x));
		vp:=List(vp,x->Sum([1..depth],j->(x[j]-1)*degree^(depth-j))+1);
		vp:=Filtered(vp,i->IsBound(hash[depth][i]));
		vp:=List(vp,i->[hash[depth][i][1]..hash[depth][i][1]+hash[depth][i][2]]);
		vp:=Filtered(Concatenation(vp),x->
			ForAll([2..degree],i->(i^V[x] in np[i-1]) and not(i^V[x] in col_chunks[i]))
		);
		pi[level+1]:=vp;
		return vp<>[];
	end;

	# main search part
	# printing the task ["", "all", "all proper", "one", "one proper", "all exact", "one exact"]
	
	old_infolevel:=InfoLevel(InfoRightQuasigroups);
	SetInfoLevel(InfoRightQuasigroups,infolevel);

	if task="" then task:="all"; fi;
	if task="all" then 
		only_one:=false;
		only_proper:=false;
		only_exact:=false;
		Info( InfoRightQuasigroups, 1, "RQ: ### Search for all loops in the given group ###");
	elif task="all proper" then 
		only_one:=false;
		only_proper:=true;
		only_exact:=false;
		Info( InfoRightQuasigroups, 1, "RQ: ### Search for all nonassociative loops in the given group ###");
	elif task="one" then 
		only_one:=true;
		only_proper:=false;
		only_exact:=false;
		Info( InfoRightQuasigroups, 1, "RQ: ### Search for one loop in the given group ###");
	elif task="one proper" then 
		only_one:=true;
		only_proper:=true;
		only_exact:=false;
		Info( InfoRightQuasigroups, 1, "RQ: ### Search for one nonassociative loops loops in the given group ###");
	elif task="all exact" then 
		only_one:=false;
		only_proper:=false;
		only_exact:=true;
		Info( InfoRightQuasigroups, 1, "RQ: ### Search for all loops with given multiplication group ###");
	elif task="one exact" then 
		only_one:=true;
		only_proper:=false;
		only_exact:=true;
		Info( InfoRightQuasigroups, 1, "RQ: ### Search for one loop with given multiplication group ###");
	else 
		SetInfoLevel(InfoRightQuasigroups,old_infolevel);
		return fail; 
	fi;

	time_start:=Runtime();
	degree:=NrMovedPoints(g);
	depth:=ldepth;
	Info(InfoRightQuasigroups, 1, "RQ: # Size of the input group: ", Size(g));
	Info(InfoRightQuasigroups, 1, "RQ: # Degree of the permutation group: ", degree);

	# analysis of the fixed point free elements
	fpf_classes:=Filtered(ConjugacyClasses(g),x->NrMovedPoints(Representative(x))=degree);
	if Length(fpf_classes)=1 then 
		V:=Elements(fpf_classes[1]);
	else
		V:=Union(fpf_classes);;
	fi;
	MakeImmutable(V);
	reps:=List(fpf_classes,x->Minimum(Elements(x)));
	reps:=Set(reps,x->Position(V,x));
	Info(InfoRightQuasigroups, 1, "RQ: # ", RQ_MltSearchRuntime(time_start), " Search started."); 

	Info(InfoRightQuasigroups, 1, "RQ: # Collected the fixed point free elements." );
	Info(InfoRightQuasigroups, 1, "RQ: # Number of conjugacy classes = ", Size(reps), "." );
	Info(InfoRightQuasigroups, 1, "RQ: # Number of fixed point free elements = ", Size(V), "." );

	# hash tables
	hash:=List([1..depth],i->[]);
	x0:=0*[1..depth];
	k:=0*[1..depth];
	for j in [1..Length(V)] do
		x1:=List([1..depth],i->i^V[j]);
		for i in [1..depth] do
			if x0{[1..i]}<>x1{[1..i]} then 
				k[i]:=Sum([1..i],t->(x1[t]-1)*degree^(i-t))+1;
				hash[i,k[i]]:=[j,0];
			else
				hash[i][k[i]][2]:=hash[i][k[i]][2]+1;
			fi;
		od;
		x0:=ShallowCopy(x1);
	od;
	MakeImmutable(hash);
	Info(InfoRightQuasigroups, 1, "# ", RQ_MltSearchRuntime(time_start), " Hash table of depth ", depth, " created." );

	# initialization
	row:=['*'];
	pi:=[[],ShallowCopy(reps)];
	col_chunks:=List([1..degree],i->[i]);
	level:=1;
	results:=[];

	while next_node() do
		if level=2 then 
			Info(InfoRightQuasigroups, 1, "RQ: # ", RQ_MltSearchRuntime(time_start), 
				" We have ", Length(pi[2])+1, " more step(s)." ); 
		fi;
		if level=degree then 
			ct:=List(V{row{[2..degree]}},ListPerm);
			ct:=Concatenation([[1..degree]],ct);
			rs:=Immutable(List(ct,PermList));
			ls:=Immutable(List(TransposedMat(ct),PermList));
			if only_exact then 
				takeit:=(Group(Union(ls,rs))=g);
			elif only_proper then
				takeit:=(ForAll(ls,y->y in g) and 
						not ForAll(rs,x->ForAll(x*rs,y->y in rs)));
			else 
				takeit:=ForAll(ls,y->y in g);
			fi;
			if takeit then 
				Add(results, ShallowCopy(row));
				Info(InfoRightQuasigroups, 2, "RQ: ##############################");
				Info(InfoRightQuasigroups, 1, "RQ: # ", RQ_MltSearchRuntime(time_start), 
					" Hit number ", Length(results));
				Info(InfoRightQuasigroups, 2, ct);
				if only_one then break; fi;
			fi;
		else
			possible_next_rows();
			if pi[level+1]=[] then Unbind(pi[level+1]); fi;
		fi;
	od;

	Info(InfoRightQuasigroups, 1, "RQ: ##############################");
	Info(InfoRightQuasigroups, 1, "RQ: # ", RQ_MltSearchRuntime(time_start), " Finished. ", Length(results), " loops found." );
	
	SetInfoLevel(InfoRightQuasigroups,old_infolevel);
	return List(results,x->Concatenation([()],V{x{[2..degree]}}));
end;

# auxiliary function
RQ_MltSearchInputCheck:=function( G, depth, infolevel, task )
	local degree;
    if not IsPermGroup( G ) then 
		Info( InfoRightQuasigroups, 1, "RQ: <G> must be a permutation group" );
		return false;
	fi;
	degree:=NrMovedPoints(G);
	if MovedPoints(G)<>[1..degree] then 
		Info( InfoRightQuasigroups, 1, "RQ: G> must act transitively on [1..degree]" );
		return false;
	fi;
	if depth=fail then depth := Maximum(1,LogInt(Size(G),degree)-1); fi;
    if not IsInt(depth) or depth<=0 then 
		Info( InfoRightQuasigroups, 1, "RQ: <depth> must be a positive integer" ); 
		return false;
	fi; 
	if infolevel=fail then infolevel:=1; fi;
	if not IsInt(infolevel) or infolevel<0 then 
		Info( InfoRightQuasigroups, 1, "RQ: <infolevel> must be a positive integer" ); 
		return false;
	fi; 
	if not task in ["", "all", "all proper", "one", "one proper", "all exact", "one exact"] then
		Info( InfoRightQuasigroups, 1, "RQ; <task> must be one of the following:");
		Info( InfoRightQuasigroups, 1, "\t\"\", \"all\", \"all proper\", \"one\", ", 
			"\"one proper\", \"all exact\", \"one exact\"." ); 
		return false;
	fi;
	return [G,depth,infolevel,task];
end;

# AllLoopsWithMltInGroup(G[, depth[, infolevel]] )

InstallMethod( AllLoopsWithMltInGroup, "for a group",
    [ IsGroup ],
function( G )
	local depth, infolevel, task, a;
	task:="all";
	depth:=fail;
	infolevel:=fail;
	a:=RQ_MltSearchInputCheck(G,depth,infolevel,task);
	if a<>false then
		return RQ_TableSearchNC(a[1],a[2],a[3],a[4]);
	else
		return fail;
	fi;
end);

InstallOtherMethod( AllLoopsWithMltInGroup, "for a group and integer",
    [ IsGroup, IsInt ],
function( G, depth )
	local infolevel, task, a;
	task:="all";
	infolevel:=fail;
	a:=RQ_MltSearchInputCheck(G,depth,infolevel,task);
	if a<>false then
		return RQ_TableSearchNC(a[1],a[2],a[3],a[4]);
	else
		return fail;
	fi;
end);

InstallOtherMethod( AllLoopsWithMltInGroup, "for a group and two integers",
    [ IsGroup, IsInt, IsInt ],
function( G, depth, infolevel )
	local task, a;
	task:="all";
	a:=RQ_MltSearchInputCheck(G,depth,infolevel,task);
	if a<>false then
		return RQ_TableSearchNC(a[1],a[2],a[3],a[4]);
	else
		return fail;
	fi;
end);

# AllNonassociativeLoopsWithMltInGroup(G[, depth[, infolevel]] )

InstallMethod( AllNonassociativeLoopsWithMltInGroup, "for a group",
    [ IsGroup ],
function( G )
	local depth, infolevel, task, a;
	task:="all proper";
	depth:=fail;
	infolevel:=fail;
	a:=RQ_MltSearchInputCheck(G,depth,infolevel,task);
	if a<>false then
		return RQ_TableSearchNC(a[1],a[2],a[3],a[4]);
	else
		return fail;
	fi;
end);

InstallOtherMethod( AllNonassociativeLoopsWithMltInGroup, "for a group and integer",
    [ IsGroup, IsInt ],
function( G, depth )
	local infolevel, task, a;
	task:="all proper";
	infolevel:=fail;
	a:=RQ_MltSearchInputCheck(G,depth,infolevel,task);
	if a<>false then
		return RQ_TableSearchNC(a[1],a[2],a[3],a[4]);
	else
		return fail;
	fi;
end);

InstallOtherMethod( AllNonassociativeLoopsWithMltInGroup, "for a group and two integers",
    [ IsGroup, IsInt, IsInt ],
function( G, depth, infolevel )
	local task, a;
	task:="all proper";
	a:=RQ_MltSearchInputCheck(G,depth,infolevel,task);
	if a<>false then
		return RQ_TableSearchNC(a[1],a[2],a[3],a[4]);
	else
		return fail;
	fi;
end);

# OneLoopWithMltInGroup(G[, depth[, infolevel]] )

InstallMethod( OneLoopWithMltInGroup, "for a group",
    [ IsGroup ],
function( G )
	local depth, infolevel, task, a;
	task:="one";
	depth:=fail;
	infolevel:=fail;
	a:=RQ_MltSearchInputCheck(G,depth,infolevel,task);
	if a<>false then
		return RQ_TableSearchNC(a[1],a[2],a[3],a[4]);
	else
		return fail;
	fi;
end);

InstallOtherMethod( OneLoopWithMltInGroup, "for a group and integer",
    [ IsGroup, IsInt ],
function( G, depth )
	local infolevel, task, a;
	task:="one";
	infolevel:=fail;
	a:=RQ_MltSearchInputCheck(G,depth,infolevel,task);
	if a<>false then
		return RQ_TableSearchNC(a[1],a[2],a[3],a[4]);
	else
		return fail;
	fi;
end);

InstallOtherMethod( OneLoopWithMltInGroup, "for a group and two integers",
    [ IsGroup, IsInt, IsInt ],
function( G, depth, infolevel )
	local task, a;
	task:="one";
	a:=RQ_MltSearchInputCheck(G,depth,infolevel,task);
	if a<>false then
		return RQ_TableSearchNC(a[1],a[2],a[3],a[4]);
	else
		return fail;
	fi;
end);

# OneNonassociativeLoopWithMltInGroup(G[, depth[, infolevel]] )

InstallMethod( OneNonassociativeLoopWithMltInGroup, "for a group",
    [ IsGroup ],
function( G )
	local depth, infolevel, task, a;
	task:="one proper";
	depth:=fail;
	infolevel:=fail;
	a:=RQ_MltSearchInputCheck(G,depth,infolevel,task);
	if a<>false then
		return RQ_TableSearchNC(a[1],a[2],a[3],a[4]);
	else
		return fail;
	fi;
end);

InstallOtherMethod( OneNonassociativeLoopWithMltInGroup, "for a group and integer",
    [ IsGroup, IsInt ],
function( G, depth )
	local infolevel, task, a;
	task:="one proper";
	infolevel:=fail;
	a:=RQ_MltSearchInputCheck(G,depth,infolevel,task);
	if a<>false then
		return RQ_TableSearchNC(a[1],a[2],a[3],a[4]);
	else
		return fail;
	fi;
end);

InstallOtherMethod( OneNonassociativeLoopWithMltInGroup, "for a group and two integers",
    [ IsGroup, IsInt, IsInt ],
function( G, depth, infolevel )
	local task, a;
	task:="one proper";
	a:=RQ_MltSearchInputCheck(G,depth,infolevel,task);
	if a<>false then
		return RQ_TableSearchNC(a[1],a[2],a[3],a[4]);
    else
		return fail;
	fi;
end);

# AllLoopsWithMltGroup(G[, depth[, infolevel]] )

InstallMethod( AllLoopsWithMltGroup, "for a group",
    [ IsGroup ],
function( G )
	local depth, infolevel, task, a;
	task:="all exact";
	depth:=fail;
	infolevel:=fail;
	a:=RQ_MltSearchInputCheck(G,depth,infolevel,task);
	if a<>false then
		return RQ_TableSearchNC(a[1],a[2],a[3],a[4]);
	else
		return fail;
	fi;
end);

InstallOtherMethod( AllLoopsWithMltGroup, "for a group and integer",
    [ IsGroup, IsInt ],
function( G, depth )
	local infolevel, task, a;
	task:="all exact";
	infolevel:=fail;
	a:=RQ_MltSearchInputCheck(G,depth,infolevel,task);
	if a<>false then
		return RQ_TableSearchNC(a[1],a[2],a[3],a[4]);
	else
		return fail;
	fi;
end);

InstallOtherMethod( AllLoopsWithMltGroup, "for a group and two integers",
    [ IsGroup, IsInt, IsInt ],
function( G, depth, infolevel )
	local task, a;
	task:="all exact";
	a:=RQ_MltSearchInputCheck(G,depth,infolevel,task);
	if a<>false then
		return RQ_TableSearchNC(a[1],a[2],a[3],a[4]);
	else
		return fail;
	fi;
end);

# OneLoopWithMltGroup(G[, depth[, infolevel]] )

InstallMethod( OneLoopWithMltGroup, "for a group",
    [ IsGroup ],
function( G )
	local depth, infolevel, task, a;
	task:="one exact";
	depth:=fail;
	infolevel:=fail;
	a:=RQ_MltSearchInputCheck(G,depth,infolevel,task);
	if a<>false then
		return RQ_TableSearchNC(a[1],a[2],a[3],a[4]);
	else
		return fail;
	fi;
end);

InstallOtherMethod( OneLoopWithMltGroup, "for a group and integer",
    [ IsGroup, IsInt ],
function( G, depth )
	local infolevel, task, a;
	task:="one exact";
	infolevel:=fail;
	a:=RQ_MltSearchInputCheck(G,depth,infolevel,task);
	if a<>false then
		return RQ_TableSearchNC(a[1],a[2],a[3],a[4]);
	else
		return fail;
	fi;
end);

InstallOtherMethod( OneLoopWithMltGroup, "for a group and two integers",
    [ IsGroup, IsInt, IsInt ],
function( G, depth, infolevel )
	local task, a;
	task:="one exact";
	a:=RQ_MltSearchInputCheck(G,depth,infolevel,task);
	if a<>false then
		return RQ_TableSearchNC(a[1],a[2],a[3],a[4]);
	else
		return fail;
	fi;
end);