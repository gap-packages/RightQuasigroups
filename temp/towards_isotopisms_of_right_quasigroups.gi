PerfectBipartiteMatching := function( A, n )
# A[i,j]=1 iff there is an edge from U[i] to V[j]
	local edges, NV, NU, i, j, M, u, v, eM, eN, m, vnext, p, posv, posu;
	edges := []; # edges of the graph
	NV := List([1..n],i->[]); # NV[i] = { j in V: [i,j] in edghes }
	NU := List([1..n],i->[]); # NU[j] = { i in U: [i,j] in edghes }
	for i in [1..n] do for j in [1..n] do
		if A[i,j]=1 then
			AddSet( NV[i], j );
			AddSet( NU[j], i );
			AddSet( edges, [i,j] );
		fi;
	od; od;
	if IsEmpty( edges ) then
		return fail;
	fi;
	M := [ edges[ 1 ] ];
	# partial matching to be enlarged
	while Length( M ) < n do
		# construct data for an alternating path
		u := []; v := []; eM := []; eN := [];
		# ...find a vertex of U not in M
		u[1] := First( [1..n], i -> not (i in List(M,e->e[1])) ); if u[1]=fail then return fail; fi;
		# ...find any edge adjacent to u[1]
		v[1] := First( NV[u[1]] ); if v[1]=fail then return fail; fi;
		eN := [[u[1],v[1]]];
		m := 1;
		while v[m] in List(M,e->e[2]) do # last vertex of v is in M, continue
			u[m+1] := First( NU[v[m]], i -> [i,v[m]] in M ); if u[m+1]=fail then return fail; fi;
			Add( eM, [u[m+1],v[m]] );
			vnext := First( [1..n], i -> (not (i in v)) and (not IsEmpty( Intersection( NU[i], u ) ) ) ); # has some edge to a vertex of u
			if vnext=fail then return fail; fi;
			v[m+1] := vnext;
			Add( eN, [ First( Intersection( NU[vnext], u ) ), vnext ] );
			m := m+1;
		od;
		# construct the alternating path
		p := [];
		posv := m;
		repeat 
			posu := First( [1..posv], i -> [u[i],v[posv]] in eN );
			Add( p, [u[posu],v[posv]] );
			if posu > 1 then
				posv := First( [1..posu], j -> [u[posu],v[j]] in eM );
				Add( p, [u[posu],v[posv]] );
			fi;
		until posu = 1;
		# replace M with a larger matching
		M := Union( Difference( M, p ), Difference( p, M ) );
	od;
	return M;
end;

ItpRQ := function( Q1, Q2 )
# naive approach that works for n<=8 or so
# 1) try all f
# 2) try all g[1]
# 3) this forces h
# 4) this restricts choices for g, solved as a Hall marriage problem
	local n, t1, t2, f, g, h, e, x, S, y, A, M;
	n := Size( Q1 );
	t1 := MultiplicationTable( Q1 );
	t2 := MultiplicationTable( Q2 );
	for f in SymmetricGroup( n ) do
		g := 0*[1..n];
		h := 0*[1..n];	
		for e in [1..n] do
			g[1] := e;
			# build h
			for x in [1..n] do
				h[ t1[x][1] ] := t2[ x^f, e ];
			od;
			# try to complete g
			S := List([1..n], i -> [] ); # g(y) must lie in S[y];
			S[1] := [e];
			for y in [2..n] do
				S[y] := [1..n];
				for x in [1..n]	do
					S[y] := Filtered( S[y], z -> t2[x^f,z] = h[t1[x,y]] );
				od;
			od;
			# solve the marriage problem for g
			# build bipartite graph
			A := List([1..n], i->0*[1..n]);
			for x in [1..n] do for y in [1..n] do
				if y in S[x] then
					A[x][y] := 1;
				fi;	
			od; od;
			M := PerfectBipartiteMatching( A, n );
			if M<>fail then # done, report results
				# build g
				for x in M do # x is an edge of M
					g[x[1]] := x[2];
				od;
				g := PermList( g );
				h := PermList( h );
				return [f,g,h];
			fi;		
		od;
	od;
	return fail;
end;

ItpRQ2 := function( Q1, Q2 )
# better approach, working modulo an automorphism group
# it speeds up things when the automorphism group of Q2 is large
# 1) try all f in the right Transversal of Aut(Q2) in Sym(|Q2|)
# 2) try all g[1]
# 3) this forces h
# 4) this restricts choices for g, solved as a Hall marriage problem
	local n, t1, t2, R2, f, g, h, e, x, S, y, A, M;
	n := Size( Q1 );
	t1 := MultiplicationTable( Q1 );
	t2 := MultiplicationTable( Q2 );
	R2 := RightTransversal( SymmetricGroup( n ), AutomorphismGroup( Q2 ) );
	for f in R2 do
		g := 0*[1..n];
		h := 0*[1..n];	
		for e in [1..n] do
			g[1] := e;
			# build h
			for x in [1..n] do
				h[ t1[x][1] ] := t2[ x^f, e ];
			od;
			# try to complete g
			S := List([1..n], i -> [] ); # g(y) must lie in S[y];
			S[1] := [e];
			for y in [2..n] do
				S[y] := [1..n];
				for x in [1..n]	do
					S[y] := Filtered( S[y], z -> t2[x^f,z] = h[t1[x,y]] );
				od;
			od;
			# solve the marriage problem for g
			# build bipartite graph
			A := List([1..n], i->0*[1..n]);
			for x in [1..n] do for y in [1..n] do
				if y in S[x] then
					A[x][y] := 1;
				fi;	
			od; od;
			M := PerfectBipartiteMatching( A, n );
			if M<>fail then # done, report results
				# build g
				for x in M do # x is an edge of M
					g[x[1]] := x[2];
				od;
				g := PermList( g );
				h := PermList( h );
				return [f,g,h];
			fi;		
		od;
	od;
	return fail;
end;

# testing
n := 12;
Q1 := ProjectionRightQuasigroup( n );
Sn := SymmetricGroup( n );
Q2 := RightQuasigroupIsotope( Q1, Random(Sn), Random(Sn), Random(Sn) );
ret := ItpRQ2(Q2,Q1);

RepStatRow := function( Q, x )
	local n, v, i, j;
	n := Size( Q );
	v := 0*[1..n];
	for i in [1..n] do
		j := ParentInd( x*Q.(i) );
		v[j] := v[j]+1;
	od;
	return SortedList( v );
end;

RepStat := function( Q )
	return List( Q, x -> RepStatRow( Q, x ) );	
end;

IncSelector := function( selector, blocks )
	local n, pos, i;
	n := Length( selector );
	if selector[1]=0 then 
		return List([1..n], i->1);
	fi;
	pos := First([1..n], i -> selector[n-i+1] < Length( blocks[n-i+1] ) );
	if pos=fail then
		return fail;
	fi;
	pos := n-pos+1;
	selector[pos]:=selector[pos]+1;
	for i in [pos+1..n] do
		selector[i]:=1;
	od;
	return selector;
end;

FirstConflict := function( selector, blocks )
	local n, used, i, z;
	n := Length( selector );
	used := 0*[1..n];
	for i in [1..n] do
		z := blocks[i][selector[i]];
		if used[z]=1 then # conflict
			return i;
		fi;
		used[z]:=1;
	od;
	return fail;
end;

NextCompatible := function( selector, blocks )
	local n, m, i;
	n := Length( selector );
	repeat 
		selector := IncSelector( selector, blocks );
		if selector=fail then 
			return fail;
		fi;
		m := FirstConflict( selector, blocks );
		if m = fail then # no conflict
			return selector;
		fi;
		for i in [m+1..n] do
			selector[i] := Length(blocks[i]);
		od;
	until 0<>0;
end;

ItpRQ3 := function( Q1, Q2 )
# using blocks of allowable values for every f[x]
# it speeds up things when the automorphism group of Q2 is large
# 1) try all f in the right Transversal of Aut(Q2) in Sym(|Q2|)
# 2) try all g[1]
# 3) this forces h
# 4) this restricts choices for g, solved as a Hall marriage problem
	local n, t1, t2, s1, s2, blocks, selector, f, g, h, e, x, S, y, A, M;
	n := Size( Q1 );
	t1 := MultiplicationTable( Q1 );
	t2 := MultiplicationTable( Q2 );
	s1 := RepStat( Q1 );
	s2 := RepStat( Q2 );
	# x and f(x) must have the same statistics (no matter what g and h do)
	blocks := List([1..n], i -> Filtered( [1..n], j -> s1[i] = s2[j] ) );
	if ForAny( blocks, b -> IsEmpty(b) ) then
		return fail;
	fi;
	selector := 0*[1..n]; # current selector from blocks
	repeat
		selector := NextCompatible( selector, blocks );
		if selector = fail then
			return fail;
		fi;
		f := PermList( List( [1..n], i->blocks[i][selector[i]] ) );
		g := 0*[1..n];
		h := 0*[1..n];	
		for e in [1..n] do
			g[1] := e;
			# build h
			for x in [1..n] do
				h[ t1[x][1] ] := t2[ x^f, e ];
			od;
			# try to complete g
			S := List([1..n], i -> [] ); # g(y) must lie in S[y];
			S[1] := [e];
			for y in [2..n] do
				S[y] := [1..n];
				for x in [1..n]	do
					S[y] := Filtered( S[y], z -> t2[x^f,z] = h[t1[x,y]] );
				od;
			od;
			# solve the marriage problem for g
			# build bipartite graph
			A := List([1..n], i->0*[1..n]);
			for x in [1..n] do for y in [1..n] do
				if y in S[x] then
					A[x][y] := 1;
				fi;	
			od; od;
			M := PerfectBipartiteMatching( A, n );
			if M<>fail then # done, report results
				# build g
				for x in M do # x is an edge of M
					g[x[1]] := x[2];
				od;
				g := PermList( g );
				h := PermList( h );
				return [f,g,h];
			fi;		
		od;
	until 0<>0;
end;





