# Extensions.gd
# Nuclear and central extensions
# =============================================================================

# chapter continues
#! @Chapter Nilpotency and solvability

# NUCLEAR AND CENTRAL EXTENSIONS OF LOOPS
# _____________________________________________________________________________

#! @Section Nuclear and central extensions of loops

#! <P/>A loop $Q$ is an **extension**<Index>extension</Index> of $K$ by $F$ if
#! $K$ is a normal subloop of $Q$ such that $Q/K$ is isomorphic to $F$.
#! If $K$ is in the nucleus of $Q$ then the extension is **nuclear**<Index Subkey="nuclear">extension</Index>,
#! while if $K$ is in the center of $Q$ then the enxtension is **central**<Index Subkey="central">extension</Index>.

#! <P/>If $K$, $F$ are loops then $\theta:F\times F\to K$ is a **loop cocycle**<Index>cocycle</Index>
#! if $\theta(x,1_F)=\theta(1_F,x)=1_K$ for all $x\in F$. In &RightQuasigroups;, a loop cocycle is represented
#! as an $|F|\times |F|$ table with entries in $[1..|K|]$.

#! <P/>Every nuclear extension of $K$ by $F$ with $K$ an abelian group is isomorphic to the loop
#! $(K\times F,\circ)$, where $(a,x)\circ(b,y) = (a\phi_x(b)\theta(x,y),xy)$ for some
#! homomorphism $\phi:F\to\mathrm{Aut}(K)$ and some loop cocycle $\theta:F\times F\to K$.

#! <P/>Every central extension of $K$ by $F$ is isomorphic to the loop
#! $(K\times F,\circ)$, where $(a,x)\circ(b,y) = (ab\theta(x,y),xy)$ for some loop
#! cocycle $\theta:F\times F\to K$. This loop will also be denoted by $K\times_\theta F$.

#! @Arguments K, F, theta
#! @Returns `true` if <Arg>theta</Arg> is a loop cocycle from <Arg>F</Arg> to <Arg>K</Arg>,
#! else returns `false`.
#! A loop cocycle is an $|F|\times |F|$ table with entries in $[1..|K|]$ such that all entries
#! in the row and in the column corresponding to the neutral element of $F$ are equal to
#! the neutral element of $K$.
DeclareOperation( "IsLoopCocycle", [ IsLoop, IsLoop, IsRectangularTable ] );
# there is also a version K, F, theta, reportErrors

#! @Arguments K, F, phi, theta[, constructorStyle]
#! @Returns the nuclear extension of the abelian group <Arg>K</Arg> by the loop <Arg>F</Arg>
#! via the homomorphism <Arg>phi</Arg> and cocycle <Arg>theta</Arg>. The arguments must be
#! formatted as follows:
#! <Arg>K</Arg> is a commutative and associative loop of size `nK`,
#! <Arg>F</Arg> is a loop of size `nF`,
#! <Arg>phi</Arg> is a list of length `nF` consisting of permutations of `[1..nK]` (each permutation
#! standing for an automorphism of <Arg>K</Arg>),
#! <Arg>theta</Arg> is an `nF` by `nF` matrix with entries in `[1..nK]`.
#! We also support giving the first four arguments as a list.
#! The loop is returned as index based loop with the underlying set equal to the carthesian product of
#! the underlying sets of `K` and `F`, and with multiplication 
#! `[a,x]*[b,y] = [a * b^phi[x] * theta[x,y], x*y ]`. 
DeclareOperation( "LoopByNuclearExtension", [ IsLoop, IsLoop, IsList, IsRectangularTable ] );

#! @Arguments K, F, theta[, constructorStyle]
#! @Returns the central extension of the abelian group <Arg>K</Arg> by the loop <Arg>F</Arg>
#! via the cocycle <Arg>theta</Arg>. The format of arguments and of the resulting loop are the same
#! as in `LoopByNuclearExtension`. We also support giving the first three arguments as a list.
DeclareOperation( "LoopByCentralExtension", [ IsLoop, IsLoop, IsRectangularTable ] );

#! @BeginExampleSession
#! gap> K := AsLoop( CyclicGroup( 4 ) );;
#! gap> F := LoopByCayleyTable( [ [ "a", "b" ], [ "b", "a" ] ] );;
#! gap> AutomorphismGroup( K );
#! Group([ (2,4) ])
#! gap> phi := [ (), (2,4) ];; # homomorphism from F to Aut( K )
#! gap> theta := [ [ 1, 1 ], [ 1, 4 ] ]; # loop cocycle from FxF to K
#! gap> IsLoopCocycle( K, F, theta );
#! true
#! gap> Q := LoopByNuclearExtension( K, F, phi, theta );
#! <loop of size 8>
#! gap> S := NucleusOfLoop( Q );
#! <associative loop of size 4>
#! gap> IsNormal( Q, S );
#! true
#! gap> IsomorphismLoops( F, Q/S );
#! ()
#! gap> Display( MultiplicationTable( Q ) );
#! [ [  1,  2,  3,  4,  5,  6,  7,  8 ],
#!   [  2,  3,  4,  1,  6,  7,  8,  5 ],
#!   [  3,  4,  1,  2,  7,  8,  5,  6 ],
#!   [  4,  1,  2,  3,  8,  5,  6,  7 ],
#!   [  5,  8,  7,  6,  4,  3,  2,  1 ],
#!   [  6,  5,  8,  7,  1,  4,  3,  2 ],
#!   [  7,  6,  5,  8,  2,  1,  4,  3 ],
#!   [  8,  7,  6,  5,  3,  2,  1,  4 ] ]
#! gap> Q.1; # the underlying set of Q is the carthesian product of underlying sets K x F
#! l[ <identity> of ..., "a" ]
#! gap> LoopByCentralExtension( K, F, theta );
#! <loop of size 8>
#! gap> Center( last );
#! <associative loop of size 8>
#! @EndExampleSession

#! @Arguments ls
#! @Returns If <Arg>ls</Arg> is a list of length `n^2` for some `n`, returns
#! <Arg>ls</Arg> as a square table, a list of lists of length `n`.
#! @Description Note: This function is convenient for transforming row vector cocycles obtained
#! by `LoopCocyclesInVariety` and by similar functions to square table cocycles required by the
#! `LoopByNuclearExtensions` function.
DeclareOperation( "AsSquareTable", [ IsList ] );

#! @Arguments Q, K
#! @Returns the parameters `[K,F,phi,theta]` for a nuclear extension of <Arg>K</Arg> by <Arg>Q</Arg>`/`<Arg>K</Arg>.
#! Here, <Arg>K</Arg> must be a commutative normal subloop of the nucleus of the loop <Arg>Q</Arg>.
#! The returned loop `K` is identical to the argument <Arg>K</Arg> and the returned factor loop `F` is identical
#! to <Arg>Q</Arg>`/`<Arg>K</Arg>.
DeclareOperation( "NuclearExtensionByNormalSubloop", [ IsLoop, IsLoop ] );

#! @Arguments Q, K
#! @Returns the parameters `[K,F,theta]` for a central extension of <Arg>K</Arg> by <Arg>Q</Arg>`/`<Arg>K</Arg>.
#! Here, <Arg>K</Arg> must be a subloop of the center of the loop <Arg>Q</Arg>.
#! The returned loop `K` is identical to the argument <Arg>K</Arg> and the returned factor loop `F` is identical
#! to <Arg>Q</Arg>`/`<Arg>K</Arg>.
DeclareOperation( "CentralExtensionByNormalSubloop", [ IsLoop, IsLoop ] );

#! @BeginExampleSession
#! gap> Q := MoufangLoop(32,3);;
#! gap> Nuc( Q ); # here, the nucleus is commutative and properly contains the center
#! <associative loop of size 4>
#! gap> Center( Q );
#! <associative loop of size 2>
#! gap> ext := NuclearExtensionByNormalSubloop( Q, Nuc( Q ) );
#! [ <associative loop of size 4>, <Moufang loop of size 8>, [ (), (), (), (2,4), (), (2,4), (2,4), (2,4) ],
#!   [ [ 1, 1, 1, 1, 1, 1, 1, 1 ], [ 1, 1, 1, 1, 1, 1, 3, 3 ], [ 1, 3, 1, 1, 3, 1, 1, 1 ], [ 1, 1, 1, 1, 3, 1, 1, 3 ],
#!       [ 1, 3, 1, 1, 3, 1, 3, 3 ], [ 1, 1, 1, 1, 3, 1, 3, 1 ], [ 1, 3, 1, 1, 1, 1, 1, 3 ], [ 1, 3, 1, 1, 1, 1, 3, 1 ]
#!      ] ]
#! gap> copyQ := LoopByNuclearExtension( ext[1],ext[2],ext[3],ext[4] );;
#! gap> IsomorphismLoops( Q, copyQ );
#! (4,9,11,28,29,24,22,21,20,25,27,12,13,8,6,5)(7,10)(14,30)(15,31)(16,32)(23,26)
#! gap> ext := CentralExtensionByNormalSubloop( Q, Center( Q ) );;
#! gap> copyQ := LoopByCentralExtension( ext );; # extension can also be given as a list
#! gap> IsomorphismLoops( Q, copyQ );
#! ()
#! @EndExampleSession

# CENTRAL EXTENSIONS IN A VARIETY
# _____________________________________________________________________________

#! @Section All central extensions in a variety of loops

#! <P/>If a loop $Q$ has a nontrivial center, it is a central extension of an abelian group $K$ of order $p$
#! by a loop $F$ of order $|Q|/p$. The following functions construct all central extensions of
#! the cyclic group of order $p$ by a given loop $F$ in a given variety of loops.

#! <P/>Just like all $p$-groups are centrally nilpotent, all $p$-loops in certain varieties
#! of loops (e.g., Moufang loops) are centrally nilpotent. The following functions therefore
#! allow us in principle to construct all $p$-loops of small orders in such varieties.

#! <P/>The enveloping variety is specified as a finite list of loop identities that the built-in
#! parser can understand (see Section <Ref Sect="Section_Parser"/>).

#! <P/>Given a variety $V$, an abelian group $K\in V$ and a loop $F\in V$, let $\mathrm{Coc}_V(F,K)$ be the
#! vector space of cocycles $\theta:F\times F\to K$ such that the central extension $K\times_\theta F$
#! is a loop in $V$. 

#! <P/>The vector space of coboundaries $\mathrm{Cob}_V(F,K)$ is generated by the cocycles
#! $\theta:F\times F\to (K,+)$ of the form $\theta(x,y) = f(xy)-f(x)-f(y)$, where
#! $f:F\to\mathbb K$ satisfies $f(1)=0$.

#! <P/>If $\theta,\mu\in\mathrm{Coc}_V(F,K)$ then the central extensions
#! $K\times_\theta F$ and $K\times_\mu F$ are isomorphic if $\theta$ and $\mu$ differ by a coboundary,
#! that is, $\theta-\mu\in\mathrm{Cob}_V(F,K)$.

#! <P/>Finally, the group $G=\mathrm{Aut}(K)\times\mathrm{Aut}(F)$ acts on
#! the cohomology group $\mathrm{Coc}_V(F,K)/\mathrm{Cob}_V(F,K)$ (details ommitted)
#! and two central extensions are isomorphic if their cocycles lie in the same orbit of $G$.

#! <P/>In &RightQuasigroups;, the elements of $\mathrm{Coc}_V(F,\mathbb Z_p)$
#! are represented as vectors of length $|F|^2$ with entries in $GF(p)$. (In particular, a cocycle
#! from $\mathrm{Coc}_V(F,\mathbb Z_p)$ must be transformed into a square matrix to be suitable as an argument
#! for `LoopByCentralExtension`.)

#! @Arguments F, p, equationalBasis
#! @Returns a basis of the vector space of cocycles $\mathrm{Coc}_V(F,\mathbb Z_p)$, where <Arg>F</Arg> is a loop,
#! <Arg>p</Arg> is a prime and $V$ is a variety of loops defined by the list of loop identities
#! <Arg>equationalBasis</Arg>. It is checked that all identities of <Arg>equationalBasis</Arg> hold in the
#! (multiplicative) cyclic group of order <Arg>p</Arg> and in the loop <Arg>F</Arg>, else en error message is generated.
DeclareOperation( "LoopCocyclesInVariety", [ IsLoop, IsPosInt, IsList ] );

#! @BeginExampleSession
#! gap> F := AsLoop( Group( (1,2), (3,4) ) );; # the Klein group
#! gap> coc := LoopCocyclesInVariety( F, 2, [ "x*(y*(x*z)) = (x*(y*x))*z" ] );; # basis of left Bol cocycles
#! gap> Length( coc ); # dimension of the vector space of cocycles
#! 6
#! gap> theta := coc[3];; Display( theta ); # one cocycle
#! [ 0*Z(2), 0*Z(2), 0*Z(2), 0*Z(2), 0*Z(2), 0*Z(2), 0*Z(2), 0*Z(2), 0*Z(2), Z(2)^0, Z(2)^0, 0*Z(2), 0*Z(2), 0*Z(2),
#! 0*Z(2), 0*Z(2) ]
#! gap> theta := List( theta, x -> IntFFE( x ) + 1 );; Display( theta ); # converting cocycle entries to [1..Size(K)]
#! [ 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 1, 1, 1, 1, 1 ]
#! gap> theta := AsSquareTable( theta );; Display( theta ); # converting cocycle to square table
#! [ [  1,  1,  1,  1 ],
#!   [  1,  1,  1,  1 ],
#!   [  1,  2,  2,  1 ],
#!   [  1,  1,  1,  1 ] ]
#! gap> Q := LoopByCentralExtension( AsLoop( CyclicGroup(2) ), F, theta );
#! <loop of size 8>
#! gap> IsLeftBolLoop( Q );
#! true
#! @EndExampleSession

#! @Arguments F, p
#! @Returns a basis of the vector space of coboundaries $\mathrm{Cob}_V(F,\mathbb Z_p)$, where <Arg>F</Arg> is a loop
#! and <Arg>p</Arg> is a prime. Note that $V$ is irrelevant here.
DeclareOperation( "LoopCoboundaries", [ IsLoop, IsPosInt ] );

#! @Arguments F, p, coc, cob
#! @Returns Given a loop <Arg>F</Arg>, prime <Arg>p</Arg>, basis <Arg>coc</Arg> of the vector space
#! $\mathrm{Coc}_V(F,\mathbb Z_p)$ and a basis <Arg>cob</Arg> of the vector space 
#! $\mathrm{Cob}_V(F,\mathbb Z_p)$, returns a list of cocycles from $\mathrm{Coc}_V(F,\mathbb Z_p)$,
#! one from each orbit of $\mathrm{Aut}(\mathbb Z_p)\times\mathrm{Aut}(F)$ acting on the cohomology
#! group $\mathrm{Coc}_V(F,\mathbb Z_p)/\mathrm{Cob}_V(F,\mathbb Z_p)$. Once the central extensions
#! corresponding to the cocycles are constructed, they are guaranteed to contain all central extensions of $\mathbb Z_p$ 
#! by $\mathbb F$ up to isomorphism (with possible duplications).
DeclareOperation( "LoopCocyclesModAction", [ IsLoop, IsPosInt, IsList, IsList ] );

#! @Arguments F, p, equationalBasis
#! @Returns Given a loop <Arg>F</Arg>, prime <Arg>p</Arg> and a list of loop identities 
#! <Arg>equationalBasis</Arg> defining a variety $V$, returns a list of cocycles from $\mathrm{Coc}_V(F,\mathbb Z_p)$
#! modulo coboundaries and modulo the action described in `LoopCocyclesModAction`.
DeclareOperation( "AllLoopCocyclesInVariety", [ IsLoop, IsPosInt, IsList ] );

#! @Arguments F, p, equationalBasis
#! @Returns a list of loops constructed as central extensions of the cyclic group of order <Arg>p</Arg>
#! by the loop <Arg>F</Arg> via the cocycles obtained by
#! `AllLoopCocyclesInVariety( `<Arg>F</Arg>`, `<Arg>p</Arg>`, `<Arg>equationalBasis</Arg>` )`.
#! The returned list is guaranteed to contain all central extensions of the cyclic group of order <Arg>p</Arg>
#! by the loop <Arg>F</Arg> in the variety of loops defined by the identities <Arg>equationalBasis</Arg>,
#! but it might contain duplicate loops of the same isomorphism type. (See `LoopsUpToIsomorphism`.)
DeclareOperation( "AllLoopCentralExtensionsInVariety", [ IsLoop, IsPosInt, IsList ] );

#! @BeginExampleSession
#! gap> F := AsLoop( Group( (1,2), (3,4 ) ) );; # Klein group
#! gap> coc := LoopCocyclesInVariety( F, 2, [ "x*(y*(x*z)) = (x*(y*x))*z" ] );; Length( coc ); # basis of left Bol cocycles
#! 6
#! gap> cob := LoopCoboundaries( F, 2 );; Length( cob ); # basis of coboundaries
#! 1
#! gap> Length( LoopCocyclesModAction( F, 2, coc, cob ) ); # cocycles modulo coboundaries and action of Aut(K)x Aut(F)
#! 10
#! gap> Length( AllLoopCocyclesInVariety( F, 2, [ "x*(y*(x*z)) = (x*(y*x))*z" ] ) ); # the same in one step
#! #I   - Calculating coboundaries
#! #I   - Coboundaries have dimension 1
#! #I   - Calculating cocycles
#! #I   - Cocycles have dimension 6
#! 10
#! gap> lps := AllLoopCentralExtensionsInVariety( F, 2, [ "x*(y*(x*z)) = (x*(y*x))*z" ] );; Length( lps ); # all central extensions in one step
#! #I   - Calculating coboundaries
#! #I   - Coboundaries have dimension 1
#! #I   - Calculating cocycles
#! #I   - Cocycles have dimension 6
#! 10
#! gap> ForAll( lps, IsLeftBolLoop );
#! true
#! gap> Length( LoopsUpToIsomorphism( lps ) ); # filtering up to isomorphism (no isomorphisms in this example)
#! 10
#! @EndExampleSession