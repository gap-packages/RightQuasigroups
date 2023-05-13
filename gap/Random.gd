# Random.gd
# Random right quasigroups, quasigroups and loops
# =============================================================================

# chapter continues
#! @Chapter Constructors

# RANDOM RIGHT QUASIGROUPS, QUASIGROUPS AND LOOPS
# _____________________________________________________________________________

#! @Section Random right quasigroups, quasigroups and loops

#! <P/>Random right quasigroups are easy to construct since the right translations
#! of individual elements are independent from each other. It therefore suffices
#! to construct their right section $(R_x:x\in Q)$ as a random tuple of permutations of $Q$.

#! <P/>Random quasigroups and loops are harder to construct. The Jacobson and Matthews
#! random walk method for random latin squares is implemented in &RightQuasigroups;
#! with some ad hoc mixing parameters.
#! We always start with the multiplication table of the cyclic group of order $n$ and perform
#! certain random local changes in the multiplication table. It is proved by Jacobson and Matthews
#! that the resulting random walk visits all latin squares uniformly. If a loop is desired, we normalize
#! the random latin square in the last step. 

#! @Arguments S[, constructorStyle ]
#! @Returns a random right quasigroup on the underlying set <Arg>S</Arg>. We also allow <Arg>S</Arg> to be a positive integer `n`, 
#! in which case the underlying set will be `[1..n]`.
#! @Description Note: The value of <Arg>constructorStyle</Arg>`.checkArguments` of the optional argument <Arg>constructorStyle</Arg>
#! does not come into play and need not be specified.
DeclareOperation( "RandomRightQuasigroup", [ IsCollection ] );

#! @BeginGroup
#! @GroupTitle Random quasigroups and loops

# RQ_RandomAlgebra( category, S, iter )
# auxiliary function
# Returns random quasigroup or loop on the underlying set S, using iter initial steps in the J&M algorithm.
# We also allow S to be the order instead of the underlying set.
DeclareOperation( "RQ_RandomAlgebra", [ IsOperation, IsCollection, IsPosInt ] );

#! @Arguments S[, iter]
#! @Returns a random quasigroup (loop) on the underlying set <Arg>S</Arg>, using <Arg>iter</Arg> number of
#! random steps to move into an initial position in the Jacobson and Matthews algorithm.
#! We also allow <Arg>S</Arg> to be a positive integer `n`, in which case the underlying set will be `[1..n]`.
#! If the optional argument <Arg>iter</Arg> is not given, its value will be set to $n^3$.
#! The resulting quasigroup or loop will be always index based (since the algorithm constructs
#! a multiplication table anyway).
DeclareOperation( "RandomQuasigroup", [ IsCollection ] );

#! @Arguments S[, iter]
DeclareOperation( "RandomLoop", [ IsCollection ] );

#! @BeginExampleSession
#! gap> RandomRightQuasigroup( 4 );
#! <right quasigroup of size 4>
#! gap> Elements( RandomQuasigroup( ["a","b","c"] ) );
#! [ qa, qb, qc ]
#! gap> RandomLoop( ["a","b","c"] );
#! <loop of size 3>
#! @EndExampleSession

#! @EndGroup

#! @Arguments lst 
#! @Returns a random nilpotent loop.
#! <Arg>lst</Arg> must be a list of positive integers and/or finite abelian groups.
#! If <Arg>lst</Arg>` = [a1,..,am]` and `a1` is an integer, returns a central extension
#! of a random abelian group of order `a1` by `RandomNilpotentLoop( [a2,...,am] )`.
#! If <Arg>lst</Arg>` = [a1,..,am]` and `a1` is an abelian group, returns a central extension
#! of `a1` by `RandomNilpotentLoop( [a2,...,am] )`.
#! @Description
#! To determine the nilpotency class `c` of the resulting loop, assume that
#! <Arg>lst</Arg> has length at least 2, contains only integers bigger than 1 (the "1" entries are trivial),
#! and let `m` be the last entry of <Arg>lst</Arg>. If `m>2` then `c=Length(`<Arg>lst</Arg>`)`,
#! else `c = Length(`<Arg>lst</Arg>`)-1`.
DeclareOperation( "RandomNilpotentLoop", [ IsList ] );