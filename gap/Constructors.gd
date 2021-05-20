# Constructors.gd
# Constructing right quasigroups
# =============================================================================

#! @Chapter Constructors

#! @Section Optional arguments in constructors

#! <P/>Almost all constructors in &RightQuasigroups; (exceptions will be noted) allow an optional argument
#! `constructorStyle`, a record that determines if the constructed right quasigroup will be index based
#! (cf. Section <Ref Sect="Section_IndexBased"/>) and if the arguments of the constuctor will be checked. 

#! <P/>The default value of this optional record is `RQ_defaultConstructorStyle := rec( indexBased := true, checkArguments := false )`,
#! which means that, unless otherwise specified by the user, all created right quasigroups will be index based
#! and no arguments will be checked.

#! <P/>The default constructor style for all constructors can be changed by calling `SetDefaultConstructorStyle`.
#! This should have no side effects for the code of &RightQuasigroups; since no particular value of
#! `RQ_defaultConstructorStyle` is assumed in the code.

#! <P/>The constructor style can also be overridden one constructor at a time by providing the constructor style
#! as an optional argument, either direcly, e.g., `rec( indexBased := false, checkArguments := false )`,
#! or by inserting the record returned by `ConstructorStyle`. Furthermore, in the former case
#! we allow the provided constructor style to be incomplete, that is, having only one (or none) of the
#! components `indexBased` and `checkArguments`; the missing components are assumed to have default values then.

#! <P/>**Note:** In regard to checking of arguments in right quasigroup constructors of &RightQuasigroups;, the optional
#! component `constructorStyle.checkArguments` replaces the standard naming mechanism of &GAP; in which `FunctionNC` does not check its
#! arguments while the corresponding `Function` does. Outside of those constructors, the `NC` convention is kept.

#! @Arguments indexBased, checkArguments
#! @Returns `true` and changes the default constructor style according to the bool values <Arg>indexBased</Arg> and <Arg>checkArguments</Arg>.
DeclareOperation( "SetDefaultConstructorStyle", [ IsBool, IsBool ] );

#! @Arguments indexBased, checkArguments
#! @Returns the record with elements named `indexBased` and `checkArguments` set to bool values <Arg>indexBased</Arg> and <Arg>checkArguments</Arg>,
#! respectively. The returned record can then be used as an optional argument in constructors.
DeclareOperation( "ConstructorStyle", [ IsBool, IsBool ] );

# auxiliary function
# completes a partial constructor style record
# returns a list of strings (names of componets) that needed to be completed
DeclareOperation( "RQ_CompleteConstructorStyle", [ IsRecord ] );

BindGlobal( "RQ_defaultConstructorStyle", rec( indexBased := true, checkArguments := false) );

# MATHEMATICAL TESTS OF CATEGORIES
# _____________________________________________________________________________

#! @Section Converting betwen right quasigroups, quasigroups and loops

#! <P/>While &GAP; does not automatically recognize a right quasigroup as a quasigroup or a loop (if it happens to 
#! be one mathematically), it is possible to test these properties and convert the algebras up and down as desired.

# RQ_IsBijectiveFunction( S, f )
# quickly checks if the function f is bijective on S
DeclareGlobalFunction( "RQ_IsBijectiveFunction" );

# MultiplicativeNeutralElement
# already declared as attribute for magmas

#! @BeginGroup
#! @GroupTitle IsRightQuasigroupMagma, IsLeftQuasigroupMagma, IsQuasigroupMagma and IsLoopMagma

#! @Arguments M
#! @Returns `true` if the magma <Arg>M</Arg> is a right quasigroup (resp. left quasigroup, quasigroup, loop)
#! mathematically, else returns `false`. 
DeclareProperty( "IsRightQuasigroupMagma", IsMagma );

#! @Arguments M
DeclareProperty( "IsLeftQuasigroupMagma", IsMagma );

#! @Arguments M
DeclareProperty( "IsQuasigroupMagma", IsMagma ); 

#! @Arguments M
DeclareProperty( "IsLoopMagma", IsMagma );

#! <P/>We also support `IsLatinMagma` as a synonym of `IsQuasigroupMagma` since the multiplication
#! table of quasigroups are Latin squares.

#! <P/>Note that an explicit check will be carried out by each of `IsRightQuasigroupMagma`,
#! `IsLeftQuasigroupMagma`, `IsQuasigroupMagma` and `IsLoopMagma` even if the underlying magma
#! is declared to be a right quasigroup, quasigroup, loop, group, etc.

#! To check only if a magma `M` contains a neutral element, it is also possible to call the &GAP; function
#! `MultiplicativeNeutralElement( M )`, which returns a neutral element of `M` if one exists
#! and `fail` otherwise.

#! @EndGroup

DeclareSynonymAttr( "IsLatinMagma", IsQuasigroupMagma ); 

# CONVERSIONS OF MAGMAS TO RIGHT QUASIGROUPS, QUASIGROUPS, LOOPS
# _____________________________________________________________________________

# RQ_AsAlgebra( category, M, style )
# returns M as an algebra in the appropriate category, if possible
DeclareOperation( "RQ_AsAlgebra", [ IsObject, IsDomain, IsRecord ] );
# REVISIT: a) It would be better to restrict the first argument more, but neither IsCategory nor IsFilter works. If a change is made, make it everywhere. 
# b) The second filter must accommodate IsMagma and IsAdditiveGroup.

#! @BeginGroup
#! @GroupTitle AsRightQuasigroup, AsQuasigroup and AsLoop

#! @Arguments M[, constructorStyle ]
DeclareOperation( "AsRightQuasigroup", [ IsDomain ] ); 

#! @Arguments M[, constructorStyle ] 
DeclareOperation( "AsQuasigroup", [ IsDomain ] );

#! @Arguments M[, constructorStyle ]
#! @Returns the right quasigroup, quasigroup or loop, respectively, corresponding
#! to the magma or additive group <Arg>M</Arg> that is mathematically a right quasigroup, quasigroup or loop,
#! else returns `fail`. If <Arg>M</Arg> is an additive group, the additive operation is used
#! as multiplication.
#! See Section <Ref Sect="Section_OptionalArguments"/> for the optional argument <Arg>constructorStyle</Arg>.
DeclareOperation( "AsLoop", [ IsDomain ]);

#! @BeginExampleSession
#! gap> M := MagmaByMultiplicationTable( [ [1,1], [2,2] ] );;
#! gap> Elements( M );
#! [ m1, m2 ]
#! gap> IsRightQuasigroupMagma( M );
#! true
#! gap> R := AsRightQuasigroup( M );
#! <right quasigroup of size 2>
#! gap> Elements( R );
#! [ rm1, rm2 ]
#! gap> IsQuasigroupMagma( R ); # synonym of IsLatinMagma
#! false
#! gap> AsQuasigroup( M ); # arguments of constructors are by default not checked!
#! <quasigroup of size 2>
#! gap> AsQuasigroup( M, rec( checkArguments := true ) ); # the optional argument will force a check
#! fail
#! gap> G := Group((1,2,3));; IsLoopMagma( G );
#! true
#! gap> L := AsLoop( G ); # multiplicative group
#! <associative loop of size 3>
#! gap> Elements( L );
#! [ l(), l(1,2,3), l(1,3,2) ]
#! gap> AsRightQuasigroup(GF(7)^2); # additive group
#! <associative right quasigroup of size 49>
#! @EndExampleSession

#! @EndGroup

# CONSTRUCTOR OF SHELL ALGEBRA
# _____________________________________________________________________________

#! @Section *A two-step constructor

#! <P/>Most constructors of &RightQuasigroups; work in two steps. In the first step, a shell of an algebra is created,
#! with the underlying set and elements stored but with no multiplication function assigned.
#! In the second step, the multiplication function is added, depending
#! on the data provided by the user in the constructor.

#! <P/>The reason for this approach, which is particularly useful for non-index based right quasigroups,
#! is that the multiplication function might depend on 
#! data that needs to be stored in the right quasigroup and be accessible. The first
#! step creates the desired &GAP; object so that the multiplication function added
#! in the second step can point to the stored data.

#! @Arguments category, S[, constructorStyle]
#! @Returns an algebra shell of the appropriate category on the underlying set <Arg>S</Arg>.
#! The value of <Arg>category</Arg> must be `IsRightQuasigroup`, `IsQuasigroup` or `IsLoop`.
#! @Description Note: The value of <Arg>constructorStyle</Arg>`.checkArguments` of the optional argument <Arg>constructorStyle</Arg>
#! does not come into play and need not be specified.
DeclareOperation( "RQ_AlgebraShell", [ IsObject, IsCollection ] ); 

#! @Arguments Q 
#! @Returns `true`
#! @Description This method adds all required unary and binary operations to the right quasigroup shell <Arg>Q</Arg>.
#! The implied right division is always added. The implied left division is added if <Arg>Q</Arg> is a declared quasigroup.
#! The implied neutral element is added if <Arg>Q</Arg> is a declared loop.
#! Let `F = FamilyObj( `<Arg>Q</Arg>`.1 )`. If <Arg>Q</Arg> is index based, it is assumed that `F!.multTable` is bound.
#! If <Arg>Q</Arg> is not index based, it is assumed that `F!.mult` is bound.
DeclareOperation( "RQ_AddDefaultOperations", [ IsRightQuasigroup ] );

#! <P/>The following simple examples illustrates the approach, first for non-index based algebras

#! @BeginExampleSession
#! gap> Q := RQ_AlgebraShell( IsRightQuasigroup, GF(5), rec( indexBased := false ) );
#! <right quasigroup shell of size 5>
#! gap> F := FamilyObj( Q.1 );;
#! gap> F!.mult := function(x,y) return x+y; end;;
#! gap> Q; # still a shell since right division is not bound
#! <right quasigroup shell of size 5> 
#! gap> Q.1*Q.1; # multiplication already works since Q is not index based and F!.mult is bound
#! r0*Z(5)
#! gap> RQ_AddDefaultOperations( Q );
#! true
#! gap> Q; # all needed operations for the right quasigroup are now bound
#! <right quasigroup of size 5>
#! gap> Q.1/Q.1;
#! r0*Z(5)
#! @EndExampleSession

#! <P/>and now for index based algebras:

#! @BeginExampleSession
#! gap> Q := RQ_AlgebraShell( IsRightQuasigroup, GF(5) );
#! <right quasigroup shell of size 5>
#! gap> F := FamilyObj( Q.1 );
#! gap> F!.multTable := List( GF(5), x-> List( GF(5), y -> Position( Elements(GF(5)), x+y ) ) );;
#! gap> Q; # still a shell since right division is not bound
#! <right quasigroup shell of size 5> 
#! gap> Q.1*Q.1; # multiplication already works since Q is index based and F!.multTable is bound
#! r0*Z(5)
#! gap> RQ_AddDefaultOperations( Q );
#! true
#! gap> Q; # all needed operations for the right quasigroup are now bound
#! <right quasigroup of size 5>
#! gap> Q.1/Q.1;
#! r0*Z(5)
#! @EndExampleSession

# CONSTRUCTORS BY CAYLEY TABLE
# _____________________________________________________________________________

#! @Section Constructors by Cayley table

#! @Subsection Cayley tables versus multiplication tables

#! <P/>Given a right quasigroup `Q` of size $n$ with underlying set $S$, the <Index>Cayley table</Index>
#! **Cayley table** of `Q` is the $n\times n$ array with rows and columns
#! implictly labeled by the elements of (the sorted list) $S$
#! such that the cell in row $x$ and column $y$ contains the element $xy\in S$.

#! <P/>In accordance with &GAP; conventions, given a magma `M` of size $n$ with $m_i$ denoting the $i$ith element of `M`,
#! the <Index>multiplication table</Index>**multiplication table** of `Q`
#! is the $n\times n$ array with rows and columns implicitly labeled by $[1..n]$ such that
#! the cell in row $i$ and column $j$ contains $k$ iff $m_im_j=m_k$.

#! <P/>It is also possible to form an $n\times n$ array with rows and columns implicitly labeled by &GAP; elements
#! of `Q` so that the cell in row `x` and column `y` contains the &GAP; element `x*y`. Such a table can be quickly
#! constructed from the Cayley table `ct` of `Q` by `List( ct, row -> List( row, x -> Q[x] ) )`, for instance.
#! We do not introduce terminology for such tables, nor do we provide any methods for dealing with them.

#! <P/>Analogous conventions apply to right division and left division.

#! @BeginGroup
#! @GroupTitle Cayley tables and multiplication tables of right quasigroups

# PROG: We do not store Cayley tables are as attributes since
# a) they are typically cheap to calculate, and
# b) they would have to be recalculated when the underlying set is changed by the user.

#! @Arguments Q
#! @Returns the multiplication table, Cayley table, right division table,
#! right division Cayley table, left division table and left division Cayley table of right quasigroup <Arg>Q</Arg>,
#! respectively. For the left divisions, <Arg>Q</Arg> must be a declared quasigroup.
#! @Description Note that the Cayley tables are not stored as attributes since they are cheap to 
#! calculate and they are affected by a possible change of the underlying set of <Arg>Q</Arg>.
DeclareAttribute( "MultiplicationTable", IsRightQuasigroup );

#! @Arguments Q
DeclareOperation( "CayleyTable", [ IsRightQuasigroup ] );

#! @Arguments Q
DeclareAttribute( "RightDivisionTable", IsRightQuasigroup );

#! @Arguments Q
DeclareOperation( "RightDivisionCayleyTable", [ IsRightQuasigroup ] ); 

#! @Arguments Q
DeclareAttribute( "LeftDivisionTable", IsQuasigroup );

#! @Arguments Q
DeclareOperation( "LeftDivisionCayleyTable", [ IsQuasigroup ] ); 

#! @EndGroup

# Functions that quickly construct division tables from a multiplication table.

#! @BeginGroup
#! @GroupTitle Constructing division tables from multiplication tables

#! @Arguments Q
DeclareOperation( "RightDivisionTableFromMultiplicationTable", [ IsRectangularTable ] );

#! @Arguments Q
DeclareOperation( "LeftDivisionTableFromMultiplicationTable", [ IsRectangularTable ] );

#! <P/>It is possible to construct a right division table from the multiplication table
#! of a right quasigroup <Arg>Q</Arg> of size $n$ in $O(n^2)$, rather than naively in $O(n^3)$.
#! The fast algorithm is used in the above two methods which 
#! return the right division table and the left division table of a right quasigroup <Arg>Q</Arg>, respectively.
#! For the left division, <Arg>Q</Arg> must be a declared quasigroup.

#! @EndGroup

#! @BeginGroup
#! @GroupTitle Testing Cayley tables

# RQ_IsAlgebraCayleyTable( category, ct, reportErrors )
# returns true if <Arg>ct</Arg> is a Cayley table for an algebra in the given category
# The underlying set it understood to be the ordered set of elements contained in the firts column of <Arg>ct</Arg>.
DeclareOperation( "RQ_IsAlgebraCayleyTable", [ IsObject, IsRectangularTable, IsBool ] );

#! @Arguments ct
DeclareOperation( "IsRightQuasigroupCayleyTable", [ IsRectangularTable ] );

#! @Arguments ct
DeclareOperation( "IsQuasigroupCayleyTable", [ IsRectangularTable ] );

#! @Arguments ct
#! @Returns `true` if <Arg>ct</Arg> is a Cayley table of a right quasigroup, quasigroup or loop, respectively,
#! otherwise returns `false`.
#! The underlying set it understood to be the ordered set of elements contained in the first column of <Arg>ct</Arg>.
#! This is a logical choice for right quasigroups.
DeclareOperation( "IsLoopCayleyTable", [ IsRectangularTable ] );

#! @EndGroup

#! @BeginGroup
#! @GroupTitle Normalizing Cayley tables

#! <P/>A right quasigroup Cayley table is <Index>normalized right quasigroup Cayley table</Index>**normalized**
#! if the entries in the first column are sorted. Every right quasigroup Cayley table can be normalized by
#! permuting its rows.

#! <P/>A quasigroup Cayley table is <Index>normalized quasigroup Cayley table</Index>**normalized**
#! if the entires in the first column and the entries in the first row are sorted. 
#! Every right quasigroup Cayley table can be normalized by permuting its rows and columns.

#! @Arguments ct
#! @Returns a normalized version of the (right) quasigroup Cayley table <Arg>ct</Arg>.
DeclareOperation( "NormalizedRightQuasigroupCayleyTable", [ IsRectangularTable ] );

#! @Arguments ct
DeclareOperation( "NormalizedQuasigroupCayleyTable", [ IsRectangularTable ] );

#! @BeginExampleSession
#! gap> ct := [[1,1],[0,0]];;
#! gap> NormalizedRightQuasigroupCayleyTable( ct );
#! [ [ 0, 0 ], [ 1, 1 ] ]
#! gap> ct := [["b","a"],["a","b"]];;
#! gap> NormalizedQuasigroupCayleyTable( ct );
#! [ [ "a", "b" ], [ "b", "a" ] ]
#! @EndExampleSession

#! @EndGroup

# constructors based on Cayley table

#! @BeginGroup
#! @GroupTitle Creating right quasigroups by Cayley table

DeclareOperation( "RQ_AlgebraByCayleyTable", [ IsObject, IsRectangularTable, IsRecord ] ); # category, ct, constructorStyle

#! @Arguments ct[, constructorStyle]
DeclareOperation( "RightQuasigroupByCayleyTable", [ IsRectangularTable ] );

#! @Arguments ct[, constructorStyle]
DeclareOperation( "QuasigroupByCayleyTable", [ IsRectangularTable ] );

#! @Arguments ct[, constructorStyle]
#! @Returns the right quasigroup, quasigroup or loop with Cayley table <Arg>ct</Arg>, respectively.
#! See Section <Ref Sect="Section_OptionalArguments"/> for the optional argument <Arg>constructorStyle</Arg>.
DeclareOperation( "LoopByCayleyTable", [ IsRectangularTable ] );

#! @BeginExampleSession
#! gap> ct := [ [ "red", "white", "white" ], [ "blue", "blue", "red" ], [ "white", "red", "blue" ] ];;
#! gap> IsRightQuasigroupCayleyTable( ct );
#! true
#! gap> Q := RightQuasigroupByCayleyTable( ct );
#! <right quasigroup of size 3>
#! gap> Display( MultiplicationTable( Q ) ); # note the ordering of elements, with rows and columns implicitly labeled 1, 2, 3
#! [ [  2,  3,  3 ],
#!   [  1,  1,  2 ],
#!   [  3,  2,  1 ] ]
#! gap> Display( CayleyTable( Q ) );
#! [ [ "red", "white", "white" ],
#!   [ "blue", "blue", "red" ],
#!   [ "white", "red", "blue" ] ]
#! gap> Elements( Q );
#! [ rblue, rred, rwhite ]
#! gap> IsQuasigroupCayleyTable( ct );
#! false
#! gap> ct2 := [ [0,1],[1,0] ];;
#! gap> [ IsQuasigroupCayleyTable(ct2), IsLoopCayleyTable(ct2) ];
#! [ true, true ]
#! gap> QuasigroupByCayleyTable(ct2);
#! <quasigroup of size 2>
#! gap> LoopByCayleyTable(ct2);
#! <loop of size 2>
#! gap> One(last);
#! l0
#! @EndExampleSession

#! @EndGroup

# CONSTRUCTORS BY FUNCTION
# _____________________________________________________________________________

# For algebras created by functions (multiplication, right division [, left division, one]).

#! @Section Constructors by function

#! <P/>In this section we describe how to construct right quasigroups, quasigroups and loops by functions.

#! <P/>To fix terminology, we say that a function $f:S\times S\to S$ is a <Index>right quasigroup function</Index>
#! **right quasigroup function** if for every $x\in S$ the function $R_x:S\to S$ defined by
#! $R_x(y) = f(y,x)$ is a permutation of $S$. A right quasigroup function $f:S\times S\to S$ is
#! a <Index>quasigroup function</Index>**quasigroup function** if for every $x\in S$ the function
#! $L_x:S\to S$ defined by $L_x(y) = f(x,y)$ is a permutation of $S$. Finally, a quasigroup function
#! $f:S\times S\to S$ is a <Index>loop function</Index>**loop function** if there is $e\in S$ such that
#! $f(e,x)=x=f(x,e)$ for every $x\in S$.

#! @Subsection Arguments used in constructors by function
#! @SubsectionLabel ConstructorArguments

#! <P/>When constructing right quasigroups, quasigroups and loops by function(s), &RightQuasigroups; assumes
#! that the arguments are given as follows:

#! <List>
#! <Item>The first argument is the underlying set `S` of the algebra.</Item>
#! <Item>The second argument is the multiplication function.</Item>
#! <Item>Optional arguments that lie in the filter `IsFunction` correspond to right division
#! and left division, in this order. (In particular, it is not possible to specify left division
#! without first specifying right division.)</Item>
#! <Item>The first optional argument that lies in `S` is the neutral element.</Item>
#! <Item>If the last optional argument is a record, it is the constructor style, else
#! the default constructor style will be used.</Item>
#! </List>

#! <P/>If the optional right division function is not given, it will be automatically inferred from
#! the multiplication function. In case of quasigroups an loops,
#! if the optional left division function is not given, it will be automatically inferred from
#! the multiplication function.
#! Likewise, in case of loops, if the optional neutral element is not given, it will be automatically inferred
#! from the multiplication function.

#! <P/>Note that providing the division functions as arguments of non-index based algebras
#! will typically make the division operations faster than by relying on the inferred divisions.
#! For instance, if $f(x,y)=x+y$ is the multiplication function and the user 
#! also provides the right division function $g(x,y) = x-y$, the right division will be performed faster
#! than the default right division that searches for the first element $z$ such that $x=f(z,y)$.

#! <P/>Similar conventions hold for the methods that check whether given function(s) constitute 
#! right quasigroup, quasigroup or loop function(s), except that the constructor style 
#! is void and the missing operations are not automatically inferred from the multiplication.

# RQ_AreAlgebraFunctions( category, S, mult, ops, reportErrors ) 
# checks if the operations are valid for the given category on S
# ops can be any sublist of [rdiv,ldiv,one]
DeclareOperation( "RQ_AreAlgebraFunctions", [ IsObject, IsCollection, IsFunction, IsList, IsBool ] );

#! @Arguments S, mult[, rdiv]
#! @Returns `true` if <Arg>mult</Arg> is a right quasigroup function on <Arg>S</Arg>, else returns `false`.
#! (See Subsection <Ref Subsect="Subsection_ConstructorArguments"/> for how the optional arguments are treated.)
#! If <Arg>rdiv</Arg> is given, it is checked that it is the right division on <Arg>S</Arg> corresponding
#! to the multiplication <Arg>mult</Arg>.
DeclareGlobalFunction( "IsRightQuasigroupFunction" );

#! @Arguments S, mult[, rdiv, ldiv]
#! @Returns `true` if <Arg>mult</Arg> is a quasigroup function on <Arg>S</Arg>, else returns `false`.
#! (See Subsection <Ref Subsect="Subsection_ConstructorArguments"/> for how the optional arguments are treated.) 
#! If <Arg>rdiv</Arg> is given, it is checked that it is the right division
#! on <Arg>S</Arg> corresponding to the multiplication <Arg>mult</Arg>. If <Arg>ldiv</Arg> is given,
#! it is checked that it is the left division on <Arg>S</Arg> corresponding to the multiplication <Arg>mult</Arg>.
DeclareGlobalFunction( "IsQuasigroupFunction");

#! @BeginExampleSession
#! gap> mult := function( x, y ) return (x+2*y) mod 4; end;
#! function( x, y ) ... end
#! gap> IsRightQuasigroupFunction( [0..3], mult );
#! true
#! gap> IsQuasigroupFunction( [0..3], mult );
#! false
#! @EndExampleSession

#! @Arguments S, mult[, rdiv, ldiv, one]
#! @Returns `true` if <Arg>mult</Arg> is a loop function on <Arg>S</Arg>, else returns `false`.
#! (See Subsection <Ref Subsect="Subsection_ConstructorArguments"/> for how the optional arguments are treated.) 
#! If <Arg>rdiv</Arg> is given, it is checked that it is the right division
#! on <Arg>S</Arg> corresponding to the multiplication <Arg>mult</Arg>. If <Arg>ldiv</Arg> is given,
#! it is checked that it is the left division on <Arg>S</Arg> corresponding to the multiplication <Arg>mult</Arg>.
#! If <Arg>one</Arg> is given, it is checked that it is is the neutral element with respect to <Arg>mult</Arg>.
DeclareGlobalFunction( "IsLoopFunction" );

#! @BeginExampleSession
#! gap> mult := \+;;
#! gap> rdiv := \-;;
#! gap> ldiv := function( x,y ) return -x+y; end;;
#! gap> one := Zero( GF(5) );;
#! gap> IsLoopFunction( GF(5), mult ); 
#! true
#! gap> IsLoopFunction( GF(5), mult, rdiv );
#! true
#! gap> IsLoopFunction( GF(5), mult, rdiv, ldiv );
#! true
#! gap> IsLoopFunction( GF(5), mult, one );
#! true
#! gap> IsLoopFunction( GF(5), mult, rdiv, one );
#! true
#! gap> IsLoopFunction( GF(5), mult, rdiv, ldiv, one );
#! true
#! gap> IsLoopFunction( GF(5), mult, ldiv ); # returns false because right division is expected before left division
#! false
#! @EndExampleSession

#! @BeginGroup ThreeOperations
#! @GroupTitle Multiplication, right division and left division functions

#! @Arguments Q
DeclareOperation( "MultiplicationFunction", [ IsRightQuasigroup] );

#! @Arguments Q
DeclareOperation( "RightDivisionFunction", [ IsRightQuasigroup ] );

#! @Arguments Q
#! @Description Returns, respectively, the multiplication function, right division function and left division function
#! of right quasigroup <Arg>Q</Arg> as a &GAP; function. For the left division, <Arg>Q</Arg> must be a declared
#! quasigroup. If the requested function is not bound, returns `fail`. See Section <Ref Sect="Section_IndexBased"/> for 
#! more details on multiplication and division functions in index based and non-index based right quasigroups.
DeclareOperation( "LeftDivisionFunction", [ IsQuasigroup ] );

#! @EndGroup

# RQ_AlgebraByFunction( category, S, mult, rest )
# returns algebra in the appropriate category based on the specified (subset of) operations
# rest is any subset of [rdiv,ldiv,one,style]
DeclareOperation( "RQ_AlgebraByFunction", [ IsObject, IsCollection, IsFunction, IsList ] );

#! @Arguments S, mult[, rdiv][, constructorStyle]
#! @Returns a right quasigroup with underlying set <Arg>S</Arg> and multiplication function <Arg>mult</Arg>.
#! If <Arg>rdiv</Arg> is given, it becomes the right division function, else the right division function
#! is automatically inferred from <Arg>mult</Arg>. 
#! See Subsection <Ref Subsect="Subsection_ConstructorArguments"/> for the optional argument <Arg>constructorStyle</Arg>.
DeclareGlobalFunction( "RightQuasigroupByFunction" ); 

#! @Arguments S, mult[, rdiv, ldiv][, constructorStyle]
#! @Returns a quasigroup with underlying set <Arg>S</Arg> and multiplication function <Arg>mult</Arg>.
#! If <Arg>rdiv</Arg> (resp. <Arg>ldiv</Arg>) is given, it is set as the right (resp. left) division function,
#! else the right (resp. left) division function is automatically inferred from <Arg>mult</Arg>.
#! See Subsection <Ref Subsect="Subsection_ConstructorArguments"/> for the optional argument <Arg>constructorStyle</Arg>.
DeclareGlobalFunction( "QuasigroupByFunction" ); 

#! @BeginExampleSession
#! gap> S := GF(5);;
#! gap> mult := function( x, y ) return x+2*y; end;;
#! gap> rdiv := function( x, y ) return x-2*y; end;;
#! gap> ldiv := function( x, y ) return (y-x)/2; end;;
#! gap> IsQuasigroupFunction( S, mult, rdiv, ldiv );
#! true
#! gap> QuasigroupByFunction( S, mult, rdiv, ldiv );
#! <quasigroup of size 5>
#! @EndExampleSession

#! @Arguments S, mult[, rdiv, ldiv, one][, constructorStyle]
#! @Returns a loop with underlying set <Arg>S</Arg> and multiplication function <Arg>mult</Arg>.
#! If <Arg>rdiv</Arg> (resp. <Arg>ldiv</Arg>) is given, it is set as the right (resp. left) division function,
#! else the right (resp. left) division function is automatically inferred from <Arg>mult</Arg>. 
#! If <Arg>one</Arg> is given, it is set as the neutral element, else it is automatically inferred from <Arg>mult</Arg>.
#! See Subsection <Ref Subsect="Subsection_ConstructorArguments"/> for the optional argument <Arg>constructorStyle</Arg>.
DeclareGlobalFunction( "LoopByFunction" ); 

#! <P/>The following constructor produces a loop of size just over 1 million near instantaneously since nothing is checked.

#! @BeginExampleSession
#! gap> p := 1000003;;
#! gap> S := Difference( GF(p), [ Zero( GF( p ) ) ] );; # nonzero elements of GF(p)
#! gap> Q := LoopByFunction( S, \*, ConstructorStyle( false, false ) ); # not index based, no arguments checked
#! <loop of size 1000002>
#! gap> One( Q );
#! lZmodpZObj( 1, 1000003 )
#! gap> Q.10*Q.100;
#! lZmodpZObj( 1000, 1000003 )
#! gap> Q.100/Q.10;
#! lZmodpZObj( 10, 1000003 )
#! gap> Inverse( Q.10 );
#! lZmodpZObj( 300001, 1000003 )
#! @EndExampleSession

# CONSTRUCTORS BY RIGHT SECTION
# _____________________________________________________________________________

#! @Section Constructors by right section

#! <P/>For a right quasigroup $(Q,\cdot)$, the <Index>right section</Index>**right section**
#! is the ordered tuple $R_Q = (R_x:x\in Q)$ of right translations of $Q$. Since $xy = R_y(x)$,
#! the multiplication in $(Q,\cdot)$ can be fully reconstructed from its right section.

#! <P/>A right section $(Q,R_Q)$ is a <Index>quasigroup right section</Index>
#! **quasigroup right section** (resp. <Index>loop right section</Index>
#! ** loop right section**) if the corresponding right quasigroup is a quasigroup (resp. loop).

#! <P/>In &RightQuasigroups;, a right section consists of the underlying set $S$ of size $n$
#! and an ordered collection of $n$ group elements that act on $S$. Implicit
#! right sections are also allowed, that is, a set $S$ of size $n$ and an ordered
#! collection of permutations on $[1..n]$, in which case $x^f=y$ iff $x$ is the $i$th 
#! element of $S$, $i^f=j$ and $y$ is the $j$th element of $S$.

# RQ_IsAlgebraRightSection( category, S, section, reportErrors )
# checks if section on S is a right section for the given category
# Accepts two kinds of sections:
# - if section is any set of group elements acting on S, we use that action
# - if section is a set of permutations, we implicitly act on the ordered set S
DeclareOperation( "RQ_IsAlgebraRightSection", [ IsObject, IsCollection, IsCollection, IsBool ] );

#! @BeginGroup
#! @GroupTitle Testing right sections

#! @Arguments [S,] section
DeclareOperation( "IsRightSection", [ IsCollection, IsCollection ] );

#! @Arguments [S,] section
DeclareOperation( "IsQuasigroupRightSection", [ IsCollection, IsCollection ] );

#! @Arguments [S,] section
#! @Returns `true` if <Arg>section</Arg> is a right section (resp. quasigroup right section,
#! loop right section) on <Arg>S</Arg>, else returns `false`. If <Arg>S</Arg> is omitted, <Arg>section</Arg> must be
#! a list of permutations; the underlying set is then understood to be `[1..Length(`<Arg>section</Arg>`)]`.
DeclareOperation( "IsLoopRightSection", [ IsCollection, IsCollection ] );

#! @EndGroup

#! @BeginGroup
#! @GroupTitle Constructing right quasigroups, quasigroups and loops by right section

# RQ_AlgebraByRightSection( category, S, section, style )
# returns the corresponding algebra by right section
DeclareOperation( "RQ_AlgebraByRightSection", [ IsObject, IsCollection, IsCollection, IsRecord ] );

#! @Arguments [S,] section[, constructorStyle]
DeclareOperation( "RightQuasigroupByRightSection", [ IsCollection, IsCollection ] );

#! @Arguments [S,] section[, constructorStyle]
DeclareOperation( "QuasigroupByRightSection", [ IsCollection, IsCollection ] );

#! @Arguments [S,] section[, constructorStyle]
#! @Returns a right quasigroup, quasigroup or loop on <Arg>S</Arg> with right section <Arg>section</Arg>, respectively.
#! If <Arg>S</Arg> is omitted, <Arg>section</Arg> must be a list of permutations; the underlying set is then understood
#! to be `[1..Length(`<Arg>section</Arg>`)]`.
#! See Section <Ref Sect="Section_OptionalArguments"/> for the optional argument <Arg>constructorStyle</Arg>.
DeclareOperation( "LoopByRightSection", [ IsCollection, IsCollection ] );

#! <P/>Here is an example of a right quasigroup constructed by a right section consisting of permutations.

#! @BeginExampleSession
#! gap> section := [ (), (1,2,3,4), (), (1,3)(2,4) ];;
#! gap> IsRightSection( [1..4], section );
#! true
#! gap> IsRightSection( section ); # the underlying set can be omitted
#! false
#! gap> IsRightSection( "abcd", section ); # implicit permutation action on the set ['a','b','c','d']
#! true
#! gap> IsQuasigroupRightSection( [1..4], section );
#! false
#! gap> Q := RightQuasigroupByRightSection( section ); # the underlying set can be omitted
#! <right quasigroup of size 4>
#! gap> Q[3]*Q[2];
#! r4
#! gap> RightQuasigroupByRightSection( "abcd", section );
#! <right quasigroup of size 4>
#! gap> Elements( last );
#! [ r'a', r'b', r'c', r'd' ]
#! @EndExampleSession

#! @EndGroup

#! <P/>Here is an example of a right quasigroup constructed by a right section consisting of
#! group elements acting on the underlying set, in this case of matrices acting on `GF(2)^2`.

#! @BeginExampleSession
#! gap> i := Z(2)*[[1,0],[0,1]];;
#! gap> m := Z(2)*[[1,1],[0,1]];;
#! gap> section := [m,m,i,i];;
#! gap> IsRightSection( GF(2)^2, section );
#! true
#! gap> Q := RightQuasigroupByRightSection( GF(2)^2, section );
#! <right quasigroup of size 4>
#! gap> Q.1;
#! r[ 0*Z(2), 0*Z(2) ]
#! @EndExampleSession

#! <P/>We conclude with an example of a loop constructed by a right section.
#! Note that the neutral element is not the first element of the underlying set.

#! @BeginExampleSession
#! gap> section := [(1,2),()];;
#! gap> IsQuasigroupRightSection( [1,2], section );
#! true
#! gap> QuasigroupByRightSection( [1,2], section );
#! <right quasigroup of size 2>
#! gap> IsLoopRightSection( [1,2], section );
#! true
#! gap> LoopByRightSection( [1,2], section );
#! <loop of size 2>
#! gap> One( last );
#! l2
#! @EndExampleSession

# CONSTRUCTORS BY RIGHT FOLDER
# _____________________________________________________________________________

#! @Section Constructors by right folder (transversal)

#! <P/>The triple $(G,H,T)$ is a <Index>right folder</Index>**right folder**
#! if $G$ is a group, $H$ is a subgroup of $G$ and $T$ is a right transversal to $H$ in $G$.

#! <P/>Given a right folder $(G,H,T)$, we can define a magma $(T,\circ)$
#! by setting $x\circ y = z$ iff $xy\in Hz$, for $x,y,z\in T$. Then $(T,\circ)$
#! is always a right quasigroup. Not every right quasigroup is obtained in this way.

#! <P/>The right folder $(G,H,T)$ is a <Index>quasigroup right folder</Index>**quasigroup
#! right folder** (resp. <Index>loop right folder</Index>**loop right folder**) if
#! $(T,\circ)$ is a quasigroup (resp. loop). Group-theoretically, $(G,H,T)$ is a quasigroup
#! right folder iff $T$ is a right transversal to every conjugate $H^g$ of $H$ in $G$. Furthermore,
#! a quasigroup right folder $(G,H,T)$ is a loop right folder iff $T$ intersects the
#! core of $H$ in $G$, i.e., the normal subgroup $\bigcap_{g\in G}H^g$.

#! <P/>Up to isomorphism, every quasigroup (resp. loop) is obtained as $(T,\circ)$ for some
#! quasigroup (resp. loop) folder $(G,H,T)$.

#! <P/>In functions that require right folders, we allow two types of arguments:
#! <List>
#! <Item>a single argument which is a &GAP; right transversal `T` (in which case the group and subgroup
#! are implicit as `G=T!.group` and `H=T!.subgroup`),</Item>
#! <Item>three arguments `G`, `H`, `T` standing for the right folder `(G,H,T)`, 
#! where `T` can be a subset of `G` or a &GAP; right transversal to `H` in `G`.</Item>
#! </List>
#! The reason for the second options is that while &GAP; calculates a right transversal to `H` in `G` via
#! `RightTransversal( G, H )`, it might not be the desired right transversal required for
#! the construction of a particular right quasigroup, quasigroup or loop.

#! @BeginGroup
#! @GroupTitle Checking right folders

# RQ_RightSectionFromRightFolder( G, H, T )
# auxiliary function
# returns the right section corresponding to the right folder [G,H,T]
DeclareOperation( "RQ_RightSectionFromRightFolder", [ IsGroup, IsGroup, IsSet ] );

# RQ_IsAlgebraRightFolder(category, G, H, T, reportErrors )
# auxiliary function
# returns true if G, H, T is a right folder for the given category
DeclareOperation( "RQ_IsAlgebraRightFolder", [ IsObject, IsGroup, IsGroup, IsList, IsBool ] ); 

#! @Arguments arg
DeclareOperation( "IsRightFolder", [ IsRightTransversal ] ); # T or G,H,T

#! @Arguments arg
DeclareOperation( "IsQuasigroupRightFolder", [ IsRightTransversal ]); # T or G,H,T

#! @Arguments arg
#! @Returns `true` if the argument is a right folder (resp. quasigroup right folder, loop right folder).
#! See above for the two possible formats of the argument <Arg>arg</Arg>, that is, `T` or `G,H,T`.
DeclareOperation( "IsLoopRightFolder", [ IsRightTransversal ] ); # T or G,H,T

#! @EndGroup

#! @BeginGroup
#! @GroupTitle Creating right quasigroups, quasigroups and loops by right folders

# RQ_AlgebraByRightFolder( category, G, H, T, style )
# returns the appropriate algebra from the right folder G, H, T
DeclareOperation( "RQ_AlgebraByRightFolder", [ IsObject, IsGroup, IsGroup, IsList, IsRecord ] ); 

#! @Arguments arg[, constructorStyle]
DeclareOperation( "RightQuasigroupByRightFolder", [ IsRightTransversal ] ); # T or G,H,T, then [, constructorStyle ]

#! @Arguments arg[, constructorStyle]
DeclareOperation( "QuasigroupByRightFolder", [ IsRightTransversal ] ); # T or G,H,T, then [, constructorStyle ]

#! @Arguments arg[, constructorStyle]
#! @Returns a right quasigroup (resp. quasigroup or loop) from the given right folder
#! (resp. quasigroup right folder, loop right folder). See above for the two possible
#! formats of the argument <Arg>arg</Arg>, that is, `T` or `G,H,T`.
#! See Section <Ref Sect="Section_OptionalArguments"/> for the optional argument <Arg>constructorStyle</Arg>.
DeclareOperation( "LoopByRightFolder", [ IsRightTransversal ] ); # T or G,H,T, then [, constructorStyle ]

#! <P/>Here is an example of a right quasigroup constructed from a right folder.

#! @BeginExampleSession
#! gap> G := SymmetricGroup( 5 );;
#! gap> H := Subgroup( G, [ (1,2),(1,3) ] );;
#! gap> T := RightTransversal( G, H );;
#! gap> [ IsRightFolder( T ), IsRightFolder( G, H, T ) ];
#! [ true, true ]
#! gap> Q1 := RightQuasigroupByRightFolder( T );
#! <right quasigroup of size 20>
#! gap> Q2 := RightQuasigroupByRightFolder( G, H, T );
#! <right quasigroup of size 20>
#! gap> Q3 := RightQuasigroupByRightFolder( G, H, Elements( T ) );
#! <right quasigroup of size 20>
#! gap> IsomorphismRightQuasigroups( Q1, Q2 );
#! ()
#! gap> UnderlyingSet( Q1 ) = Elements( T );
#! true
#! gap> IsQuasigroupRightFolder( T );
#! false
#! @EndExampleSession

#! <P/>And here is an example of a loop constructed from a (trivial) right folder.

#! @BeginExampleSession
#! gap> G := SymmetricGroup( 2 );;
#! gap> H := Subgroup( G, [()] );;
#! gap> T := RightTransversal( G, H );;
#! gap> IsQuasigroupRightFolder( T );
#! true
#! gap> IsLoopRightFolder( T );
#! true
#! gap> QuasigroupByRightFolder( T );
#! <quasigroup of size 2>
#! gap> LoopByRightFolder( T );
#! <loop of size 2>
#! gap> Elements( last );
#! [ l(), l(1,2) ]
#! @EndExampleSession

#! @EndGroup

# SPECIAL TYPES OF RIGHT QUASIGROUPS
# _____________________________________________________________________________

#! @Section Constructors for special types of right quasigroups

#! Here we collect a few constructors for special types of right quasigroups.

#! @Arguments S[, constructorStyle]
#! @Returns the projection right quasigroup on the set <Arg>S</Arg> that is, a magma on <Arg>S</Arg> with mutliplication `x*y=x`.
#! We also allow <Arg>S</Arg> to be a positive integer `n`, in which case the underlying set will be `[1..n]`.
#! @Description Note: The value of <Arg>constructorStyle</Arg>`.checkArguments` of the optional argument <Arg>constructorStyle</Arg>
#! does not come into play and need not be specified.
DeclareOperation( "ProjectionRightQuasigroup", [ IsCollection ] );

#! @BeginExampleSession
#! gap> ProjectionRightQuasigroup( [1..1000], rec( indexBased := false ) );; # 16 ms to construct, 
#! gap> Q := ProjectionRightQuasigroup( [1..1000] );; # 375 ms to construct
#! gap> Q.123*Q.456;
#! r123
#! @EndExampleSession