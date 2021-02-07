# Representations.gd
# Representing right quasigroups in GAP
# =============================================================================

# INFO CLASS
# _____________________________________________________________________________

DeclareInfoClass( "InfoRightQuasigroups" );

#! @Chapter Introduction

# GAP CATEGORIES AND REPRESENTATIONS
# _____________________________________________________________________________

#! @Section Categories and representations for right quasigroups, quasigroups and loops

#! <P/>Given a magma $(Q,\cdot)$ and $x\in Q$, the **right translation**<Index Subkey="right">translation</Index>
#! by $x$ in $Q$ is the mapping $R_x:Q\to Q$ defined by $R_x(y)=yx$, while the **left translation**
#! <Index Subkey="left">translation</Index> by $x$ in $Q$ is the mapping $L_x:Q\to Q$ defined by $L_x(y)=xy$.
#! The binary operation $\cdot$ will be referred to as **multiplication**<Index>multiplication</Index>.

#! <P/>A magma $(Q,\cdot)$ is a **right quasigroup**<Index Subkey="right">quasigroup</Index>
#! if for every $x\in Q$ the right translation $R_x$ is a permutation of $Q$. We
#! then denote $R_x^{-1}(y)$ by $y/x$ and refer to $/$ as **right division**<Index Subkey="right">division</Index>.

#! <P/>Dually, a magma $(Q,\cdot)$ is a **left quasigroup**<Index Subkey="left">quasigroup</Index>
#! if for every $x\in Q$ the left translation $L_x$ is a permutation of $Q$. We
#! then denote $L_x^{-1}(y)$ by $x\backslash y$ and refer to $\backslash$ as **left division**<Index Subkey="left">division</Index>.

#! <P/>If $(Q,\cdot)$ is both a right quasigroup and a left quasigroup, it is a **quasigroup**<Index>quasigroup</Index>.

#! <P/>A **loop**<Index>loop</Index> is a quasigroup $(Q,\cdot)$ with a neutral element
#! $e\in Q$ satisfying $x\cdot e=x=e\cdot x$ for every $x\in Q$.

#! <P/>In &GAP;, right quasigroups and their elements are constructs that mimic the mathematical objects. The set 
#! on which a right quasigroup is based is called the **underlying set**<Index>underlying set</Index>,
#! cf. Section <Ref Sect="Section_Displaying"/>.

#! <P/>From a universal-algebraic point of view, right quasigroups, quasigroups and loops have different signatures, which is
#! why there are separate, nested representations for the three kinds of algebras and their elements.

# element of a right quasigroup
# -----------------------------

#! @BeginGroup RightQuasigroupCategory
#! @GroupTitle Categories and representations for right quasigroup elements and right quasigroups

#! @Arguments object
#! @Returns `true` or `false`.
DeclareCategory( "IsRightQuasigroupElement", IsMultiplicativeElement );

#! @Arguments object
DeclareRepresentation( "IsRightQuasigroupElmRep", IsPositionalObjectRep and IsMultiplicativeElement, [1] );

# an auxiliary category for &GAP; to tell apart IsMagma and IsRightQuasigroup
DeclareCategory( "IsRightQuasigroupAux", IsObject );

#! @Arguments object
DeclareCategory( "IsRightQuasigroup", IsMagma and IsRightQuasigroupAux );

#! <P/>These are the &GAP; categories and representations for right quasigroup elements and right quasigroups.
#! `IsRightQuasigroupElement` is contained in the filter `IsMultiplicativeElement`,
#! `IsRightQuasigroupElmRep` is contained in the filters `IsPositionalObjectRep`
#! and `IsMultiplicativeElement`, and `IsRightQuasigroup` is contained in the
#! filter `IsMagma`.

#! @EndGroup

# element of a quasigroup
# -----------------------

#! @BeginGroup QuasigroupCategory
#! @GroupTitle Categories and representations for quasigroup elements and quasigroups

# an auxiliary category for &GAP; to tell apart <C>IsRightQuasigroupElement</C> and <C>IsQuasigroupElement</C>.
DeclareCategory( "IsQuasigroupElementAux", IsObject );

#! @Arguments object
DeclareCategory( "IsQuasigroupElement", IsRightQuasigroupElement and IsQuasigroupElementAux );

#! @Arguments object
DeclareRepresentation( "IsQuasigroupElmRep", IsPositionalObjectRep and IsMultiplicativeElement, [1] );

# auxiliary category for &GAP; to tell apart IsRightQuasigroup and IsQuasigroup
DeclareCategory( "IsQuasigroupAux", IsObject );

#! @Arguments object
DeclareCategory( "IsQuasigroup", IsRightQuasigroup and IsQuasigroupAux );

#! <P/>These are the &GAP; categories and representations for quasigroup elements and quasigroups.
#! `IsQuasigroupElement` is contained in the filter `IsRightQuasigroupElement`,
#! `IsQuasigroupElmRep` is contained in the filters `IsPositionalObjectRep`
#! and `IsMultiplicativeElement`, and `IsQuasigroup` is contained in the
#! filter `IsRightQuasigroup`.

#! @EndGroup

# loop
# ----

#! @BeginGroup LoopCategory
#! @GroupTitle Categories and representations for loop elements and loops

#! @Arguments object
DeclareCategory( "IsLoopElement", IsQuasigroupElement and IsMultiplicativeElementWithInverse );

#! @Arguments object
DeclareRepresentation( "IsLoopElmRep", IsPositionalObjectRep and IsMultiplicativeElementWithInverse, [1] );

#! @Arguments object
#! @Description
DeclareCategory( "IsLoop", IsQuasigroup and IsMultiplicativeElementWithInverseCollection);

#! <P/>These are the &GAP; categories and representations for loop elements and loops.
#! `IsLoopElement` is contained in the filters `IsQuasigroupElement` and
#! `IsMultiplicativeElementWithInverse`, 
#! `IsLoopElmRep` is contained in the filters `IsPositionalObjectRep`
#! and `IsMultiplicativeElementWithInverse`, and `IsLoop` is contained in the
#! filters `IsQuasigroup` and `IsMultiplicativeElementWithInverseCollection`.

#! <P/>Note that these declarations do not imply that every loop element has an inverse. 

#! @EndGroup

#! @Arguments Q
#! @Returns the smallest category from among `IsRightQuasigroup`, `IsQuasigroup` and 
#! `IsLoop` to which the right quasigroup <Arg>Q</Arg> belongs.
#! @Description All declared right quasigroups, quasigroups and loops belong to the filter `IsRightQuasigroup`.
#! It is often useful to know if a right quasigroup is in fact declared as a quasigroup or a loop,
#! which is what the above method furnishes.
DeclareOperation( "CategoryOfRightQuasigroup", [ IsRightQuasigroup ] );

#! @BeginExampleSession
#! gap> Q := QuasigroupByCayleyTable( [[0,1],[1,0]] ); # declared quasigroup, in fact a group mathematically
#! <quasigroup of size 2>
#! gap> [ IsMagma( Q ), IsRightQuasigroup( Q ), IsQuasigroup( Q ), IsLoop( Q ), IsGroup( Q ) ];
#! [ true, true, true, false, false ]
#! gap> CategoryOfRightQuasigroup( Q );
#! <Category "IsQuasigroup">
#! @EndExampleSession

# DISPLAYING RIGHT QUASIGROUPS AND THEIR ELEMENTS
# _____________________________________________________________________________

#! @Section Displaying right quasigroups and their elements

# RQ_ViewObjPrintObjString( Q )
# returns the common portion of the string for ViewObj, PrintObj for a right quasigroup Q
DeclareOperation( "RQ_ViewObjPrintObjString",  [ IsRightQuasigroup ] ); 

# ViewObj

# PrintObj

#! @Subsection Displaying right quasigroups, quasigroups and loops

#! <P/>The standard `ViewObj` function is implemented for right quasigroups `Q` 
#! and it always displays at least the size of `Q`, namely `&lt;right quasigroup of size n&gt;`, 
#! `&lt;quasigroup of size n&gt;` or `&lt;loop of size n&gt;`, depending on whether `Q`
#! is declared as a right quasigroup, quasigroup or loop.
#! When additional properties of `Q` become known, one of the strongest properties of `Q` is displayed,
#! e.g., `&lt;associative loop of order n&gt;`.
#! Library objects are displayed with their catalog numbers, e.g., `&lt;Moufang loop 64/12&gt;`.

#! <P/>Likewise, the standard `PrintObj` function is implemented for right quasigroups. It displays
#! in addition up to the first 5 elements of the underlying set of `Q`.

#! @BeginExampleSession
#! gap> Q := QuasigroupByCayleyTable( [[0,1],[1,0]] );
#! <quasigroup of size 2>
#! gap> IsAssociative( Q );
#! true
#! gap> Q;
#! <associative quasigroup of size 2>
#! gap> Print( AsLoop( SymmetricGroup( 4 ) ) );
#! <associative loop of size 24 on (), (3,4), (2,3), (2,3,4), (2,4,3), ...>
#! @EndExampleSession

# PrintObj

#! @Subsection Displaying right quasigroup elements

#! <P/>The `PrintObj` function is implemented to display elements of right quasigroups.
#! By default, if `Q` is a right quasigroup and `x` is an element of the underlying set of
#! `Q` then the corresponding element of `Q` is displayed as `rx`, `qx`
#! or `lx`, depending on whether `Q` is declared as a right quasigroup, a quasigroup or a loop.

#! @BeginGroup
#! @GroupTitle Changing the name of right quasigroup elements

#! @Arguments Q, s
#! @Returns `true`.
#! @Description
#! Changes the name of all elements of the parent of <Arg>Q</Arg> for the purposes of displaying so that the name
#! of every element starts with the prefix (string) <Arg>s</Arg>. Note that it is possible to change the
#! prefix to an empty string, in which case the elements of the parent of <Arg>Q</Arg> will be displayed
#! exactly as the elements of the underlying set; this improves legibility but might lead to confusion.
DeclareOperation( "SetRightQuasigroupElementsName", [ IsRightQuasigroup, IsString ] );

#! @Arguments Q, s
DeclareOperation( "SetQuasigroupElementsName", [ IsQuasigroup, IsString ] );

#! @Arguments Q, s
DeclareOperation( "SetLoopElementsName", [ IsLoop, IsString ] );

#! @BeginExampleSession
#! gap> Q := AsLoop( Group( (1,2) ) );
#! <associative loop of size 2>
#! gap> Elements( Q );
#! [ l(), l(1,2) ]
#! gap> SetLoopElementsName( Q, "g" );; Elements( Q );
#! [ g(), g(1,2) ]
#! gap> SetLoopElementsName( Q, "" );; Elements( Q ); # better legibility but perhaps confusing
#! [ (), (1,2) ]
#! gap> IsPerm( last[1] );
#! false
#! @EndExampleSession

#! @EndGroup

# THE UNDERLYING SET
# _____________________________________________________________________________

#! @Section The underlying set

#! <P/>Every right quasigroup `Q` consists of &GAP; elements returned via `Elements( Q )`.
#! In addition, every right quasigroup also has an underlying set accesible via `UnderlyingSet( Q )`.
#! The underlying set is used in displaying &GAP; elements of right quasigroups
#! (cf. Section <Ref Sect="Section_Displaying"/>)
#! and in Cayley tables (cf. Section <Ref Sect="Section_Cayley"/>).

#! <P/>If a right quasigroup is not index based (cf. Section <Ref Sect="Section_IndexBased"/>), then
#! the underlying set plays a critical role since its elements are used as arguments of the
#! multiplication function. In non-index based right quasigroups the underlying set is merely
#! cosmetic and can be changed at any time.

#! @Arguments obj
#! @Returns If <Arg>obj</Arg> is a right quasigroup element, returns the corresponding element
#! of the underlying set. If <Arg>obj</Arg> is a list of right quasigroup elements (possibly
#! from different right quasigroups), returns the list of the corresponding elements
#! of the underlying set. If <Arg>obj</Arg> is a right quasigroup, returns the underlying set
#! of <Arg>obj</Arg>.
DeclareOperation ( "UnderlyingSetElm", [ IsRightQuasigroupElement ] );

#! @Arguments Q
#! @Returns the underlying set of the right quasigroup <Arg>Q</Arg>. Note that `UnderlyingSet( `<Arg>Q</Arg>` )`
#! has the same effect as `UnderlyingSetElm( `<Arg>Q</Arg>` )`.
DeclareOperation( "UnderlyingSet", [ IsRightQuasigroup ] ); 

#! @Arguments Q, S
#! @Returns `true`.
#! @Description If <Arg>Q</Arg> is a right quasigroup that is index based and its own parent,
#! `ChangeUnderlyingSet( `<Arg>Q</Arg>`, `<Arg>S</Arg>` )` changes the underlying set of <Arg>Q</Arg> to <Arg>S</Arg>.
#! If the Cayley table of <Arg>Q</Arg> was previously stored, it will be recalculated.
#! The argument `S` must be a collection and it is internally sorted.
#! <P/>Note that is it not possible to change the underlying set of a non-index based right quasigroup
#! because the multiplication function then depends on the underlying set and it would have to be changed as well.
#! In addition, we do not support changing the underlying set for a single element or for a list of elements
#! because the underlying set of a parent quasigroup is sorted and the order might not be maintained
#! by local changes.
DeclareOperation( "ChangeUnderlyingSet", [ IsRightQuasigroup, IsCollection ] );

#! @BeginExampleSession
#! gap> Q := AsLoop( SymmetricGroup( 3 ) );;
#! gap> UnderlyingSetElm( Q.1 );
#! ()
#! gap> UnderlyingSet( Q );
#! [ (), (2,3), (1,2), (1,2,3), (1,3,2), (1,3) ]
#! gap> ChangeUnderlyingSet( Q, ['a','b','c','d','e','f'] );
#! true
#! gap> UnderlyingSet( Q );
#! "abcdef"
#! gap> CayleyTable( Q );
#! [ "abcdef", "badcfe", "ceafbd", "dfbeac", "ecfadb", "fdebca" ]
#! @EndExampleSession

# ELEMENTWISE OPERATIONS
# _____________________________________________________________________________

#! @Section Accessing elements and elementwise right quasigroup operations

# \=

# \<

# \[\] 
# returns an element of the algebra from an element of the underlying set

# \.
# returns an element of the algebra from an index

#! <P/>The $i$th element of a right quasigroup `Q` can be obtained by `Elements( Q )[ i ]`.

#! <P/>The $i$the element of the parent `Parent( Q )` or a right quasigroup `Q` can be obtained
#! by `Q.i` (see Section <Ref Sect="Section_Parent"/>). Note that `Q.i` need not be the same element
#! as `Elements( Q )[ i ]`, in fact, it need not even be an element of `Q`.

#! <P/>Finally, if `x` is an element of the underlying set of `Parent( Q )`, the corresponding
#! element of `Parent( Q )` is obtained by `Q[x]`. Note that `Q[x]` therefore need not 
#! be an element of `Q`.

# \*

# \/

#! <P/>The product of two right quasigroup elements `x` and `y` is obtained by `x*y`. 
#! The right quotient of `x` and `y` is obtained via `x/y`, `RightQuotient( x, y )` or `RightDivision( x, y )`.
#! In case of quasigroups, the left division is obtained via `LeftQuotient( x, y )` or `LeftDivision( x, y )`
#! but not by `x\y` since `\` is not supported in &GAP; as a binary operation symbol.

#! <P/>In each of these operations, one of the two arguments can be a list of right quasigroup elements,
#! in which case the corresponding list is returned.

#! @Arguments x,y
#! @Returns the right quotient of right quasigroup elements <Arg>x</Arg> and <Arg>y</Arg>, that is, the unique element
#! `z` such that <Arg>x</Arg>` = z*`<Arg>y</Arg>. The synonym `RightDivision( `<Arg>x</Arg>`, `<Arg>y</Arg>` )`
#! is also supported.
DeclareOperation( "RightQuotient", [ IsRightQuasigroupElement, IsRightQuasigroupElement ] );

DeclareSynonym( "RightDivision", RightQuotient );

# LeftQuotient alredy declared in GAP.

#! @Arguments x,y
#! @Returns the left quotient of quasigroup elements <Arg>x</Arg> and <Arg>y</Arg>, that is, the unique element
#! `z` such that <Arg>y</Arg>` = `<Arg>x</Arg>`*z`. The synonym `RightDivision( `<Arg>x</Arg>`, `<Arg>y</Arg>` )`
#! is also supported.
DeclareOperation( "LeftQuotient", [ IsQuasigroupElement, IsQuasigroupElement ] );

DeclareSynonym( "LeftDivision", LeftQuotient );

# \^

# REVISIT: How are powers defined.

#! @BeginGroup
#! @GroupTitle Inverses in loops

# OneOp already declared

#! @Arguments Q
DeclareAttribute( "One", IsLoopElement );

#! @Arguments x
DeclareAttribute( "RightInverse", IsLoopElement );

#! @Arguments x
DeclareAttribute( "LeftInverse", IsLoopElement );
# InverseOp already declared

#! <P/>In a loop `Q`, the neutral element is returned by `One( Q )`. Note that the neutral
#! element need not be the first element `Elements( Q )[ 1 ]` of `Q`.

#! <P/>If `e` is the neutral element of `Q`, then for every `x` in `Q`
#! there are `y` and `z` in `Q` such that `x*y = z*x = e`. The element
#! `y` is the <Index>right inverse</Index>**right inverse** of `x` and is returned by
#! `RightInverse( x )`. Dually, the element `z` is the <Index>left inverse</Index>**left inverse**
#! of `x` and is returned by `LeftInverse( x )`.

#! <P/>If the two inverses of `x` coincide, the <Index>two-sided inverse</Index>**two-sided inverse**
#! of `x` is returned by `Inverse(x)` or by `x^-1`. 

#! @EndGroup

#! @Subsection Powers, order and exponent

# Order already declared for multiplicative elements.

# Exponent already declared for groups.

#! <P/>Products without specified parentheses are evalutated from left to right, i.e., `x*y*z = (x*y)*z`.
#! A magma `M` is said to be <Index>power associative</Index>**power associative** if for every `x` in `M`
#! the submagma generated by `x` is a group. In particular, powers `x^n` are well-defined in power
#! associative magmas. Even if `M` is not power-associative, `x^n` with positive `n` returns the element
#! of `M` obtained from the binary expansion of the exponent `n`.

#! <P/>If `M` is a magma with neutral element `e` and `x` is an element of `M`, then `Order( x )` returns
#! the smallest nonnegative integers `n` such that `x^n=e`, if possible, else returns `fail`. Note that
#! in view of the above remarks, `Order( x )` always returns a nonegative integer if `x` is an element 
#! of a finite loop.

#! <P/>The exponent `Exponent( Q )` returns the smallest nonnegative integer `n` such that `x^n` is 
#! the neutral element of `Q`, if such an integer exists, else it returns `fail`. Note again that
#! `Exponent( Q )` will never fail for a finite loop `Q` in &RightQuasigroups;, even if `Q` is not
#! power associative.

#! <P/>If `n` is negative and `x` has a two-sided inverse, then `x^n` is calculated as `(x^(-1))^(-n)`.

#! @Arguments x, y
#! @Returns the commutator of `x` and `y`, that is, the unique element
#! `z = (x*y)/(y*x)` satisfying `x*y = z*(y*x)`. This is a logical choice for the
#! elementwise commutator in right quasigroups.
DeclareOperation( "Commutator", [ IsRightQuasigroupElement, IsRightQuasigroupElement ] );

#! <P/>We also support group-like commutators via `Comm( x, y )`. If `x` and `y` are
#! quasigroup elements, `Comm( x, y )` returns the
#! unique element `z = LeftQuotient( y*x, x*y )` that satisfies `x*y = (y*x)*z`. When
#! the underlying quasigroup is a loop with two-sided inverses in which
#! the antiautomorphic inverse property $(xy)^{-1}=y^{-1}x^{-1}$ and the
#! left inverse property $x^{-1}(xy) = y$ hold, then
#! `Comm( x, y )` coincides with the &GAP; commutator `x^(-1)*y^(-1)*x*y`.

#! @Arguments x, y, z
#! @Returns the associator of `x`,  `y` and `z`, that is, the unique element
#! `u = (x*(y*z))/((x*y)*z)` satisfying `x*(y*z) = u*((x*y)*z)`.
DeclareOperation( "Associator", [ IsRightQuasigroupElement, IsRightQuasigroupElement, IsRightQuasigroupElement ] );

# FIRST EXAMPLES
# _____________________________________________________________________________

#! @Section First examples

#! Arithmetic operations in right quasigroups (quasigroup, loops) are carried out either via tables or via functions,
#! depending on whether the algebra in question is index based or not. See Section <Ref Sect="Section_IndexBased"/>
#! for more information on index based versus non-index based right quasigroups, Section <Ref Sect="Section_Cayley"/>
#! for details on multiplication tables and Cayley tables, and Section <Ref Sect="Section_Function"/> for details
#! on how functions are used as arithmetic operations. 

#! <P/>Chapter <Ref Chap="Chapter_Constructors"/> contains a comprehensive list of right quasigroup constructors.
#! Here we present two examples, starting with a right quasigroup constructor based on a multiplication function.

#! @BeginExampleSession
#! gap> Q := RightQuasigroupByFunction( [0..3], function( x,y ) return (x+2*y) mod 4; end ); # index based by default
#! <right quasigroup of size 4>
#! gap> UnderlyingSet( Q );
#! [ 0, 1, 2, 3 ]
#! gap> Elements( Q ); # default prefix "r" is assigned to right quasigroup elements
#! [ r0, r1, r2, r3 ]
#! gap> Display( CayleyTable( Q ) ); # based on the underlying set
#! [ [  0,  2,  0,  2 ],
#!   [  1,  3,  1,  3 ],
#!   [  2,  0,  2,  0 ],
#!   [  3,  1,  3,  1 ] ]
#! gap> Display( MultiplicationTable( Q ) ); # based on [1..n], here [1..4]
#! [ [  1,  3,  1,  3 ],
#!   [  2,  4,  2,  4 ],
#!   [  3,  1,  3,  1 ],
#!   [  4,  2,  4,  2 ] ]
#! gap> mult := MultiplicationFunction( Q ); # based on [1..n] since Q is index based
#! function( i, j ) ... end
#! gap> mult( 1, 1 );
#! 1
#! gap> [ Elements( Q )[ 1 ], Q.1, Q[0] ]; # three ways of accessing elements
#! [ r0, r0, r0 ]
#! gap> Q[0]*Q[1];
#! r2
#! gap> Q[0]/Q[2]; # RightQuotient and RightDivision are also supported
#! r0
#! @EndExampleSession

#! <P/>If, as in the above example, 
#! the remaining operations (right division, left division, neutral element) are not provided
#! by the user, they are automatically inferred from the given multiplication function depending on the 
#! algebra under construction. However, this might lead to slower division operations in the non-index based case.
#! For instance, if only the multiplication function for
#! a right quasigroup `Q` is given and `Q` is not index based, then the right quotient `x/y` is calculated 
#! as the first (and only) element `z` of `Q` such that `x = z*y`, which is slow when `Q` is large.

#! <P/>Here is an example of a loop constructor based on a Cayley table. The underlying set
#! is automatically understood to be the sorted list of elements contained in the first column of the Cayley table.

#! @BeginExampleSession
#! gap> Q := LoopByCayleyTable( [["a", "b"], ["b", "a"]], ConstructorStyle( false, false ) ); # the optional argument makes it non-index based and nothing is checked
#! <loop of size 2>
#! gap> UnderlyingSet( Q ); 
#! [ "a", "b" ]
#! gap> Elements( Q ); # default prefix "l" is assigned to loop elements
#! [ la, lb ]
#! gap> HasMultiplicationTable( Q );
#! false
#! gap> mult := MultiplicationFunction( Q ); # based on the underlying set since Q is not index based
#! function( x, y ) ... end
#! gap> mult( "a", "b" );
#! "b"
#! gap> One( Q );
#! la
#! gap> Commutator( Q["a"], Q["b"] );
#! la
#! gap> Associator( Q.1, Q.1, Q.2 );
#! la
#! gap> LeftQuotient( Q.1, Q.2 ); # LeftDivision is also supported
#! lb
#! @EndExampleSession

#! <P/>Note that right quasigroups that happen to be quasigroups and/or loops mathematically must be explicitly
#! declared as such in &GAP; to make quasigroup and/or loop methods available to them. For instance,
#! the above right quasigroup of size 2 is in fact an associative quasigroup (that is, a group),
#! but it will not be automatically recognized as a quasigroup, loop or group by &GAP;. There are methods
#! provided that check if a given right quasigroup is mathematically a quasigroup or a loop,
#! cf. Section <Ref Sect="Section_Converting"/>.

#! @Section The parent right quasigroup 
 
#! <P/>The parent mechanism is employed in &GAP; and in &RightQuasigroups; to save memory and to take advantage of
#! the containment of subalgebras in the enveloping algebras. The <Index>parent</Index>parent `Parent( Q )` of
#! a right quasigorup `Q` is the largest right quasigroup from which `Q` has been constructed
#! as a subalgebra. In more detail, if `Q` is constructed as a subalgebra of a right quasigroup
#! `P` then `Parent( Q ) = Parent( P )`, while if `Q` is not constructed
#! as a subalgebra of some right quasigroup then `Parent( Q ) = Q`.

#! <P/>Right quasigroup elements are created automatically every time a new right quasigroup
#! is constructed, with one exception: When `Q` is constructed as a subalgebra then the
#! elements of `Q` are inherited from `Parent( Q )`. In particular,
#! if `x` is any element of `Q` then `F = FamilyObj( x )` points to `Parent( Q )`
#! and many attributes of `P` can be accessed via `F` (see Section <Ref Sect="Section_Direct"/>). 
 
#! @BeginExampleSession
#! gap> Q := RightQuasigroupByFunction([0..5], function(x,y) return (x+y) mod 6; end );; Elements( Q ); 
#! [ r0, r1, r2, r3, r4, r5 ]
#! gap> A := Subrightquasigroup( Q, [2] );
#! <right quasigroup of size 3>
#! gap> Elements( A );
#! [ r0, r2, r4 ]
#! gap> Parent( A ) = Q;
#! true;
#! gap> Elements( A )[ 3 ]; # the 3rd element of A
#! r4
#! gap> A.3; # the 3rd element of the parent of A
#! r3
#! gap> A[4]; # the element of parent of A corresponding to the given underlying element
#! r4
#! gap> Display( CayleyTable( A ) );
#! [ [  0,  2,  4 ],
#!   [  2,  4,  0 ],
#!   [  4,  0,  2 ] ]
#! gap> Display( MultiplicationTable( A ) );
#! [ [  1,  2,  3 ],
#!   [  2,  3,  1 ],
#!   [  3,  1,  2 ] ]
#! @EndExampleSession

# INDEX BASED AND CANONICAl COPIES
# _____________________________________________________________________________

#! @Section Index based and canonical right quasigroups

#! <P/>Every right quasigroup `Q` is constructed either as an <Index>index based right quasigroup</Index>
#! **index based** right quasigroup or as a right quasigroup that is not index based.

#! <P/>Generally speaking, index based right quasigroups take longer to construct, cannot be very large (thousands of
#! elements) and can be calculated with fast, while non-index based right quasigroups are constructed quickly,
#! can be very large (millions of elements) but only basic methods will work for them.

#! <P/>If `Q` is an index based right quasigroup of size $n$,
#! then the multiplication and divisions in `Q` are carried out via multiplication and division tables. The multiplication
#! and division functions might then be present as functions $[1..n]\times[1..n]\to[1..n]$ but they are not directly involved
#! in carrying out arithmetic operations.

#! <P/>If `Q` is a non-index based right quasigroup on the underlying set $S$,
#! then the multiplication and divisions in `Q` are carried out via multiplication and division functions $S\times S\to S$.
#! The multiplication and division tables might then be present but they are not directly involved
#! in carrying out arithmetic operations.

#! <P/>In more details, if `Q` is a right quasigroup with parent `P` of size $n$, 
#! `x` is an an element of `Q` and `F=FamilyObj( x )`, then:
#! <List>
#! <Item>If `Q` is index based:
#!      <List>
#!      <Item>`x![1]` is the position of `x` among the elements of `P`, i.e.,
#!          the index of `x` (see below),</Item>
#!      <Item>the multiplication table of `P` is precalculated and stored,</Item>
#!      <Item>the division tables of `P` are precalculated and stored at first usage of the respective divisions,</Item>
#!      <Item>the multiplication and division functions of `P`, if present, are functions
#!          $[1..n]\times[1..n]\to [1..n]$ but they are not directly involved since all operations
#!          are handled by means of the tables,</Item>
#!      <Item>the fundamental piece of data is `F!.multTable`, the multiplication table of `P`.</Item>
#!      </List>
#! </Item>
#! <Item>If `Q` is not index based:
#!      <List>
#!      <Item>`x![1]` is the element of the underlying set $S$ of `Q` corresponding to `x`,</Item>
#!      <Item>the mutliplication and division tables of `P` are not precalculated,</Item>
#!      <Item>the multiplication and division functions of `P` are possibly slow functions $S\times S\to S$, 
#!          often based on some additional data provided by the user in the constructor,</Item>
#!      <Item>the fundamental piece of data is `F!.mult`, the multiplication function of `P`.</Item>
#!      </List>
#! </Item>
#! </List>

#! <P/>A right quasigroup of size $n$ is said to be <Index>canonical right quasigroup</Index>**canonical**
#! if it is index based, is its own parent and the underlying set is $[1..n]$.
#! Many computationally intensive methods of &RightQuasigroups; internally work with canonical right quasigroups.

#! @Arguments x 
#! @Returns the index of the element `x`, that is, the position of `x`
#! among the elements of the parent right quasigroup. The operation also accepts 
#! a list of right quasigroup elements or a right quasigroup as the argument, in which
#! case the corresponding list of indices is returned.
DeclareOperation( "ParentInd", [ IsRightQuasigroupElement ] );

#! @Arguments Q
#! @Returns `true` if the right quasigroup <Arg>Q</Arg> is index based, else returns `false`.
DeclareOperation( "IsIndexBased", [ IsRightQuasigroup ] );

#! @Arguments Q
#! @Returns a copy of <Arg>Q</Arg> that has the same underlying set as <Arg>Q</Arg>,
#! is index based and is its own parent. 
#! An effort is made to inherit properties from <Arg>Q</Arg>.
#! @Description Note that there is no general method available for converting index based right quasigroups
#! into right quasigroups that are not index based.
DeclareOperation( "IndexBasedCopy", [ IsRightQuasigroup ] );

#! @Arguments Q
#! @Returns `true` if the right quasigroup <Arg>Q</Arg> of size $n$ is canonical,
#! else returns `false`.
DeclareOperation( "IsCanonical", [ IsRightQuasigroup ] );

#! @Arguments Q
#! @Returns a canonical copy of the right quasigroup <Arg>Q</Arg>.
#! An effort is made to inherit properties from <Arg>Q</Arg>.
#! @Description Note that there is no general method available for converting canonical
#! right quasigroups into right quasigroups that are not canonical.
DeclareOperation( "CanonicalCopy", [ IsRightQuasigroup ] );

#! <P/>The following example illustrates basic features of non-index based right quasigroups.

#! @BeginExampleSession
#! gap> Q := RightQuasigroupByFunction( GF( 8 ), \+, rec( indexBased := false, checkArguments := false ) ); # the optioanl argument makes it not index based
#! <right quasigroup of size 8>
#! gap> [ IsIndexBased( Q ), HasMultiplicationTable( Q ) ];
#! [ false, false ]
#! gap> x := Q.1;; 
#! gap> [ x![1],  x*x ];
#! [ 0*Z(2), r0*Z(2) ]
#! gap> F := FamilyObj( Q.1 );; [ IsBound( F!.mult ), IsBound( F!.rdiv ), IsBound( F!.ldiv ) ];
#! [ true, true, false ]
#! gap> mult := MultiplicationFunction( Q );
#! <Operation "+">
#! gap> mult( Z(2), Z(2) );
#! 0*Z(2)
#! gap> rdiv := RightDivisionFunction( Q ); # the constructor does not know that this really is <Operation "-">
#! function( x, y ) ... end
#! gap> rdiv( Z(2), Z(2) );
#! 0*Z(2)
#! @EndExampleSession

#! <P/>Note how things change in index based and canonical copies.

#! @BeginExampleSession
#! gap> Q := RightQuasigroupByFunction( GF( 8 ), \+, rec( indexBased := false, checkArguments := false ) ); # same as in the above example, not index based
#! gap> R := IndexBasedCopy( Q );;
#! gap> UnderlyingSet( R ); # no change to the underlying set
#! [ 0*Z(2), Z(2)^0, Z(2^3), Z(2^3)^2, Z(2^3)^3, Z(2^3)^4, Z(2^3)^5, Z(2^3)^6 ]
#! gap> [ HasMultiplicationTable( R ), IsCanonical( R ) ];
#! [ true, false ]
#! gap> x := R.1;; [ x![1], x*x ];
#! [ 1, r0*Z(2) ]
#! gap> F := FamilyObj( R.1 );; [ IsBound( F!.mult ), IsBound( F!.rdiv ), IsBound( F!.ldiv ) ];
#! [ true, true, false ]
#! gap> mult := MultiplicationFunction( R ); # the multiplication function is now based on indices
#! function( i, j ) ... end
#! gap> mult( 1, 1 );
#! 1
#! gap> C := CanonicalCopy( Q );
#! <right quasigroup of size 8>
#! gap> UnderlyingSet( C ); # underlying set has changed to [1..n]
#! [ 1, 2, 3, 4, 5, 6, 7, 8 ]
#! @EndExampleSession

# GENERATORS AND COMPARISON OF RIGHT QUASIGROUPS
# _____________________________________________________________________________

#! @Section Generators and comparison of right quasigroups

#! <P/>We support `GeneratorsOfRightQuasigroup`, `GeneratorsOfQuasigroup` and `GeneratorsOfLoop` as synonyms of `GeneratorsOfMagma`.

DeclareSynonymAttr( "GeneratorsOfRightQuasigroup", GeneratorsOfMagma );
DeclareSynonymAttr( "GeneratorsOfQuasigroup", GeneratorsOfMagma );
DeclareSynonymAttr( "GeneratorsOfLoop", GeneratorsOfQuasigroup );

# Attribite SmallGeneratingSet already declared for groups.

#! <P/>The function `SmallGeneratingSet( Q )` returns a small generating set of a right quasigroup `Q` obtained by a greedy
#! algorithm that starts with the empty set of generators and in every steps adds the first element of `Q` that enlarges
#! the generated subalgebra the most. There is no guarantee that `SmallGeneratingSet` returns a generating set of smallest
#! possible cardinality.

# Attribute GeneratorsSmallest already declared for groups.

#! <P/>The elements of a right quasigroup inherit a linear ordering from the underlying set. The attribute
#! `GeneratorsSmallest( Q )` returns the smallest generating set of `Q` with respect to the lexicographic
#! ordering of elements of `Q`.

# \< (comparing two right quasigroups with common parent)

#! <P/>Finally, if `A` and `B` are right quasigroups with the same parent, cf. Section <Ref Sect="Section_Parent"/>,
#! then `A&lt;B` iff `GeneratorsSmallest( A )&lt;GeneratorsSmallest( B )`.

#! @BeginExampleSession
#! gap> Q := AsLoop( GF(8) );;
#! gap> Length( GeneratorsOfLoop( Q ) );
#! 8
#! gap> GeneratorsSmallest( Q );
#! [ lZ(2^3)^4, lZ(2^3)^5, lZ(2^3)^6 ]
#! gap> SmallGeneratingSet( Q );
#! [ lZ(2)^0, lZ(2^3), lZ(2^3)^2 ]
#! @EndExampleSession

# NON-QUALIFIED OPERATIONS
# _____________________________________________________________________________

#! @Section Non-qualified operations

#! <P/>There are several so-called **non-qualified operations** in &GAP;. These are 
#! operations which are not attributes or properties but whose result depends on the type of
#! the argument. As far as &RightQuasigroups; is concerned, the non-qualified operations are
#! `DerivedSeries`, `IsNilpotent`, `IsSimple`, `IsSolvable`, `LowerCentralSeries` and `UpperCentralSeries`.
#! For all such operations we provide qualified methods, e.g., `DerivedSeriesOfLoop( Q )` called with
#! a loop `Q` as argument, but also support the non-qualified versions, e.g., `DerivedSeries( Q )`.

# AUXILIARY PROGRAMMING METHODS
# _____________________________________________________________________________

# RQ_OptionalError( reportErrors, errorString )
# if reportErrors = true, results in Error( errorString ), otherwise returns false
# This is useful when some condition is being checked. When the conditions fails, 
# sometimes we wish to produce an error, sometimes we wish to return false.
DeclareGlobalFunction( "RQ_OptionalError" );

# RQ_GroupByGenerators( gens )
# returns the group generated by gens
# This fixes a glitch in GAP where Group([]) results in an error.
DeclareGlobalFunction( "RQ_GroupByGenerators" );

#! @Section *Direct access to the parent right quasigroup record

#! <P/>Let `Q` be a right quasigroup, `F` the family object of any element of `Q`
#! (that is, `F = FamilyObj(Q.1)`) and `P` the parent of `Q`
#! (see Section <Ref Sect="Section_Parent"/>). Then:

#! <List>
#! <Item>`F!.cayleyTable` is the Cayley table of `P`.</Item>
#! <Item>`F!.indexBased` determines is `P` and hence also `Q` are index based.</Item>
#! <Item>`F!.ldiv` is the left division function of `P`, if `Q` is a declared quasigroup.</Item>
#! <Item>`F!.ldivTable` is the left division table of `P`, if `Q` is a declared quasigroup.</Item>
#! <Item>`F!.mult` is the multiplication function of `P`.</Item>
#! <Item>`F!.multTable` is the multiplication table of `P`.</Item>
#! <Item>`F!.names` is the prefix used for all elements of `P`.</Item>
#! <Item>`F!.one` is the neutral element of `P` and hence also of `Q`, if `Q` is a declared loop.</Item>
#! <Item>`F!.parent` is `P`.</Item>
#! <Item>`F!.rdiv` is the right division function of `P`.</Item>
#! <Item>`F!.rdivTable` is the right division table of `P`.</Item>
#! <Item>`F!.rSection` is the right section for `P`.</Item>
#! <Item>`F!.set` is the set of &GAP; elements of `P`.</Item>
#! <Item>`F!.size` is the size of `P`.</Item>
#! <Item>`F!.uSet` is the underlying set of `P`. Note that if `Q` is a proper
#!      subalgebra of `P` then `UnderlyingSet( Q )` is a proper subset of `F!.uSet`.</Item>
#! </List>
# REVISIT: ADD MORE TO THIS LIST.

#! <P/>Note that not all of the above components of `F` are necessarily bound,
#! depending on the constructor used for `P`. 

# REVISIT: How can we see the list of bound components? `NamesOfComponents( F )` does not work.
 
###################
## TO DO:
# [ ] TOWARD RACKS AND QUANDLES:
# [ ] displacement group
# [ ] checking basic properties (is affine?)
# [ ] affine representations
# [ ] better isom checks
# [ ] ID function
# [ ] library of small racks and quandles
# [ ] documentation, InfoClass

