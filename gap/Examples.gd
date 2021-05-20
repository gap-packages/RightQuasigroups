# Examples.gd
# Libraries of loops, racks and quandles
# =============================================================================


#! @Chapter Libraries of loops, racks and quandles

#! @Section *A typical library

#! A library named **my algebras** is stored in file <File>data/my_algebras.tbl</File>
#! and the corresponding data structure is named `RQ_my_algebras`.
#! For example, when the library is called **left Bol loops**, the corresponding data file is called
#! <File>data/left_Bol_loops.tbl</File> and the corresponding data structure is named `RQ_left_Bol_loops`.

#! <P/>In most cases, the array `RQ_my_algebras` consists of three lists:
#! <List>
#! <Item>`RQ_my_algebras[1]` is a list of orders for which there is at least one algebra in the library,</Item>
#! <Item>`RQ_my_algebras[2,k]` is the number of algebras of order `RQ_my_algebras[1,k]` in the library,</Item>
#! <Item>`RQ_my_algebras[3][k][s]` contains data necessary to produce the  `s`th algebra of order `RQ_my_algebras[1,k]` in the library.</Item>
#! </List>
#! The format of `RQ_my_algebras[3]` depends heavily on the particular library and is not standardized in any way. The data is often coded to save space.

#! <P/>The library names are, in alphabetical order:
#! <List>
#! <Item>"automorphic loops",</Item>
#! <Item>"CC loops",</Item>
#! <Item>"connected quandles",</Item>
#! <Item>"code loops",</Item>
#! <Item>"interesting loops",</Item>
#! <Item>"itp small loops",</Item>
#! <Item>"LCC loops",</Item>
#! <Item>"left Bol loops",</Item>
#! <Item>"left Bruck loops",</Item>
#! <Item>"Moufang loops",</Item>
#! <Item>"nilpotent loops",</Item>
#! <Item>"Paige loops",</Item>
#! <Item>"right Bol loops",</Item>
#! <Item>"right Bruck loops",</Item>
#! <Item>"RCC loops",</Item>
#! <Item>"small loops",</Item>
#! <Item>"small quandles",</Item>
#! <Item>"small racks",</Item>
#! <Item>"Steiner loops".</Item>
#! </List>

# DISPLAYING INFORMATION ABOUT A LIBRARY
# _____________________________________________________________________________

#! @Section Accessing algebras in the library

# RQ_LibraryByName( name )
# returns the data structure for the library `name`
DeclareGlobalFunction( "RQ_LibraryByName" );

#! @Arguments name
#! @Returns Brief information about the loops contained in the library named <Arg>name</Arg>.
DeclareOperation( "DisplayLibraryInfo", [ IsString ] );

#! @BeginExampleSession
#! gap> DisplayLibraryInfo("nilpotent loops");
#! The library contains all nonassociative nilpotent loops of order less than 12.
#! ------
#! Extent of the library:
#!    2 loops of order 6
#!    134 loops of order 8
#!    8 loops of order 9
#!    1043 loops of order 10
#! true
#! @EndExampleSession

# ACCESSING ALGEBRAS IN THE LIBRARY
# _____________________________________________________________________________

#! @Arguments name, n, m
#! @Returns the <Arg>m</Arg>th algebra of order <Arg>n</Arg> from the library named <Arg>name</Arg>s.
#! (Note that the trailing "s" is omitted from <Arg>name</Arg>, e.g., the singular "Moufang loop"
#! is used as <Arg>name</Arg> rather than the plural "Moufang loops".)
#! @Description It is aso possible to access individual libraries directly, e.g., `MoufangLoop( `<Arg>n</Arg>`, `<Arg>m</Arg>` )`.
DeclareOperation( "LibraryAlgebra", [ IsString, IsPosInt, IsPosInt ] );

#! @BeginExampleSession
#! gap> LibraryAlgebra( "Moufang loop", 64, 10 );
#! <Moufang loop 64/10>
#! gap> MoufangLoop( 64, 10 );
#! <Moufang loop 64/10>
#! @EndExampleSession

# auxiliary function for the construction of various loops
DeclareGlobalFunction( "RQ_SmallestNonsquare" );
DeclareGlobalFunction( "RQ_ActivateLeftBolLoopPQ" );
DeclareGlobalFunction( "RQ_ActivateLeftBolLoop" );
DeclareGlobalFunction( "RQ_ActivateMoufangLoop" );
DeclareGlobalFunction( "RQ_ActivateSteinerLoop" );
DeclareGlobalFunction( "RQ_ActivateRCCLoop" );
DeclareGlobalFunction( "RQ_ActivateCCLoop" );
DeclareGlobalFunction( "RQ_ActivateNilpotentLoop" );
DeclareGlobalFunction( "RQ_ActivateAutomorphicLoop" );
DeclareGlobalFunction( "RQ_ActivateRightBruckLoop" );
DeclareGlobalFunction( "RQ_ActivateRackOrQuandle" );

#! @Section Library of left Bol loops and right Bol loops

#! <P/>The library named **left Bol loops** contains all nonassociative left Bol loops of order less than 17,
#! including Moufang loops, as well as all left Bol loops of order $pq$ for primes $p>q>2$.
#! There are 6 such loops of order 8, 1 of order 12, 2 of order 15, 2038 of order 16, and $(p+q-4)/2$ of order $pq$.

#! <P/>The classification of left Bol loops of order 16 was first accomplished by Moorhouse
#! <Cite Key="Mo"/>.
#! Our library was generated independently and it agrees with Moorhouse's results.
#! The left Bol loops of order $pq$ were classified in
#! <Cite Key="KiNaVo2015"/>.

#! <P/>Note: Only left Bol loops are stored in the library. Right Bol loops are retrieved by calling `OppositeLoop`
#! on left Bol loops.

#! @BeginGroup
#! @GroupTitle LeftBolLoop and RightBolLoop

#! @Arguments n, m
#! @Returns the <Arg>m</Arg>th left Bol loop (right Bol loop) of order <Arg>n</Arg>. 
DeclareOperation( "LeftBolLoop", [ IsPosInt, IsPosInt ] );

#! @Arguments n, m
DeclareOperation( "RightBolLoop", [ IsPosInt, IsPosInt ] );

#! @EndGroup

#! @Section Library of left Bruck loops and right Bruck loops

#! <P/>The library named **left Bruck loops** contains all left Bruck loops of orders
#! 3, 9, 27 and 81 (there are 1, 2, 7 and 72 such loops, respectively).

#! <P/>For an odd prime $p$, left Bruck loops of order $p^k$ are centrally nilpotent and hence
#! central extensions of the cyclic group of order $p$ by a left Bruck loop of order $p^{k-1}$.
#! It is known that left Bruck loops of order $p$ and $p^2$ are abelian groups; we have included them
#! in the library because of the iterative nature of the construction of nilpotent loops.

#! @BeginGroup
#! @GroupTitle LeftBruckLoop and RightBruckLoop

#! @Arguments n, m
#! @Returns the <Arg>m</Arg>th left Bruck loop (right Bruck loop) of order <Arg>n</Arg>. 
DeclareOperation( "LeftBruckLoop", [ IsPosInt, IsPosInt ] );

#! @Arguments n, m
DeclareOperation( "RightBruckLoop", [ IsPosInt, IsPosInt ] );

#! @EndGroup

#! @Section Library of Moufang Loops

#! The library named **Moufang loops** contains all nonassociative Moufang loops of order $n\le 64$ and $n\in\{81,243\}$.

#! <P/>For $n\le 63$, our catalog numbers coincide with those of Goodaire et al.
#! <Cite Key="Go"/>.
#! The classification of Moufang loops of order 64 and 81 was carried out in
#! <Cite Key="NaVo2007"/>.
#! The classification of Moufang loops of order 243 was carried out by Slattery and Zenisek
#! <Cite Key="SlZe2011"/>.

#! <P/>The extent of the library is summarized below:
#! $$
#! \begin{array}{r|rrrrrrrrrrrrrrrrrr}
#!        order&amp;12&amp;16&amp;20&amp;24&amp;28&amp;32&amp;36&amp;40&amp;42&amp;44&amp;48&amp;52&amp;54&amp;56&amp;60&amp;64&amp;81&amp;243\cr
#!        loops&amp;1 &amp;5 &amp;1 &amp;5 &amp;1 &amp;71&amp;4 &amp;5 &amp;1 &amp;1 &amp;51&amp;1 &amp;2 &amp;4 &amp;5 &amp;4262&amp; 5 &amp;72
#! \end{array}
#! $$

#! <P/>The **octonion loop**<Index>octonion loop</Index><Index Subkey="octonion">loop</Index> of order 16
#! (i.e., the multiplication loop of the basis elements in the 8-dimensional standard real octonion algebra)
#! can be obtained as `MoufangLoop(16,3)`.

#! @Arguments n, m
#! @Returns the <Arg>m</Arg>th Moufang loop of order <Arg>n</Arg>. 
DeclareOperation( "MoufangLoop", [ IsPosInt, IsPosInt ] );

#! @Section Library of code loops

#! <P/>The library named **code loops** contains all nonassociative code loops of order less than 65.
#! There are 5 such loops of order 16, 16 of order 32, and 80 of order 64, all Moufang.
#! The library merely points to the corresponding Moufang loops. See
#! <Cite Key="NaVo2007"/>
#! for a classification of small code loops.

#! @Arguments n, m
#! @Returns the <Arg>m</Arg>th code loop of order <Arg>n</Arg>. 
DeclareOperation( "CodeLoop", [ IsPosInt, IsPosInt ] );

#! @Section Library of Steiner loops

#! <P/>Here is how the libary named **Steiner loops** is described within &RightQuasigroups;:

#! @BeginLogSession
#! gap> DisplayLibraryInfo( "Steiner loops" );
#! The library contains all nonassociative Steiner loops of order less or equal to 16.
#! It also contains the associative Steiner loops of order 4 and 8.
#! ------
#! Extent of the library:
#!    1 loop of order 4
#!    1 loop of order 8
#!    1 loop of order 10
#!    2 loops of order 14
#!    80 loops of order 16
#! true
#! @EndLogSession

#! <P/>Our labeling of Steiner loops of order 16 coincides with the labeling of Steiner triple systems of order 15 in
#! <Cite Key="CoRo"/>.

#! @Arguments n, m
#! @Returns the <Arg>m</Arg>th Steiner loop of order <Arg>n</Arg>. 
DeclareOperation( "SteinerLoop", [ IsPosInt, IsPosInt ] );

#! @Section Library of conjugacy closed loops

#! <P/>The library named **RCC loops** contains all nonassocitive right conjugacy closed loops of order $n\le 27$
#! up to isomorphism. The data for the library was generated by Katharina Artic
#! <Cite Key="Artic"/> 
#! who can also provide additional data for all right conjugacy closed loops of order $n\le 31$.

#! <P/>Let $Q$ be a right conjugacy closed loop, $G$ its right multiplication group and $T$ its right section.
#! Then $\langle T\rangle = G$ is a transitive group, and $T$ is a union of conjugacy classes of $G$.
#! Every right conjugacy closed loop of order $n$ can therefore be represented as a union of certain conjugacy
#! classes of a transitive group of degree $n$. This is how right conjugacy closed loops of order less
#! than $28$ are represented in &RightQuasigroups;.

#! <P/>The following table summarizes the number of right conjugacy closed loops of a given order up to isomorphism:
#! $$
#! \begin{array}{r|rrrrrrrrrrrrrr}
#!         order &amp;6&amp; 8&amp;9&amp;10&amp; 12&amp;14&amp;15&amp;  16\cr
#!         loops &amp;3&amp;19&amp;5&amp;16&amp;155&amp;97&amp; 17&amp;6317\cr
#!         \hline
#!         order &amp;18&amp;  20&amp;21&amp;   22&amp;    24&amp; 25&amp;    26&amp;    27\cr
#!         loops &amp;1901&amp;8248&amp;119&amp;10487&amp;471995&amp; 119&amp;151971&amp;152701
#! \end{array}
#! $$

#! <P/>Note: Only the right conjugacy closed loops are stored in the library.
#! Left conjugacy closed loops are obtained from right conjugacy closed loops via `OppositeLoop`.

#! @BeginGroup
#! @GroupTitle Right conjugacy closed loops

#! @Arguments n, m
#! @Returns the <Arg>m</Arg>th right conjugacy closed loop of order <Arg>n</Arg>. 
DeclareOperation( "RCCLoop", [ IsPosInt, IsPosInt ] );

#! @Arguments n, m
#! @Returns the <Arg>m</Arg>th right conjugacy closed loop of order <Arg>n</Arg>. 
DeclareOperation( "RightConjugacyClosedLoop", [ IsPosInt, IsPosInt ] );

#! @EndGroup

#! @BeginGroup
#! @GroupTitle Left conjugacy closed loops

#! @Arguments n, m
#! @Returns the <Arg>m</Arg>th left conjugacy closed loop of order <Arg>n</Arg>. 
DeclareOperation( "LCCLoop", [ IsPosInt, IsPosInt ] );

#! @Arguments n, m
#! @Returns the <Arg>m</Arg>th left conjugacy closed loop of order <Arg>n</Arg>. 
DeclareOperation( "LeftConjugacyClosedLoop", [ IsPosInt, IsPosInt ] );

#! @EndGroup

#! <P/>The library named **CC loops** contains all CC loops of order $2\le 2^k\le 64$, $3\le 3^k\le 81$,
#! $5\le 5^k\le 125$, $7\le 7^k\le 343$, all nonassociative CC loops of order less than 28,
#! and all nonassociative CC loops of order $p^2$ and $2p$ for any odd prime $p$.

#! <P/>By results of Kunen
#! <Cite Key="Kun"/>,
#! for every odd prime $p$ there are precisely 3 nonassociative conjugacy closed loops of order $p^2$.
#! Cs&#246;rg&#337; and Dr&#225;pal
#! <Cite Key="CsDr"/>
#! described these 3 loops by multiplicative formulas on $\mathbb{Z}_{p^2}$ and $\mathbb{Z}_p \times \mathbb{Z}_p$ as follows:
#! <List>
#! <Item>Case $m = 1$:Let $k$ be the smallest positive integer relatively prime to $p$ and such that $k$
#! is a square modulo $p$ (i.e., $k=1$). Define multiplication on $\mathbb{Z}_{p^2}$ by $x\cdot y = x + y + kpx^2y$.</Item>
#! <Item>Case $m = 2$: Let $k$ be the smallest positive integer relatively prime to $p$ and such that $k$
#! is not a square modulo $p$. Define multiplication on $\mathbb{Z}_{p^2}$ by $x\cdot y = x + y + kpx^2y$.</Item>
#! <Item>Case $m = 3$: Define multiplication on $\mathbb{Z}_p \times \mathbb{Z}_p$ by $(x,a)(y,b) = (x+y, a+b+x^2y )$.</Item>
#! </List>

#! <P/>Moreover, Wilson
#! <Cite Key="Wi"/>
#! constructed a nonassociative conjugacy closed loop of order $2p$ for every odd prime $p$, and Kunen
#! <Cite Key="Kun"/>
#! showed that there are no other nonassociative conjugacy closed oops of this order.
#! Here is the relevant multiplication formula on $\mathbb{Z}_2 \times \mathbb{Z}_p$:
#! $(0,m)(0,n) = ( 0, m + n )$, $(0,m)(1,n) = ( 1, -m + n )$, $(1,m)(0,n) = ( 1, m + n)$, $(1,m)(1,n) = ( 0, 1 - m + n )$.

#! @BeginGroup
#! @GroupTitle Conjugacy closed loops

#! @Arguments n, m
#! @Returns the <Arg>m</Arg>th conjugacy closed loop of order <Arg>n</Arg>. 
DeclareOperation( "CCLoop", [ IsPosInt, IsPosInt ] );

#! @Arguments n, m
#! @Returns the <Arg>m</Arg>th conjugacy closed loop of order <Arg>n</Arg>. 
DeclareOperation( "ConjugacyClosedLoop", [ IsPosInt, IsPosInt ] );

#! @EndGroup

#! @Section Library of small loops

#! <P/>The library named **small loops** contains all nonassociative loops of order 5 and 6.
#! There are 5 and 107 such loops, respectively.

#! @Arguments n, m
#! @Returns the <Arg>m</Arg>th small loop of order <Arg>n</Arg>. 
DeclareOperation( "SmallLoop", [ IsPosInt, IsPosInt ] );

#! @Section Library of Paige loops

#! <P/>**Paige loops**<Index>Paige loop</Index><Index Subkey="Paige">loop</Index> are nonassociative finite
#! simple Moufang loops. Liebeck showed 
#! <Cite Key="Li"/>
#! that there is precisely one Paige loop for every finite field.

#! <P/>The library named **Paige loops** contains a single loop, the smallest nonassociative simple Moufang loop of order 120
#! over the 2-element field.

#! @Arguments q
#! @Returns the Paige loop over the finite field of order <Arg>q</Arg>. Only the case <Arg>q</Arg>`=2` is implemented.
DeclareOperation( "PaigeLoop", [ IsPosInt ] );

#! @Section Library of nilpotent loops

#! <P/>The library named **nilpotent loops** contains all nonassociative nilpotent loops of order less than
#! 12 up to isomorphism. There are 2 nonassociative nilpotent loops of order 6, 134 of order 8,
#! 8 of order 9 and 1043 of order 10.

#! <P/>See
#! <Cite Key="DaVo"/>
#! for more on enumeration of nilpotent loops. For instance, there
#! are 2623755 nilpotent loops of order 12, and 123794003928541545927226368 nilpotent loops of order 22.

#! @Arguments n, m
#! @Returns the <Arg>m</Arg>th nilpotent loop of order <Arg>n</Arg>. 
DeclareOperation( "NilpotentLoop", [ IsPosInt, IsPosInt ] );

#! @Section Library of automorphic loops

#! <P/>The library named **automorphic loops** contains all nonassociative automorphic loops of order
#! less than 16 up to isomorphism (there is 1 such loop of order 6, 7 of order 8, 3 of order 10,
#! 2 of order 12, 5 of order 14, and 2 of order 15) and all commutative automorphic loops of order
#! 3, 9, 27 and 81 (there are 1, 2, 7 and 72 such loops).

#! <P/>It turns out that commutative automorphic loops of order 3, 9, 27 and 81 (but not 243)
#! are in one-to-on correspondence with left Bruck loops of the respective orders, see
#! <Cite Key="Greer"/>, <Cite Key="StuhlVojtechovsky"/>.
#! Only the left Bruck loops are stored in the library.

#! @Arguments n, m
#! @Returns the <Arg>m</Arg>th automorphic loop of order <Arg>n</Arg>. 
DeclareOperation( "AutomorphicLoop", [ IsPosInt, IsPosInt ] );

#! @Section Library of interesting loops

#! <P/>The library named **interesting loops** contains some loops that are illustrative in the theory of loops.
#! At this point, the library contains a nonassociative loop of order 5, a nonassociative nilpotent loop
#! of order 6, a non-Moufang left Bol loop of order 16, the loop of
#! sedenions<Index>sedenion loop</Index><Index Subkey="sedenion">loop</Index> of order 32
#! (sedenions generalize octonions), and the unique nonassociative simple right Bol loop of order 96 and exponent 2.

#! @Arguments n, m
#! @Returns the <Arg>m</Arg>th interesting loop of order <Arg>n</Arg>. 
DeclareOperation( "InterestingLoop", [ IsPosInt, IsPosInt ] );

#! @Section Library of small racks

#! <P/>The library named **small racks** contains all racks of order less than 11 up to isomorphism. There are
#! 1, 2, 6, 19, 74, 353, 2080, 16023, 159526, 2093244 such racks, respectively. (The 36265070 racks of order
#! 11 are available from the second author upon request.)

#! @Arguments n, m 
#! @Returns the <Arg>m</Arg>th rack of order <Arg>n</Arg>.
DeclareOperation( "SmallRack", [ IsPosInt, IsPosInt ] );

#! @Section Library of small quandles

#! <P/>The library named **small quandles** contains all quandles of order less than 12 up to isomorphism. There are
#! 1, 1, 3, 7, 22, 73, 298, 1581, 11079, 102771, 1275419 such racks, respectively. (The 21101335) quandles of order
#! 12 are available from the second author by request.)

#! @Arguments n, m 
#! @Returns the <Arg>m</Arg>th quandle of order <Arg>n</Arg>.
DeclareOperation( "SmallQuandle", [ IsPosInt, IsPosInt ] );

#! @Section Library of connected quandles

#! <P/>The library name **connected quandles** contains all connected quandles of order less than 48 up to isomorphism.
#! A quandle `Q` is connected if its right multiplication group acts transitively on `Q`.

#! @Arguments n, m
#! @Returns the <Arg>m</Arg>th connected quandle of order <Arg>n</Arg>.
DeclareOperation( "ConnectedQuandle", [ IsPosInt, IsPosInt ] );

#! @Section Library of small loops up to isotopism

#! <P/>For the library named **small loops** we also provide the corresponding library of loops up to isotopism. There is
#! 1 nonassociative loop of order 5 and 20 nonassociative loops of order 6 up to isotopism.

#! <P/>In general, given a library named **name**, the corresponding library of loops up to isotopism is named **itp name**,
#! and the loops can be retrieved by a function with the prefix `Itp`.

#! @BeginLogSession
#! gap> SmallLoop( 6, 14 );
#! <small loop 6/14>
#! gap> ItpSmallLoop( 6, 14 );
#! <small loop 6/42>
#! gap> LibraryAlgebra( "itp small loop", 6, 14 );
#! <small loop 6/42>
#! @EndLogSession

#!<P/>Note that loops up to isotopism form a subset of the corresponding library of loops up to isomorphism.
#! For instance, the above example shows that the 14th small loop of order 6 up to isotopism is in fact the
#! 42nd small loop of order 6 up to isomorphism.

#! @Arguments n, m
#! @Returns the <Arg>m</Arg>th small loop of order <Arg>n</Arg> up to isotopism.
DeclareOperation( "ItpSmallLoop", [ IsPosInt, IsPosInt ] );