# There is a bug in AutoDoc: It scans files in alphabetical order
# and produces documentation correspondingly, i.e., not necessarily in the order intended.

# As a workaround, we include this file (which must be alphabetically first among all scanned files
# with documentaion) in which chapters and sections of the documentation are introduced in the intended order,
# and where some opening sentences of chapters etc can be found.

#! @Chapter Introduction

#! <P/>&RightQuasigroups; is a package for &GAP; that supports calculations with finite
#! right quasigroups, quasigroups, loops and various varieties of right quasigroups, such as racks and quandles.

#! <P/>First time users should read Section <Ref Sect="Section_MainFeatures"/>, skim the rest of this chapter
#! and look inside Chapter <Ref Chap="Chapter_Constructors"/> for examples on how right quasigroups are constructed.

#! <P/>Most functions are intuitively named and can be traced in the manual from the index.

#! <P/>Sections and subsections marked by * are for users who wish to understand the inner workings of the package.

#! @Section Main features of the package
#! @SectionLabel MainFeatures

#! <P/>**Representation**<Br/>
#! Right quasigroups, quasigroups and loops are represented as a subcategory of magmas in &GAP;.
#! See Section <Ref Sect="Section_Categories"/> for more technical details and Section <Ref Sect="Section_Displaying"/>
#! for how right quasigroups and their elements are displayed.

#! <P/>**Underlying set**<Br/>
#! Every right quasigroup has &GAP; elements and also has an underlying set.
#! The underlying set is used to display &GAP; elements nicely and in Cayley tables. Furthermore,
#! the underlying set plays an important role in right quasigroups that are not index based (see below)
#! because the multiplication operation is then based on the underlying set.
#! For index based right quasigroups, the underlying set is cosmetic and can be changed.
#! See Section <Ref Sect="Section_UnderlyingSet"/>.

#! <P/>**Arithmetic operations**<Br/>
#! Right quasigroups come equipped with the arithmetic operations of multiplication and right division.
#! Quasigroups have additionally a left division operation, and loops have also a neutral element with respect
#! to multiplication. See Section <Ref Sect="Section_Elementwise"/>.

#! <P/>**Index based and non-index based right quasigroups**<Br/>
#! Every right quasigroup is constructed either as an <Index>index based right quasigroup</Index>
#! **index based** right quasigroup or as a right quasigroup that is not index based. Generally speaking,
#! index based right quasigroups take longer to construct, cannot be very large (thousands of
#! elements) and can be calculated with fast, while non-index based right quasigroups are constructed quickly,
#! can be very large (millions of elements) but only basic methods will work for them. The fundamental piece
#! of data for index based right quasigroups is the multiplication table, while for non-index based
#! right quasigroups it is the multiplication function on the underlying set. See Section
#! <Ref Sect="Section_IndexBased"/>.

#! <P/>Most constructors accept an optional argument that determines whether the resulting right
#! quasigroup will be index based and whether arguments will be checked. By default, the resulting right quasigroups
#! will be index based and arguments will not be checked (!). See Chapter <Ref Chap="Chapter_Constructors"/>.

#! <P/>**Parent**<Br/>
#! The parent mechanism is employed in &GAP; and in &RightQuasigroups; to save memory and to take advantage of
#! the containment of subalgebras in their enveloping algebras. An element of a right quasigroup `Q` knows into
#! which parent right quasigroup it belongs and it therefore has access to all data stored in the parent quasigroup.
#! See Sections <Ref Sect="Section_Parent"/> and <Ref Sect="Section_Direct"/>.

#! <P/>**Mappings**<Br/>
#! &RightQuasigroups; uses three kinds of mappings: &GAP; mappings, transformations, and
#! permutations. Transformations that represent mappings between two right quasigroups, as well as permutations
#! that represent bijective mappings on a right quasigroup, are indexed either with respect to the order of elements in the
#! source and range quasigroups (so called **canonical tranformations** and **canonical permutations**)
#! or with respect to their parents (so called **parent tranformations** and **parent permutations**).
#! See Chapter <Ref Chap="Chapter_Mappings"/>.

#! <P/>The package works with parent permutations as much as possible, cf. Chapter <Ref Chap="Chapter_PermGroups"/>.
#! But canonical permutations are useful in the context of multiplication tables, and transformations are useful for left translations
#! of right quasigroups, for instance. 

#! <P/>**Info class**<Br/>
#! The info class for the package is called `InfoRightQuasigroups` and its initial value is set to 1,
#! which will only print information on tasks that are assumed to take a long time to execute, such as reading
#! and initializing a large data file. The user can prevent all messages from &RightQuasigroups; by calling
#! `SetInfoLevel( InfoRightQuasigroups, 0 )`. On the other hand, setting the info level higher for
#! `InfoRightQuasigroups` might result in additional messages.

#! <P/>**Global variables**<Br/>
#! Finally, global variables and auxiliary functions in &RightQuasigroups; start with the prefix `RQ_`
#! and they are not fully documented in this manual. More information on these functions
#! can be found in the declaration files `gap\*.gd`.

#! @Section Categories and representations for right quasigroups, quasigroups and loops
#! @SectionLabel Categories

#! @Section Displaying right quasigroups and their elements
#! @SectionLabel Displaying

#! @Section The underlying set
#! @SectionLabel UnderlyingSet

#! @Section Accessing elements and elementwise right quasigroup operations
#! @SectionLabel Elementwise

#! @Section First examples
#! @SectionLabel Examples

#! @Section The parent right quasigroup
#! @SectionLabel Parent

#! @Section Index based and canonical right quasigroups
#! @SectionLabel IndexBased

#! @Section Generators and comparison of right quasigroups
#! @SectionLabel Generators

#! @Section Non-qualified operations
#! @SectionLabel Nonqualified

#! @Section *Direct access to the parent right quasigroup record
#! @SectionLabel Direct



#! @Chapter Constructors
#! @ChapterLabel Constructors

#! <P/>In this chapter we collect constructor methods for right quasigroups.

#! @Section Optional arguments in constructors
#! @SectionLabel OptionalArguments

#! @Section Converting betwen right quasigroups, quasigroups and loops
#! @SectionLabel Converting

#! @Section *A two-step constructor

#! @Section Constructors by Cayley table
#! @SectionLabel Cayley

#! @Section Constructors by function
#! @SectionLabel Function

#! @Section Constructors by right section

#! @Section Constructors by right folder (transversal)

#! @Section Constructors for special types of right quasigroups

#! @Section Random right quasigroups, quasigroups and loops



#! @Chapter Direct products, subalgebras and factor algebras

#! <P/>In this chapter we describe methods of universal-algebraic flavor for right quasigroups.

#! @Section Direct product of right quasigroups

#! @Section Opposite quasigroups

#! @Section Subalgebras

#! @Section Right cosets and transversals

#! @Section Right quasigroups by generators
#! @SectionLabel ByGenerators

#! @Section Intersections and joins of right quasigroups

#! @Section Congruences

#! @Section Normality and simplicity

#! @Section Factor algebras

#! @Section An example of the factor construction: Paige loops



#! @Chapter Mappings, transformations and permutations of right quasigroups
#! @ChapterLabel Mappings

#! @Section Mappings, transformations and permutations
#! @SectionLabel MappingsIntro

#! @Section Right quasigroup mapppings

#! @Section Canonical permutations and parent permutations

#! @Section Canonical transformations and parent transformations



#! @Chapter Associated permutation groups
#! @ChapterLabel PermGroups

#! @Section Translations and sections
#! @SectionLabel Translations

#! @Section Multiplication groups and relative multiplication groups
#! @SectionLabel Mlt

#! @Section Inner mappings and inner mapping groups

#! @Section Displacement groups

#! @Section Realizing permutation groups as multiplication groups of loops



#! @Chapter Nilpotency and solvability

#! @Section Nuclei, commutant and center

#! @Section Nilpotency for loops

#! @Section Solvability and congruence solvability for loops

#! @Section Frattini subloop

#! @Section Nuclear and central extensions of loops

#! @Section All central extensions in a variety of loops



#! @Chapter Properties of right quasigroups, quasigroups and loops

#! <P/>In this chapter we present functions that check various properties of right quasigroups,
#! quasigroups and loops, some on the level of magmas.
#! Any identity of right quasigroups, quasigroups and loops can be verified
#! or refuted with the custom-built parser. Specific (and typically faster) methods are provided
#! for many identities and properties of right quasigroups, and we recommend using them,
#! rather than the generic parser method, whenever possible.

#! @Section Parser
#! @SectionLabel Parser

#! @Section Properties of magmas

#! @Section Properties of right quasigroups

#! @Section Properties of quasigroups

#! @Section Inverse properties of loops

#! @Section Loops of Bol-Moufang type

#! @Section Power alternative loops

#! @Section Conjugacy closed loops and Osborn loops

#! @Section Additional varieties of loops



#! @Chapter Homomorphisms, isomorphisms and automorphisms
#! @ChapterLabel Morphisms

#! @Section Homomorphisms, isomorphisms and automorphisms of right quasigroups
#! @SectionLabel Homomorphisms

#! @Section Isomorphs of right quasigroups
#! @SectionLabel Isomorphs

#! @Section Right quasigroups up to isomorphisms

#! @Section Automorphism groups of right quasigroups



#! @Chapter Homotopisms, isotopisms and autotopisms
#! @ChapterLabel Topisms

#! @Section Homotopisms, isotopism and autotopisms of right quasigroups

#! @Section Twists of right quasigroups
#! @SectionLabel Twists

#! @Section Isotopes of right quasigroups

#! @Section Affine right quasigroups
#! @SectionLabel Affine

#! @Section Right quasigroups up to isotopism

#! @Section Autotopism groups of right quasigroups



#! @Chapter Bol loops, Bruck loops and Moufang loops

#! @Section Bol loops and Bruck loops

#! @Section Triality for Moufang loops

#! @Section Quarter modifications of Moufang loops



#! @Chapter Racks and quandles

#! <P/>A **rack**<Index>rack</Index> is a right quasigroup satisfying the right self-distributive law
#! $(x*y)*z = (x*z)*(y*z)$. A **quandle**<Index>quandle</Index> is an idempotent rack.

#! <P/>A rack $Q$ is **homogeneous**<Index Subkey="homogeneous">rack</Index>
#! if its automorphism group acts transitively on $Q$.

#! <P/>A rack $Q$ is **connected**<Index Subkey="connected">rack</Index>
#! if its right multiplication group acts transitively on $Q$.

#! <P/>A rack is **latin**<Index Subkey="latin">rack</Index> if it is a quasigroup. A latin rack is automatically a (latin) quandle.

#! <P/>A latin rack is connected and a connected rack is homogeneous.

#! <P/>Constructors of &RightQuasigroups; declare racks and quandles as right quasigroups or, in the latin case, as quasigroups.
#! If a rack/quandle is declared as a right quasigroup, it is displayed as `&lt;rack...&gt;` or `&lt;quandle...&gt;`,
#! while if it is declared as a quasigroup, it is displayed as `&lt;latin rack...&gt;` or `&lt;latin quandle...&gt;`.

#! @Section Testing properties of racks and quandles

#! @Section Constructors for racks

#! @Section Constructors for quandles

#! @Section Rack envelopes and quandle envelopes: The Joyce-Blackburn representation

#! @Section Subracks and subquandles

#! @Section Isomorphisms and isotopisms of racks and quandles



#! @Chapter Libraries of loops, racks and quandles

#! <P/>Libraries of small loops, racks an quandles form an integral part of &RightQuasigroups;.
#! The algebras are stored in libraries up to isomorphism and, occasionally, up to isotopism.

#! @Section *A typical library

#! @Section Accessing algebras in the library

#! @Section Library of left Bol loops and right Bol loops

#! @Section Library of left Bruck loops and right Bruck loops

#! @Section Library of Moufang Loops

#! @Section Library of code loops

#! @Section Library of Steiner loops

#! @Section Library of conjugacy closed loops

#! @Section Library of small loops

#! @Section Library of Paige loops

#! @Section Library of nilpotent loops

#! @Section Library of automorphic loops

#! @Section Library of interesting Loops

#! @Section Library of small racks

#! @Section Library of small quandles



#! @Section Library of small loops up to isotopism