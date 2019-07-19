#
# RightQuasigroups: Computing with one-sided quasigroups in GAP.
#
#! @Chapter Introduction
#!
#! RightQuasigroups is a package which does some
#! interesting and cool things
#!
#! @Chapter Functionality
#!
#! @Section Construction Filters
#! This section will describe the construction 
#! filters of right quasigroups.
#!
#! @Section Construction Methods
#!
#! This section will describe the construction 
#! methods of right quasigroups.

#############################################################################
##  GAP CATEGORIES AND REPRESENTATIONS
##  -------------------------------------------------------------------------

## element of a quasigroup
#! @ChapterInfo Functionality, Construction Filters
#! @Arguments object
#! @Description
#!   A &GAP; category of elements of right quasigroups.
DeclareCategory( "IsRightQuasigroupElement", IsMultiplicativeElement );

DeclareRepresentation( "IsRightQuasigroupElmRep",
    IsPositionalObjectRep and IsMultiplicativeElement, [1] );

#! @Arguments object
#! @Description
#!   An auxiliary category for &GAP; to tell apart <C>IsMagma</C> and <C>IsRightQuasigroup</C>.
DeclareCategory( "IsRightQuasigroupMagma", IsObject );

#! @Arguments object
#! @Description
#!   A &GAP; category of right quasigroups.
DeclareCategory( "IsRightQuasigroup", IsMagma and IsRightQuasigroupMagma );

#############################################################################
##  CREATING RIGHT QUASIGROUPS BY FUNCTIONS
##  -------------------------------------------------------------------------

#! @ChapterInfo Functionality, Construction Methods

#! @Arguments set
#! @Returns
#!   The projection right quasigroup on the set <A>set</A>
#! @Description
#!   The operation is defined by <M>x*y=x</M>.
DeclareOperation( "ProjectionRightQuasigroup", [ IsCollection ] );

DeclareOperation( "RightQuasigroupByFunctionsNC", [ IsCollection, IsFunction, IsFunction, IsBool ] );

#! @Arguments set, f, g
#! @Returns
#!   The right quasigroup <M>(Q,*,/)</M>, where <M>Q</M> is
#!   indexed by <A>set</A> and <M>x*y=f(x,y)</M> and 
#!   <M>x/y = g(x,y)</M>.
#! @Description
#!   The identities <M>f(g(x,y),y)=x</M> and <M>g(f(x,y),y)=x</M> must hold.
DeclareOperation( "RightQuasigroupByFunctions", [ IsCollection, IsFunction, IsFunction ] );

#! @Arguments set, f, g
#! @Returns
#!   The right quasigroup <M>(Q,*,/)</M>, where <M>Q</M> is
#!   indexed by <A>set</A> and <M>x*y=f(x,y)</M> and 
#!   <M>x/y = z</M> iff <M>z=f(x,y)</M>.
#! @Description
#!   For any <M>x,z</M> there must be a unique <M>y</M> such that <M>f(x,y)=z</M>.
DeclareOperation( "RightQuasigroupByFunction", [ IsCollection, IsFunction ] );

DeclareOperation( "SetRightQuasigroupElmName", [ IsRightQuasigroup, IsString ] );

#############################################################################
##  CREATING RIGHT QUASIGROUPS BY TABLES
##  -------------------------------------------------------------------------

DeclareAttribute( "CayleyTable", IsRightQuasigroup );
DeclareAttribute( "MultiplicationTable", IsRightQuasigroup );
DeclareAttribute( "RightDivisonTable", IsRightQuasigroup );

#############################################################################
##  TRANSLATIONS AND SECTIONS IN RIGHT QUASIGROUPS
##  -------------------------------------------------------------------------

DeclareOperation( "PosInParent", [ IsRightQuasigroupElement ] );
DeclareOperation( "RightTranslation", [ IsRightQuasigroup, IsRightQuasigroupElement ] );
DeclareAttribute ("RightSection", IsRightQuasigroup );
DeclareAttribute( "RightMultiplicationGroup", IsRightQuasigroup );
DeclareOperation( "RelativeRightMultiplicationGroup", [ IsRightQuasigroup, IsRightQuasigroup ] );

#############################################################################
##  CREATING SUBRIGHT QUASIGROUPS
##  -------------------------------------------------------------------------

DeclareOperation( "SubrightQuasigroupNC", [ IsRightQuasigroup, IsCollection ] );
DeclareOperation( "SubrightQuasigroup", [ IsRightQuasigroup, IsCollection ] );
DeclareOperation( "SubrightQuasigroup", [ IsRightQuasigroup, IsCollection, IsBool ] );
DeclareOperation( "IsSubrightQuasigroup", [ IsRightQuasigroup, IsRightQuasigroup ] );

#############################################################################
##  CREATING RIGHT QUASIGROUPS FROM A FILE
##  -------------------------------------------------------------------------

#############################################################################
##  CREATING RIGHT QUASIGROUPS BY SECTIONS AND TRANSVERSALS
##  -------------------------------------------------------------------------

DeclareOperation( "RightQuasigroupBySection", [ IsCollection, IsCollection ] );
DeclareOperation( "RightQuasigroupByCayleyTable", [ IsRectangularTable ] );
#DeclareOperation( "RightQuasigroupByFolder", [ IsGroup, IsGroup, IsMultiplicativeElementCollection ] );

#############################################################################
##  CONSTRUCTIONS
##  -------------------------------------------------------------------------

DeclareOperation( "IntoRightQuasigroup", [ IsMagma ] );
DeclareOperation( "RightCoreOfGroup", [ IsGroup ] );
