#############################################################################
##
#W  elements.gd  Elements and basic arithmetic operations [loops]
##
##
#Y  Copyright (C)  2004,  G. P. Nagy (University of Szeged, Hungary),
#Y                        P. Vojtechovsky (University of Denver, USA)
##

#############################################################################
##  DIVISION
##  -------------------------------------------------------------------------

DeclareOperation( "RightDivision",
    [ IsRightQuasigroupElement, IsRightQuasigroupElement ] );
DeclareOperation( "RightDivisionCayleyTable", [ IsRightQuasigroup ] );

#############################################################################
##  ASSOCIATORS AND COMMUTATORS
##  -------------------------------------------------------------------------

DeclareOperation( "RightAssociator",
    [ IsRightQuasigroupElement, IsRightQuasigroupElement, IsRightQuasigroupElement ] );
DeclareOperation( "RightCommutator",
    [ IsRightQuasigroupElement, IsRightQuasigroupElement ] );
