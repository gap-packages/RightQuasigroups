# Topisms in RQ: 
 
- internally the homotopisms are represented by three permutations acting on the parent indices 
   (this is how permutations act by default)
- affine RQs are defined here, using twists
- twists are (not proper) action of triples on RQs
- homotopisms are not homotopisms:
```
gap> Q1 := ProjectionRightQuasigroup( 3 );;
gap> Q2 := ProjectionRightQuasigroup( 2 );;
gap> f := Transformation( [1,1,2] );; g := Transformation( [2,1,2] );; h := f;;
gap> t := HomotopismRightQuasigroups( Q1, Q2, f, g, h );
<homotopism of right quasigroups>
gap> KnownAttributesOfObject(t);
[  ]
gap> IsRightQuasigroupHomotopism(t);
true
gap> IsHomotopismRightQuasigroups(t);
Error, no method found! ...
```
- for homomorphisms, similar role with `RespectsMultiplication`
- `AutomorphismGroup` returns permutations acting on the parent indices
- `AutotopismGroup` returns homotopism objects
- `AsParentPerm` returns the action on the parent indices

# Methods and method selections

## UptoIsomorphisms

### RQ_AlgebrasUpToIsomorphism

- Uses `RQ_IsomorphismAlgebrasWithPrecalculatedData`, which uses `IsomorphismDiscriminator`
- `IsomorphismDiscriminator` is large, not a stored attribute
- The specific methods for lists (one argument) only invoke 
  * `RightQuasigroupsUpToIsomorphism`
  * `QuasigroupsUpToIsomorphism`
  * `LoopsUpToIsomorphism`

## UptoIsotopisms

### RQ_AlgebrasUpToIsotopism