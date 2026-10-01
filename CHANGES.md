This file describes changes in the RightQuasigroups package.

## Unreleased

- Declare the needed utils package, which provides `LeftCosetsNC`

## 0.9.12 (2025-11-20)

- Add `IsJordanLoop`
- Fix `RightQuasigroupsUpToIsomorphism`, `QuasigroupsUpToIsomorphism` and
  `LoopsUpToIsomorphism` for lists of length 0 or 1
- Speed up generating subalgebras

## 0.9.1 (2023-06-28)

- Fix `IsDiassociative` calling `RQ_Subalgebra` with wrong arguments
- Fix `UpperCentralSeriesOfLoop` returning a wrong result
- Fix `LoopCoboundaries` for trivial loops and `LoopCocyclesModAction` when
  there are only coboundaries, and fix the examples for the latter

## 0.9 (2023-06-22)

- Rewrite `RightQuasigroupsUpToIsomorphism`; `IsomorphismDiscriminator` is
  now an attribute
- Fix method selection in `RightQuasigroupsUpToIsotopism` and
  `QuasigroupsUpToIsotopism`

## 0.9-beta (2023-06-16)

- Use the suggested package Digraphs to compute isomorphisms and isotopisms
  via canonical labelings, and document nice monomorphisms and their colored
  digraphs

## 0.871 (2023-05-15)

- Let `AutotopismGroup` accept right quasigroups, not only loops
- Let `RightBolLoopByExactGroupFactorization` accept two permutation groups
- Add `IsGeneratorsOfMagmaWithInverses` for collections of homotopisms
- Fix the check in `Q[x]` that `x` lies in the underlying set of `Q`
- Stop declaring a global function `LeftCosets`

## 0.8 (2021-02-15)

- Load the libraries only when needed
- Add a library of connected quandles
- Add mappings and homomorphisms between right quasigroups, quasigroups and
  loops, such as `LoopHomomorphismByImages`
- Add affine right quasigroups and affine quasigroups
- Add `RMltGr`, `RelativeRMltGr`, `IsSubrightQuasigroup`,
  `IntoRightQuasigroup` and actions by permutations
- Rename `SubrightQuasigroup` to `Subrightquasigroup`

## 0.1 (2019-07-17)
