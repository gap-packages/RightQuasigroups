# The GAP package RightQuasigroups

* Website: coming soon
* Repository: https://github.com/gap-packages/RightQuasigroups

This package implements basic methods for calculations with finite,
right quasigroups. Additional methods and libraries are provided,
for quasigroups, loops, racks and quandles.

## Installation

### Install via gzipped archive

- Download the latest release from the [Github site of the package](https://github.com/gap-packages/loops/releases).
- Extract it in the directory `GAPDIR/pkg`. 

### Install via git

Change to the `GAPDIR/pkg` directory and 

```bash
git clone https://github.com/gap-packages/RightQuasigroups.git
```
### Suggested package **Digraphs**

- Computation of isomorphisms and isotopisms of right quasigroups can be speeded up by using the associated colored digraph. 
- The GAP package [Digraphs](https://digraphs.github.io/Digraphs/) is attempted to load with **RightQuasigroups**, but **RightQuasigroups** is loaded even when **Digraphs** is not available. 
- As **Digraphs** requires compiled kernel modules, its installation needs slightly more attention.


## Usage

```
gap> LoadPackage( "RightQuasigroups" );
```

## Maintainers

* [Gábor P. Nagy](https://algebra.math.bme.hu/nagy-gabor-peter), Budapest University of Technology and Economics, Hungary.
* [Petr Vojtěchovský](http://www.math.du.edu/~petr/), University of Denver, Colorado, USA.

For questions, remarks and issues please use the [issue tracker](https://github.com/gap-packages/RightQuasigroups/issues).


## License

**RightQuasigroups** is free software; you can redistribute it and/or modify it under the terms of Version 2 of the GNU General Public License. For details see the file LICENSE.
