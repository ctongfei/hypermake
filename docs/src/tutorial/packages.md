# Packages

In Hypermake, packages are special tasks that builds a software package. 
They can depends on other packages but not tasks, and will be built differently on different environments (see next tutorial).

A package is defined as follows:
```
package $packageName -> $packageOutputName:
  # build script
```
For example, let's build [`trec_eval`](https://github.com/usnistgov/trec_eval) (a standard information retrieval evaluation toolkit from NIST)
from its C source code:
```
package trec_eval -> out:
  mkdir -p $out
  git clone https://github.com/usnistgov/trec_eval $out
  cd $out
  make
```
Here we clone the repository into a HyperMake-managed directory `$out`, and then run `make` to build the package. The binary will be built in `$out`.

To refer to this  package output, use `$trec_eval` (there is no need to specify `$trec_eval.out`). 
For example, if an evaluation task requires this package, one can write
```
task eval(trec_eval=$, pred=$, gold=$) -> out:
  $trec_eval/trec_eval $gold $pred > $out
```

#### Example 1: Copying a package from a local directory
```bash
package pack1 -> out:
  ln -s $localDir $out
```
This behavior can be written as 
```bash
import std
package pack1 = std.symlink(path=$localDir)
```

#### Example 2: Cloning from a remote repository
```bash
package pack2 -> out:
  git clone $repo out
```

#### Example 3: Call its Makefile after cloning from a repo
```bash
package pack3(repo=$) -> out:
  git clone $repo out
  cd out
  make
```

#### Example 4: Creates a Conda environment from a Python package
```bash
package pack4(pythonPackage=$) -> out:
  mkdir -p $out
  conda env create -p $out -f $pythonPackage/environment.yml
```
