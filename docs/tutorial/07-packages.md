# Packages

In Hypermake, packages are special tasks that builds a software package. 
They can depends on other packages but not tasks, and will be built differently on different environments (see next tutorial).

A package is defined as follows:
```
package <package name> -> <package output dir name>:
  <build script>
```
And to refer to a package, just use `$<package name>`. For example, if a task requires a package `mypackage`, one can write
```
task mytask(mypackage=$, ...) -> out:
  PYTHONPATH=$mypackage \
    python $mypackage/a/b/c.py --out=$out
```

#### Example 1: Copying a package from a local directory
```
package pack1 -> out:
  ln -s $localDir out
```

#### Example 2: Cloning from a remote repository
```
package pack2 -> out:
  git clone $repo out
```

#### Example 3: Call its Makefile after cloning from a repo
```
package pack3(repo=$) -> out:
  git clone $repo out
  cd out
  make
```

#### Example 4: Creates a Conda environment from a Python package
```
package pack4(pythonPackage=$) -> out:
  mkdir -p $out
  conda env create -p $out -f $pythonPackage/environment.yml
```
