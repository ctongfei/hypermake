# Packages

Our previous sketch requires some packages to be built. 
 - A `conda` package that contains a bunch of Python libraries (mainly [Pyserini](https://github.com/castorini/pyserini)) to run BM25 search;
 - The NIST [`trec_eval`](https://github.com/usnistgov/trec_eval) package to evaluate the retrieval results.

We will define these packages in HyperMake and let them be part of the whole pipeline, 
so when a user runs the pipeline, the packages will be built and installed automatically.

A package in HyperMake is defined with the `package` keyword, and it is a special kind of task.

### Creating a Conda package
```python
package pyserini -> out:
  mkdir -p $out
  conda create -y \
    -p $out \
    -c conda-forge \
    python=3.10 openjdk=21
  $out/bin/pip install torch faiss-cpu pyserini
```

We declared a package named `pyserini` that when building, creates a new Conda environment with Python 3.10 and OpenJDK 21, and installs Pyserini in it. Note that we build the package in a HyperMake-managed, separate directory `$out` (with the `-p`/`--prefix` directive of Conda) instead of a global Conda environment.

This is common so that HyperMake provided standard library subroutines to make this easier:
```python
import conda
package pyserini = conda.create(
    packages="python=3.10 openjdk=21",
    extra_args="-c conda-forge",
    extra_pip_packages="torch faiss-cpu pyserini"
)
```

### Building the `trec_eval` package
`trec_eval` is a C package built with Make. We can define a package for it as well:
```python
package trec_eval -> out:
  git clone https://github.com/usnistgov/trec_eval.git $out
  cd $out
  make
```

> In HyperMake, ***a package must have exactly 1 output***: the built package directory. To refer to the output directory, directly use the package name as a variable (e.g. `$pyserini`, `$trec_eval` here).

> What is exactly the difference between a package and a task?
> When a HyperMake pipeline is defined across multiple file systems (e.g. local, AWS, SSH, etc.), 
> a task is only run once and transferred between file systems, while a package is separately built on each file system.
