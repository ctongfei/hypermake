# Installation

### Via Homebrew

HyperMake can be installed via [`brew`](https://brew.sh) with a custom tap.
```shell
brew tap ctongfei/repo
brew install --HEAD ctongfei/repo/hypermake
```
Right now the Homebrew formula is configured to use the `HEAD` version of HyperMake, which is the latest version on the `main` branch. To reinstall if the latest version changed, do
```shell
brew reinstall ctongfei/repo/hypermake
```

### Build from source

HyperMake can also be directly built from source. It requires [`sbt`](https://www.scala-sbt.org) to build.

```shell
git clone https://github.com/ctongfei/hypermake
cd hypermake

make
make install
```
This will build the HyperMake binary and install it locally to `$HOME/.local/bin/hypermake`.
To install it elsewhere, simply modify the `$PREFIX` variable in the `Makefile`.
