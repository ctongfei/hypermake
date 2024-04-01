# HyperMake

`HyperMake` is a parameterized workflow manager (think of a `make` where tasks can be parameterized) heavily inspired by [Ducttape](https://github.com/jhclark/ducttape).

It supports the following features:

 - **Parameterization of tasks:** a task can be of multiple versions (e.g. in a ML pipeline, a task can be of different hyperparameters)
 - **Minimal juggling**: Inputs and outputs are just files/symlinks, and arguments are all passed as environment variables
 - **Automatic parallelization:** based on the dependency DAG
 - **Cloud support:** tasks can be easily be decorated run on cloud (e.g. AWS, Azure)

