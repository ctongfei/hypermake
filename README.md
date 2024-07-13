# HyperMake

`HyperMake` is a parameterized pipeline definition language (think of a `make` where tasks can be parameterized) heavily inspired by [Ducttape](https://github.com/jhclark/ducttape).


 - **Shell scripting**: Write tasks in plain Bash, just like `make`. No Python or YAML for defining tasks.
 - **Cached intermediate results**: Intermediate results are cached, so that if a task fails, it can be re-run from the last successful task.
 - **Parameterization of tasks:** a task can be of multiple versions (e.g. in a ML pipeline, a task can be of different hyperparameters)
 - **Minimal juggling**: Inputs and outputs are just files/symlinks, and arguments are all passed as environment variables
 - **Automatic parallelization:** based on the dependency DAG
 - **Cloud-agnostic:** tasks can be run locally, or on a cloud (e.g. AWS, Azure), with minimal changes to the pipeline.