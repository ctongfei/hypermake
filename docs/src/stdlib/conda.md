# Module `conda`

Enables Conda environments to be used as decorators in HyperMake.

## Function `conda.create_env`
Creates a Conda environment based on a yaml specification file.

```python
package env = conda.create_env(file="environment.yml")
```

## Class `conda.activate`

Enables a job to be run within a Conda environment.

```python
import conda

@conda.activate(environment="myenv")
task check_if_cuda_is_available():
    python -c "import torch; print(torch.cuda.is_available())"
```

You can use the returned path of `conda.create_env` as the `environment` argument.

```python
package env = conda.create_env(file="environment.yml")
@conda.activate(environment=$env)
```

This can even be expressed with nested decorators:
```python
import std
import conda

@conda.activate(environment="myenv")
@std.run(interpreter="python")
task check_if_cuda_is_available():
    import torch
    print(torch.cuda.is_available())
```
Here we first wrap the script with a `python` interpreter, then dictate that this task should run within a Conda environment.
