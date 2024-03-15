# Module `conda`

Enables Conda environments to be used as decorators in HyperMake.

## `conda.Environment`

Enables a job to be run within a Conda environment.

```python
import conda

@conda.Environment(name="myenv")
task check_if_cuda_is_available():
    python -c "import torch; print(torch.cuda.is_available())"
```

This can even be expressed with nested decorators:
```python
import std
import conda

@conda.Environment(name="myenv")
@std.Interpreter(interpreter="python")
task check_if_cuda_is_available():
    import torch
    print(torch.cuda.is_available())
```
Here we first wrap the script with a `python` interpreter, then dictate that this task should run within a Conda environment.
