# Functions

**Functions** in HyperMake are **abstract [tasks](./task.md)**: tasks whose inputs are not fully specified. Functions allow for the instantiation of tasks with different parameters at different locations in the pipeline.

### Syntax
```python
def funcName(input1, input2, ...) -> (output1, output2, ...):
    # function script
```

where 
 - `funcName` is the name of the function.
 - `input1`, `input2`, ... are the input arguments of the function.
 - `output1`, `output2`, ... are the output files of the function.

### Instantiation
To instantiate a function as a [task](./task.md), write
```python
task taskName($param1=$arg1, $param2=$arg2, ...) = 
  funcName(input1=$param1, input2=$param2, ...)
```
> Instantiating a function as a task starts with `=`, not `:` (starts a script block).

