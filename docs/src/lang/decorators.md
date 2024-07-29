# Decorators

In Hypermake, a task can be decorated with some decorators, effectively modifying its behavior. This can support

- Running with different shell;
- Running in specific virtual environments;
- Running through some cluster submission systems;
- etc.

A decorator in HyperMake is just an object with a `run` method that takes a script as input and runs a modified version.

```py
object decorator:
    def run(internal_script):
        ...
```
If a decorator admits parameters, it simply becomes a class:
```py
class decorator(args):
    def run(internal_script):
        ...
```

and when applying a decorator, one could write
```shell
@decorator(args)
task taskName(...) -> out:
  ...
```


#### Example 1: A decorator that runs a task in Python
An example that let us runs a task in Python instead of shell:
```shell
object python:
  def run(internal_script):
    python $internal_script

@python
task helloWorldInPython:
  print("Hello World" + " " + "in Python!")
```

There is no need to define this in your pipelines: it is already available in the standard library as `@std.run(interpreter="python")`.


#### Example 2: Decorates a script to run in a Conda virtual environment

In Python, a task can be run in different Conda virtual environments. This is a decorator that lets us do that.

```shell
class conda(env):
  def run(internal_conda_script):
    eval "$(command conda 'shell.bash' 'hook' 2> /dev/null)"
    conda activate $env
    . $internal_conda_script
    conda deactivate

@conda(env={Env: base myenv})
task helloWorldFromEnv:
  python -c "print('Hello World in Python from $env!')"
```

Note that in the task `helloWorldFromEnv`, the decorator `conda` has a parameterized argument: `env={Env: base myenv}`.
We can invoke both cases of the task `helloWorldFromEnv`:
```shell
hypermake tutorial/decorators.hm run 'helloWorldFromEnv[Env: *]'
```

We will see both lines
```
Hello World in Python from base!
Hello World in Python from myenv!
```
output to the terminal.

#### Example 3: Chaining decorators
We have now created two decorators:

- `@python` that executes a script using Python instead of Bash as the interpreter;
- `@conda` that runs a task in a specific Conda virtual environment.

Can we compose these decorators? Yes.

```python
@conda(env={Env: base myenv})
@python
task helloWorldInPythonFromEnv:
  import os
  print(f"Hello World in Python from {os.environ['env']}!")
```

> One can use `os.environ[var]` to get the environment variable `$var` in Python.
First, our script is wrapped by `@python`, then `@conda(env)`.
Recall that HyperMake passes parameters into the script as environment variables:
we cannot use `$env` to get the HyperMake variable in Python.

#### Example 4: A decorator that runs a compiled language: C
We can also create a decorator that runs a task in C. Since C is a compiled language, we need to compile the script first.
```python
object gcc:
  def run(internal_c_script):
    ln -s $internal_c_script source.c
    gcc source.c -o source.out
    ./source.out
```
Now we can do fun things: write C scripts in HyperMake!
```c
@gcc
task print(input="abcde"):
  #include <stdio.h>
  #include <stdlib.h>
  int main() {
    char* input = getenv("input");
    printf("%s\n", input);
    return 0;
  }
```
