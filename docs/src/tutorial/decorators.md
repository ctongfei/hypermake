# Decorators

In Hypermake, a task can be decorated with some decorators, effectively modifying its behavior. This can support 

 - Running with different shell;
 - Running in specific virtual environments;
 - Running through some cluster submission systems;
 - etc.

The general syntax for defining a decorator is:

```shell
def <decoratorName>(<arguments>) <- input=<internalScriptName>:
  <script>
```
and when applying a decorator, one could write
```shell
@<decoratorName>(<arguments>)
task taskName(...) -> out:
  ...
```

This will modify the task with the decorator. The script to be modified will be stored as `internalScriptName`.

#### Example 1: A simple decorator
An example that let us runs a task in Python instead of shell:
```shell
def python() <- internalPythonScript="script.py":
  python $internalPythonScript

@python
task helloWorldInPython:
  print("Hello World" + " " + "in Python!")
```

And we can invoke the task from the command line:
```shell
hypermake tutorial/decorators.hm run helloWorldInPython
```

Let's see what's happening under the hood here. The script in the task `helloWorldInPython` is decorated with `@python`.
The script with the line `print(...)` is stored as `script.py` as directed in the decorator. Then, in shell, the command
in the decorator is run instead: `python script.py`.

#### Example 2: A parameterized decorator

In Python, a task can be run in different Conda virtual environments. This is a decorator that lets us do that.

```shell
def conda(env) <- internalCondaScript="conda-internal.sh":
  eval "$(command conda 'shell.bash' 'hook' 2> /dev/null)"
  conda activate $env
  . $internalCondaScript
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

!!! info inline end ""
    One can use `os.environ[var]` to get the environment variable `$var` in Python.
First, our script is wrapped by `@python`, then `@conda(env)`. 
Recall that Hypermake passes parameters into the script as environment variables: 
we cannot use `$env` to get the Hypermake variable in Python.
