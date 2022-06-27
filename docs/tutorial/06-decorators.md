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

### Example 1: A simple decorator
An example that let us runs a task in Python instead of shell:
```shell
def python() <- input="script.py":
  python $input

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

### Example 2: A parameterized decorator
