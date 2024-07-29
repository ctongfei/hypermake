# Hello, world!

To introduce HyperMake, let's define our first task:


```makefile
task hello:
  echo "Hello, world!"
```

Save this file as `hello.hm`. 

We have created our first HyperMake script file that contains a single task.
This script defines a task named `hello` that prints `Hello, world!` to the console. There is no input or output for this task.

> Note the syntax here: A code block starts after the `:` at the end of the task signature.
> A code block is a consecutive list of ***indented*** lines of scripts, where each line must start with at least ***2 spaces***.
> By default, the script is written in Bash.

If you are familiar with `make`, you can think of a task as a `make` rule. The task above written as a Makefile would just be
```makefile
hello:
    echo "Hello, world!"

.PHONY: hello  # since this task does not produce any output
```

Now let's run this task!

Execute the following command in your shell:
```shell
  hypermake hello.hm run hello 
```
We should see the output "Hello, world!" printed in the terminal.

> The basic command line usage is `hypermake $script <subtask> $target`. Here the `<subtask>` is simply `run`.

