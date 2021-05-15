# Runs a simple "Hello, world" task

First let's define our first task (without any inputs or outputs declared!):

!!! note inline end
    Note the syntax here: A code block starts after the `:` at the end of the task signature.
    A code block is a consecutive list of lines of scripts, where each line must start with at least 2 spaces.
```shell
task hello:
  echo "Hello, world!"
```

Save this file as `hello.hm`. 

We have created our first Hypermake script file.
Now let's run this task!

Execute the following command in your shell:
!!! note inline end
    The basic command line usage is `hypermake <script file> <subtask> <target>`.
```shell
  hypermake hello.hm run task 
```
We should see the output "Hello, world!" printed in the terminal.

