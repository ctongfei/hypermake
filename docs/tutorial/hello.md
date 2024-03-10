# Runs a simple "Hello, world" task

First let's define our first task (without any inputs or outputs declared!):

!!! info inline end ""
    Note the syntax here: A code block starts after the `:` at the end of the task signature.
    A code block is a consecutive list of **indented** lines of scripts, where each line must start with at least 2 spaces.
```shell
task hello:
  echo "Hello, world!"
```

Save this file as `hello.hm`. 

We have created our first Hypermake script file that contains a single task.
Now let's run this task!

Execute the following command in your shell:
!!! info inline end ""
    The basic command line usage is `hypermake <script file> <subtask> <target>`.
```shell
  hypermake hello.hm run hello 
```
We should see the output "Hello, world!" printed in the terminal.

