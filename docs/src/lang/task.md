# Tasks

Tasks are the atomic unit in HyperMake pipelines. Each task is realized as a script (default in Bash, but can be in any language with the `std.run` decorator.) with a set of inputs and outputs. 

The task script is executed in a **child process**, and the inputs and outputs are passed as **environment variables** (HyperMake manages these variables). Its working directory is managed by HyperMake, and is located at `${fileSys.root}/$taskName/$taskParams`.

> `$taskParams` is the [percent-encoded](https://en.wikipedia.org/wiki/Percent-encoding) string of the set of task parameters that are not default:
  e.g. `Dropout=0.1&Lr=0.01&BatchSize=32`.

### Syntax
To define a task, write
```shell
task taskName(
  $param₀@fsI₀=$arg₀, 
  $param₁@fsI₁=$arg₁, 
  ...
) -> ($out₀@fsO₀, $out@fsO₁, ...):
  # task script
```
where 
 - `taskName` is the name of the task.
 - `$param₀`, `$param₁`, ... are the parameters of the task.
 - `$arg₀`, `$arg₁`, ... are the input arguments of the task, and has to be specified.
    - If not, the task will be considered **abstract**, and it is a [function](./function.md).
 - `$fsI₀`, `$fsI₁`, ... are the file systems that expect the input arguments to be in. If not, HyperMake will automatically transfer the files to the specified file system. If omitted, default to `local`.
 - `$out₀`, `$out₁`, ... are the output files of the task.
    - Can be files or directories.
 - `$fsO₀`, `$fsO₁`, ... are the file systems that the output files are in. If omitted, default to `local`.

### Behavior
To run a task, HyperMake works by
 - **Checking the cache**: If the outputs of the task are already in the cache and are successful, the task is considered **up-to-date** and is not run.
 - **Removing the outputs**: If the outputs exists but corrupted (e.g. task not finish successfully), the outputs are removed.
 - **Creating a working directory**: HyperMake creates a working directory for the task, at `${local.root}/$taskName/$taskParams`.
 - **Locks this directory**: HyperMake locks this directory to prevent other HyperMake instances from running this task.
 - **Linking the inputs**: HyperMake links the input files (outputs of other dependent tasks) to the working directory.
 - **Running the task script**: HyperMake runs the task script in the working directory as a child process.
 - **Checking the output**: The task is considered successfully terminated if the task script exits with a zero exit code and all outputs exists in their specified file systems.
 - **Unlocking the directory**: HyperMake unlocks the working directory.