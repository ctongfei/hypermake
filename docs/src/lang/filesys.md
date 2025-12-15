# File systems

A **file system** encapsulates the operations that can be performed on files and directories in a particular environment in HyperMake.

A file system can be specified in HyperMake script in 3 locations:
 - After an input in a task: `in@fs` means that the input `$in` is expected to be in file system `fs`. If not, HyperMake will automatically transfer the files to the specified file system `fs`.
 - After an output in a task: `out@fs` means that the output `$out` will be written to file system `fs`. HyperMake will check the existence of `$out` in `fs` to determine if the task is successfully executed.
 - After a task itself: `task my_task@fs(...)` means that the task `my_task` is run under file system `fs`: The working directory
   is created in `fs`, and all metadata, stdout, and stderr are all written to `fs`. For this to work, this file system must support **running commands** (see below). 

HyperMake provides a default file system implementation for the local file system (`local`), 
and has utilities to define file systems over common remote systems such as SFTP, AWS S3, and Azure Blob Storage. 

Additionally, it is possible to define custom file systems for different environments.

In the HyperMake language, a file system is specified as an _object_ with various member functions defined.

### Members to be implemented in a file system object

| Member                    | Description                                                                                                                                                                                                           |
|---------------------------|-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| `fs.root`                 | A string specifying the root path of all HyperMake outputs.                                                                                                                                                           |
| `fs.read(file)`           | Reads the file `$file` and outputs the content to `stdout`.                                                                                                                                                           |
| `fs.mkdir(dir)`           | Creates an empty directory `$dir`. <br> This should have the semantics of `mkdir -p`: it should create all parent <br> directories if they do not exist, and it should not fail if the directory <br> already exists. |
| `fs.exists(file)`         | Checks if `$file` exists in `fs`.                                                                                                                                                                                     |
| `fs.link(src, dst)`       | Creates a symbolic link at `$dst` that links to `$src`.                                                                                                                                                               |
| `fs.touch(file)`          | Creates an empty file at path `$file`.                                                                                                                                                                                |
| `fs.remove(file)`         | Removes file `$file` in `fs`. <br> If `$file` is a directory, it should remove the directory and all its contents.                                                                                                    |
| `fs.upload(src, dst)`     | Uploads the file or directory `$src` in `local` to `$dst` in `fs`.                                                                                                                                                    |
| `fs.download(src, dst)`   | Downloads the file or directory `$src` in `fs` to `$dst` in `local`.                                                                                                                                                  |
| `fs.execute(command, wd)` | **(Optional)** Executes the command `$command` in `fs`'s shell under working directory `wd`. <br> This can be omitted if the file system does not support running commands.                                           |

There is no need to define `local` as it is internal to HyperMake. A reference implementation of `local` is provided below.
```py
object local:
    root = "."
    
    def read(file):
        cat $file
    
    def mkdir(dir):
        mkdir -p $dir
    
    def exists(file):
        test -e $file
    
    def link(src, dst):
        ln -s $src $dst
    
    def touch(file):
        touch $file
    
    def remove(file):
        rm -r $file
       
    def upload(src, dst):
        ln -s $src $dst  # both local, so a symbolic link suffices
    
    def download(src, dst):
        ln -s $src $dst  # both local, so a symbolic link suffices
        
    def execute(command):
        bash -e $command
```
 
#### Example: define a file system over SFTP

```sh
import ssh
object my_server = ssh.server(host="...")
```

#### Example: define a file system over AWS S3
```sh
import aws
object my_bucket = aws.s3(name="...")
```

#### Example: define a file system over Azure Blob Storage
```sh
import az
object my_container = az.storage_blob(name="...")
```

### Transferring file between environments

Sometimes different parts of a pipeline are run under different environments, 
e.g., data preprocessing may happen on a local machine, whereas training is done on an SSH grid, or 
on AWS EC2 or Azure ML. 
