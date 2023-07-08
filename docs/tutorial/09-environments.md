# Environments

### Functions to define for a new environment `$env`


| Function to define      | Description                                                      | Reference implementation in `local`                         |
|-------------------------|------------------------------------------------------------------|-------------------------------------------------------------|
| `$env_read(file)`       | Reads the file `$file` and outputs the content to `stdout`.      | <pre>def local_read(file):<br>  cat $file</pre>             |
| `$env_execute(command)` | Executes the command `$command` in `$env`'s shell.               | <pre>def local_execute(command):<br>  bash -e $command</pre> |
 | `$env_mkdir(dir)`       | Creates an empty directory `$dir`.                               | <pre>def local_mkdir(dir):<br>  mkdir -p $dir</<br/>pre>    |
 | `$env_exists(file)`     | Checks if `$file` exists in `env`.                               | <pre>def local_exists(file):<br>  test -e $file</pre>       |
 | `$env_link(src, dst)`   | Creates a symbolic link at `$dst` that links to `$src`.          | <pre>def local_link(src, dst):<br>  ln -s $src $dst</pre>   |
 | `$env_touch(file)`      | Creates an empty file at path `$file`.                           | <pre>def local_touch(file):<br>  touch $file</pre>          |
 | `$env_delete(file)`     | Removes file `$file` in `env`.                                   | <pre>def local_delete(file):<br>  rm $file</pre>            |
 
#### Example: define an environment over SFTP

```sh
import "sftp.hm" with (name = "xxx", host = "...")
```


#### Example: define an environment over AWS S3
```sh
def s3_read(file):
  aws s3 cp $file -
```

### Transferring file between environments

Sometimes different parts of a pipeline are run under different environments, 
e.g., data preprocessing may happen on a local machine, whereas training is done on an SSH grid, or 
on AWS EC2 or Azure ML. Files are going to be copied to 


| `copy_from_$srcEnv_to_$dstEnv(src, dst)` | Copies a file `$src` on `srcEnv` to location `$dst` to `dstEnv`. | -                                                           |
