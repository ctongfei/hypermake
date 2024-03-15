# Module `ssh`

Enables SSH servers to be used as file systems in HyperMake.

## `ssh.Server`

Defines a SSH server in HyperMake. Note that this file system is able to execute jobs.

Example:
```py
import ssh
object my_server = ssh.Server(
    host='192.168.0.7',    # host name, in ~/.ssh/config
    root='/home/user/out'  # root of HyperMake output on the remote server
)

task my_remote_task@my_server(input) -> output@my_server:
    # This task will be executed on the remote server
    # and the input will be copied to the remote server.
    # The output is expected to appear on the remote server.
    ...
```
