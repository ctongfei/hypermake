# Module `std`

Contains some miscellaneous utilities for HyperMake.

## Function `std.link`

Creates a symbolic link as an output. This is particularly useful when referring to a local repository that is under development.
```shell
import std
package my_repo = std.link(path="path/to/my/repo")
```

## Class `std.run`

Enables a task in HyperMake to run in a custom interpreter (e.g. Python, Perl, etc.).

Example usage:
```py
import std

sender = {Sender: Alice Bob}

@std.run(interpreter="python3")
task hello_world(sender=$):
    import os
    print(f"Hello, world from {os.environ["sender"]}!")
```

Note that whatever interpreter you choose to use, HyperMake parameters are passed into the task as **environment variables**. Here in Python we use `os.environ` to access them.

