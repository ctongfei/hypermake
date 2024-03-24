# Module `std`

Contains some miscellaneous utilities for HyperMake.

## `std.Interpreter`

Enables a task in HyperMake to run in a custom interpreter (e.g. Python, Perl, etc.).

Example usage:
```py
import std

sender = {Sender: Alice Bob}

@std.Interpreter(interpreter="python3")
task hello_world(sender=$):
    import os
    print(f"Hello, world from {os.environ["sender"]}!")
```

Note that whatever interpreter you choose to use, HyperMake parameters are passed into the task as **environment variables**. Here in Python we use `os.environ` to access them.

