## Module `az.storage.blob`

Enables Azure Blob Storage support.

Usage:
```py
import az.storage.blob (name, container, extra_args)
```
It creates an environment with name `name`.

Example usage:
```py
import az.storage.blob (
    name = "az_storage", 
    container = "my_container", 
    extra_args = "--account-name xxx --account-key yyy"
)

data_path = "/path/to/data" @az_storage
```
