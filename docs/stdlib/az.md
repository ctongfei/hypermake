## Module `hypermake.az.storage.blob`

Enables Azure Blob Storage support.

Usage:
```py
import hypermake.az.storage.blob with (name, container, extra_args)
```

Example usage:
```py
import hypermake.az.storage.blob with (
    name = "az_storage", 
    container = "my_container", 
    extra_args = "--account-name xxx --account-key yyy"
)
```
