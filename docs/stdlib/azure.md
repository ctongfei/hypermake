# Module `azure`

## `azure.BlobStorage`

Enables Azure Blob Storage support.


Example usage:
```py
import azure 
object az_storage = azure.BlobStorage(
    name = "az_storage", 
    container = "my_container", 
    extra_args = "--account-name xxx --account-key yyy"
)

data_path = "/path/to/data" @az_storage
```

## `azure.DataLakeStorageGen2`

## `azure.MLCompute`
