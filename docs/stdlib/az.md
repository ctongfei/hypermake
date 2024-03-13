# Module `az`

Enables various decorators for Microsoft Azure services in HyperMake.

## `az.BlobStorage`

Enables Azure Blob Storage containers to be used as a file system in HyperMake. Behind the scenes it uses the `az storage blob` CLI command family.


Example usage:
```py
import az 
object az_storage = az.BlobStorage( 
    container="my_container", 
    extra_args="--account-name xxx --account-key yyy"
)

data_path = "/path/to/data" @az_storage
```

## `az.DataLakeStorageGen2`

Enables Azure Data Lake Storage (ADLS) Gen2 containers to be used as a file system in HyperMake. Behind the scenes it uses the `az storage fs` CLI command family.

## `az.MLCompute`
