# Module `az`

Enables various decorators for Microsoft Azure services in HyperMake.

## `az.storage_blob`

Enables Azure Blob Storage containers to be used as a file system in HyperMake. Behind the scenes it uses the `az storage blob` CLI command family.


Example usage:
```py
import az 
object az_storage = az.storage_blob( 
    container="my_container", 
    extra_args="--account-name xxx --account-key yyy"
)

data_path = "/path/to/data"@az_storage
```

## `az.storage_fs`

Enables Azure Data Lake Storage (ADLS) Gen2 containers to be used as a file system in HyperMake. Behind the scenes it uses the `az storage fs` CLI command family.

## `az.ml_job_create`

Enables Azure ML command jobs as a submitter in HyperMake. Behind the scenes it uses the `az ml job` CLI command family.
