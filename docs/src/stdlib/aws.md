# Module `aws`

## `aws.s3`
Enables AWS S3 buckets as a HyperMake file system. Behind the scenes it uses the `aws s3` CLI command family.

Example usage:
```python
import aws
object my_bucket = aws.s3(
    bucket="my_bucket",
    root=""
)
```
