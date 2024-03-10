# Module `conda`

## `conda.Environment`

Enables a job runs within a Conda environment.

```python
import conda

object env = conda.Environment(name="myenv")

@env
task check_if_cuda_is_available():
  python -c "import torch; print(torch.cuda.is_available())"
```
