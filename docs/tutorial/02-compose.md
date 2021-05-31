# Inputs, outputs, and composing tasks

A task can take parameters, and yield outputs.
```shell
task download(url=$) -> out:
  wget $url -O $out
```
