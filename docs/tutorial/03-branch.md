# Hyperparameters

We'd like to _parameterize_ our tasks: there could be multiple versions of a task.
This could be different datasets, different preprocessing pipelines, different pretrained models, etc.
Hypermake models these configurations as **parameters** to values and tasks.

For example, we are here downloading various versions of the [GloVe embeddings](https://nlp.stanford.edu/projects/glove/)
from the Stanford NLP website:

```shell
gloveUrl = {Version:
  6b="http://nlp.stanford.edu/data/glove.6B.zip",
  cc42b="http://nlp.stanford.edu/data/glove.42B.300d.zip",
  cc840b="http://nlp.stanford.edu/data/glove.840B.300d.zip",
  twitter27b="http://nlp.stanford.edu/data/glove.twitter.27B.zip"
}

task downloadGloVe(gloveUrl=$) -> out:
  wget -O glove.zip $gloveUrl
  unzip glove.zip
  mv *.txt $out
  rm glove.zip
```
!!! info inline end ""
    If the key is the same as the value, one can omit the value in the declaration, like this:
    `{Version: 6b cc42b cc840b twitter27b}`.
Note that we declared a `Dict`-like object here: `{Version: 6b=XX cc42b=XX cc840b=XX twitter27b=XX}`.
This is a parameter declaration: the name of the parameter is `Version`, and it has 4 potential keys `6b`, `cc42b`, `cc840b`, and `twitter27b`.
Each key is associated with a value that follows the key (that is the URL in this example).

Our task `downloadGloVe` is now parameterized with variable `Version`. To refer to one of these task (a **case**),
we can use the indexing notation `task[Var:key]`: here for example `downloadGloVe[Version:6b]`.

We can use the task referring expressions in the Hypermake command line interface. To download GloVe `6b` version, do
```shell
hypermake glove.hm run downloadGloVe[Version:6b]
```
One can use `[Var:*]` syntax to refer to **all** cases of the task. The following command line invocation downloads all GloVe versions:
```shell
hypermake glove.hm run downloadGloVe[Version: *]
```

