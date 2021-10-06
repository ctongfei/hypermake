# Inputs, outputs, and composing tasks

A task can take parameters, and yield outputs.
```shell
url = "https://news.ycombinator.org"
task download(url=$) -> (out="homepage.html"):
  wget $url -O $out
```

!!! info inline end ""
    Generally, when declaring a parameter whose default argument is a variable with the same name,
    one can omit the argument name by just writing `$`.
Running the task `download` will download the homepage of the Hacker News. Note the parameter is declared as `url=$`: 
This is a shorthand for `url=$url`.

This task creates a single output called `homepage.html`: You can find this file at `out/download/default` directory. 
One can simply write `out` instead of `(out="homepage.html")`: in this case the output file name will be `out`.
