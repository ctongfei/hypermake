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

Now, we'd like to extract all the headlines: their URLs and titles.
We create another task that takes the output of the `download` task as input:
```shell
task getTitles(html=$download.out) -> out:
  cat $html \
  | perl -ne 'if (/<a href="(.*?)" class="storylink">(.*?)<\/a>/) { print "$1\t$2\n" }' \
  > $out
```

Running the following command
```shell
hypermake tutorial/ycomb.hm run getTitles
```
will sequentially run the two dependent jobs: first `download` then `getTitles`,
and the resulting TSV table will be located in `out/getTitles/default/out`.

In the directory `out/getTitles/default`, you can find the following files that may be of interest:

  * `out`: the resulting TSV table
  * `script.sh`: the script that was used to run the task
  * `stdout`: the standard output of the task
  * `stderr`: the standard error of the task
  * `args`: A shell script that contains the arguments that were used to run the task
  * `exitcode`: A file that contains the exit code of the task

To recreate the task in shell, you can use the following command. It should have the same behavior as directly running from Hypermake.
```shell
. args && . script.sh
```
