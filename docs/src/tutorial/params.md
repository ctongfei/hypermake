# Parameters

For the following tutorial sections, we will gradually build our `beir.hm` pipeline.

In any ML pipeline, one starts with data. In this example, we will use the BEIR-14 subset (those with public licenses) of the BEIR benchmark.

We declare these datasets as a pipeline **parameter** `BeirDataset`:

```python
beir_dataset = {BeirDataset: 
    msmarco scifact trec-covid webis-touche2020 
    fiqa dbpedia-entity fever nfcorpus hotpotqa 
    climate-fever scidocs nq quora arguana
}
```

> Note the syntax for declaring a parameter: `{ParamName: key0 key1 ...}`. For each parameter, **the first key** is considered the **default case** -- here `msmarco`.

Now we want to download the raw data from their official location.
```python
beir_url_prefix = "https://public.ukp.informatik.tu-darmstadt.de/thakur/BEIR/datasets"
```

We proceed to write the first task of our pipeline: downloading the raw data and unzip it.
```bash
task raw_beir_data(beir_dataset=$, beir_url_prefix=$) -> out:
  wget -O dataset.zip $beir_url_prefix/$beir_dataset.zip
  unzip dataset.zip
  rm dataset.zip
  mv $beir_dataset out
```

We declared a task that takes two inputs: `beir_dataset` and `beir_url_prefix`, and produces an output directory `out`.

> The syntax `name=$` is a shorthand for `name=$name`. Here, `beir_dataset=$` introduced the `beir_dataset` parameter as an input to the task.

The task is considered *complete* when its output directory `out` exists after the task exits with a zero status code.

> In HyperMake, the success of a task is determined by 
>  - the existence of all of its specified outputs
>  - **AND** the zero exit status code of the task script.

Note that 
 - `beir_dataset` is a **parameterized value**: it can take any of the values in the `BeirDataset` parameter;
 - `beir_url_prefix` is a *singleton* (non-parameterized) value.

Hence the task `raw_beir_data` is parameterized with **all the parameters in its inputs**. Consider 
`raw_beir_data` not a single task, but a **tensor** of tasks, where it has dimension 1: the `BeirDataset` parameter.

### Invoking the task
Let's invoke this task. To download the `msmarco` dataset, we run:
```bash
hypermake beir.hm run "raw_beir_data[BeirDataset:msmarco]"
```
We will find the downloaded dataset in the `out/raw_beir_data/default` directory.
> Note the task indexing syntax: `task[Param0: key0, Param0: key1, ...]`.

Download another one:
```bash
hypermake beir.hm run "raw_beir_data[BeirDataset:scifact]"
```
We will find the downloaded dataset in the `out/raw_beir_data/BeirDataset=scifact` directory.
> The output directory will be `out/<task-name>/<non-default-params>`, where `<non-default-params>` is the URL percent-encoding of the key-value pairs.

Clearly we do not wish to invoke the downloading one by one. Let's use the wildcard `*`:
```bash
hypermake beir.hm run "raw_beir_data[BeirDataset: *]" -j8
```

> The `-j` flag specifies the number of parallel jobs to run. Here we run 8 jobs in parallel.

At this point we have downloaded all the datasets. We will proceed to the next steps in the following sections.
