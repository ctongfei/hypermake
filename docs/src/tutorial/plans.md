# Plans

We have now built a full pipeline for the BEIR-14 dataset. The pipeline is defined in `beir.hm`.

Let's first preview this in the command line:
```shell
hypermake beir.hm list
```

It shows a rendition of the DAG structure of our pipeline:
```
HyperMake 0.1.0 -- A parameterized pipeline manager
Workflow file: beir.hm

Variables:
  • Metric: { ndcg_cut_10 recall_100 map mrr }
  • BeirDataset: { msmarco scifact trec-covid webis-touche2020 fiqa dbpedia-entity fever nfcorpus hotpotqa climate-fever scidocs nq quora arguana }

Tasks:
  • pyserini@local
  │ • raw_beir_data[BeirDataset]
  │ │ • trec_eval@local
  ├─┴─│─• beir_to_trec[BeirDataset]
  ├───│─┼─• index[BeirDataset]
  └───│─┼─┴─• retrieve[BeirDataset]
      └─┴───┴─• evaluate[BeirDataset]
              └─• aggregate_metric[Metric]
```

To run them all:
```shell
hypermake beir.hm run "aggregate_metric[Metric: *]" -j8
```

Here we compute the `aggregate_metric` task for all metrics defined in `metric`, with max 8 jobs running in parallel!

Or, we can define a **plan** to define the targets we want to run:
```python
plan RunBEIR = {
    aggregate_metric[Metric: *]
}
```
> A plan definition can contain multiple targets, separated by commas.

And invoke it with:
```shell
hypermake beir.hm run RunBEIR -j8
```

The results should match the results in Table 2 (BM25 column) of the [RepLlama paper](https://arxiv.org/pdf/2310.08319). 
