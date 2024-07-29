# Reduction

At this point we have a pipeline that retrieves documents from a corpus using Pyserini. We have also evaluated the retrieval results with `trec_eval`. But there are a lot of runs: one for each dataset in BEIR-14. We would like to aggregate the evaluation results. 

This is done by **reduction** in HyperMake: think this as `reduce` in functional programming, or `.max(dim=i)` in a tensor processing library.

Recall that at the end of the pipeline we have built thus far, the `trec_eval` output files are in the format of `eval.txt` for each dataset, under the variable `$evaluate.out`. We would like to aggregate the results over all datasets in BEIR-14. Additionally, there is more than 1 metric that we cared: for example, `ndcg_cut_10`, `recall_100`, `map`, and `mrr`.

We can define a new task `aggregate_metric` that takes the evaluation results of all datasets and aggregates them. The task definition is as follows:

```python
metric = {Metric: ndcg_cut_10 recall_100 map mrr}

task aggregate_metric(
  eval_results=$evaluate[BeirDataset: *].out, 
  metric=$
) -> (out="aggregate.txt"):
  grep -E "^$metric " $eval_results/* > $out
```

Note here that we used `$evaluate[BeirDataset: *].out` to refer to the outputs of all `evaluate` tasks for each dataset in BEIR-14. The parameter `eval_results`, while logically is a dictionary from configurations to files, will be realized as a folder of files for the Shell script.

 > HyperMake maps a dictionary to a folder of files in the Shell script. This is a common pattern in HyperMake to handle multiple outputs.
 >  - `dict[key]` would be `$d/$key` in Shell.
 >  - `dict.values()` would be `$d/*` in Shell.
 >  - `for key in dict` would be `for f in $d/*` in Shell.