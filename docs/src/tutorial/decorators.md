# Decorators

Now let's convert our downloaded BEIR data to the standard TREC format.
This format is standard for information retrieval tasks.
There are 3 kinds of files in the TREC format: 
 - `*.queries`, a TSV file with two columns, query id and query text;
 - `*.qrels`, a TSV file with four columns, query id, iteration id, document id, and relevance label;
 - `corpus`, a TSV file with two columns, document id and document text.

This conversion involves some complex processing, so we will first write a Python script `beir_to_trec.py` to do this.

```python
import os
import json
import sys
import csv
from tqdm import tqdm
data = sys.argv[1]  # The directory containing the downloaded BEIR data
out = sys.argv[2]  # The directory to write the TREC format data
os.mkdir(out)
with open(f"{data}/corpus.jsonl") as f_in, open(f"{out}/corpus", 'w') as f_out:
  for line in tqdm(f_in):
    obj = json.loads(line)
    id = obj['_id']
    text = obj['text'].replace('\n', ' ').replace('\t', ' ').replace('\r', ' ')
    title = obj.get('title', "")
    trec_line = f"{id}\t{title}: {text}" if title != "" else f"{id}\t{text}" 
    # Concatenate title and text
    print(trec_line, file=f_out)
queries = {}
with open(f"{data}/queries.jsonl") as f:
  for line in tqdm(f):
    obj = json.loads(line)
    id = obj['_id']
    text = obj['text']
    queries[id] = text
for partition in os.listdir(f"{data}/qrels"):
  partition = os.path.splitext(partition)[0]
  with open(f"{data}/qrels/{partition}.tsv") as f_in, open(f"{out}/{partition}.qrels", 'w') as f_out:
    query_ids = set()
    for row in tqdm(csv.DictReader(f_in, delimiter='\t')):
      query_ids.add(row['query-id'])
      print(f"{row['query-id']}\t0\t{row['corpus-id']}\t{row['score']}", file=f_out)
  with open(f"{out}/{partition}.queries", 'w') as f:
    for query_id in query_ids:
      print(f"{query_id}\t{queries[query_id]}", file=f)
```
Now we can write a task to run this script.

```bash
task beir_to_trec(data=$raw_beir_data.out) -> out:
  python beir_to_trec.py $data out
```
This task takes the output of the `raw_beir_data` task as input and produces a directory `out` containing the TREC format data.

But to run this task, before invoking `hypermake` from the command line, we need to first activate the Conda environment that contains the Python dependencies required by the script `beir_to_trec.py`. This is not ideal -- recall that we just built the `pyserini` Conda environment in the previous section. We would like to run this task in the `pyserini` environment.

Let's decorate this task with a `@conda` **decorator** that activates the `pyserini` environment.

```python
import conda

@conda.activate(environment=$pyserini)
task beir_to_trec(data=$raw_beir_data.out) -> out:
    python beir_to_trec.py $data out
```

> What is the magic behind this decorator? A HyperMake decorator takes a script and returns a new wrapped script.
  To implement your own decorator, you need an object with a `run` function.
> If you are curious, you can find the implementation of the `conda.activate` decorator [here](https://github.com/ctongfei/hypermake/blob/main/src/main/hypermake/conda.hm).

### Next steps

Let's continue building our pipeline, starting from indexing the corpus with Pyserini.


At this step, we run a Bash script under the Pyserini conda environment.
```python
@conda.activate(environment=$pyserini)
task index(data=$beir_to_trec.out) -> out:
  mkdir corpus
  cat $data/corpus \
    | jq -Rc 'inputs | split("\t") | {id: .[0], contents: .[1]}' \
    > corpus/corpus.json  # Convert TREC format to Pyserini JSON
  python -m pyserini.index.lucene \
    --collection JsonCollection \
    --input corpus \
    --index $out \
    --generator DefaultLuceneDocumentGenerator \
    --threads $(nproc) \
    --storePositions \
    --storeDocvectors \
    --storeRaw
```

Run the actual retrieving with Pyserini.
```python
@conda.activate(environment=$pyserini)
task retrieve(
  data=$beir_to_trec.out, 
  test_partition=$, 
  index=$index.out
) -> (out="result.qres"):
  ln -s $data/$test_partition.queries test.tsv
  python -m pyserini.search.lucene \
    --index $index \
    --topics test.tsv \
    --output $out \
    --batch-size 32 \
    --hits 100 \
    --threads $(nproc) \
    --remove-duplicates --remove-query --bm25
```

Evaluate the retrieval results with `trec_eval`.
### Step 7: Evaluate the retrieval results with `trec_eval`
```bash
task evaluate(
  data=$beir_to_trec.out,
  result=$retrieve.out,
  test_partition=$,
  trec_eval=$
) -> (out="eval.txt"):
  $trec_eval/trec_eval -m all_trec $data/$test_partition.qrels $result > $out
```

> Here we referenced the output of the `trec_eval` package as `$trec_eval`. This is because the `trec_eval` package is a separate package that we built in the previous section. 
  We can refer to the output of a package directly by its name.