# Runners

Runners in HyperMake are responsible for executing the task scripts. They specify how a job is executed on a specific file system (host). They abstract over the following scenarios:
 - Run a job locally.
 - Run a job under `tmux` to avoid SSH disconnection.
 - Run a job on a remote host via a job scheduler (e.g. Slurm, PBS, etc.).
 - Submit a job to a cloud service (e.g. AWS Batch, Azure ML, etc.).

Runners are specified as a special `@@` decoration on a task.

A runner in HyperMake is an object with the following members implemented:
 
 - `runner.submit(script_file)`: Submits the job to the runner and returns the job ID. The job id could just be the pid if locally.
 - `runner.poll(job_id)`: Polls the job for its exit code. If the job is not done, returns an empty string.
 - `runner.cancel(job_id)`: Cancels the job.

The implementation of `submit` and `poll` should guarantee that the `stdout` and `stderr` of the job are written to the working directory on the host of the task.

A hypothetical reference implementation of a local runner would be
```sh
object popen:
  def submit(script_file):
    {
      source $script_file > stdout 2> stderr
      echo $? > exitcode
    } &
    echo $! > pid

  def poll(pid):
    if kill -0 $pid > /dev/null 2>&1
    then
      :
    else
      if [ -f exitcode ]
      then
        cat exitcode
      else
        echo -1
      fi
    fi

  def cancel(pid):
    kill -9 $pid
```
