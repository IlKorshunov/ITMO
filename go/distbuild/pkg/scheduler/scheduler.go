//go:build !solution

package scheduler

import (
	"context"
	"time"

	"go.uber.org/zap"

	"gitlab.com/slon/shad-go/distbuild/pkg/api"
	"gitlab.com/slon/shad-go/distbuild/pkg/build"
)

var TimeAfter = time.After

type JobQueue struct {
	queue []*PendingJob
}

func newJobQueue() *JobQueue {
	return &JobQueue{
		queue: make([]*PendingJob, 0),
	}
}

type PendingJob struct {
	Job      *api.JobSpec
	Finished chan struct{}
	Result   *api.JobResult
}

type Config struct {
	CacheTimeout time.Duration
	DepsTimeout  time.Duration
}

type Scheduler struct {
	logger *zap.Logger
	config Config
	queue *JobQueue
}

func NewScheduler(l *zap.Logger, config Config) *Scheduler {
	return &Scheduler{
		logger: l,
		config: config,
		queue:  newJobQueue(),
	}
}

func (jq *JobQueue) dequeue() (*PendingJob) {
	if len(jq.queue) == 0 {
		return nil
	}
	out := jq.queue[0]
	jq.queue = jq.queue[1:]
	return out
}

func (jq *JobQueue) push(job *PendingJob) {
	jq.queue = append(jq.queue, job)
}

func (jq *JobQueue) findById(id build.ID) (job *PendingJob){
	for _, curJob := range jq.queue {
		if curJob.Job.Job.ID == id {
			return curJob
		}
	}
	return nil
}

// вернуть любой воркер, который хранит в кэше заданный артифакт
func (c *Scheduler) LocateArtifact(id build.ID) (api.WorkerID, bool) {
	Job := c.queue.findById(id)
	if Job != nil {
		return  Job.Job.Artifacts[id], true
	}
	return "-1", false
}

// после того, как воркер закончил выполнение job, вызвать этот метод
func (c *Scheduler) OnJobComplete(workerID api.WorkerID, jobID build.ID, res *api.JobResult) bool {
	job := c.queue.findById(jobID)
	if job != nil {
		job.Result = res
		close(job.Finished) 
		return true
	}
	return false
}


// передать Job в шедулер. Должна помещать job в очередь или возвращать ссылку на существующий pendingJob
func (c *Scheduler) ScheduleJob(job *api.JobSpec) *PendingJob {
	actualJob := c.queue.findById(job.ID)
	if actualJob != nil {
		return actualJob
	}

	outJob := &PendingJob {
		Job: job,
		Finished: make(chan struct{}),
		Result:   nil,
	}

	c.queue.push(outJob)
	c.logger.Info("new job was pushed to the queue", zap.String("jobID", job.ID.String()), zap.String("jobName", job.Name))
	return outJob
}

// воркер забирает job из шедулера
func (c *Scheduler) PickJob(ctx context.Context, workerID api.WorkerID) *PendingJob {
	select {
	case <- ctx.Done():
		return nil
	default:
		return c.queue.dequeue()
	}
}

func (c *Scheduler) Stop() {
	c.logger.Info("Stopping scheduler...")
	for _, job := range c.queue.queue {
		close(job.Finished)
	}
	c.queue.queue = nil

	c.logger.Info("Scheduler stopped.")
}
