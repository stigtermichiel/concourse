package exec

import (
	"fmt"
	"io"
	"os"
	"strings"

	"github.com/tedsuo/ifrit"
)

func Compose(a Step, b Step) Step {
	return composed{a: a, b: b}
}

type composed struct {
	a Step
	b Step

	source ArtifactSource

	firstSource  ArtifactSource
	secondSource ArtifactSource
}

func (step composed) Using(source ArtifactSource) ArtifactSource {
	step.source = source
	return &step
}

func (step *composed) Run(signals <-chan os.Signal, ready chan<- struct{}) error {
	step.firstSource = step.a.Using(step.source)

	processA := ifrit.Background(step.firstSource)
	readyA := processA.Ready()

	var signalled bool
	var waitErr error

danceA:
	for {
		select {
		case <-readyA:
			close(ready)
			readyA = nil

		case waitErr = <-processA.Wait():
			if readyA != nil {
				// if the process exits quickly enough we may miss the ready notification
				close(ready)
			}

			break danceA

		case sig := <-signals:
			processA.Signal(sig)
			signalled = true
		}
	}

	if signalled || waitErr != nil {
		return waitErr
	}

	step.secondSource = step.b.Using(step.firstSource)

	return step.secondSource.Run(signals, make(chan struct{}))
}

func (step *composed) Release() error {
	errorMessages := []string{}

	if step.firstSource != nil {
		if err := step.firstSource.Release(); err != nil {
			errorMessages = append(errorMessages, "first step: "+err.Error())
		}
	}

	if step.secondSource != nil {
		if err := step.secondSource.Release(); err != nil {
			errorMessages = append(errorMessages, "second step: "+err.Error())
		}
	}

	if len(errorMessages) > 0 {
		return fmt.Errorf("sources failed to release:\n%s", strings.Join(errorMessages, "\n"))
	}

	return nil
}

func (step *composed) StreamFile(filePath string) (io.ReadCloser, error) {
	return step.secondSource.StreamFile(filePath)
}

func (step *composed) StreamTo(dst ArtifactDestination) error {
	return step.secondSource.StreamTo(dst)
}
