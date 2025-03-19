package tparallel

import (
	"sync"
)

type T struct {
	started   bool
	finished  bool
	parallel  bool
	parent    *T
	children  []*T
	startChan chan struct{}
	doneChan  chan struct{}
}

func NewT() *T {
	return &T{
		startChan: make(chan struct{}),
		doneChan:  make(chan struct{}),
	}
}

func (t *T) Parallel() {
	if !t.parallel {
		t.parallel = true
		// Закрываем канал, только если он еще не закрыт.
		select {
		case <-t.startChan: // Проверяем, был ли канал уже закрыт.
			// Канал уже закрыт, ничего не делаем.
		default:
			// Канал еще открыт, закрываем его.
			close(t.startChan)
		}
	}
}

func (t *T) Run(subtestFunc func(t *T)) {
	subT := NewT()                        // Создаем контекст для подтеста
	t.children = append(t.children, subT) // Добавляем подтест к дочерним элементам текущего теста

	go func() {
		// Запускаем подтест и сразу разрешаем его выполнение
		subtestFunc(subT)
		if !subT.parallel {
			// Если подтест не параллельный, закрываем канал, указывая на его завершение
			close(subT.doneChan)
		}
	}()

	if !subT.parallel {
		// Ожидаем завершения подтеста, если он не параллельный
		<-subT.doneChan
	}
}

// Run запускает тесты, учитывая их параллельность и последовательность.
func Run(topTests []func(t *T)) {
	var wg sync.WaitGroup

	for _, test := range topTests {
		t := NewT()
		wg.Add(1)

		go func(t *T, test func(t *T)) {
			defer wg.Done()
			t.started = true
			close(t.startChan)
			test(t)
			t.finished = true
			close(t.doneChan)
			for _, child := range t.children {
				<-child.doneChan // Ожидаем завершения всех подтестов
			}
		}(t, test)

		if !t.parallel {
			<-t.doneChan // Если тест не параллельный, ожидаем его завершения
		}
	}

	wg.Wait() // Ожидаем завершения всех тестов
}
