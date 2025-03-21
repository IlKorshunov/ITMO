# Набор данных

## Part 1

**Выберите любой набор данных для задачи бинарной классификации:**
  - Желательно использовать набор данных из предыдущей лабораторной работы.
  - Если набор данных изначально не предназначен для бинарной классификации, объедините несколько классов.

**Преобразование и нормализация:**
  - Преобразуйте данные в числовой вид.
  - Нормализуйте данные.

**Разбиение набора данных:**
  - Разбейте данные на тренировочную и тестовую части.

**Целевая функция:**
  - Выберите целевую функцию ошибки или качества для задачи бинарной классификации.

# Алгоритмы

**Линейная регрессия с гребневой регуляризацией:**
  - Реализуйте алгоритм линейной регрессии в матричном виде.
  - Можно использовать библиотеки для работы с матрицами.
  - Преобразуйте алгоритм в алгоритм линейной классификации, заменив целевой признак на ±1.

**Линейная классификация на основе градиентного спуска:**
  - Реализуйте алгоритм линейной классификации, основанный на градиентном спуске.
  - Алгоритм должен поддерживать не менее трёх эмпирических рисков, вычисляемых через отступ.
  - Реализуйте Elastic Net регуляризацию.
  - Обеспечьте возможность настройки скорости градиентного спуска.
  - Производные и градиент должны быть вычислены аналитически.

**Метод опорных векторов через градиентный спуск с восстановлением условий:**
  - Реализуйте метод опорных векторов (SVM) через градиентный спуск с восстановлением условий.
  - SMO использовать нельзя.
  - Алгоритм должен поддерживать не менее трёх ядер.

# Задача

**Выбор числа итераций:**
  - Выберите число итераций для линейной классификации и метода опорных векторов так, чтобы они выполняли асимптотически равное число операций.

**Подбор гиперпараметров:**
  - Найдите лучшие гиперпараметры для каждого алгоритма.

**Построение кривых обучения:**
  - Постройте кривую обучения со сглаженным эмпирическим риском на тренировочном множестве для линейной классификации и метода опорных векторов.
  - Постройте кривую обучения с целевой функцией ошибки или качества на тестовом множестве для линейной классификации и метода опорных векторов.
    - Если итераций много, не обязательно замерять целевую функцию на каждой итерации.
    - Переберите разные разбиения на тренировочную и тестовую части.
    - Отметьте доверительный интервал на графике.

**Сравнение с линейной регрессией:**
  - На предыдущем графике отметьте значение целевой функции на тестовом множестве для линейной регрессии (например, в виде горизонтальной прямой).

## Part 2

# Алгоритмы

**Дерево принятия решений:**
  - Реализуйте алгоритм построения дерева принятия решений.
  - Алгоритм должен поддерживать не менее 3 гиперпараметров для ограничения размера дерева.

**Бустинг и случайный лес:**
  - Реализуйте алгоритм бустинга.
  - Реализуйте алгоритм случайного леса.

# Задание

**Дерево принятия решений (библиотечная реализация):**
  - Выберите библиотечную реализацию дерева принятия решений.
  - Не ограничивая высоту, переберите по очереди разные значения числовых гиперпараметров.
  - Постройте график зависимости высоты полученного дерева.

**Дерево принятия решений (ваша реализация):**
  - Повторите предыдущий пункт для вашей реализации дерева принятия решений.
  - Постройте график зависимости целевой функции ошибки или качества от высоты дерева на тренировочном и тестовом множествах для вашей и библиотечной реализации.

**Случайный лес:**
  - Постройте график зависимости целевой функции ошибки или качества от числа деревьев на тренировочном и тестовом множествах для вашей и библиотечной реализации алгоритмов случайного леса.

**Бустинг:**
  - Преобразуйте набор данных так, чтобы у него была бинарная целевая категория.
  - Повторите предыдущий пункт для вашего и библиотечного алгоритма бустинга.