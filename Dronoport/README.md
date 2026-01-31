# Dronoport


## Структура

```
Dronoport/
├── agents/
│   ├── q_learning.py
│   ├── double_q_learning.py
│   ├── sarsa.py
│   ├── expected_sarsa.py
│   ├── ucb_agent.py
│   └── prospect_agent.py
├── environment/
│   ├── drone_env.py
│   ├── obstacles.py
│   └── wind.py
├── training/
│   └── trainer.py
├── evaluation/
│   └── evaluator.py
├── visualization/
│   └── plots.py
├── utils/
│   ├── discretizer.py
│   └── hyperopt.py
├── config/
│   └── config.py
├── demo/
│   ├── demo.ipynb
│   └── lambda.ipynb
├── results/
├── main.py
└── requirements.txt
```

## Запуск

```bash
python3 -m venv venv
source venv/bin/activate
pip install -r requirements.txt
python main.py
```

## Агенты

> Реализация агентов основана на материалах курса [Practical RL](https://github.com/yandexdataschool/Practical_RL) (ШАД)

| Агент | Описание |
|-------|----------|
| Q-Learning | Табличный off-policy, eps-greedy |
| Double Q-Learning | Две Q-таблицы|
| SARSA | On-policy TD(0) |
| Expected SARSA | Ожидание по всем действиям |
| UCB | Многорукий бандит |
| Prospect Theory | Поведенческая модель (λ=2.35) |

## Результаты

```
results/
├── comparison/
│   ├── all_agents_bars.png
│   └── all_agents_table.png
├── data/
│   ├── results.csv
│   └── training_history.csv
├── hyperopt/
│   └── best_params.json
├── learning/
│   └── all_agents_learning.png
└── trajectories/
    ├── all_agents_best.png
    ├── qlearning_trajectories.png
    ├── sarsa_trajectories.png
    ├── double_q_trajectories.png
    └── ucb_trajectories.png
```
