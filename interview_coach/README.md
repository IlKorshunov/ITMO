# Multi-Agent Interview Coach

Система из нескольких AI-агентов для проведения технических интервью.

## Агенты

- Interviewer - ведёт диалог, задаёт вопросы
- Observer - анализирует ответы, детектит галлюцинации
- Manager - генерирует финальный фидбэк

## Установка

```bash
python -m venv venv
source venv/bin/activate 
pip install -r requirements.txt
cp .env.example .env
```

## Настройка LLM

### Локальная модель (Ollama)

```bash
ollama serve
ollama pull qwen2.5:7b
```

В `.env`:
```
LLM_PROVIDER=local
LOCAL_MODEL_URL=http://localhost:11434/v1
```

### HuggingFace API

```
LLM_PROVIDER=huggingface_api
HF_API_TOKEN=hf_your_token_here
HF_MODEL=Qwen/Qwen2.5-7B-Instruct
```

## Запуск

```bash
python main.py
```

### С параметрами

```bash
python main.py --name "Алекс" --position "Backend Developer" --grade Junior
```

### Автоматический режим

```bash
python main.py --auto --personality strong_junior
```

Пресеты: `strong_junior`, `weak_junior`, `confident_wrong`, `nervous`, `experienced`, `off_topic`, `questioner`

### Параметры

| Параметр | Описание |
|----------|----------|
| `--provider` | huggingface_api, local |
| `--model` | Имя модели |
| `--output` | Папка для логов |
| `--debug` | Debug режим |
| `--name` | Имя кандидата |
| `--position` | Позиция |
| `--grade` | Junior, Middle, Senior |
| `--auto` | Автоматический режим |
| `--personality` | Пресет личности |
| `--knowledge` | Уровень знаний (0.0-1.0) |
| `--nervousness` | Нервозность (0.0-1.0) |
| `--max-turns` | Максимум ходов |

## Структура

```
interview_coach/
├── main.py
├── interview_coach/
│   ├── agents/
│   ├── core/
│   ├── models/
│   ├── llm/
│   └── utils/
├── tests/
└── output/
```
