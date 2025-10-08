# ocron

A cron expression parser and scheduler utility that generates the next occurrence times for cron expressions.

## Features

- Parse standard cron expressions
- Generate next N occurrences from a cron schedule
- Specify custom start times
- CSV output format for easy parsing

## Installation

This project uses `uv` for dependency management. First, install `uv` if you haven't already:

```bash
curl -LsSf https://astral.sh/uv/install.sh | sh
```

Then sync the project dependencies:

```bash
uv sync
```

## Usage

Run the program using `uv`:

```bash
# Get the next occurrence from now
uv run python main.py "0 0 * * *"

# Get next 5 occurrences
uv run python main.py --count 5 "0 0 * * *"

# Specify a start time
uv run python main.py --start_from "2024-01-01 00:00" "0 0 * * *"

# Combine options
uv run python main.py --start_from "2024-01-01 00:00" --count 10 "*/15 * * * *"
```

### Arguments

- `cron_expr` (required): The cron expression to parse (must be the last argument)
- `--start_from`: Start time in format "YYYY-MM-DD HH:MM" (default: current time)
- `--count`: Number of occurrences to generate (default: 1)

### Cron Expression Format

Standard cron format with 5 fields:

```
* * * * *
│ │ │ │ │
│ │ │ │ └─── Day of week (0-6, Sunday=0)
│ │ │ └───── Month (1-12)
│ │ └─────── Day of month (1-31)
│ └───────── Hour (0-23)
└─────────── Minute (0-59)
```

### Output Format

The program outputs each occurrence as CSV in the format:

```
year,month,day,hour,minute
```

## Examples

```bash
# Daily at midnight
uv run python main.py "0 0 * * *"

# Every 15 minutes
uv run python main.py --count 4 "*/15 * * * *"

# Weekly on Monday at 9 AM, starting from a specific date
uv run python main.py --start_from "2024-01-01 00:00" --count 5 "0 9 * * 1"

# First day of every month at noon
uv run python main.py "0 12 1 * *"
```

## Dependencies

- Python 3.8+
- croniter: For parsing and calculating cron occurrences