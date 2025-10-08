import sys
import argparse
from croniter import croniter
from datetime import datetime


def main():
    parser = argparse.ArgumentParser(
        description='Generate next occurrence times for a cron expression'
    )

    parser.add_argument(
        '--start_from',
        type=str,
        help='Start time in format "YYYY-MM-DD HH:MM" (default: current time)',
        default=None
    )

    parser.add_argument(
        '--count',
        type=int,
        help='Number of occurrences to generate (default: 1)',
        default=1
    )

    parser.add_argument(
        'cron_expr',
        type=str,
        help='Cron expression (e.g., "0 0 * * *")'
    )

    args = parser.parse_args()

    # Parse start_from if provided, otherwise use current time
    if args.start_from:
        try:
            start_time = datetime.strptime(args.start_from, "%Y-%m-%d %H:%M")
        except ValueError:
            print(f"ERROR: Invalid start_from format. Use 'YYYY-MM-DD HH:MM'", file=sys.stderr)
            sys.exit(1)
    else:
        start_time = datetime.now()

    # Generate cron occurrences
    try:
        cron = croniter(args.cron_expr, start_time)
        for _ in range(args.count):
            next_time = cron.get_next(datetime)
            print(next_time.strftime("%Y-%m-%d %H:%M:%S"))
    except Exception as e:
        print(f"ERROR: {e}", file=sys.stderr)
        sys.exit(1)


if __name__ == "__main__":
    main()
