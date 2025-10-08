def main():
	import sys
	from croniter import croniter
	from datetime import datetime

	cron_expr = '%s'
	start_time = datetime(%d, %d, %d, %d, %d)
	count = %d

	try:
	    cron = croniter(cron_expr, start_time)
	    for _ in range(count):
	        next_time = cron.get_next(datetime)
	        print(f"{next_time.year},{next_time.month},{next_time.day},{next_time.hour},{next_time.minute}")
	except Exception as e:
	    print(f"ERROR: {e}", file=sys.stderr)
	    sys.exit(1)


if __name__ == "__main__":
    main()
