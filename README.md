A cron parser library written in OCaml.

## A comparison of datetime libraries in OCaml

Library            |  Precision   |  Timezone Support      |  DST-Aware  |  Calendar Arithmetic  |  Dependencies        |  Key Strengths                                        |  Key Weaknesses
-------------------+--------------+------------------------+-------------+-----------------------+----------------------+-------------------------------------------------------+---------------------------------------------------
Calendar           |  Second      |  Yes                   |  Yes        |  Yes                  |  Minimal             |  Full-featured, mature, timezone support              |  Less precise than newer options
Timedesc           |  Nanosecond  |  Yes (comprehensive)   |  Yes        |  Yes (advanced)       |  Moderate            |  Complex temporal queries, natural language parsing   |  More complex API, heavier dependencies
Timmy              |  Nanosecond  |  Yes (enforced)        |  Yes        |  Yes                  |  Ptime + extensions  |  Timezone safety by design, prevents common bugs      |  Relatively new, smaller ecosystem
Core.Time/Time_ns  |  Nanosecond  |  Yes (extensive)       |  Yes        |  Yes                  |  Jane Street Core    |  Production-grade, battle-tested in finance           |  Heavy dependencies, Jane Street ecosystem lock-in
Jiff (Rust)        |  Nanosecond  |  Yes (IANA database)   |  Yes        |  Yes                  |  Minimal             |  High-level API design, fast, lossless serialization  |  Rust-only, relatively new (2024)
