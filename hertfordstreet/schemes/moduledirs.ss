#lang scheme

(require scheme/date)

(printf "Today is ~s~n" (date->string (seconds->date (current-seconds))))

(require setup/dirs)
(find-collects-dir)
(find-user-collects-dir)
(get-collects-search-dirs)