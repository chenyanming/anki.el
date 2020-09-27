;;; anki/tests/anki-bench.el -*- lexical-binding: t; -*-

(benchmark 1 '(let ((file (concat (file-name-as-directory anki-collection-dir) "collection.anki2"))
                    (temp (concat (file-name-as-directory temporary-file-directory) "collection.anki2")))
                (if (file-exists-p file)
                    (progn
                      (copy-file file temp t)) nil)))

(benchmark 1 '(anki-core-cards-list))
(benchmark 1 '(anki-core-parse-cards))
(benchmark 1 '(anki-core-cards))

(benchmark 1 '(anki-core-parse-decks))
(benchmark 1 '(anki-core-parse-models))
(benchmark 1 '(anki-core-parse-cards))

(benchmark 1 '(anki-search-update))

(profiler-start 'cpu)
(profiler-stop)
(anki-core-parse-cards)
(profiler-report)

(profiler-start 'cpu)
(profiler-stop)
(anki-core-parse-cards)
(profiler-report)
