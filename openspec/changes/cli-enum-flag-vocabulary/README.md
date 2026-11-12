# cli-enum-flag-vocabulary

Make every enum-valued CLI option accept the vocabulary its own `--help` prints.
Six options are parsed with `option auto` (the derived `Read` instance), so they
only accept Haskell constructor spellings (`Sparse`, `FullPush`) while help, the
README, the workflow docs and spec `12-neo4j-push` all document lowercase tokens
(`sparse`, `full`). Replaces the three independent strings with one token table
per enum from which the reader, the metavar and the help text are all derived.
