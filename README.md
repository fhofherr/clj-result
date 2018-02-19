# clj-result

`clj-result` allows to perform a set of operations until either all
have succeeded or one has failed.

## Usage

Add

```clojure
[fhofherr/clj-result "0.1.0-SNAPSHOT"]
```

to your projects dependencies.

Use the `attempt` and `attempt-v` macros to perform a set of potentially
failing operations.

## License

Copyright © 2018 Ferdinand Hofherr

Distributed under the MIT License.
