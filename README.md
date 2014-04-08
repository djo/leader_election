## Bully Election Algorithm

[The bully algorithm](http://en.wikipedia.org/wiki/Bully_algorithm) is one of methods in [distributed computing](http://en.wikipedia.org/wiki/Distributed_computing) for implementation of the [leader election](http://en.wikipedia.org/wiki/Leader_election) process for dynamically electing a coordinator by process ID number. The process with the highest number is selected as the coordinator.

### Build

To run the algorithm you need a working installation of Erlang R15BXX (or later).

    $ make get-deps
    $ make compile-all

### Usage

Configure `timeout` and `nodes` in the `bully.config`.

    $ ./start.sh 1@127.0.0.1
    $ ./start.sh 2@127.0.0.1
    $ ./start.sh 3@127.0.0.1
    $ ./start.sh 4@127.0.0.1

Then you can kill a node and see logs.

Copyright (C) 2014 [Andrew Djoga](http://andrewdjoga.com), released under the MIT license.
