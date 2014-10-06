## Leader Election Algorithm
[![Build Status](https://travis-ci.org/Djo/leader_election.svg)](https://travis-ci.org/Djo/leader_election)

[The bully algorithm](http://en.wikipedia.org/wiki/Bully_algorithm) is one of methods in [distributed computing](http://en.wikipedia.org/wiki/Distributed_computing) for implementation of the [leader election](http://en.wikipedia.org/wiki/Leader_election) process for dynamically electing a coordinator by process ID number. The process with the highest number is selected as the coordinator.

### Build

To run the algorithm you need a working installation of Erlang R15BXX (or later).

    ~~~ sh
    $ make get-deps
    $ make compile-all
    ~~~

### Usage

Configure `timeout` and `nodes` in the `bully.config`.

    ~~~ sh
    $ ./start.sh 1@127.0.0.1
    $ ./start.sh 2@127.0.0.1
    $ ./start.sh 3@127.0.0.1
    $ ./start.sh 4@127.0.0.1
    ~~~

Then you can kill a node and see logs.

### Description

Implementation consists of monitoring of the current leader and election a new one
when the current is unavailable. The election of a new leader should be done as soon as possible.
You have a distributed network from `N` nodes where each node described by an unique identity number.

#### Monitoring of the current leader

Each node sends `ping` every `T` seconds to the current leader and waits `4*T` seconds for `pong` reply.
On the waiting timeout the leader election process should be started.

#### Leader election process

1. A node started the election starts sending `announce election` to all nodes
   with a higher identity number and waits for `ok`.
    * If no one `ok` after `T` seconds the node starts leading and sends `new leader` to all nodes.
    * If received `ok` the node starts waiting for `new leader` next `T` seconds.
      On `new leader` waiting timeout the node starts announcing election again.
2. When receiving `announce election` a node replies with `ok` and starts the election process itself.
    * If the node received `announce election` is with the highest identity number it starts leading
      with sending `new leader` to all nodes.
3. When receiving `new leader` a node saves a new leader and starts monitoring it.

#### Example

![Example](https://raw.github.com/Djo/leader_election/master/example.png)

Inspired by an article about a test task from [aboutecho.com](http://www.echorussia.ru/jobs/serverside-june-2013.html).

Copyright (C) 2014 [Andrew Djoga](http://andrewdjoga.com), released under the MIT license.
