#!/bin/bash

set -e
set -u
set -o pipefail

stop_riak_fn() {
    set +e
    echo "stopping riak dev1"
    /home/vagrant/riak/dev/dev1/bin/riak stop
    echo "stopping riak dev2"
    /home/vagrant/riak/dev/dev2/bin/riak stop
    echo "stopping riak dev3"
    /home/vagrant/riak/dev/dev3/bin/riak stop
    echo "stopping riak dev4"
    /home/vagrant/riak/dev/dev4/bin/riak stop
    echo "stopping riak dev5"
    /home/vagrant/riak/dev/dev5/bin/riak stop
    echo "stopping riak dev6"
    /home/vagrant/riak/dev/dev6/bin/riak stop
    echo "stopping riak dev7"
    /home/vagrant/riak/dev/dev7/bin/riak stop
    echo "stopping riak dev8"
    /home/vagrant/riak/dev/dev8/bin/riak stop
    set -e
}

start_riak_fn() {
    echo "Start node 1"
    /home/vagrant/riak/dev/dev1/bin/riak start
    echo "Start node 2"
    /home/vagrant/riak/dev/dev2/bin/riak start
    echo "Start node 3"
    /home/vagrant/riak/dev/dev3/bin/riak start
    echo "Start node 4"
    /home/vagrant/riak/dev/dev4/bin/riak start
    echo "Start node 5"
    /home/vagrant/riak/dev/dev5/bin/riak start
    echo "Start node 6"
    /home/vagrant/riak/dev/dev6/bin/riak start
    echo "Start node 7"
    /home/vagrant/riak/dev/dev7/bin/riak start
    echo "Start node 8"
    /home/vagrant/riak/dev/dev8/bin/riak start
}

setup_tmux_fn() {

    tmux new-session -s riak -n editor -d
    tmux send-keys -t riak 'cd /home/vagrant/riak' C-m
    tmux split-window -h -t riak

    tmux split-window -v -t riak
    #tmux split-window -v -t riak

    tmux select-layout -t riak main-vertical

    tmux send-keys -t riak:1.1 'cd /home/vagrant/riak_kv && clear' C-m
    tmux send-keys -t riak:1.2 '/home/vagrant/riak/dev/dev1/bin/riak attach-direct' C-m
    #tmux send-keys -t riak:1.2 '/home/vagrant/riak/dev/dev2/bin/riak attach-direct' C-m
    tmux send-keys -t riak:1.3 'cd /home/vagrant/vagrant_data/riak && clear' C-m

    tmux attach
}

main() {
    stop_riak_fn
    start_riak_fn
    setup_tmux_fn
}

main
