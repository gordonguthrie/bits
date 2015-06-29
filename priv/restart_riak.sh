#!/bin/bash

set -u
set -e
set -o pipefail

restart_riak_fn() {
    echo "restarting riak dev1"
    /home/vagrant/riak/dev/dev1/bin/riak restart
    echo "restarting riak dev2"
    /home/vagrant/riak/dev/dev2/bin/riak restart
    echo "restarting riak dev3"
    /home/vagrant/riak/dev/dev3/bin/riak restart
    echo "restarting riak dev4"
    /home/vagrant/riak/dev/dev4/bin/riak restart
    echo "restarting riak dev5"
    /home/vagrant/riak/dev/dev5/bin/riak restart
    echo "restarting riak dev6"
    /home/vagrant/riak/dev/dev6/bin/riak restart
    echo "restarting riak dev7"
    /home/vagrant/riak/dev/dev7/bin/riak restart
    echo "restarting riak dev8"
    /home/vagrant/riak/dev/dev8/bin/riak restart
}

main() {
    restart_riak_fn
}

main
