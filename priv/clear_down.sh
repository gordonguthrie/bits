#!/bin/bash

set -e
set -u
set -o pipefail

clear_logs_fn() {
    echo "Clearing all the logs"
    for i in `seq 1 8`;
    do
	DIR="/home/vagrant/riak/dev/dev${i}/log/*"
	echo "clearing ${DIR}"
	rm -f ${DIR}
    done
}

clear_dbs_fn() {
    echo ""
    echo "Clearing all the dbs"
    for i in `seq 1 8`;
    do
	DIR="/home/vagrant/riak/dev/dev${i}/data/leveldb/*"
	echo "clearing ${DIR}"
	rm -rf ${DIR}
    done
}

stop_riak_fn() {
    set +e
    echo ""
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
    echo ""
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

clear_gg_log_fn() {
    echo ""
    echo "Clearing gg.log"
    rm /tmp/gg.log
}

main() {
    stop_riak_fn
    clear_logs_fn
    clear_dbs_fn
    clear_gg_log_fn
    start_riak_fn
}

main
