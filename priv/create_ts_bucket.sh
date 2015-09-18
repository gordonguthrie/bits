#!/bin/bash
/home/vagrant/riak/dev/dev1/bin/riak-admin bucket-type create "GeoCheckin" '{"props": {"n_val": 3, "table_def": "CREATE TABLE GeoCheckin (geohash varchar not null, user varchar not null, time timestamp not null, weather varchar not null, temperature varchar, PRIMARY KEY((quantum(time, 15, s)), time, user))"}}'
/home/vagrant/riak/dev/dev1/bin/riak-admin bucket-type activate "GeoCheckin"

