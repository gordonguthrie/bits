TS Runner
---------

End to end runner to mucking about with Time Series stuff

Setup
-----

On the main riak do
`make stagedevrel`

Edit the files
`$RIAK/dev/devN/etc/riak.conf`

and set the following:
`storage_backedn=leveldb`

Writing Data to the backend
---------------------------

1 Setup the bucket type and stuff

`ts_runner:setup()`

This needs to be run every session at the mo - as the DDL module don't persist between node restarts (yet)

2 Load up some time series data into eleveldb

Creates 10 records and fires them into the test table

`ts_runnner:load()`

3 See what is in eleveldb

`ts_runner:dump()`

Other Utilities
---------------

You can look at the test DDL with

`ts_runner:get_ddl()`

You can see the test bucket properties with

`ts_runner:show()`


Query Dev (Reading Data)
------------------------

Stuff that is not really usable unless you are deving queries 'cos it ain't working yet

`ts_runner:prep_q()`

`ts_runner:qry()`

`ts_runner:qry(#{riak_kv_li_index_v1{} = Q)`

'ts_runner:bounce_qry()`