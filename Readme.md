# ClusterBase Example

ClusterBase aims to facilitate development of applications based on **cluster architecture**.

It lets you create **fault resilient** application equipped with **load balancing, consistent datastore, task scheduling, status monitoring**.

Al that comes as **configurable and extensible** as possible. Try this example to check it out!


## Getting Started

These instructions will get you a copy of the project up and running on your local machine for development and testing purposes. 
See deployment for notes on how to deploy the project on a live system.


### Prerequisities

Required software:

- python 2.7.x interpreter
- docker 1.11.x

Ad that's it!

## Running the example

#### Build

Build the project via:

```
./make.py complete_rel
```

#### Run

And run project with simple configuration:

```
bamboos/docker/env_up.py example_env.json
```

This will yield running couple of docker instances. 

#### Status check

To check status of the cluster examine XML returned via:

```
curl -v http://<IP of any worker>:6666/nagios
```

IP of worker may be retrieved by 
```
docker inspect <Docker ContainerID>
```

DockerID can belong to any worker. To find worker containers look for names like *worker@...* in output of 
```
docker ps
```

### Playing with example

Example is simple - it allows you to store values via http and retrieve them.
Just select any node to send 'set' request:

```
curl "http://<IP of any worker>:8080/example" -G --data-urlencode "value=<your value>"
```

In response you will retrieve a key which lets you to read the data.
Just select any other worker to read the value.

```
curl "http://<IP of any worker>:8080/example" -G --data-urlencode "id=<key>"
```

Your saved values will be accessible from any worker.

#### Running the tests

If you happen to add any coverage to the example app, launch unit tests via 

```
./make.py eunit
```

## Overview

### Ubiquitous language

**Worker** - node that handles your requests. Application consists of several workers, database nodes and single cm node. 

**CM** - cluster management node; it orchestrates your workers and database nodes.

**Datastore** - service provided by the application; it provides various functionalities focused on persisting and caching your data.

### Features

#### Datastore

Datastore lets you save any key value data across the cluster. 

You may make saved data available across all workers or save data on selected nodes only.

You may as well select caching policy - should data be cached at global level (all nodes share cache) or local level (single worker does the caching).

#### Task Manager

Task Manager lets you schedule yor own task to execute. 

It enables you to easely create tasks executed across all the application. You may even schedule tasks at 'persistent' 
level what gives you guarantee that task is executed (TODO: explain persistent level)

#### Request Dispatcher (Worker Proxy)

It provides you with load balancing. Just invoke your callbacks via Worker Proxy.

#### Worker Host

Enhance your application as simple as possible - simply implement one behaviour, add to config and your own handler is ready.
Handler can do just anything and it is already distributed all over cluster.
Moreover is is equiped with lifecycle management, load balancing and all other benefits provided by this project.

#### Status checks

All parts of the applications are monitored. You can check it via REST interface. Your own, custom checks can be simply added.

### Extensions

Enhancing this solution nmay involve **registering plugins**. Do this via adding entry *{plugin_name, your_module_name}* to in [app.config](rel/files/app.config).

#### Datastore

To extend default datastore abilities you can define own structures, that you want to persist.

1. Define records like in [example_model_def.hrl](include/datastore/example_model_def.hrl)
2. Implement models like in [example_shared_model.erl](src/modules/datastore/models/example_shared_model.erl) and  [example_model.erl](src/modules/datastore/models/example_model.erl)
3. Tell datastore to use it via [datastore_config_pluginl](src/modules/datastore/atastore_config_plugin.erl)
3. Register the plugin in [app.config](rel/files/app.config).

When defining the models you can select persistence policy via '''model_init''' callback.

Available strategies:

- GLOBAL_ONLY_LEVEL - persist in db so all nodes can access, do not cache
- LOCAL_ONLY_LEVEL - save on node which performs save, do not cache
- LOCALLY_CACHED_LEVEL - persist in db, each worker has own cache
- GLOBALLY_CACHED_LEVEL - persist in db, all workers share cache

#### Status checks

1. To select which checks should be performed, implement ```healthcheck_endpoints``` callback in [http_worker_plugin_behaviour](deps/cluster_worker/src/modules/http/http_worker_plugin_behaviour.erl)
2. Add potential check by implementing [endpoint_healthcheck_behaviour](deps/cluster_worker/src/modules/http/endpoint_healthcheck_behaviour.erl)

#### Worker plugins

Worker carry out various operations - it is up to you what your worker does. We provide framework for defining own plugins and enhancing workers.
 
1. Create own worker like [example_worker](src/modules/example_worker/example_worker.erl)
2. Tell cluster to load it via ```modules_with_args``` callback in [node_manager_plugin](src/cluster_elements/node_manager/node_manager_plugin.erl)
3. Register the plugin in [app.config](rel/files/app.config).

#### Listeners

Listeners allow you to create own http endpoints or to accept tcp/udp connections.

Listener is not a toolkit - it is only something that will attach and work as long as application does. 
See [rest_listener](src/modules/rest/rest_listener.erl).

#### Node Manager 

Node manager handles lifetime of worker plugins and listeners. You may extend it to provide handling of own messages.

See gen-server-like callbacks in  [node_manager_plugin](src/cluster_elements/node_manager/node_manager_plugin.erl).

#### HTTP 

Http worker plugin can extend default http handler (cowboy handler) with your dedicated behaviour. 
It also defines what to check in order to handle healthcheck REST requests.

1. Implement plugin like [http_worker_plugin](src/modules/http_worker/http_worker_plugin.erl).
2. Register the plugin in [app.config](rel/files/app.config).


#### Other configuration 

As in this example it is encouraged to use *bamboos scripts* and json files. 

But you can configure your clusters manually. Just implement callbacks ```cm_nodes``` and ```db_nodes``` in [node_manager_plugin](src/cluster_elements/node_manager/node_manager_plugin.erl)
Do remember about setting cookie in vm.args as well.


## License

See the [LICENSE.txt](LICENSE.txt) file for details

