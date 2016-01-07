"""Author: Michal Zmuda
Copyright (C) 2015 ACK CYFRONET AGH
This software is released under the MIT license cited in 'LICENSE.txt'

Brings up a set of cluster-worker nodes. They can create separate clusters.
"""

import os
import subprocess
import sys
from . import common, docker, worker, globalregistry

DOCKER_BINDIR_PATH = '/root/build'

def up(image, bindir, dns_server, uid, config_path, logdir=None, app_name="cluster_worker"):
    return worker.up(image, bindir, dns_server, uid, config_path, ClusterWorkerConfigurator(app_name), logdir)


class ClusterWorkerConfigurator:
    def __init__(self, app_name):
        self.app_name_value = app_name
        pass

    def tweak_config(self, cfg, uid):
        return cfg

    def configure_started_instance(self, bindir, instance, config, output):
        pass

    def extra_volumes(self, config):
        return []

    def app_name(self):
        return self.app_name_value

    def domains_attribute(self):
        return "cluster_domains"

    def domain_env_name(self):
        return "cluster_domain"

    def nodes_list_attribute(self):
        return "cluster_worker_nodes"