# coding=utf-8
"""Authors: Łukasz Opioła, Konrad Zemek
Copyright (C) 2015 ACK CYFRONET AGH
This software is released under the MIT license cited in 'LICENSE.txt'

Brings up dockers with full onedata environment.
"""

import os
import copy
import subprocess
import json
from . import appmock, client, common, globalregistry, cluster_manager, \
    provider_worker, cluster_worker, docker, dns


def default(key):
    return {'image': 'onedata/worker:v25',
            'bin_am': '{0}/appmock'.format(os.getcwd()),
            'bin_gr': '{0}/globalregistry'.format(os.getcwd()),
            'bin_op_worker': '{0}/op_worker'.format(os.getcwd()),
            'bin_cluster_worker': '{0}/cluster_worker'.format(os.getcwd()),
            'bin_cluster_manager': '{0}/cluster_manager'.format(os.getcwd()),
            'bin_oc': '{0}/oneclient'.format(os.getcwd()),
            'logdir': None,
            'app_name': 'cluster_worker'}[key]


def up(config_path, image=default('image'), bin_am=default('bin_am'),
       bin_gr=default('bin_gr'), bin_cluster_manager=default('bin_cluster_manager'),
       bin_op_worker=default('bin_op_worker'), bin_cluster_worker=default('bin_cluster_worker'),
       bin_oc=default('bin_oc'), logdir=default('logdir'), app_name=default('app_name')):
    config = common.parse_json_file(config_path)
    uid = common.generate_uid()

    output = {
        'docker_ids': [],
        'gr_nodes': [],
        'gr_db_nodes': [],
        'cluster_manager_nodes': [],
        'op_worker_nodes': [],
        'cluster_worker_nodes': [],
        'appmock_nodes': [],
        'client_nodes': []
    }

    # Start DNS
    [dns_server], dns_output = dns.maybe_start('auto', uid)
    common.merge(output, dns_output)

    # Start appmock instances
    if 'appmock_domains' in config:
        am_output = appmock.up(image, bin_am, dns_server,
                               uid, config_path, logdir)
        common.merge(output, am_output)
        # Make sure appmock domains are added to the dns server.
        # Setting first arg to 'auto' will force the restart and this is needed
        # so that dockers that start after can immediately see the domains.
        dns.maybe_restart_with_configuration('auto', uid, output)

    # Start globalregistry instances
    if 'globalregistry_domains' in config:
        gr_output = globalregistry.up(image, bin_gr, dns_server,
                                      uid, config_path, logdir)
        common.merge(output, gr_output)
        # Make sure GR domains are added to the dns server.
        # Setting first arg to 'auto' will force the restart and this is needed
        # so that dockers that start after can immediately see the domains.
        dns.maybe_restart_with_configuration('auto', uid, output)

    # Start provider cluster instances
    setup_worker(provider_worker, bin_op_worker, 'provider_domains',
                 bin_cluster_manager, config, config_path, dns_server, image, logdir, output, uid, app_name)

    # Start stock cluster worker instances
    setup_worker(cluster_worker, bin_cluster_worker, 'cluster_domains',
                 bin_cluster_manager, config, config_path, dns_server, image, logdir, output, uid, app_name)

    # Start oneclient instances
    if 'oneclient' in config:
        oc_output = client.up(image, bin_oc, dns_server, uid, config_path)
        common.merge(output, oc_output)

    # Setup global environment - providers, users, groups, spaces etc.
    if 'globalregistry_domains' in config \
            and 'provider_domains' in config \
            and 'global_setup' in config:
        providers_map = {}
        for provider_name in config['provider_domains']:
            providers_map[provider_name] = {
                'nodes': [],
                'cookie': ''
            }
            for cfg_node in config['provider_domains'][provider_name][
                'op_worker'].keys():
                providers_map[provider_name]['nodes'].append(
                    provider_worker.worker_erl_node_name(cfg_node,
                                                         provider_name, uid))
                providers_map[provider_name]['cookie'] = \
                    config['provider_domains'][provider_name]['op_worker'][
                        cfg_node]['vm.args']['setcookie']

        env_configurator_input = copy.deepcopy(config['global_setup'])
        env_configurator_input['provider_domains'] = providers_map

        # For now, take only the first node of the first GR
        # as multiple GRs are not supported yet.
        env_configurator_input['gr_cookie'] = \
            config['globalregistry_domains'].values()[0][
                'globalregistry'].values()[0]['vm.args']['setcookie']
        env_configurator_input['gr_node'] = output['gr_nodes'][0]

        env_configurator_dir = '{0}/../../env_configurator'.format(
            common.get_script_dir())

        # Newline for clearer output
        print('')
        # Run env configurator with gathered args
        command = '''epmd -daemon
        ./env_configurator.escript \'{0}\''''
        command = command.format(json.dumps(env_configurator_input))
        docker.run(
            image='onedata/builder:v25',
            interactive=True,
            tty=True,
            rm=True,
            workdir='/root/build',
            name=common.format_hostname('env_configurator', uid),
            volumes=[(env_configurator_dir, '/root/build', 'ro')],
            dns_list=[dns_server],
            command=command
        )

    return output


def setup_worker(worker, bin_worker, domains_name, bin_cm, config, config_path, dns_server, image, logdir, output, uid, app_name):
    if domains_name in config:
        # Start cluster_manager instances
        cluster_manager_output = cluster_manager.up(image, bin_cm, dns_server, uid, config_path, logdir,
                                                    domains_name=domains_name)
        common.merge(output, cluster_manager_output)

        # Start op_worker instances
        cluster_worker_output = worker.up(image, bin_worker, dns_server, uid, config_path, logdir, app_name)
        common.merge(output, cluster_worker_output)
        # Make sure OP domains are added to the dns server.
        # Setting first arg to 'auto' will force the restart and this is needed
        # so that dockers that start after can immediately see the domains.
        dns.maybe_restart_with_configuration('auto', uid, output)
