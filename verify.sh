#!/bin/bash

echo '
'
echo -------------------------------
echo building
echo ./make.py complete_rel
./make.py complete_rel

echo '
'
echo -------------------------------
echo running
echo bamboos/docker/env_up.py --bin-cluster-worker .  --bin-cm cluster_manager --app-name example example_env.json
ENV_DESC=`bamboos/docker/env_up.py --bin-cluster-worker .  --bin-cm cluster_manager --app-name example example_env.json`
echo $ENV_DESC

IP1=`docker ps -a | grep 'worker1' | awk '{print $1}' | xargs docker inspect --format '{{ .NetworkSettings.IPAddress }}'`
IP2=`docker ps -a | grep 'worker2' | awk '{print $1}' | xargs docker inspect --format '{{ .NetworkSettings.IPAddress }}'`
IP3=`docker ps -a | grep 'worker3' | awk '{print $1}' | xargs docker inspect --format '{{ .NetworkSettings.IPAddress }}'`

echo '
'
echo -------------------------------
echo healthcheck $IP1:
echo curl http://{$IP1}:6666/nagios
curl http://{$IP1}:6666/nagios

echo '
'
echo -------------------------------
echo healthcheck $IP2
echo http://{$IP2}:6666/nagios
curl http://{$IP2}:6666/nagios

echo '
'
echo -------------------------------
echo healthcheck $IP3
echo http://{$IP3}:6666/nagios
curl http://{$IP3}:6666/nagios

echo '
'
echo -------------------------------
echo saving at $IP1
echo curl "http://${IP1}:8080/example" -G --data-urlencode "value=TEST_VALUE"
KEY=`curl "http://${IP1}:8080/example" -G --data-urlencode "value=TEST_VALUE"`

echo '
'
echo -------------------------------
echo got ID: $KEY


echo '
'
echo -------------------------------
echo waiting for store getting consistent
echo sleep 5s
sleep 5s

echo '
'
echo -------------------------------
echo accessing at $IP1
echo curl "http://{$IP1}:8080/example" -G --data-urlencode "id=${KEY}"
VALUE1=`curl "http://{$IP1}:8080/example" -G --data-urlencode "id=${KEY}"`

echo '
'
echo -------------------------------
echo accessing at $IP2
echo curl "http://{$IP2}:8080/example" -G --data-urlencode "id=${KEY}"
VALUE2=`curl "http://{$IP2}:8080/example" -G --data-urlencode "id=${KEY}"`

echo '
'
echo -------------------------------
echo accessing at $IP3
echo curl "http://{$IP3}:8080/example" -G --data-urlencode "id=${KEY}"
VALUE3=`curl "http://{$IP3}:8080/example" -G --data-urlencode "id=${KEY}"`

echo '
'
echo -------------------------------
echo cleanup
echo $ENV_DESC | grep -Po '"docker_ids": \[\K[^\]]*' | awk -F "\"" '{print $2; print $4; print $6; print $8; print $10; print $12 }' | xargs docker rm -fv


echo '
'
echo -------------------------------
echo checking values consistency {$VALUE1, $VALUE2, $VALUE3, TEST_VALUE}

if [ "$VALUE1" != "TEST_VALUE" ] || [ "$VALUE2" != "TEST_VALUE" ] || [ "$VALUE3" != "TEST_VALUE" ]; then
  echo ERROR
  exit 1
else
  echo OK
fi
