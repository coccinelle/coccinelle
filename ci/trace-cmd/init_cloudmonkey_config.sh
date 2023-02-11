set -ex
cat >~/.cmk/config <<EOF
asyncblock = true
output = json
timeout = 1800
[ci]                                              
url = https://sesi-cloud-ctl1.inria.fr/client/api/
apikey = $CLOUDSTACK_API_KEY
secretkey = $CLOUDSTACK_SECRET_KEY
EOF
