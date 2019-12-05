#!/usr/bin/env bash

EMAIL=${EMAIL:-fap@gaia}
SSH_KEYFILE_PASSWORD=${SSH_KEYFILE_PASSWORD:-}
KEYFILE="$HOME/.ssh/id_rsa"

# see https://stackoverflow.com/a/246128/4266296
SOURCE="${BASH_SOURCE[0]}"
while [ -h "$SOURCE" ]; do # resolve $SOURCE until the file is no longer a symlink
  DIR="$( cd -P "$( dirname "$SOURCE" )" >/dev/null 2>&1 && pwd )"
  SOURCE="$(readlink "$SOURCE")"
  [[ $SOURCE != /* ]] && SOURCE="$DIR/$SOURCE" # if $SOURCE was a relative symlink, we need to resolve it relative to the path where the symlink file was located
done
DIR="$( cd -P "$( dirname "$SOURCE" )" >/dev/null 2>&1 && pwd )"

# navigate to project root directory
cd $DIR/..

sudo apt-get update

if [ ! -f "$KEYFILE" ]; then
    ssh-keygen -t rsa -b 4096 -C $EMAIL -f "$KEYFILE" -N "$SSH_KEYFILE_PASSWORD"
fi

sudo apt-get install openssh-server

sudo cp -f $DIR/../files/sshd_config /etc/ssh/sshd_config

# cat ~/.ssh/id_rsa.pub > ~/.ssh/authorized_keys

# sudo systemctl restart sshd

sudo apt-get install python3 python3-distutils

echo "PATH=\$PATH:~/.local/bin" >> "$HOME/.bashrc"
export PATH=$PATH:~/.local/bin

curl https://bootstrap.pypa.io/get-pip.py -o get-pip.py

python3 get-pip.py --user

pip3 install ansible --user

ansible-playbook setup.yml
