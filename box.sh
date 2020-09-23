username=$1

set -x
set -e

sudo apt update --fix-missing
sudo apt install -y ansible
curl -s https://raw.githubusercontent.com/mshogin/setup/master/ansible/setup-box.yml -o /tmp/mshogin-setup-box.yml
ansible-playbook -b /tmp/mshogin-setup-box.yml
