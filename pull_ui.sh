git -C ~/ui/ stash save --keep-index
git -C ~/ui/ stash drop
git -C ~/ui/ fetch origin
git -C ~/ui/ merge origin/master
sed -i -e 's/8091/8191/g' server.R
sudo rsync * /opt/shiny-server/openroads/
