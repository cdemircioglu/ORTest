git -C ~/ui/ stash save --keep-index
git -C ~/ui/ stash drop
git -C ~/ui/ fetch origin
git -C ~/ui/ merge origin/master
sudo rsync * /opt/shiny-server/openroads/
