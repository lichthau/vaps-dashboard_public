#!/usr/bin/env bash

echo "Installing R and shiny-server"
echo "install R"

sudo cp /vagrant/provision/sources_list_vhost /etc/apt/sources.list > /dev/null

sudo apt-key adv --keyserver keyserver.ubuntu.com --recv-keys E084DAB9

sudo apt-get update
sudo apt-get -y upgrade

sudo apt-get -y install r-base r-base-dev > /dev/null
sudo su - -c "R -e \"install.packages('shiny', repos='http://cran.rstudio.com/', quiet=TRUE)\""
sudo su - -c "R -e \"install.packages('plyr', repos='http://cran.rstudio.com/', quiet=TRUE)\""
sudo su - -c "R -e \"install.packages('ggplot2', repos='http://cran.rstudio.com/', quiet=TRUE)\""

echo "install map visualization stuff"
sudo apt-get -y install libgeos-dev libgdal-dev libproj-dev > /dev/null
sudo su - -c "R -e \"install.packages('gpclib',  repos='http://cran.rstudio.com/', quiet=TRUE)\""
sudo su - -c "R -e \"install.packages('rgdal',  repos='http://cran.rstudio.com/', quiet=TRUE)\""
sudo su - -c "R -e \"install.packages('rgeos',  repos='http://cran.rstudio.com/', quiet=TRUE)\""
sudo su - -c "R -e \"install.packages('maptools',  repos='http://cran.rstudio.com/', quiet=TRUE)\""
sudo su - -c "R -e \"install.packages('R.cache',  repos='http://cran.rstudio.com/', quiet=TRUE)\""
sudo su - -c "R -e \"install.packages('RPostgreSQL',  repos='http://cran.rstudio.com/', quiet=TRUE)\""
sudo su - -c "R -e \"install.packages('leaflet',  repos='http://cran.rstudio.com/', quiet=TRUE)\""
sudo su - -c "R -e \"install.packages('jsonlite',  repos='http://cran.rstudio.com/', quiet=TRUE)\""
sudo su - -c "R -e \"install.packages('plotly',  repos='http://cran.rstudio.com/', quiet=TRUE)\""
sudo su - -c "R -e \"install.packages('plyr',  repos='http://cran.rstudio.com/', quiet=TRUE)\""
sudo su - -c "R -e \"install.packages('reshape2',  repos='http://cran.rstudio.com/', quiet=TRUE)\""

echo "install curl"
sudo apt-get -y install libcurl4-gnutls-dev > /dev/null
sudo su - -c "R -e \"install.packages('httr', repos='http://cran.rstudio.com/', quiet=TRUE)\""
echo "install gdebi"
sudo apt-get -y install gdebi-core > /dev/null
sudo wget -q http://download3.rstudio.org/ubuntu-12.04/x86_64/shiny-server-1.3.0.403-amd64.deb
sudo gdebi -q -n shiny-server-1.3.0.403-amd64.deb


# merge default shiny-server directory with linked by vagrant one
if [ 'find /www-shiny -maxdepth 0 -empty | read v' ] 
then 
  sudo rm -rf /srv/shiny-server
  sudo ln -s /www-shiny/ /srv/shiny-server
fi

sudo restart shiny-server



echo "Installing Nginx"
sudo apt-get -y install libpcre3 libpcre3-dev
sudo apt-get -y install zlibc zlib1g zlib1g-dev
sudo apt-get -y install libssl-dev
wget -q http://nginx.org/download/nginx-1.9.1.tar.gz
tar xvfz nginx-1.9.1.tar.gz 
cd nginx-1.9.1/
./configure --with-http_auth_request_module --with-http_ssl_module
make
sudo make install

echo "Configuring Nginx"
sudo cp /vagrant/provision/nginx_vhost /usr/local/nginx/conf/nginx.conf > /dev/null
sudo chown -R vagrant /usr/local/nginx/
cd /usr/local/nginx/sbin
sudo ./nginx

echo "Installing Git"
sudo apt-get install git -y > /dev/null