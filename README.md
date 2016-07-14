# VAPS-dashboard

Development environment currently based on preconfigured [shiny server](https://github.com/nsh87/shinyVM). 

##Prerequisites for development

* VirtualBox installed [https://www.virtualbox.org/wiki/Downloads](https://www.virtualbox.org/wiki/Downloads)
* Vagrant installed: [https://www.vagrantup.com/downloads](https://www.vagrantup.com/downloads)
* Fabric installed: [http://www.fabfile.org/installing.html](http://www.fabfile.org/installing.html)
* Python installed: [https://www.python.org/downloads/](https://www.python.org/downloads/)

```
git clone git@github.com:maruhnth/vaps-dashboard.git
```
follow insttructions from preconfigured shiny-server link,
```
cd vaps-dashboard
vagrant up
```

There might be an issue during installation after running ```fab vagrant setup_vagrant```, see [issue](https://github.com/wutali/vagrant-fabric/issues/5) for how to solve and run command again.