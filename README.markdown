Start/Stop pop3 server
=======================

Start ~~~> sbt run runner.Pop3ServerRunner
Stop  ~~~> echo +STOP | curl --data-ascii - telnet://localhost:111


Lift Project setup
===================

Install Java, Simple Build Tool, and Git

* Clone this repository with:
    git clone git://github.com/liftweb/lift_sbt_prototype.git

* Then cd into lift_sbt_prototype and type:
    sbt
    
* At the sbt prompt, type:
    update

* Then:
    jetty-run

* Point your browser to:
    http://localhost:8080/

Voila a running Lift app