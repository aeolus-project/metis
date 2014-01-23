Metis
=======

METIS - Modern Engineered Tool for Installing Software systems

1. Introduction
2. Download, compile and run
3. User guide

Introduction
------------

Metis is a tool for the automatic synthesis of deployment plans.
Starting from a list of the available components, it generates a sequence of actions to be performed in order to deploy a target component in a desired state. 
Each component is described by an automaton representation that captures the important states involved in the deployment process, as well as the functionalities required and provided.
This formal declarative description enables Metis to find the way to deploy a target component.
The deployment plan output by Metis consists of a sequence of actions such as component creation, state change and wiring to other components.

The Metis tool is a part of the [Aeolus Project](http://www.aeolus-project.org).


Download, compile and run
-------------------------

####Download

The latest version of the tool is available at the Metis git repository at github: https://github.com/aeolus-project/metis.

You can get it by cloning the repository to your machine (you obviously need [git](http://git-scm.com/) to do this):

```sh
git clone git@github.com:aeolus-project/metis.git
```

####Compile

To compile the Metis tool simply type `make all`.
<!---
There is also the possibility to compile a `VERBOSE` mode useful for debugging and seeing what happens at every step, by typing `make verbose`. 
-->

####Run

To check if Metis works properly you can run a simple test:

```sh
./metis.native -u ./input/dummy_two_instances.json -c C1 -s s2 -o ./results/dummy_two_instances_results.txt
```

User Guide
----------

####Basics

To obtain a short description of basic options available when using the tool type `./metis.native -help`.

For simple examples of use see the test scripts "test*.sh" in the "demo" 
directory.

For several other test examples see files in the "input" directory. 
Results are stored in the "results" directory.

