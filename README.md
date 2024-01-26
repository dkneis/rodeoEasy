``rodeoEasy`` - ODE-based models for teaching
======================================================

About
---------------------------------------------

- ``rodeoEasy`` is an add-on package for the [``R ``](https://www.r-project.org/) statistical computing software. It provides a set of high-level functions for the simulation of system dynamics based on simultaneous ODE.

- ``rodeoEasy`` is built on top of the [``rodeo``](https://cran.r-project.org/package=rodeo) package. See the documentation of the latter to learn about fundamental concepts.

Installation
---------------------------------------------

The ``rodeoEasy`` package is not in the CRAN repository. Hence, it needs to be installed from this repository like so:

```
library("remotes")
remotes::install_github("dkneis/rodeoEasy")
library("rodeoEasy")
```

Built-in ODE models
---------------------------------------------

The ``rodeoEasy`` package comes with a set of built-in ODE models. They are implemented in workbooks which can be opened with common spreadsheet software. The following R command prints the name of the folder containing the workbooks on your local computer.

```
print(system.file("models", package="rodeoEasy"))
```

Use your file manager to navigate to this folder and inspect the workbooks.


Minimum example
---------------------------------------------

Use the examples provided as part of the standard documentation as a starting point.

```
library("rodeoEasy")
example("run.scenarios")
```

Want to learn more about it?
---------------------------------------------

Write to the package developer, David Kneis (firstname.lastname@tu-dresden.de), to obtain a set of fully documented teaching examples.
