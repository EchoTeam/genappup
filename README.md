genappup
========

Tool for generate appup file, based on git information

It checks diff between current branch and specified branch, generates appup file and store it into src/{appname}.appup.src


Workflow with genappup
======================

Common workflow for developers is:

- make branch from master, for instance myfeature

  git checkout -b myfeature

- some code developing,fixing, etc

- git commit

- go to application root dir (dir that has name the same as application name, with src/ subdir)

- run genappup. It checks which .erl  files was changed, generates appup term and store it into src/myfeature.appup.src . If appup file already exists,
  genappup runs merge application ( vimdiff by default, but can be specified by environment variable MERGETOOL)
  
  genappup master


How to build genappup
======================

- Clone genappup sources from github 
- goto root dir
- make.

