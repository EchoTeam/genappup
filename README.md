# genappup

A tool to generate Erlang appup files, based on git information.

It analyzes diff between current HEAD and specified branch/revision, generates appup file and store it into src/{appname}.appup.src


## Basic workflow

Common workflow for developers is:

- make branch from master, for instance myfeature

  `git checkout -b myfeature`

- some code developing, fixing, etc

- `git commit`

- go to application root dir

- run `genappup master`. It analyzes which .erl files were changed, generates appup term and store it into src/{appname}.appup.src. If appup file already exists,
  genappup runs merge application (vimdiff by default, but can be specified by environment variable MERGETOOL)


## How to build and install genappup

    git clone git@github.com:EchoTeam/genappup.git
    cd genappup
    make
    
Make genappup available through your PATH variable, for example, by this:

    sudo cp genappup /usr/local/bin/
