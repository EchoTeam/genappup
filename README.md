# genappup

A tool to generate appup-file, based on git information.

It checks diff between current HEAD and specified branch/revision, generates appup file and store it into src/{appname}.appup.src


## Workflow with genappup

Common workflow for developers is:

- make branch from master, for instance myfeature

  `git checkout -b myfeature`

- some code developing,fixing, etc

- `git commit`

- go to application root dir (dir that has name the same as application name, with src/ subdir)

- run `genappup master`. It checks which .erl files were changed, generates appup term and store it into src/{appname}.appup.src. If appup file already exists,
  genappup runs merge application ( vimdiff by default, but can be specified by environment variable MERGETOOL)


## How to build genappup

    git clone git@github.com:EchoTeam/genappup.git
    cd genappup
    make

