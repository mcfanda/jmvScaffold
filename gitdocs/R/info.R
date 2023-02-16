# folders should be absolute or relative to the docssource folder
# do not use the trailing slash
## full path
MODULE_FOLDER="~/Skinner/Forge/jamovi/jmvScaffold"

## full path of project
PROJECT_FOLDER="~/Skinner/Forge/jamovi/jmvScaffold/gitdocs"

## where the source Rmd are, it is the folder where _site.yaml is.
SOURCE_FOLDER="docssource"


MODULE_REPO="myrepo"
MODULE_REPO_OWNER="mcfanda"
# Mantainer email
MODULE_EMAIL="mcfanda@gmail.com"
## first version to list in the release notes
FIRST_VERSION="Version.0.0.0"
MODULE_NAME="jmvScaffold"
MODULE_LINK="https://github.com/mcfanda/jmvScaffold"
## commits you do not want in the release note
## if you do not need selection of commits, just put something absurd in them
BANNED_COMMITS_GREP=list("^#","^!","^Merge branch","spelling")
BANNED_COMMITS=list(".FuckIGotItAllWrong.")

# These handle the R vignettes

VIGNETTES_FOLDER=""
