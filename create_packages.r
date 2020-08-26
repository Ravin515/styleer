# - create a template

# create("C:\Users\MrStylee\OneDrive\Code\styleer")

# - put all .R files into the R folder

# - generate document 



# setwd(stringr::str_c('/mnt/c/Users/MrStylee/Onedrive/app/r/'))

# setwd(stringr::str_c(Sys.getenv("OneDrive"), "\\App\\R"))
devtools::document()

# - install the lib!

devtools::install('styleer')

# - push to github!
# - Actually, you don't need to push to Github to have `styleer` available across machines. That's because `styleer` has already been synced via OneDrive. So for every new machine, sign in your Onedrive, sync the `styleer` folder, enter it, and run `devtools::install('styleer')`. There you go!

# install your package from github!
devtools::install_github('ravin515/styleer')