#' A simplified save function using saveRDS() or save().
#'
#' @export
#' @param objname expression or char. The object to be saved, use its R object name, e.g., sv(dt).
#' @param svname expression or char. The file name of the serialized object. Default to objname, e.g., sv(dt, dt_new_name)
#' @param compress Whether the object should be compressed
#' @param path To which subfolder the data is saved. Default to '../data'. If './', save to current directory. 
#' @param svtype The extension of saved file. Currently only `Rdata`, `rds` and `feather` are available. Default to `rds`. 
#'
#' @examples
#' sv(dt) # equals to sv('dt')
#' sv(dt, dt_new_name, path='../Rdata')
sv <- function(objname, svname=NULL, svtype = 'Rdata', path = "./data", compress = T) {
    
    start <- Sys.time()

    # if folder not exists, create one
    if (!file.exists(path) & (!path %in% c('./', ''))) {
        dir.create(path)
        message(paste0("Folder ", "-", path, "- created"))
    }

    # create save directory
    if (is.null(substitute(filename))) {
        filename = as.character(substitute(objname))
    }
    svdir <- paste0(path, '/', substitute(svname), '.', svtype)

    # serialize with `saveRDS` or `save`
    if (svtype == 'Rdata') {
        save(list = deparse(substitute(objname)), file = svdir, compress = compress, compression_level = 3)
    } else if (svtype == "rds"){
        saveRDS(objname, file = svdir, compress = compress)
    } else if (svtype == "feather") {
        feather :: write_feather(objname, svdir)
    }
    cat(stringr::str_c("-", substitute(svname), "- saved  "))
    
    end <- Sys.time()
    gap <- end - start
    cat(stringr::str_c("(", round(gap, 2), ' ', units(gap), ")\n"))
}


#' A simplified load function using readRDS(), load() or read_feather().
#'
#' @export
#' @param filename expression or char. The file name to be loaded, e.g. ld(x) equals to load(file='dt.rds')
#' @param objname expression or char. The object name in R. If NULL then equals to filename.
#' @param force Whether the object should be reloaded if it's already in the current environment.
#' @param path From which subfolder to read the data. Default to '../data'. If './', load from current directory.
#' @param ldtype `Rdata`, `rds` or `feather`. The default is `Rdata`.
#'
#' @examples
#' ld(dt)
#' ld(filename=dt, objname=dt_new, path='../Rdata')
ld <- function(filename, objname=NULL, ldtype = 'Rdata', path = './data', force = F) {
    start <- Sys.time()

    # create load dir
    lddir <- paste0(path, '/', substitute(filename), ".", ldtype)

    # check if the file exists
    if (!file.exists(lddir)) {
        stop("Object not exists!", lddir)
    }

    # load data
    if (force == F) {
        if (is.null(substitute(objname))) { # if objname is null
            # if filename not exist in global, load
            if (!exists(as.character(substitute(filename)))) {
                if (ldtype == 'Rdata') {
                    load(lddir, envir = .GlobalEnv)
                } 
                else {
                    if (ldtype == 'rds') {
                        val = readRDS(lddir)
                    }
                    else if (ldtype == 'feather') {
                        val = feather::read_feather(lddir) %>% data.table::setDT()
                    }
                    assign(as.character(substitute(filename)), val, env=.GlobalEnv)
                }
                
                cat(paste0("-", substitute(filename), "- loaded"))
            } 
            else {
                # if filename exist, Not load
                cat(paste0("-", substitute(filename), "- already exists, will NOT load again!"))
            }
        } 
        else { # if objname isn't null
            # if objname not exist in global, load
            if (!exists(as.character(substitute(objname)))) {
                if (ldtype == 'Rdata') {
                    load(lddir, envir = .GlobalEnv)
                }
                else {
                    if (ldtype == 'rds') {
                        val = readRDS(lddir)
                    }
                    else if (ldtype == 'feather') {
                        val = feather::read_feather(lddir) %>% data.table::setDT()
                    }
                    assign(as.character(substitute(filename)), val, env = .GlobalEnv)
                }
                cat(paste0("-", substitute(filename), "- loaded as -", substitute(objname), "-"))

            } 
            else {
                # if objname exist in global, Not load
                cat(paste0("-", substitute(objname), "- already exists, will NOT load again!"))
            }
        }
    } 
    else if (force == T) {
        if (is.null(substitute(objname))) {
            if (ldtype == 'Rdata') {
                load(lddir, envir = .GlobalEnv)
            }
            else {
                if (ldtype == 'rds') {
                    val = readRDS(lddir)
                }
                else if (ldtype == 'feather') {
                    val = feather::read_feather(lddir) %>% data.table::setDT()
                }
                assign(as.character(substitute(filename)), val, env = .GlobalEnv)
            }
            cat(paste0("-", substitute(filename), "- loaded"))
        } else {
            #assign(as.character(substitute(objname)), val, env=.GlobalEnv)
            cat(paste0("-", substitute(filename), "- loaded as -", substitute(objname), "-"))
        }
    }
        
    # output time elapsed
    end <- Sys.time()
    gap <- end - start
    cat(paste0("  (", round(gap, 2), ' ', units(gap), ")\n"))
}

#' A simplified read batches function using fread().
#'
#' @export
#' @param path char. The files' path to be loaded. The default corresponds to the the './data' under working directory.
#' @param pattern an optional regular expression. Only file names which match the regular expression will be returned. 
#' @param ... arguments in fread(). Details refer to ?fread().
#'
#' @examples
#' fbread(pattern = "*.csv")
#' fbread(pattern = "*.csv", encoding = "UTF-8")

fbread <- function(path = './data', pattern = NULL, ...) {
    # start <- Sys.time()

    # check if the path exists
    if (!file.exists(path)) {
        stop("Object not exists!")
    }

    # read data
    files.name <- list.files(path = path, pattern = pattern) # 

    data <- data.table(file_id = files.name)

    data[,  file_content := lapply(file_id, function(x) fread(paste0(path, "/", x), ...))]

    # bind all the batches
    flat.data <- data[, rbindlist(.SD[['file_content']], fill = T), by = .(file_id)]
    flat.data

    # output time elapsed
    # end <- Sys.time()
    # gap <- end - start
    # cat(str_c("  (", round(gap, 2), ' ', units(gap), ")\n"))
}