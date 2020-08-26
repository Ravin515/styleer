#' A simplified save function using saveRDS() or save().
#'
#' @export
#' @param objname expression or char. The object to be saved, use its R object name, e.g., sv(dt).
#' @param filename expression or char. The file name of the serialized object. Default to objname, e.g., sv(dt, dt_new_name)
#' @param compress Whether the object should be compressed
#' @param path To which subfolder the data is saved. Default to '../data'. If './', save to current directory. 
#' @param rds Whether the object should be saved as '.rds' file. The default value is 'TRUE', which the object will be saved as '.Rdata' file.
#'
#' @examples
#' sv(dt) # equals to sv('dt')
#' sv(dt, dt_new_name, path='../Rdata')
sv <- function(objname, filename=NULL, path = "./data", compress = T, rds = T) {
    
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
    svdir <- c(
                paste0(path, '/', substitute(filename), ".rds"), 
                paste0(path, '/', substitute(filename), ".Rdata")
                )

    # serialize with `saveRDS` or `save`
    if (rds == T) {
        saveRDS(objname, file = svdir[1], compress = compress)
    } else {
        save(list = deparse(substitute(objname)), file = svdir[2], compress = compress, compression_level = 3)
    }
    
    cat(str_c("-", substitute(filename), "- saved  "))
    
    end <- Sys.time()
    gap <- end - start
    cat(str_c("(", round(gap, 2), ' ', units(gap), ")\n"))
}


#' A simplified load function using readRDS() or load().
#'
#' @export
#' @param filename expression or char. The file name to be loaded, e.g. ld(x) equals to load(file='dt.rds')
#' @param objname expression or char. The object name in R. If NULL then equals to filename
#' @param force Whether the object should be reloaded if it's already in the current environment.
#' @param path From which subfolder to read the data. Default to '../data'. If './', load from current directory.
#' @param rds Whether the object should be load from '.rds' file. The default value is 'FALSE', which the object will be load from '.Rdata' file.
#'
#' @examples
#' ld(dt)
#' ld(filename=dt, objname=dt_new, path='../Rdata')
ld <- function(filename, objname=NULL, path = './data', force = F, rds = F) {
    start <- Sys.time()

    # create load dir
    lddir <- c(paste0(path, '/', substitute(filename), ".rds"), paste0(path, '/', substitute(filename), ".Rdata"))

    # check if the file exists
    if (sum(file.exists(lddir)) == 0) {
        stop("Object not exists!")
    }
    # create a function list containing `readRDS` and `load`
    importfunc <- function(rds = rds) {
        if (rds == F) {
            load(lddir[2], env = .GlobalEnv)
        } else {
            readRDS(lddir[1])
        }
    }


    # load data
    if (force == F) {
        if (is.null(substitute(objname))) { # if objname is null
            # if filename not exist in global, load
            if (!exists(as.character(substitute(filename)))) {
                val = importfunc
                assign(as.character(substitute(filename)), val, env=.GlobalEnv)
                cat(str_c("-", substitute(filename), "- loaded"))
            } else {
                # if filename exist, Not load
                cat(str_c("-", substitute(filename), "- already exists, will NOT load again!"))
            }
        } else { # if objname isn't null
            # if objname not exist in global, load
            if (!exists(as.character(substitute(objname)))) {
                val = importfunc
                assign(as.character(substitute(objname)), val, env=.GlobalEnv)
                cat(str_c("-", substitute(filename), "- loaded as -", substitute(objname), "-"))
            } else {
                # if objname exist in global, Not load
                cat(str_c("-", substitute(objname), "- already exists, will NOT load again!"))
            }
        }
    } else if (force == T) {
        val = importfunc
        if (is.null(substitute(objname))) {
            assign(as.character(substitute(filename)), val, env=.GlobalEnv)
            cat(str_c("-", substitute(filename), "- loaded"))
        } else {
            assign(as.character(substitute(objname)), val, env=.GlobalEnv)
            cat(str_c("-", substitute(filename), "- loaded as -", substitute(objname), "-"))
        }
    }
        
    # output time elapsed
    end <- Sys.time()
    gap <- end - start
    cat(str_c("  (", round(gap, 2), ' ', units(gap), ")\n"))
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
    flat.data <- data[, rbindlist(.SD[['file_content']], fill = T, idcol = "file_id")]
    flat.data

    # output time elapsed
    # end <- Sys.time()
    # gap <- end - start
    # cat(str_c("  (", round(gap, 2), ' ', units(gap), ")\n"))
}