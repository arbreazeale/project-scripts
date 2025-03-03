# ------------------------------------------------------------------------------------------------ #
#                                          Path functions                                          #
# ------------------------------------------------------------------------------------------------ #

get_paths <- function(directory) { 
# function stores standard paths for data directory
# function should be included early in any project script

    if(missing(directory)) { 
        directory <- readline(prompt="Enter directory: ")
    }

    base     <- directory 
    archive  <- file.path(base, "archive")
    scripts  <- file.path(base, "scripts")
    meetings <- file.path(base, "meetings")
    working  <- file.path(base, "working")
    inputs   <- file.path(base, "inputs")
    outputs  <- file.path(base, "outputs")
    figures  <- file.path(outputs, "figures")
    logs     <- file.path(outputs, "logs")
    tables   <- file.path(outputs, "tables")
    tex      <- file.path(outputs, "tex")

    filepaths <- c(
        "base"     = base   , 
        "archive"  = archive, 
        "scripts"  = scripts, 
        "meetings" = meetings,
        "working"  = working, 
        "inputs"   = inputs , 
        "outputs"  = outputs, 
        "figures"  = figures, 
        "logs"     = logs   , 
        "tables"   = tables , 
        "tex"      = tex    
    )

    return(filepaths)
 
}

create_paths <- function(directory) { 
# function creates directory folders using get_paths()
# function only needs to be run once (during initial setup)
# if no directory specified, function will prompt for directory

    path <- get_paths(directory)
    base     <- path["base"]
    archive  <- path["archive"]
    scripts  <- path["scripts"]
    meetings <- path["meetings"]
    working  <- path["working"]
    inputs   <- path["inputs"]
    outputs  <- path["outputs"]
    figures  <- path["figures"]
    logs     <- path["logs"]
    tables   <- path["tables"]
    tex      <- path["tex"]

    if (!dir.exists(base)){
        dir.create(base)
    }

    filepaths <- list(archive, scripts, meetings, working, inputs, outputs, figures, logs, tables, tex)
    for (folder in filepaths) { 
        dir.create(folder)
    }

}

# ------------------------------------------------------------------------------------------------ #
#                                    Data exploration functions                                    #
# ------------------------------------------------------------------------------------------------ #

dictionary <- function(data) { 
# function shows key summary values for a dataframe or tibble
# adapted from code by Patrick Ward, PhD
# https://github.com/pw2/Data-Dictionary-Function/blob/master/Data%20Dictionary%20Function.R 

    var_info <- data.frame(
        variable = names(data), 
        type = sapply(lapply(data, class), "[[", 1), 
        missing = sapply(data, function(x) sum(length(which(is.na(x))))), 
        uniques = sapply(lapply(data, unique), length), 
        description = sapply(sapply(data, function(x) attr(x, "comment")), function(x) ifelse(is.null(x), "", x)), 
        row.names = NULL
    )

    return(var_info)
    
}

# ------------------------------------------------------------------------------------------------ #
#                                    Control document functions                                    #
# ------------------------------------------------------------------------------------------------ #

shelltidy <- function(directory, pattern = ".") {
# function removes files in specified directory with specified pattern
# BE VERY CAREFUL WITH THIS FUNCTION

    # avoid execution on inputs folder
    warning <- grep("input", directory, value=FALSE)
    if (length(warning) > 0) stop("Cannot execute shelltidy on inputs folder")
    else{
        files <- grep(pattern, list.files(directory), value=TRUE)
        for (file in files) { 
        file.remove(file.path(directory, file))
        }
    }
}

source_rmd <- function(filename) {
# function takes filename and knits output using personal directory paths
# function is meant to be used within the "control.R" document for large projects 

    inpath <- file.path(paths["scripts"], filename)
    outfile <- paste(as.character(Sys.time()), stringr::str_replace(filename, ".Rmd", ""))
    if (grep(":", outfile)) {
        outfile <- sub(":", "h", outfile)
        outfile <- sub(":", "m", outfile)
        # outfile <- paste0(outfile, "s")
        print(outfile)
    }
    outfile <- sub(" ", "_", outfile)
    outfile <- sub(" ", "s_", outfile)
    outfile <- paste0(outfile, ".html")
    outpath <- file.path(paths["logs"], outfile)
    rmarkdown::render(inpath, output_file=outpath)
}

source_rmd_chunk <- function(filename, chunk_label) {
# function imports and executes a chunk from another .Rmd file
# the chunk is identified using the filename and chunk label 
# function is meant to be used for replicable code without multiple copy-paste 
# adapted from code written by Bryan Shalloway
# https://gist.github.com/brshallo/e963b9dca5e4e1ab12ec6348b135362e

    # allow duplicate chunk labels 
    options(knitr.duplicate.label = "allow")

    # temporary file to use 
    temp <- tempfile(fileext=".R")
    knitr::purl(filename, output=temp, quiet=TRUE)
    
    # read the appropriate chunk
    text <- readr::read_file(temp)
    text <- purrr::map(chunk_label, ~stringr::str_extract(text, glue::glue("(## ----{var})(.|[:space:])*?(?=(## ----)|$)", var = .x))) %>% 
        stringr::str_c(collapse = "\n")    

    # write chunk to temporary R script
    readr::write_file(text, temp)

    # execute chunk
    source(temp)

}
