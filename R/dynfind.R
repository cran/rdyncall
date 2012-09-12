# Package: rdyncall
# File: R/dynfind.R
# Description: locating system libraries in common places  

# ----------------------------------------------------------------------------
# function: pathsFromEnv
# description: get paths character vector from environment variable such as LD_LIBRARY_PATH.
pathsFromEnv <- function(name)
  unlist( strsplit( unname( Sys.getenv(name) ), .Platform$path.sep ) )


.libLocations <- c("/lib","/lib64", "/usr/lib", "/usr/lib64", "/usr/local/lib",
"/usr/local/lib64", "/opt/local/lib")

.sysname <- Sys.info()[["sysname"]]

if (.sysname == "Darwin")
{
  .libLocations <- c(.libLocations, "/Library/Frameworks/R.framework/Resources/lib/" )
}

try.framework.locations <- c("/Library/Frameworks","/System/Library/Frameworks")
dynfind.darwin.framework <- function(frameworks, auto.unload=TRUE)
{
  try.frameworks <- frameworks
  for (location in try.framework.locations) {
    for (framework in try.frameworks) {
      path <- paste( location, "/", framework, ".framework/", framework, sep="")    
      x <- .dynload(path, auto.unload)
      if (!is.null(x)) return(x)
    }
  }
  return(NULL)
}

dynfind <- function(libnames, auto.unload=TRUE)
{
  if ( .sysname == "Windows" ) {    
    try.locations <- pathsFromEnv("PATH")
    filesep <- "\\"
  } else { # unix
    try.locations <- c("/lib","/lib64", "/lib/amd64", "/lib/sparcv9", "/usr/lib", "/usr/lib64", "/usr/local/lib","/usr/local/lib64", "/opt/local/lib", pathsFromEnv("LD_LIBRARY_PATH") )
    filesep <- "/"
  }
  try.prefixes <- c("","lib")
  try.suffixes <- c("",.Platform$dynlib.ext)  
  try.names <- libnames
  if ( .sysname == "Darwin" ) {
    try.locations <- c(try.locations, "/Library/Frameworks/R.framework/Resources/lib/")
    handle <-dynfind.darwin.framework(libnames, auto.unload=auto.unload)
    if( !is.null(handle) ) return(handle)
    try.suffixes <- c(".dylib",try.suffixes)
  }

  # remove "" entries and duplicates
  try.locations <- unique( try.locations[try.locations != ""] )
  
  # put '""' at the very end
  # try.locations <- c(try.locations,"")
  
  for (location in try.locations)
  {
    for (prefix in try.prefixes)
    {
      for(suffix in try.suffixes)
      {
        for(libname in try.names) 
        {
          path <- paste( location, filesep, prefix, libname, suffix, sep="" )
          x <- .dynload(path, auto.unload=auto.unload)
          if (!is.null(x)) return(x)
        }
      }
    }
  }
  # try directly
  for(libname in try.names) 
  {
    x <- .dynload(libname, auto.unload=auto.unload)
    if (!is.null(x)) return(x)
  }  
}

