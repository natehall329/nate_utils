#' llsync: Synchronize local and remote files using rsync
#'
#' This function syncs files between the local machine and a remote server,
#' specifically the Longleaf cluster at UNC. The synchronization is done using the
#' rsync command. The function checks if either the source or the destination path
#' starts with "/proj", and if so, it adds the remote server address before the path.
#'
#' @param path_from character, the source path of the file(s) to be synced.
#' @param path_to character, the destination path where the file(s) will be synced.
#' @param onyen user's onyen. defaults to nate's.
#' @return Prints the output of the rsync command.
#' @export
#' @examples
#' llsync("/local/path/file", "/proj/remote/path/")
#' llsync("/proj/remote/path/file", "/local/path/")

llsync <- function(path_from, path_to, onyen = "natehall"){
  ## the ll path needs to be tagged with the remote. can use paths to determine this.
  ##
  if (startsWith(path_from, "/proj")) {
    path_from <- paste0(onyen, "@longleaf.unc.edu:", path_from)
  } else if (startsWith(path_to, "/proj")) {
    path_to <- paste0("natehall@longleaf.unc.edu:", path_to)
  } else {
    stop("Error: None of the paths begin with '/proj'")
  }

  cmd <- paste0("rsync -avz ", path_from, " ", path_to)
  out <- system(cmd,intern = TRUE)
  cat("System prompt: ", cmd, "\n\n")
  cat(out, sep = "\n")
}


