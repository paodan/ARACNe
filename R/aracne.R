#' ARACNe-ap
#' @param expr the expression file.
#' @param tfs the file of TF list.
#' @param outputFolder the output folder.
#' @param pvalue the p-value threshold for the MI to be significant
#' (1E-8 usually).
#' @param seed the optional seed, to make the threshold mode and the
#' bootstrap reproducible.
#' @param calculateThreshold  telling Java to run it in threshold mode.
#' @param consolidate telling java to run aracne in consolidate mode
#' (that is, you point it to a directory with bootstraps, and they
#' will be consolidated).
#' @param threads the number of threads (it is used only in standard
#' mode, i.e. bootstrap).
#' @param nodpi logic, if TRUE, it tells ARACNE not to run DPI.
#' @param nobootstrap logic, if TRUE, it tells ARACNE not to do bootstrapping.
#' @param nobonferroni logic, if TRUE, it removes the Bonferroni correction.
#'
#' @examples
#' \dontrun{
#' expr = paste0(system.file('extdata', package = 'ARACNe'), "/matrix.txt")
#' tfs = paste0(system.file('extdata', package = 'ARACNe'), "/tfs.txt")
#' # 1. calculate threshold with a fixed seed
#' t = aracne(expr = expr, tfs = tfs, outputFolder = "./outputFoulder/",
#'            calculateThreshold = T)
#'
#' # 2. run ARACNe on a single bootstrap
#' net = aracne(expr = expr, tfs = tfs, outputFolder = "./outputFoulder/")
#'
#' # 3. run 100 reproducible bootstraps
#' nets = lapply(setNames(1:100, paste0("b", 1:100)), function(x) {
#'  aracne(expr = expr, tfs = tfs,
#'         outputFolder = "./outputFoulder/", seed = x)})
#'
#' # 4. consolidate bootstraps in the output folder
#' net = aracne(outputFolder = "./outputFoulder/",
#'              consolidate = TRUE)
#'
#' # 5. run a single ARACNE with no bootstrap and no DPI (data processing
#' inequality)
#' net = aracne(expr = expr, tfs = tfs,
#'              outputFolder = "./outputFoulder/",
#'              nobootstrap = TRUE, nodpi = TRUE)
#'
#' # 6. consolidate bootstraps without Bonferroni correction
#' net = aracne(outputFolder = "./outputFoulder/",
#'              consolidate = TRUE, nobonferroni = TRUE)
#' }
#' @export
aracne = function(expr = NULL, tfs = NULL, outputFolder = "./",
                  pvalue = 1E-8, seed = 1,
                  calculateThreshold = FALSE,
                  consolidate = FALSE, threads = 1, nodpi = FALSE,
                  nobootstrap = FALSE, nobonferroni = FALSE){
  jarFile = paste0(system.file('extdata', package = 'ARACNe'), "/aracne.jar")

  if (!file.exists(outputFolder)){
    message(outputFolder, " does not exist, so making this directory.")
    dir.create(outputFolder, recursive = TRUE)
  }
  cmd = paste("java -jar", jarFile)
  if (!is.null(expr)){
    cmd = paste(cmd, "-e", expr)
  }
  if (!is.null(outputFolder)){
    cmd = paste(cmd, "-o", outputFolder)
  }
  if (!is.null(tfs)){
    cmd = paste(cmd, "--tfs", tfs)
  }
  if (!is.null(pvalue)){
    cmd = paste(cmd, "--pvalue", pvalue)
  }
  if (!is.null(seed)){
    cmd = paste(cmd, "--seed", seed)
  }
  if (!is.null(threads)){
    cmd = paste(cmd, "--threads", threads)
  }
  if (calculateThreshold){
    cmd = paste(cmd, "--calculateThreshold")
  }

  if (consolidate){
    cmd = paste(cmd, "--consolidate")
  }

  if (nodpi){
    cmd = paste(cmd, "--nodpi")
  }

  if (nobootstrap){
    cmd = paste(cmd, "--nobootstrap")
  }

  if (nobonferroni){
    cmd = paste(cmd, "--nobonferroni")
  }

  res = system(cmd, intern = TRUE)
  if (calculateThreshold){
    res = as.numeric(strsplit(grep("MI threshold: ",
                                   res, value = T), ": ")[[1]][2])
    names(res) = "MI_threshold"
    return(res = res)
  }

  if(consolidate){
    file = paste0(outputFolder, "/",
                  list.files(outputFolder, pattern = "^network.txt$"))
    res = read.csv(file, header = TRUE, sep = "\t", as.is = TRUE)
    return(res = res)
  }

  if (nobootstrap){
    file = paste0(outputFolder, "/",
                  list.files(outputFolder, pattern = "^nobootstrap_network.txt$"))
    res = read.csv(file, header = TRUE, sep = "\t", as.is = TRUE)
  } else {
    file = paste0(outputFolder, "/",
                  list.files(outputFolder, pattern = "^bootstrapNetwork.*.txt$"))
    if (length(file) == 0){
      stop("No network file is found")
    } else if (length(file) > 1){
      fileInfo = file.info(file)
      file = rownames(fileInfo)[order(fileInfo$atime, decreasing = TRUE)][1]
    }
    res = read.csv(file, header = TRUE, sep = "\t", as.is = TRUE)
  }
  return(res = res)
}
