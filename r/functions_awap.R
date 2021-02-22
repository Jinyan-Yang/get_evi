# function taken from SWISH
# https://github.com/swish-climate-impact-assessment

ExecutableFileName7Zip <- function(){
  executableName <- "C:/Program Files/7-Zip/7z.exe"
  
  if(file.exists(executableName))
  {
    return (executableName)
  }
  
  #other executable file names and ideas go here ...
  stop("failed to find 7zip")
}
# 
RunProcess = function(executable, arguments)
{
  command = paste(sep="", "\"", executable,  "\" ", arguments);
  
  print (command)
  
  exitCode = system(command, intern = FALSE, ignore.stdout = FALSE, ignore.stderr = FALSE, wait = TRUE, input = NULL
                    , show.output.on.console = TRUE
                    #, minimized = FALSE
                    , invisible = FALSE
  );
  if(exitCode != 0)
  {
    stop("Process returned error");
  }
  return (exitCode)
}

# need to have 7zip installed
Decompress7Zip <- function(zipFileName, outputDirectory, delete){
  executableName <- ExecutableFileName7Zip()
  
  #   fileName = GetFileName(zipFileName)
  #   fileName = PathCombine(outputDirectory, fileName)
  
  
  #   if(file.exists(fileName))
  #   {
  #     unlink(zipFileName);
  #   }
  
  arguments <- paste(sep="",
                     "e ",
                     "\"", zipFileName, "\" ",
                     "\"-o", outputDirectory, "\" ",
                     "")
  
  print( arguments)
  
  RunProcess(executableName, arguments)
  
  if(delete)
  {
    unlink(zipFileName);
  }
}