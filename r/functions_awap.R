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

# 
variableslist <- function(){
  variablesList<-
    "variable,measure,timestep
rainfall,totals,daily
temperature,maxave,daily
temperature,minave,daily
vprp,vprph09,daily
vprp,vprph15,daily
solar,solarave,daily
ndvi,ndviave,month"
  variablesList <- read.csv(textConnection(variablesList), stringsAsFactors = F)
  return(variablesList)
}
# 
get_data_range<-function(variable,measure,timestep,startdate,enddate){
  if (timestep == "daily"){
    thisdate<-startdate
    while (thisdate<=enddate){
      get_data(variable,measure,timestep,format(as.POSIXct(thisdate),"%Y%m%d"),format(as.POSIXct(thisdate),"%Y%m%d"))
      thisdate<-thisdate+as.double(as.difftime(1,units="days"),units="secs")
    }
  } else if (timestep == "month" | timestep == "monthly"){
    timestep <- "month"
    # Make sure that we go from begin of the month
    startdate <- as.POSIXlt(startdate)
    startdate$mday <- 1
    # Find the first and last day of each month overlapping our range
    data.period.start <- seq(as.Date(startdate), as.Date(enddate), by = 'month')
    data.period.end <- as.Date(sapply(data.period.start, FUN=function(x){as.character(seq(x, x + 40, by = 'month')[2] - 1)}))
    # Download them
    for (i in 1:length(data.period.start))
    {
      # i <- 1
      get_data(variable,measure,timestep,
               format(as.POSIXct(data.period.start[i]),"%Y%m%d"),
               format(as.POSIXct(data.period.end[i]),"%Y%m%d")
      )
    }
    
  } else {
    stop("Unsupported timestep, only 'daily' and 'month' are currently supported")
  }
}
# 
get_data<-function(variable,measure,timestep,startdate,enddate){
  url="http://www.bom.gov.au/web03/ncc/www/awap/{variable}/{measure}/{timestep}/grid/0.05/history/nat/{startdate}{enddate}.grid.Z"
  url=gsub("{variable}",variable,url,fixed=TRUE)
  url=gsub("{measure}",measure,url,fixed=TRUE)
  url=gsub("{timestep}",timestep,url,fixed=TRUE)
  url=gsub("{startdate}",startdate,url,fixed=TRUE)
  url=gsub("{enddate}",enddate,url,fixed=TRUE)
  
  try(download.file(url,sprintf("%s_%s%s.grid.Z",measure,startdate,enddate),mode="wb"))
}

# mdofied
get_awap_data <- function(start, end, measure_i,fold.work){
  
  if(!dir.exists(fold.work)){dir.create(fold.work)}
  
  # change wd to the folder select 
  current.wd <- getwd()
  on.exit(setwd(current.wd))
  setwd(fold.work)
  
  # 
  variableslist <- variableslist()  
  variable <- variableslist[which(variableslist$measure == measure_i),]
  vname <- as.character(variable[,1])
  datelist <- seq(as.Date(start), as.Date(end), 1)
  
  for(date_i in datelist){
    date_i <- as.Date(date_i, origin = '1970-01-01')
    sdate <- as.character(date_i)
    edate <- date_i
    
    
    if(!file.exists(sprintf("%s_%s%s.grid",measure_i,gsub("-","",sdate),gsub("-","",edate))))
    {
      get_data_range(variable=as.character(variable[,1]),
                     measure=as.character(variable[,2]),
                     timestep=as.character(variable[,3]),
                     startdate=as.POSIXct(sdate),
                     enddate=as.POSIXct(edate))
      
      fname <- sprintf("%s_%s%s.grid.Z",measure_i,gsub("-","",sdate),gsub("-","",edate))
      if(file.info(fname)$size == 0)
      {
        file.remove(fname)
        next
      }
      
      # unzip
        Decompress7Zip(zipFileName= fname, outputDirectory=getwd(), TRUE)
    }else{
      print('file exist.')
    }
  }
  
}


