library(oro.dicom)

directory <- "~/Desktop/Dehydration"

individualFolders <- list.dirs(directory,recursive = FALSE,full.names = FALSE)
fullFilePaths <- list.dirs(directory,recursive = FALSE)

timeTable <- c()
subjectID <- c()
individualTimes <- c()

i <- 1
for (folder in fullFilePaths){
  print(folder)
  dicomDirectoryPaths <- list.dirs(folder,recursive = FALSE, full.names = TRUE)
  id <- individualFolders[i]
  subjectID <- append(subjectID,id)
  
  if (length(dicomDirectoryPaths) > 1){ 
    # If there is more than one folder, find the T1 files and choose the latest scan
    print("Greater than 1")
    x <- 0
    acquisitionTime = 0
    for (f in dicomDirectoryPaths){
      dicomFiles <- list.files(f,recursive=FALSE,full.names = TRUE)
      dicomFile <- readDICOMFile(dicomFiles[1])
      if ((acquisitionTime*3600 )< str2time(extractHeader(dicomFile$hdr,"AcquisitionTime"))$time){
        print("Change")
        acquisitionTime <- str2time(extractHeader(dicomFile$hdr,"AcquisitionTime"))$time
        #acquisitionTime <- str2time(extractHeader(dicomFile$hdr,"InstanceCreationTime"))$time
        acquisitionTime <- acquisitionTime / 3600
      }
      if (acquisitionTime == 0){
        if ((acquisitionTime*3600) < str2time(extractHeader(dicomFile$hdr,"InstanceCreationTime"))$time){
          acquisitionTime <- str2time(extractHeader(dicomFile$hdr,"InstanceCreationTime"))$time
          acquisitionTime <- acquisitionTime / 3600
        }
      }
      print(acquisitionTime)
      seriesDescription <- extractHeader(dicomFile$hdr,"SeriesDescription",numeric=F)
      if(grepl("PHANTOM",seriesDescription)){
        print("Removing phantom scan")
        unlink(f,recursive = TRUE)
      }
      if(grepl("T2",seriesDescription)){
        print("Removing T2 scan")
        unlink(f,recursive = TRUE)
      }
      if(grepl("VISTA",seriesDescription)){
        print("Removing VISTA scan")
        unlink(f,recursive = TRUE)
      }
      if(grepl("Head_Scout",seriesDescription)){
        print("Removing scout scan")
        unlink(f,recursive = TRUE)
      }
      if(grepl("DTI",seriesDescription)){
        print("Removing dti scan")
        unlink(f,recursive = TRUE)
      }
      if(grepl("Range",seriesDescription)){
        print("Removing range scan")
        unlink(f,recursive = TRUE)
      }
      if(grepl("REFORMAT",seriesDescription)){
        print("Removing reformat scan")
        unlink(f,recursive = TRUE)
      }
      if(grepl("AX",seriesDescription)){
        print("Removing axial scan")
        unlink(f,recursive = TRUE)
      }
      print(seriesDescription)
    }
    
  }
  else if (length(dicomDirectoryPaths) == 1){ # FIXME display the scan type just in case..
    print("Equals 1")
    dicomFiles <- list.files(dicomDirectoryPaths,recursive=FALSE,full.names = TRUE)
    dicomFile <- readDICOMFile(dicomFiles[1])
    acquisitionTime <- str2time(extractHeader(dicomFile$hdr,"AcquisitionTime"))$time
    if (acquisitionTime == 0){
      acquisitionTime <- str2time(extractHeader(dicomFile$hdr,"InstanceCreationTime"))$time
    }
    acquisitionTime <- acquisitionTime / 3600
    print(acquisitionTime)
  }
  else if (length(dicomDirectoryPaths) == 0){
    print("Less than 1")
    dicomFiles <- list.files(folder,recursive=FALSE,full.names = TRUE)
    dicomFile <- readDICOMFile(dicomFiles[1])
    acquisitionTime <- str2time(extractHeader(dicomFile$hdr,"AcquisitionTime"))$time
    if (acquisitionTime == 0){
      acquisitionTime <- str2time(extractHeader(dicomFile$hdr,"InstanceCreationTime"))$time
    }
    acquisitionTime <- acquisitionTime / 3600
    print(acquisitionTime)
  }
  individualTimes <- append(individualTimes,acquisitionTime)
  
  # Add acquisition time to a matrix with the subject id
  
  i = i + 1
}

csvTable <- cbind(subjectID,individualTimes)

write.csv(csvTable,file = paste(directory,"acquisitionTimes.csv"))
