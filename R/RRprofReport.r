RRprofReport<-function(file.name="RRprof.out", notepad.path="C:/Program Files/Notepad++/notepad++.exe",reportname="my_report"){
  
  profdata <- readLines(file.name)                                                                                                     
  interval <- as.numeric(strsplit(profdata[1L], "=")[[1L]][2L]) / 1e+06                                                          
  filelines <- grep("#File", profdata)                                                                                          
  files <- profdata[filelines]
  files<-strsplit(files,": ")
  files<- sapply(files, "[", 2)
  files<-gsub("\\\\","/",files)
  filesnames <- unlist(lapply(files, function(files) {x <- strsplit(files[[1]], "/|\\\\"); x <- x[[1]][length(x[[1]])]; x <- strsplit(x,".R")[[1]][1];}))

  
  filesread <- list()
  for (i in 1:length(files))
    filesread[[i]]<-suppressWarnings(try(readLines(files[i],warn=FALSE),silent=TRUE))
  
  fileserror<-which(unlist(lapply(filesread, function(x) !is(x,"try-error"))))
  
  if (length(filelines) > 1)
    profdata<-profdata[-filelines[2:length(filelines)]]
  if (length(filelines) > 0)
    profdata <- profdata[-1:-filelines[1]]
  profdata=profdata[grep("#",profdata)]
  total.time2 <- interval * length(profdata)
  profdata <- gsub("\\\"| $", "", profdata)                                                                                       
    
  
  #Total time
  total.time<-list();
  self.time<-list();
  
  for (i in fileserror){  
    total.time[[i]]<-rep(0,length(filesread[[i]]));
    self.time[[i]]<-rep(0,length(filesread[[i]]));
    for (j in 1:length(filesread[[i]])){
      tmp<-grep(paste(i,'#',j," ",sep=''),profdata)
      total.time[[i]][j]<-length(tmp)
      if (length(tmp)>0){
        for (k in 1:length(tmp)){
          tmp2<-strsplit(strsplit(profdata[tmp[k]],"#")[[1]][1]," ")[[1]][length(strsplit(strsplit(profdata[tmp[k]],"#")[[1]][1]," ")[[1]])]==i&strsplit(strsplit(profdata[tmp[k]],"#")[[1]][2]," ")[[1]][1]==j;
          if(tmp2){
            self.time[[i]][j]<-self.time[[i]][j]+1
          }
        }
      }
    }
  }
  total.time<-lapply(total.time, function(x,y) return(x*y),interval)
  self.time<-lapply(self.time, function(x,y) return(x*y),interval)
  total.time.sum<-unlist(lapply(total.time,function(i)sum(i)))
  self.time.sum<-unlist(lapply(self.time,function(i)sum(i)))
  total.time.ptg<-as.numeric(total.time.sum/total.time2*100)
  self.time.ptg<-as.numeric(self.time.sum/total.time2*100)
  maxtime<-suppressWarnings(unlist(lapply(total.time,function(x)max(x))))
  color<-list()
  for (i in fileserror)
  color[[i]]<-unlist(lapply(total.time[[i]], function(x,y) return(floor(x/y*255)),max(maxtime)));
  
  
  
  r <- newCustomReport( "Profile Report" );
  
  script1<-"<script type=\"text/javascript\" language=\"javascript\">
  
  function RunFile"
  
  script2<-paste("(x) {
                 
                 WshShell = new ActiveXObject(\"WScript.Shell\");
                 
                 WshShell.Run('\"",notepad.path,"\"",sep="")
  script3<- " -n'+x);
  
}
  
  </script>"
  
  
  list2<-fileserror
  wd<-getwd()
  setwd("~")
  list1<-grep("~",files)
  if(length(list1>0)){
    list2<-list2[-list1]
  }
  script<-list()
  for (i in list1){
    script[[i]]<-newHtml(paste(script1,i,script2," \"",getwd(),strsplit(files[i],"~")[[1]][2],"\"",script3,sep=""))
    r <- addTo(r,script[[i]])  
  }
  for (i in list2){
    script[[i]]<-newHtml(paste(script1,i,script2," \"",files[i],"\"",script3,sep=""))
    r <- addTo(r,script[[i]])  
  }
  
  setwd(wd)
  
  
  
  summary.table<-data.frame(filesnames[fileserror],total.time.sum[fileserror],self.time.sum[fileserror],total.time.ptg[fileserror],self.time.ptg[fileserror])
  colnames(summary.table)<-c("File","Total time","Self time","Total time (%)","Self time (%)")
  

  
  
  s1 <- newSection( "Summary" );
  s2<-list()
  
  for (i in fileserror)
    s2[[i]] <- newSection(filesnames[i]);
  s3<-newSection("Call graph")
  
  sum <- newTable( summary.table ); # w/ caption
  
  
  code<-list()
  for(i in fileserror)
    code[[i]]<-rep(" ",length(filesread[[i]]))
  
  
  
  
  for (i in fileserror){
    for (j in 1:length(filesread[[i]])){
      code[[i]][j]<-asCode(filesread[[i]][j])
    }
  }
  
  files.tables<-list()
  f.tables<-list()
  
  
  table0<-"<div class=\"table\" id="

  table1<-">
  
  <table class=\"resulttable tablesorter sortabletable\">"
  
  table2<-"                 <thead>
  <tr>
  <th class=\"header\">
  time
  </th>
  <th class=\"header headerSortDown\">
  line
  </th>
  <th class=\"header\">
  </th>
  </tr>
  </thead>
  <tbody>"
  
  table3<-"</tbody>
  </table>
  </div>"
  
  for(i in fileserror){
    files.tables[[i]]<-paste(table0,"\"File",i,"\"",table1,table2,sep="")
  }
  
  for(i in fileserror){
    for(j in 1:length(filesread[[i]])){
      if (total.time[[i]][j]>0)
        files.tables[[i]]<-paste(files.tables[[i]],"<tr><td>",total.time[[i]][j],"</td><td><a onclick=\"RunFile",i,"(",j,")\" href=\"#\">",j,"</a></td><td>","<span style=\"background-color: #",sprintf("FF%02X%02X",255-color[[i]][j],255-color[[i]][j]),"\">",filesread[[i]][j],"</span>","</td></tr>",sep="")
      else
        files.tables[[i]]<-paste(files.tables[[i]],"<tr><td></td><td><a>",j,"</a></td><td>",filesread[[i]][j],"</td></tr>",sep="")
    }
    files.tables[[i]]<-paste(files.tables[[i]],table3)
    f.tables[[i]]<-newHtml(files.tables[[i]])
  }
  
  
  
  
  
  for (i in fileserror){
    s2[[i]]<-addTo(s2[[i]],f.tables[[i]]);
  }
  
  
  s1 <- addTo( s1, sum );
  
   
  r <- addTo( r, s1);
  
  for (i in fileserror)
    r <- addTo( r, s2[[i]]);
  

  #render report to file  
  setwd(tempdir());
  writeReport( r, filename=reportname ); # w/o extension
  setwd(wd);
  if(Sys.info()["sysname"]=="Windows"){
    aux<-suppressWarnings(try(system2("C:/Program Files/Internet Explorer/iexplore.exe",args=paste(tempdir(),"\\",reportname,".html",sep=""),wait=FALSE)))
    if(aux!=0)
      browseURL(paste(tempdir(),"/",reportname,".html",sep=""))
  }
  else
    browseURL(paste(tempdir(),"/",reportname,".html",sep=""))
}