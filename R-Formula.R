# 参数设置

DataFile='C:/Users/Ann/Desktop/AutoAnnotatoR/R-formula'     # data position
file.list1<-'example data.xlsx'      #file name



Function_20220207<-function(DataFile='',
                            file.list1=''
){
  if(DataFile==''){
    return()
  }
  setwd(DataFile)
  
  if(!require(xlsx)){
    install.packages('xlsx')
    if (require("xlsx")) {
      packageStartupMessage(paste("packages:xlsx installed and loaded!"))
    } else {
      stop("could not install xlsx")
    }
  }
  
  
  if(!dir.exists('Result_Out')){
    dir.create('Result_Out')
  }
  
  
  # list-1
  packageStartupMessage(paste0(file.list1,"文件读取."))
  data<-read.xlsx(file.list1, 1, header=T, encoding="UTF-8", stringsAsFactors = FALSE)
  naTF<-apply(data, 1, function(x) sum(is.na(x))==length(x))
  if(sum(naTF)>0){
    data<-data[-which(naTF),]
  }
  data1<-data[,1:5]
  data0<-data[1,7:11]
  names(data1)<-c('name',"C" ,"H", "O","N")
  names(data0)<-c('name',"C" ,"H", "O","N")
  
  data1[,1]<-gsub("^\\s+|\\s+$", "", data1[,1])
  lab<-data1[,1]
  lab.type<-gsub("[0-9]", "", lab)
  lab.type.uni<-unique(lab.type) # "A" "B" "C" "D" "E" "F" "G" "H"
  
  list_1<-list()
  for(i in 1:length(lab.type.uni)){
    dcurr<-which(lab.type==lab.type.uni[i])
    if(i==1){
      for(curr in dcurr){
        list_1<-c(list_1,list(curr))
      }
    }else{
      list_2<-list()
      for(i2 in 1:length(list_1)){
        for(curr in dcurr){
          v<-c(list_1[[i2]],curr)
          list_2<-c(list_2,list(v))
        }
      }
      list_1<-list_2
    }
  }
  
  no=0
  fin_d<-data.frame()
  for(i in 1:length(list_1)){
    no=no+1
    idx<-list_1[[i]]
    d1<-data1[idx,]
    d<-rbind(d1,data0)
    NO<-paste0('J',no)
    ID<-paste(d1[,1],collapse = '')
    val<-apply(d[,-1],2,sum)
    Formula<-paste0("C",val[1] ,"H",val[2], "O",val[3],"N",val[4])
    fin_d0<-data.frame(NO.=NO,ID=ID,C=val[1],H=val[2],O=val[3],N=val[4],Formula=Formula)
    fin_d<-rbind(fin_d,fin_d0)
  }
  
  if(nrow(fin_d)>0){
    write.xlsx(fin_d,  paste0("Result_Out/L1.xlsx"), row.names = F, showNA=F)
  }else{
    packageStartupMessage(paste("step finally >>> L1.xlsx: Data is empty and cannot be output!",sep=''))
    return()
  }
  fin_d2<-data.frame()
  uni.Formula<-unique(fin_d$Formula)
  for(i in uni.Formula){
    d<-fin_d[which(fin_d$Formula==i),]
    NO<-paste(d$NO.,collapse = ';')
    ID<-paste(d$ID,collapse = '\n')
    fin_d0<-data.frame(NO.=NO,ID=ID)
    fin_d0<-cbind(fin_d0,d[1,3:7])
    fin_d2<-rbind(fin_d2,fin_d0)
  }
  if(nrow(fin_d2)>0){
    write.xlsx(fin_d2,  paste0("Result_Out/L2.xlsx"), row.names = F, showNA=F)
  }else{
    packageStartupMessage(paste("step finally >>> L2.xlsx: Data is empty and cannot be output!",sep=''))
    return()
  }
  
  packageStartupMessage(paste("Finished."))
  
  
}




Function_20220207(DataFile=DataFile, 
                  file.list1=file.list1
)