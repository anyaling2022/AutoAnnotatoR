
# 参数设置

DataFile='C:/Users/Ann/Desktop/AutoAnnotatoR/R Step2/example 2-1' # data position
file.collection<-'example data 2-1.xlsx'  #data name


diff_rt2=30   
diff_mz2=0.02 


Function_mgf_extract2<-function(DataFile='',    
                                 diff_rt2=20,   
                                 diff_mz2=0.03,  
                                 CE.character='HCD'
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
 
  
  
  data1<-read.xlsx(file.collection, 1, header=T, encoding="UTF-8", stringsAsFactors = FALSE)
  names(data1)<-c('Retention time',	'm/z','最佳HCD')
  
  f<-list.files(DataFile)
  mgf.files.all<-f[which(grepl('.mgf',f))]
  lab.v<-as.numeric(gsub("[^0-9,.]", "", sapply(mgf.files.all, function(x) strsplit(x,CE.character)[[1]][2])))
  idx<-order(lab.v) # mgf目录排序
  mgf.files.all<-mgf.files.all[idx]
  print(data.frame(mgf=mgf.files.all))
  lab.v<-as.numeric(gsub("[^0-9,.]", "", sapply(mgf.files.all, function(x) strsplit(x,CE.character)[[1]][2])))
  
  mgf_matrix_list<-list()
  D_MS2_NA_int<-data.frame()
  for(i_f in 1:length(mgf.files.all)){
    packageStartupMessage(paste0(mgf.files.all[i_f],"文件读取."))
    mgf_file<-mgf.files.all[i_f]
    mgf_data <- scan(mgf_file, what = character(0), sep = "\n")  # Read mgf file
    mgf_matrix <- createmgfmatrix(mgf_data)  # create mgf_matrix
    mgf_matrix<-apply(mgf_matrix, 2, as.numeric)
    mgf_matrix<-as.data.frame(mgf_matrix)
    
    NA_int_id<-which(is.na(mgf_matrix$Pepmass_intensity))
    if(length(NA_int_id)>0){
      d<-mgf_matrix[NA_int_id,]
      d$mgf_file<-mgf.files.all[i_f]
      print(d)
      D_MS2_NA_int<-rbind(D_MS2_NA_int,d)
    }
    mgf_matrix_list<-c(mgf_matrix_list,list(mgf_matrix))
  }
  
  if(nrow(D_MS2_NA_int)>0){
    names(D_MS2_NA_int)<-c('Begin_rowid_inmgf','End_rowid_inmgf','tr_inmgf','Pepmass_mz','Pepmass_intensity')
    write.xlsx(D_MS2_NA_int,  paste0("Result_Out/mgf-PEPMASS-缺失信息.xlsx"), row.names = F, showNA=F)
    stop('mgf-PEPMASS含有缺失信息，具体信息见 Result_Out/mgf-PEPMASS-缺失信息.xlsx')
  }
  
  packageStartupMessage(paste("提取信息..."))
  
  mgf_matrix2<-cbind(mgf_matrix[1,],mgf_file=NA)[-1,]
  
  data1$sign<-NA
  for(i in 1:nrow(data1)){
    vhcd<-data1$最佳HCD[i]
    if(grepl(',',vhcd)){
      vhcd<-strsplit(vhcd,',')[[1]]
    }
    temp<-cbind(mgf_matrix[1,],mgf_file=NA)[-1,]
    for(i2 in 1:length(vhcd)){
      hcdid<-match(vhcd[i2],lab.v)
      mgf_matrix<-mgf_matrix_list[[hcdid]]
      mzid<-which(abs(data1$`m/z`[i]-mgf_matrix$Pepmass_num)<=diff_mz2)
      rtid<-which(abs(data1$`Retention time`[i]-mgf_matrix$TR_num)<=diff_rt2)
      interid<-intersect(mzid,rtid)
      if(length(interid)==0){
        next
      }else{
        if(length(interid)>1){
          int<-mgf_matrix$Pepmass_intensity[interid]
          id<-order(int,decreasing = T)[1]
          interid<-interid[id]
        }
        temp2<-cbind(mgf_matrix[interid,],mgf_file=mgf.files.all[hcdid])
        temp<-rbind(temp,temp2)
      }
    }
    if(nrow(temp)>0){
      data1$sign[i]<-1
      id<-order(temp$Pepmass_intensity,decreasing = T)[1]
      mgf_matrix2<-rbind(mgf_matrix2,temp[id,])
    }else{
      data1$sign[i]<-0
    }
  }
  
  mgf_data_new<-c()
  uni.mgf<-unique(mgf_matrix2$mgf_file)
  for(i_f in uni.mgf){
    # print(i_f)
    packageStartupMessage(paste0(i_f,"文件读取."))
    mgf_data <- scan(i_f, what = character(0), sep = "\n")  # Read mgf file
    mgf_matrix3<-mgf_matrix2[which(mgf_matrix2$mgf_file==i_f),]
    for(i in 1:nrow(mgf_matrix3)){
      mgf_data_new<-c(mgf_data_new, mgf_data[mgf_matrix3$Begin_num[i]:mgf_matrix3$End_num[i]])
    }
  }
  
  write.xlsx(data1,  paste0("Result_Out/",gsub('.xlsx','',file.collection),"-new.xlsx"), row.names = F, showNA=F)
  
  con <- file(paste0("Result_Out/new.mgf"), "w")
  writeLines(mgf_data_new, con)
  close(con)
  
  packageStartupMessage(paste("Finished."))
  
}

# Function: exact a matrix from mgf_data
##########
createmgfmatrix <- function(mgf_data){
  Begin_num <- grep("BEGIN IONS", mgf_data)
  Pepmass_num <- grep("PEPMASS=",mgf_data)
  TR_num <- grep("RTINSECONDS=",mgf_data)
  End_num <- grep("END IONS", mgf_data)
  Pepmass_intensity <- grep("PEPMASS=",mgf_data)
  mgf_matrix <- cbind(Begin_num,End_num,TR_num,Pepmass_num,Pepmass_intensity)
  
  for (i in c(1:length(Pepmass_num)))
  {
    pepmass <- strsplit(mgf_data[Pepmass_num[i]],' ')[[1]][1]
    pepmass <- gsub("[^0-9,.]", "", pepmass)
    mgf_matrix[i,"Pepmass_num"] <- pepmass
    mgf_matrix[i,"Pepmass_intensity"] <- strsplit(mgf_data[Pepmass_num[i]],' ')[[1]][2]
    
    tr <- gsub("[^0-9,.]", "", mgf_data[TR_num[i]])
    mgf_matrix[i,"TR_num"] <- tr
  }
  return(mgf_matrix)
} 

# 函数调用
Function_mgf_extract2(DataFile=DataFile, 
                       diff_rt2=diff_rt2,    
                       diff_mz2=diff_mz2
)










