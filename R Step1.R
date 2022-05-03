
# 参数设置

DataFile='C:/Users/Ann/Desktop/AutoAnnotatoR/R Step1' # data position
file.collection<-'example list.xlsx'  #data name


diff_rt2=30   
diff_mz2=0.02 

mz_set=180


Function_Optimizing_HCD<-function(DataFile='',    
                                  file.collection='',
                                  diff_rt2=30,    
                                  diff_mz2=0.02,  
                                  mz_set=180,    
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
  if(!require(ggplot2)){
    install.packages('ggplot2')
    if (require("ggplot2")) {
      packageStartupMessage(paste("packages:ggplot2 installed and loaded!"))
    } else {
      stop("could not install ggplot2")
    }
  }
  
  if(!dir.exists('Result_Out')){
    dir.create('Result_Out')
  }
  
  
 
  data1<-read.xlsx(file.collection, 1, header=T, encoding="UTF-8", stringsAsFactors = FALSE)
  print(names(data1))
  idx.rt<-which(grepl('time',names(data1)))
  if(idx.rt==2){
    name1<-c('m/z','Retention time')
  }else{
    name1<-c('Retention time',	'm/z')
  }
  if(ncol(data1)==2){
    names(data1)<-name1
  }else{
    names(data1)<-c(name1,'Intensity')
  }
  
  
  
  f<-list.files(DataFile)
  mgf.files.all<-f[which(grepl('.mgf',f))]
  lab.v<-as.numeric(gsub("[^0-9,.]", "", sapply(mgf.files.all, function(x) strsplit(x,CE.character)[[1]][2])))
  idx<-order(lab.v) # mgf目录排序
  mgf.files.all<-mgf.files.all[idx]
  print(data.frame(mgf=mgf.files.all))
  lab.v<-as.numeric(gsub("[^0-9,.]", "", sapply(mgf.files.all, function(x) strsplit(x,CE.character)[[1]][2])))
  CE.label<-paste(CE.character,lab.v,sep='')
  
  mat1<-matrix(NA,nrow = nrow(data1),ncol = length(CE.label))
  colnames(mat1)<-CE.label
  table1<-table2<-cbind(data1[c('Retention time',	'm/z')],mat1)
  
  D_MS2_NA_int<-data.frame()
  for(i_f in 1:length(mgf.files.all)){
    packageStartupMessage(paste0(mgf.files.all[i_f],"文件信息提取."))
    mgf_file<-mgf.files.all[i_f]
    mgf_data <- scan(mgf_file, what = character(0), sep = "\n")  # Read mgf file
    mgf_matrix <- createmgfmatrix(mgf_data)  # create mgf_matrix
    mgf_matrix<-apply(mgf_matrix, 2, as.numeric)
    mgf_matrix<-as.data.frame(mgf_matrix)
    
    NA_int_id<-which(is.na(mgf_matrix$intensity_ms1))
    if(length(NA_int_id)>0){
      d<-mgf_matrix[NA_int_id,]
      d$mgf_file<-mgf.files.all[i_f]
      print(d)
      D_MS2_NA_int<-rbind(D_MS2_NA_int,d)
      next
    }
 
    for(i in 1:nrow(data1)){
      mzid<-which(abs(data1$`m/z`[i]-mgf_matrix$Pepmass_num)<=diff_mz2)# 0.03 Da
      rtid<-which(abs(data1$`Retention time`[i]-mgf_matrix$TR_num)<=diff_rt2)# 20s
      interid<-intersect(mzid,rtid)
      
      if(length(interid)>0){
        interid<-interid[which(mgf_matrix$intensity_ms1[interid]==max(mgf_matrix$intensity_ms1[interid]))]
        
       
        mz_ms2.all<-c()
        int_ms2.all<-c()
        for (j in mgf_data[mgf_matrix$Begin_num[interid]:mgf_matrix$End_num[interid]]){
          if (grepl("[a-zA-Z]", j)){
            next()
          }else{
            mz_ms2 <- as.numeric(unlist(strsplit(j, " "))[1])
            int_ms2 <- as.numeric(unlist(strsplit(j, " "))[2])
            mz_ms2.all<-c(mz_ms2.all,mz_ms2)
            int_ms2.all<-c(int_ms2.all,int_ms2)
          }
        }
        
        table1[i,i_f+2]<-mz_ms2.all[which(int_ms2.all==max(int_ms2.all))]
        
        intsum_ms2<-sum(int_ms2.all)
        intsum_ms2_mz_set_sum<-sum(int_ms2.all[which(mz_ms2.all<mz_set)])
        
        table2[i,i_f+2]<-intsum_ms2_mz_set_sum/intsum_ms2*100
      }else{
        table1[i,i_f+2]<-0
        table2[i,i_f+2]<-0
      }
    }
  }
  
  if(nrow(D_MS2_NA_int)>0){
    D_MS2_NA_int<-D_MS2_NA_int[c('Begin_num','End_num','Pepmass_num','mgf_file')]
    names(D_MS2_NA_int)<-c('Begin_rowid_inmgf','End_rowid_inmgf','Pepmass_mz','mgf_file')
    write.xlsx(D_MS2_NA_int,  paste0("Result_Out/mgf-PEPMASS-缺失信息.xlsx"), row.names = F, showNA=F)
    stop('mgf-PEPMASS含有缺失信息，具体信息见 Result_Out/mgf-PEPMASS-缺失信息.xlsx')
  }
  
  
  table_bestHCD<-data1[c('Retention time',	'm/z')]
  table_bestHCD['最佳HCD']<-NA
  for(i in 1:nrow(data1)){
    mat<-table1[i,CE.label]
    mat2<-table2[i,CE.label]
    TF0<-sum(mat==0)
    if(TF0>3){
      table_bestHCD[i,3]<-0
    }else{
      id1<-which(mat<mz_set)
      id2<-which(mat2>60)
      interid<-intersect(id1,id2)
      if(length(interid)==0){
        # print(i)
        table_bestHCD[i,3]<-0
        next
      }
      interid<-min(interid)
      if(interid==1){
        table_bestHCD[i,3]<-lab.v[interid]
      }else{
        if(mat[,interid-1]==0){
          table_bestHCD[i,3]<-0
        }else{
          table_bestHCD[i,3]<-lab.v[interid]
        }
      }
    }
  }
  
  write.xlsx(table1,  paste0("Result_Out/ions.xlsx"), row.names = F, showNA=F)
  write.xlsx(table2,  paste0("Result_Out/mz_180.xlsx"), row.names = F, showNA=F)
  write.xlsx(table_bestHCD,  paste0("Result_Out/optimal_HCD.xlsx"), row.names = F, showNA=F)
  
  packageStartupMessage(paste("Finished."))
  
}

# Function: exact a matrix from mgf_data
##########
createmgfmatrix <- function(mgf_data){
  Begin_num <- grep("BEGIN IONS", mgf_data)
  Pepmass_num <- grep("PEPMASS=",mgf_data)
  TR_num <- grep("RTINSECONDS=",mgf_data)
  End_num <- grep("END IONS", mgf_data)
  intensity_ms1 <- grep("PEPMASS=",mgf_data)
  mgf_matrix <- cbind(Begin_num,TR_num,Pepmass_num,intensity_ms1,End_num)
  
  for (i in c(1:length(Pepmass_num)))
  {
    pepmass <- strsplit(mgf_data[Pepmass_num[i]],' ')[[1]][1]
    pepmass <- gsub("[^0-9,.]", "", pepmass)
    mgf_matrix[i,"Pepmass_num"] <- pepmass
    mgf_matrix[i,"intensity_ms1"] <- strsplit(mgf_data[Pepmass_num[i]],' ')[[1]][2]
    
    tr <- gsub("[^0-9,.]", "", mgf_data[TR_num[i]])
    mgf_matrix[i,"TR_num"] <- tr
  }
  return(mgf_matrix)
} 

# 函数调用
Function_Optimizing_HCD(DataFile=DataFile,
                        file.collection=file.collection,
                        diff_rt2=diff_rt2,
                        diff_mz2=diff_mz2,
                        mz_set=mz_set
)










