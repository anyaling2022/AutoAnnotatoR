# 参数设置

DataFile='D:/桌面/AutoAnnotatoR/R Step4' # data position
file.CIR<-'example data.xlsx'  #data name
file.list1<-'Compound_library_1.xlsx'
file.list2<-'Compound_library_2.xlsx'


diff_ppm<-20

diff_rt<-0.15  # 单位：min  


Function_20220120_2<-function(DataFile='',   
                                file.CIR='',
                                file.list1='',  
                                file.list2='', 
                                diff_ppm=10,  
                                diff_rt=0.2 
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
  
  packageStartupMessage(paste0(file.CIR,"文件读取."))
  data_CIR<-read.xlsx(file.CIR, 1, header=T, encoding="UTF-8", stringsAsFactors = FALSE)
  names(data_CIR)<-c('Rt/min','m/z','Ions','Type','Score','Class1')
  data_CIR$Type<-gsub("^\\s+|\\s+$", "", data_CIR$Type)
  
  packageStartupMessage(paste0(file.list1,"文件读取."))
  data_Compound_library1<-read.xlsx(file.list1, 1, header=T, encoding="UTF-8", stringsAsFactors = FALSE)
  names(data_Compound_library1)<-c('Type','Name',	'CAS',	'Formula',	'm/z',	'Rt/min')
  data_Compound_library1$Type<-gsub("^\\s+|\\s+$", "", data_Compound_library1$Type)
  
  packageStartupMessage(paste0(file.list2,"文件读取."))
  data_Compound_library2<-read.xlsx(file.list2, 1, header=T, encoding="UTF-8", stringsAsFactors = FALSE)
  names(data_Compound_library2)<-c('Type','Name',	'Formula',	'm/z')
  data_Compound_library2$Type<-gsub("^\\s+|\\s+$", "", data_Compound_library2$Type)
  
  final_data<-data.frame()
  for(i in 1:nrow(data_CIR)){
    type<-data_CIR$Type[i]
    mz<-data_CIR$`m/z`[i]
    rt<-data_CIR$`Rt/min`[i]
    # ppm<-mz*diff_ppm*10^(-6)
    final_data1<-data.frame()
    # 到Compound library-1.xlsx中寻找
    d<-data_Compound_library1[which(data_Compound_library1$Type==type),]
    mzdiffppm<-(mz-d$`m/z`)/d$`m/z`*1000000
    idx<-which(abs(mzdiffppm)<=diff_ppm)
    
    final_data1<-data.frame()
    
    if(length(idx)>0){
      d$ppm<-mzdiffppm
      d<-d[idx,]
      rt.idx<-which(!is.na(d$`Rt/min`))
      if(length(rt.idx)>0){
        
        idx2<-which(abs(d$`Rt/min`[rt.idx]-rt)<=diff_rt)
        if(length(idx2)>0){
          for(i2 in 1:length(idx2)){
            if(i2==1){
              final_data1<-data_CIR[i,]
            }else{
              final_data1<-rbind(final_data1,data_CIR[i,])
            }
          }
          rt.idx<-rt.idx[idx2]
          final_data2<-d[rt.idx,c('Name','Formula','CAS','ppm')]
          final_data1<-cbind(final_data1,final_data2)
          final_data1$Class<-'1'
        }else{
          
          d<-d[-rt.idx,]
          if(nrow(d)>0){
            final_data1<-data_CIR[i,]
            final_data1$Name<-paste(d$Name,collapse = ';')
            final_data1$Formula<-paste(unique(d$Formula),collapse = ';')
            final_data1$CAS<-paste(unique(d$CAS),collapse = ';')
            final_data1$ppm<-paste(unique(d$ppm),collapse = ';')
            final_data1$Class<-'2'
          }
        }
      }else{
        
        final_data1<-data_CIR[i,]
        final_data1$Name<-paste(d$Name,collapse = ';')
        final_data1$Formula<-paste(unique(d$Formula),collapse = ';')
        final_data1$CAS<-paste(unique(d$CAS),collapse = ';')
        final_data1$ppm<-paste(unique(d$ppm),collapse = ';')
        final_data1$Class<-'2'
      }
    }
    
    if(nrow(final_data1)==0){  
      
     
      d<-data_Compound_library2[which(data_Compound_library2$Type==type),]
      mzdiffppm<-(mz-d$`m/z`)/d$`m/z`*1000000
      idx<-which(abs(mzdiffppm)<=diff_ppm)
      if(length(idx)>0){
        d$ppm<-mzdiffppm
        d<-d[idx,]
        final_data1<-data_CIR[i,]
        final_data1$Name<-paste(d$Name,collapse = ';')
        final_data1$Formula<-paste(unique(d$Formula),collapse = ';')
        final_data1$CAS<-paste(unique(d$CAS),collapse = ';')
        final_data1$ppm<-paste(unique(d$ppm),collapse = ';')
        final_data1$Class<-'3'
      }else{
        
        final_data1<-data_CIR[i,]
        final_data1$Name<-'unknown'
        final_data1$Formula<-''
        final_data1$CAS<-''
        final_data1$ppm<-''
        final_data1$Class<-'4'
      }
    }
    final_data<-rbind(final_data,final_data1)
  }
  
  if(nrow(final_data)>0){
    rownames(final_data)<-1:nrow(final_data)
    write.xlsx(final_data,  paste0("Result_Out/compound_result.xlsx"), row.names = F, showNA=F)
  }else{
    packageStartupMessage(paste("step finally >>> compound_result.xlsx: Data is empty and cannot be output!",sep=''))
    return()
  }
  packageStartupMessage(paste("Finished."))
  
  
}

Function_20220120_2(DataFile=DataFile,
                    file.CIR=file.CIR,
                    file.list1=file.list1,  
                    file.list2=file.list2, 
                    diff_ppm=diff_ppm,  
                    diff_rt=diff_rt 
)

