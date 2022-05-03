
# 参数设置

DataFile='C:/Users/Ann/Desktop/AutoAnnotatoR/R Step3' #data position
file.list1<-'list1.xlsx'
file.list2<-'list2.xlsx'
file.mgf<-'example data.mgf'   #data name




diff_ppm2<-20  


Function_20220120_1<-function(DataFile='',    
                                file.list1='',  
                                file.list2='', 
                                file.mgf='',
                                diff_ppm=10,  
                                diff_ppm2=10 
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
  if(!require(readxl)){
    install.packages('readxl')
    if (require("readxl")) {
      packageStartupMessage(paste("packages:readxl installed and loaded!"))
    } else {
      stop("could not install readxl")
    }
  }
  
  if(!dir.exists('Result_Out')){
    dir.create('Result_Out')
  }
  
  
  # list-1
  packageStartupMessage(paste0(file.list1,"文件读取."))
  data1_sheet1<-read.xlsx(file.list1, 1, header=T, encoding="UTF-8", stringsAsFactors = FALSE)
  # data1_sheet2<-read.xlsx(file.list1, 2, header=T, encoding="UTF-8", stringsAsFactors = FALSE)
  data1_sheet1$type<-gsub("^\\s+|\\s+$", "", data1_sheet1$type)
  # data1_sheet2$type<-gsub("^\\s+|\\s+$", "", data1_sheet2$type)
  
  # list-2
  packageStartupMessage(paste0(file.list2,"文件读取."))
  excel_sheetname<-readxl::excel_sheets(file.list2)
  excel_sheetname<-gsub("^\\s+|\\s+$", "", excel_sheetname)
  data2_list<-list()
  for(i in 1:length(excel_sheetname)){
    data2<-read.xlsx(file.list2, i, header=T, encoding="UTF-8", stringsAsFactors = FALSE)
    naTF<-apply(data2, 1, function(x) sum(is.na(x))==length(x))
    if(sum(naTF)>0){
      data2<-data2[-which(naTF),]
    }
    data2_list<-c(data2_list,list(data2))
  }
  names(data2_list)<-excel_sheetname
  
  
  D_MS2_NA_int<-data.frame()
  packageStartupMessage(paste0(file.mgf,"文件读取."))
  mgf_data <- scan(file.mgf, what = character(0), sep = "\n")  # Read mgf file
  mgf_matrix <- createmgfmatrix(mgf_data)  # create mgf_matrix
  mgf_matrix<-apply(mgf_matrix, 2, as.numeric)
  mgf_matrix<-as.data.frame(mgf_matrix)
  
  NA_int_id<-which(is.na(mgf_matrix$Pepmass_intensity))
  if(length(NA_int_id)>0){
    d<-mgf_matrix[NA_int_id,]
    d$mgf_file<-file.mgf
    print(d)
    D_MS2_NA_int<-rbind(D_MS2_NA_int,d)
  }
  
  if(nrow(D_MS2_NA_int)>0){
    names(D_MS2_NA_int)<-c('Begin_rowid_inmgf','End_rowid_inmgf','tr_inmgf','Pepmass_mz','Pepmass_intensity')
    write.xlsx(D_MS2_NA_int,  paste0("Result_Out/mgf-PEPMASS-缺失信息.xlsx"), row.names = F, showNA=F)
    stop('mgf-PEPMASS含有缺失信息，具体信息见 Result_Out/mgf-PEPMASS-缺失信息.xlsx')
  }
  mgf_data_new<-mgf_data
  
  
  
  
 
  data1_sheet1_new<-data.frame()
  for(i in 1:nrow(data1_sheet1)){
    d<-data.frame(type=data1_sheet1[i,1],ion=as.numeric(data1_sheet1[i,-1]),stringsAsFactors = F)
    data1_sheet1_new<-rbind(data1_sheet1_new,d)
  }
  data1_sheet1_new<-na.omit(data1_sheet1_new)
  data1_sheet1_num<-table(data1_sheet1_new$type)
  
  
  final_d<-data.frame()
  
  ppm2<-diff_ppm2*10^(-6)
  for(i0 in 1:nrow(mgf_matrix)){
    mgf_data1<-mgf_data_new[mgf_matrix$Begin_num[i0]:mgf_matrix$End_num[i0]]
    idx<-which(!grepl("[a-zA-Z]", mgf_data1))-1+mgf_matrix$Begin_num[i0]
    mz_ms2.all<-c()
    int_ms2.all<-c()
    for (j in mgf_data1){
      if (grepl("[a-zA-Z]", j)){
        next()
      }else{
        mz_ms2 <- as.numeric(unlist(strsplit(j, " "))[1])
        int_ms2 <- as.numeric(unlist(strsplit(j, " "))[2])
        mz_ms2.all<-c(mz_ms2.all,mz_ms2)
        int_ms2.all<-c(int_ms2.all,int_ms2)
      }
    }
    D_ion<-data.frame(mz_ms2=mz_ms2.all,int_ms2=int_ms2.all,noid=idx,stringsAsFactors = F)
    
    data_A<-D_ion[which(D_ion$mz_ms2<165),]
    if(nrow(data_A)>20){
      data_A<-data_A[order(data_A$int_ms2,decreasing=T)[1:20],]
    }
    
    data_B<-D_ion[which(D_ion$mz_ms2>300),]
    
    if(nrow(data_A)>10){
      data_C<-data_A[1:10,]
    }else{
      data_C<-data_A
    }
    
    
    typec<-c()
    final_d0<-data.frame()
    
   
    
    id.c<-c()
    
    if(nrow(data_C)>0){
      idx.c<-sapply(data_C$mz_ms2,function(x) which(x<=(data1_sheet1_new$ion+data1_sheet1_new$ion*ppm2) & x>=(data1_sheet1_new$ion-data1_sheet1_new$ion*ppm2)))
      id.c<-unlist(idx.c)
      id.c<-unique(id.c)
      if(length(id.c)>0){
        t<-table(data1_sheet1_new[id.c,'type'])
        TF<-data1_sheet1_num[names(t)]==t
        if(sum(TF)>0){
          typec<-names(t)[TF]
        }
      }
    }
    typec<-unique(typec)
    
    if(length(typec)>1){
      int_typec<-c()
      for(type1 in typec){
        temp<-data1_sheet1_new[which(data1_sheet1_new$type==type1),]
        idx.c<-sapply(data_C$mz_ms2,function(x) which(x<=(temp$ion+temp$ion*ppm2) & x>=(temp$ion-temp$ion*ppm2)))
        d.c<-data_C[which(unlist(lapply(idx.c,length))!=0),]
        int_typec<-c(int_typec,mean(d.c$int_ms2))
      }
      typec<-typec[order(int_typec,decreasing = T)[1]]
    }
    
    
    
    
    if(nrow(data_B)>1){
      mzb<-data_B$mz_ms2
      groupB<-combn(1:nrow(data_B),2)
      diff<-apply(groupB,2,function(x) abs(mzb[x[1]]-mzb[x[2]]))
      group_ion<-apply(groupB,2,function(x) paste(round(mzb[x[1]],4),round(mzb[x[2]],4),sep = ';'))
    }
    
    
    if(length(typec)>0){
        for(type1 in typec){
          temp<-data2_list[[type1]]
          ion<-data1_sheet1_new$ion[which(data1_sheet1_new$type==type1)]
          idx.a<-sapply(data_A$mz_ms2,function(x) which(x<=(temp$ion1+temp$ion1*ppm2) & x>=(temp$ion1-temp$ion1*ppm2)))
          id.a<-unlist(idx.a)
          s<-sum(temp[id.a,2])
          Ions<-paste(round(data_A$mz_ms2[which(unlist(lapply(idx.a,length))!=0)],4) ,collapse = ';')
          ion<-temp$ion2[which(!is.na(temp$ion2))]
          id.b<-unlist(sapply(diff, function(x) which(x<=(ion+0.008) & x>=(ion-0.008))) )
          id.b<-unique(id.b)
          if(length(id.b)>0){
            ion<-ion[id.b]
            s2<-sum(temp[match(ion,temp$ion2),4])
            curridx<-sapply(diff, function(x) which(x<=(ion+0.008) & x>=(ion-0.008)))
            Ions2<-paste(group_ion[which(unlist(lapply(curridx,length))!=0)] ,collapse = ';')
            s<-s+s2
            Ions<-paste(Ions,Ions2,sep = ';')
          }
          final_d1<-data.frame(rt=mgf_matrix$TR_num[i0],mz=mgf_matrix$Pepmass_num[i0],Ions=Ions,Type=type1,Score=s,Class='a',stringsAsFactors = F)
          final_d0<-rbind(final_d0,final_d1)
        }
    }else{
      s.c<-c()
      Ions.c<-c()
      for(type1 in names(data2_list)){
        temp<-data2_list[[type1]]
        ion<-data1_sheet1_new$ion[which(data1_sheet1_new$type==type1)]
        idx.a<-sapply(data_A$mz_ms2,function(x) which(x<=(temp$ion1+temp$ion1*ppm2) & x>=(temp$ion1-temp$ion1*ppm2)))
        id.a<-unlist(idx.a)
        s<-sum(temp[id.a,2])
        Ions<-paste(round(data_A$mz_ms2[which(unlist(lapply(idx.a,length))!=0)],4) ,collapse = ';')
        ion<-temp$ion2[which(!is.na(temp$ion2))]
        id.b<-unlist(sapply(diff, function(x) which(x<=(ion+0.008) & x>=(ion-0.008))) )
        id.b<-unique(id.b)
        if(length(id.b)>0){
          ion<-ion[id.b]
          s2<-sum(temp[match(ion,temp$ion2),4])
          curridx<-sapply(diff, function(x) which(x<=(ion+0.008) & x>=(ion-0.008)))
          Ions2<-paste(group_ion[which(unlist(lapply(curridx,length))!=0)] ,collapse = ';')
          s<-s+s2
          Ions<-paste(Ions,Ions2,sep = ';')
        }
        s.c<-c(s.c,s)
        Ions.c<-c(Ions.c,Ions)
      }
      maxid<-order(s.c,decreasing = T)[1]
      s<-s.c[maxid]
      Ions<-Ions.c[maxid]
      final_d0<-data.frame(rt=mgf_matrix$TR_num[i0],mz=mgf_matrix$Pepmass_num[i0],Ions=Ions,Type=paste0('unknown_',names(data2_list)[maxid]),Score=s,Class='b',stringsAsFactors = F)
    }
    final_d<-rbind(final_d,final_d0)
  }
  
  final_d<-unique(final_d)
  if(nrow(final_d)>0){
    names(final_d)<-c('rt','m/z','Ions','Type','Score','Class')
    final_d.type<-final_d[,c('rt','m/z','Type')]
    unknownid<-which(grepl('unknown_',final_d$Type))
    final_d.type$Type[unknownid]<-'unknown' 
    final_d$Type<-gsub('unknown_','',final_d$Type)
    write.xlsx(final_d.type,  paste0("Result_Out/type.xlsx"), row.names = F, showNA=F)
    write.xlsx(final_d,  paste0("Result_Out/Score_result.xlsx"), row.names = F, showNA=F)
  }else{
    packageStartupMessage(paste("step finally >>> Score_result.xlsx: Data is empty and cannot be output!",sep=''))
    return()
  }
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
Function_20220120_1(DataFile=DataFile, 
                      file.list1=file.list1,    
                      file.list2=file.list2,
                      file.mgf=file.mgf,
                      diff_ppm=diff_ppm,
                      diff_ppm2=diff_ppm2
)








