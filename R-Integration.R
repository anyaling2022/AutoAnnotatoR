# 参数设置

DataFile='D:/桌面/AutoAnnotatoR/R-Integration/example data' # data position
set_diff_rt<-0.3  # 单位min
set_diff_mz<-0.02 # Da




Function_20220419<-function(DataFile='',    
                            set_diff_rt=0.15,
                            set_diff_mz=0.01
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
  }else{
    unlink('Result_Out', recursive=TRUE)
    dir.create('Result_Out')
  }
  
  # list-1
  packageStartupMessage(paste0("文件读取."))
  all_files<-list.files(DataFile,recursive = F, full.names = F)
  all_files<-all_files[which(grepl('.xlsx',all_files))]
  all_data<-data.frame()
  for(i in 1:length(all_files)){
    packageStartupMessage(paste0(all_files[i]))
    data<-read.xlsx(all_files[i], 1, header=T, encoding="UTF-8", stringsAsFactors = FALSE)
    names(data)<-c("Rt/min",  "m/z",     "Ions" ,   "Type",    "Score",   "Name" ,   "Formula", "CAS",     "ppm",     "Class",   "Source" )
    naTF<-apply(data, 1, function(x) sum(is.na(x))==length(x))
    if(sum(naTF)>0){
      data<-data[-which(naTF),]
    }
    data$file<-all_files[i]
    if(i==1){
      all_data<-data
    }else{
      all_data<-rbind(all_data,data)
    }
  }
  if(sum(is.na(all_data$Formula))>0){
    all_data$Formula[which(is.na(all_data$Formula))]<-''
  }
  all_data$Class<-factor(all_data$Class,levels = sort(unique(all_data$Class),decreasing = T))
  if(class(all_data$Score)!='numeric'){
    all_data$Score<-as.numeric(all_data$Score)
  }
  if(class(all_data$ppm)!='numeric'){
    all_data$ppm<-as.numeric(all_data$ppm)
  }
  all_data$ppm2<-abs(all_data$ppm)
  ppm2<-unique(abs(all_data$ppm))
  ppm2_nona<-ppm2[-which(is.na(ppm2))]
  d.ppm<-data.frame(ppm2=sort(ppm2_nona,decreasing = T),ppm_ord=1:length(ppm2_nona))
  all_data2<-merge(all_data,d.ppm,by=c('ppm2'),all.x = T)
  all_data2$ppm_ord[which(is.na(all_data2$ppm_ord))]<-0
  
  group_same_find<-function(x,x0){
    inter<-intersect(x,x0)
    TF=F
    if(length(inter)==0){
      TF=F
    }else{
      if(sum(inter%in%x)>0){
        TF=T
      }
    }
    return(TF)
  }
  
  fin_d<-data.frame()
  uni.Formula<-unique(all_data$Formula)
  
  for(i in 1:length(uni.Formula)){
    curr<-uni.Formula[i]
    d<-all_data2[which(all_data2$Formula==curr),]
    uni.type<-unique(d$Type)
    for(i2 in 1:length(uni.type)){
      d2<-d[which(d$Type==uni.type[i2]),] # 同一type下
      if(nrow(d2)>1){
        groupD<-combn(1:nrow(d2),2)
        id1<-as.numeric(groupD[1,])
        id2<-as.numeric(groupD[2,])
        idx_diffrt<-which(abs(d2$`Rt/min`[id1]-d2$`Rt/min`[id2])<=set_diff_rt)
        
        if(curr==''){ 
          idx_diffmz<-which(abs(d2$`m/z`[id1]-d2$`m/z`[id2])<=set_diff_mz)
          idx<-intersect(idx_diffrt,idx_diffmz)
          
        }else{ 
         
          idx<-idx_diffrt
        }
        
        if(length(idx)>0){
          
          idx.list<-list()
          for(i3 in 1:length(idx)){
            g1<-groupD[,idx[i3]]  
            if(i3==1){
              idx.list<-list(g1)
              next
            }
            gTF<-unlist(lapply(idx.list,function(x) group_same_find(x,g1) ))
            if(sum(gTF)>0){
              newid<-unique(c(unlist(idx.list[which(gTF)]),g1))
              idx.list<-idx.list[-which(gTF)]
              idx.list<-c(idx.list,list(newid))
            }else{
              idx.list<-c(idx.list,list(g1))
            }
          }
          d4<-data.frame()
          for(i4 in 1:length(idx.list)){
            idx2<-idx.list[[i4]]
            d3<-d2[idx2,]
            ord<-order(d3$Class,d3$Score,d3$ppm_ord,decreasing = T)
            d4_1<-d3[ord[1],]
            d4_1$Species<-paste(unique(d3$Source),collapse = ';') 
            d4<-rbind(d4,d4_1)
          }
          
        
          other.idx<-setdiff(1:nrow(d2),unique(as.numeric(groupD[,idx])))
          if(length(other.idx)>0){
            d3<-d2[other.idx,]
            d3$Species<-d3$Source
            d4<-rbind(d4,d3)
          }
        }else{ 
          d4<-d2
          d4$Species<-d2$Source
        }
        
      }else{ 
        d4<-d2
        d4$Species<-d2$Source
      }
      fin_d<-rbind(fin_d,d4)
    }
  
  }
  fin_d2<-fin_d[,c("Rt/min",  "m/z",     "Ions" ,   "Type",    "Score",   "Name" ,   "Formula", "CAS",     "ppm",     "Class",   "Species")]
  if(nrow(fin_d2)>0){
    write.xlsx(fin_d2,  paste0("Result_Out/Resultdata.xlsx"), row.names = F, showNA=F)
  }else{
    packageStartupMessage(paste("step finally >>> Data is empty and cannot be output!",sep=''))
    return()
  }
  
  packageStartupMessage(paste("Finished."))
  
}

# 函数调用
Function_20220419(DataFile=DataFile, 
                  set_diff_rt=set_diff_rt,
                  set_diff_mz=set_diff_mz
)