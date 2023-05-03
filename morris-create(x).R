###--This function has the goal to create a sample with the Morris method, in order to be analyzed further.

#---input arguments for the function
pasta=c("name of the folder/subfolder")
input.r=80
input.l=8

#morris.create(pasta,input.r,input.l)
morris.create=function (pasta,input.r=10,input.l=4){
      #---Open the 'sensitivity' package
      library(sensitivity)
      
      #---File which contains the factors data "input.csv"
      factors.path=c(paste(pasta,"/morris-input.csv",sep=""))
      #---Class from each column from the input file "input.csv"
      factors.classes=c("character","character","numeric","numeric")
      #---Read the input file "input.csv"
      factors=read.table(file=factors.path,header=TRUE,sep=",",colClasses=factors.classes)
      
      #---Discretize the file to facilitate the calculation
      factors.names=factors[[1]]
      factors.lower=factors[[3]]
      factors.upper=factors[[4]]
      
      #---Apply the morris() function to create a sample and other information
      morris.funcao=morris(model=NULL,factors=factors.names,r=input.r,
                           design=list(type="oat",levels=input.l,grid.jump=1),
                           binf=factors.lower,bsup=factors.upper,scale=FALSE)
      
      #---Save the file in R format without losing the sample information
      morris.dput=dput(morris.funcao,file=c(paste(pasta,"/morris-dput.R",sep="")),control="all")
      
      #---Save the sample in CSV format, also
      morris.amostra=morris.funcao[[8]]
      morris.amostra.path=c(paste(pasta,"/morris-sample.csv",sep=""))
      morris.amostra.csv=write.table(x=morris.amostra,file=morris.amostra.path,sep=",",row.names=FALSE)
      
      return(c("Morris sample created with success *-*"))
}
