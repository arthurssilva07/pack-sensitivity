###--Esta FUNÇÃO tem o objetivo de determinar uma amostra com o método de Morris.

pasta=c("Tese_Parte 2/2. Morris SA")
input.r=80
input.l=8

#morris.create(pasta,input.r,input.l)
morris.create=function (pasta,input.r=10,input.l=4){
      #---Abrir o package 'sensitivity'
      library(sensitivity)
      
      #---Arquivo que contém os dados dos fatores "input.csv"
      factors.path=c(paste(pasta,"/morris-input.csv",sep=""))
      #---Class de cada coluna do arquivo de entrada "input.csv"
      factors.classes=c("character","character","numeric","numeric")
      #---Ler o arquivo de entrada "input.csv"
      factors=read.table(file=factors.path,header=TRUE,sep=",",colClasses=factors.classes)
      
      #---Discretizar o arquivo para facilitar os cálculos
      factors.names=factors[[1]]
      factors.lower=factors[[3]]
      factors.upper=factors[[4]]
      
      #---Aplicar a função "morris()" para calcular a amostra e todas as demais informações
      morris.funcao=morris(model=NULL,factors=factors.names,r=input.r,
                           design=list(type="oat",levels=input.l,grid.jump=1),
                           binf=factors.lower,bsup=factors.upper,scale=FALSE)
      
      #---Salvar o arquivo gerado em formato R para ler depois de simular, sem perder a amostra
      morris.dput=dput(morris.funcao,file=c(paste(pasta,"/morris-dput.R",sep="")),control="all")
      
      #---Salvar a amostra em CSV
      morris.amostra=morris.funcao[[8]]
      morris.amostra.path=c(paste(pasta,"/morris-sample.csv",sep=""))
      morris.amostra.csv=write.table(x=morris.amostra,file=morris.amostra.path,sep=",",row.names=FALSE)
      
      return(c("Arquivos de Morris criados com sucesso *-*"))
}