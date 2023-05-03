###--Esta FUNÇÃO tem o objetivo de análisar a Sensibilidade com Morris.

pasta=c("Tese_Parte 2/2. Morris SA")

#morris.analyse(pasta)
morris.analyse=function (pasta){
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
      
      #---Buscar a amostra salva antes da simulação
      dget.folder=c(paste(pasta,"/morris-dput.R",sep=""))
      morris.funcao=dget(dget.folder)
      #morris.funcao=dget("Morris teste 2/morris-dput.R")
      
      #---Buscar os resultados das simulações em quantas variáveis houverem
      results.path=c(paste(pasta,"/morris-results.csv",sep=""))
      results=read.table(file=results.path,header=TRUE,sep=",",colClasses="numeric")
      results.ncol=ncol(results)
      
      #---Atribuir valores iniciais NULL para as variáveis da iteração
      output.mu=NULL
      output.mustar=NULL
      output.sigma=NULL
      output.all=NULL
      output.mu.names=NULL
      output.mustar.names=NULL
      output.sigma.names=NULL
      output.all.names=NULL
      
      #---Processo iterativo para analisar em cada variável dependente
      for(i in 1:results.ncol){
            
            #---Usar a função tell() para retornar aos resultados de Morris de entrada
            morris.results=tell(x=morris.funcao,y=results[[i]])
            #---Buscar o nome das colunas (variáveis dependentes)
            variable.name=names(results[i])
            
            #---Organizar os resultados de MU, MU* e SIGMA conforme o package 'sensitivity' recomenda
            morris.mu=apply(morris.results$ee,2,mean)
            morris.mustar=apply(morris.results$ee,2,function(morris.results) mean(abs(morris.results)))
            morris.sigma=apply(morris.results$ee,2,sd)
            
            #---Atribuir nomes diferentes para cada variável dependente em cada medida de sensibilidade
            n1=c(paste(variable.name,c("_MU"),sep=""))
            n2=c(paste(variable.name,c("_MU*"),sep=""))
            n3=c(paste(variable.name,c("_SIGMA"),sep=""))
            
            #---Calcular as medidas de sensibilidade em 4 tabelas diferentes: MU, MU*, SIGMA e ALL
            output.mu=cbind(output.mu,morris.mu,deparse.level=0)
            output.mustar=cbind(output.mustar,morris.mustar,deparse.level=0)
            output.sigma=cbind(output.sigma,morris.sigma,deparse.level=0)
            output.all=cbind(output.all,morris.mu,morris.mustar,morris.sigma,deparse.level=0)
            
            #---Calcular o nome das medidas de sensibilidade em 4 tabelas diferentes
            output.mu.names=rbind(output.mu.names,n1,deparse.level=0)
            output.mustar.names=rbind(output.mustar.names,n2,deparse.level=0)
            output.sigma.names=rbind(output.sigma.names,n3,deparse.level=0)
            output.all.names=rbind(output.all.names,n1,n2,n3,deparse.level=0)
            
      }
      #---Transformar a matriz de nomes em vetores
      vector.n1=as.vector(as.data.frame.matrix(output.mu.names)[[1]])
      vector.n2=as.vector(as.data.frame.matrix(output.mustar.names)[[1]])
      vector.n3=as.vector(as.data.frame.matrix(output.sigma.names)[[1]])
      vector.n4=as.vector(as.data.frame.matrix(output.all.names)[[1]])
      
      #---Calcular o data.frame com as tabelas das medidas de sensibilidade
      a1=as.data.frame.matrix(output.mu)      #Com os resultados de MU
      a2=as.data.frame.matrix(output.mustar)  #Com os resultados de MU*
      a3=as.data.frame.matrix(output.sigma)   #Com os resultados de SIGMA
      a4=as.data.frame.matrix(output.all)     #Com os resultados de ALL
      
      #---Atribuir nomes às colunas de cada tabela, conforme feito anteriormente
      names(a1)=vector.n1
      names(a2)=vector.n2
      names(a3)=vector.n3
      names(a4)=vector.n4
      
      #---Atribuir nomes para os arquivos de saída com as medidas de sensibilidade
      output.name1=c(paste(pasta,"/morris-output-MU.csv",sep=""))
      output.name2=c(paste(pasta,"/morris-output-MUSTAR.csv",sep=""))
      output.name3=c(paste(pasta,"/morris-output-SIGMA.csv",sep=""))
      output.name4=c(paste(pasta,"/morris-output-ALL.csv",sep=""))
      
      #---Criar os arquivos de saída, sem o nome das linhas (variáveis de entrada)
      o1=write.table(x=a1,file=output.name1,sep=",",row.names=TRUE,col.names=NA)
      o2=write.table(x=a2,file=output.name2,sep=",",row.names=TRUE,col.names=NA)
      o3=write.table(x=a3,file=output.name3,sep=",",row.names=TRUE,col.names=NA)
      o4=write.table(x=a4,file=output.name4,sep=",",row.names=TRUE,col.names=NA)
      
      return(c("Análise de sensibilidade realizada com sucesso"))
}