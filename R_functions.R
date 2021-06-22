loadLibrarys = function(){
  library(viridis)
  library(dplyr)
  library(ggplot2)
  library(hrbrthemes)
  library(tidyverse)
  library(gtable)
  library(gridExtra)
  #setwd("C:/Users/ricar/Desktop/Ricardo/Mestrado/1? Ano/Metodologias/Projeto 1/Projeto_MEI/Meta2")
}

#Functions  - Hypothesis Tests - P2P

#H0: O tempo total de espera dos Schedulers s?o iguais
#H1: O tempo total de espera dos Schedulers s?o diferentes

testHypothesis = function(scheduler1, scheduler2, parameter,workload){
  cat("Test Hypothesis\n\n")
  algorithm1 <- paste("sim_", scheduler1,"_", workload, sep='')
  algorithm2 <- paste("sim_",  scheduler2,"_", workload, sep='')
  
  # N - Lenght
  n1 <<-length(get(algorithm1)$scheduler)
  n2 <<-length(get(algorithm2)$scheduler)
  
  # Xbar - Mean
  xbar1 <<- mean(get(algorithm1)%>% pull(parameter))
  xbar2 <<- mean(get(algorithm2)%>% pull(parameter))
  
  # S - Variance
  s1 <<- sd(get(algorithm1)%>% pull(parameter))
  s2 <<- sd(get(algorithm2)%>% pull(parameter))
  
  variance_similar_check <<- s1^2 / s2^2
  if(variance_similar_check > 0.5 && variance_similar_check < 2){
    cat("As variaveis são similares.\n")
  }else{
    cat("As variaveis NãO são similares.\n")
  }
  
  #Compute T Value
  t_critical <<- qt(1-0.05, n1+n2-2)
  sp <<- sqrt(((n1-1)*s1^2+(n2-1)*s2^2)/(n1+n2-2))
  t_value <<- (xbar1 - xbar2)/(sp*sqrt((1/n1)+(1/n2)))

  if(t_value > (-1*t_critical) && t_value < t_critical){
    cat("Não podemos rejeitar H0.\n")
  }else{
    cat("Estamos em condições de rejeitar H0.\n")
  }
  #Compute P-Value
  p_value <<- 2*pt(-abs(t_value), df= (n1+n2-2))
  
  if(p_value > 0.05){
    cat("Não podemos rejeitar H0.\n")
  }else{
    cat("Estamos em condições de rejeitar H0.\n")
  }
  cat("P-Value: ", p_value)
  
}

testOneAnova = function(workload, parameter){
  cat("One-Way ANOVA\n\n")
  
  wl_fcfs <- paste("sim_fcfs_", workload, sep='')
  wl_rr5 <- paste("sim_rr5_", workload, sep='')
  wl_sj <- paste("sim_sjf_", workload, sep='')
  wl_srtf <- paste("sim_srtf_", workload, sep='')
  
  sch_fcfs <- rep("FCFS",30)
  sch_rr5 <- rep("RR",30)
  sch_sjf <- rep("SJF",30)
  sch_srtf <- rep("SRTF",30)
  
  value1 <- get(wl_fcfs) %>% pull(parameter)
  value2 <- get(wl_rr5) %>% pull(parameter)
  value3 <- get(wl_sj) %>% pull(parameter)
  value4 <- get(wl_srtf) %>% pull(parameter)
  
  scheduler1 <- get(wl_fcfs)$scheduler
  scheduler2 <- get(wl_rr5)$scheduler
  scheduler3 <- get(wl_sj)$scheduler
  scheduler4 <- get(wl_srtf)$scheduler
  
  meansParam1 <<- array()
  meansParam2 <<- array()
  meansParam3 <<- array()
  meansParam4 <<- array()
  
  seed <- get(wl_fcfs)$seed
  seeds1 <<- array()
  seeds2 <<- array()
  seeds3 <<- array()
  seeds4 <<- array()
  
  algorithms1 <<- array()
  algorithms2 <<- array()
  algorithms3 <<- array()
  algorithms4 <<- array()
 
  dataset1 <<- data.frame(value1,seed,scheduler1)
  dataset2 <<- data.frame(value2,seed,scheduler2)
  dataset3 <<- data.frame(value3,seed,scheduler3)
  dataset4 <<- data.frame(value4,seed,scheduler4)
  
  for(i in 0:29){
    dataset1.sub <<- subset(dataset1, seed == i)
    dataset2.sub <<- subset(dataset2, seed == i)
    dataset3.sub <<- subset(dataset3, seed == i)
    dataset4.sub <<- subset(dataset4, seed == i)
    
    meansParam1 <- c(meansParam1, mean(dataset1.sub$value))
    meansParam2 <- c(meansParam2, mean(dataset2.sub$value))
    meansParam3 <- c(meansParam3, mean(dataset3.sub$value))
    meansParam4 <- c(meansParam4, mean(dataset4.sub$value))
    
    seeds1 <- c(seeds1, i)
    seeds2 <- c(seeds2, i)
    seeds3 <- c(seeds3, i)
    seeds4 <- c(seeds4, i)
    
  }
  
  finalMeans <- c(meansParam1,meansParam2,meansParam3,meansParam4)
  finalSeeds <- c(seeds1,seeds2,seeds3,seeds4)
  finalGroups <- c(sch_fcfs,sch_rr5,sch_sjf,sch_srtf)
  
  finalMeans <- finalMeans[!is.na(finalMeans)]
  finalSeeds <- finalSeeds[!is.na(finalSeeds)]

  resultData <<- data.frame("Value" = finalMeans, "Scheduler" = finalGroups, "Seed" = finalSeeds)
  

  resultData$finalGroups <- as.factor(resultData$Scheduler)
  resultData$finalSeeds <- as.factor(resultData$Seed)
  
  #boxplot(Value ~ Scheduler, data=resultData)
  print(ggplot(resultData, aes(y=Value, x=Scheduler)) + geom_boxplot()) + labs(x = "legenda eixo x",y = "legenda eixo y")
  
  aov.out <<- aov(finalMeans ~ finalSeeds +finalGroups, data=resultData)

  #cat("Summary\n\n")
  #summary(aov.out)
  # 
  # cat("Shapiro-Test\n\n")
  # df = data.frame("res" = aov.out$res)
  # print(ggplot(df, aes(sample = res)) + stat_qq() + stat_qq_line(colour="red"))
  #shapiro.test(aov.out$res)
  # 
  #bartlett.test(finalMeans~finalGroups, data=resultData)
  
  # cat("Post-hoc Analysis\n\n")
  # cat("Pairwise Test\n\n")
  # pairwise.t.test(resultData$Value, resultData$Scheduler, paired=T, p.adj="bonf")
  # 
  # cat("Tukey HSD \n\n")
  # 
  t = TukeyHSD(aov.out)
  print(t)
  plot(t)
  
}

calcMeans = function(){
  t <- split(wl1_sim,f=wl1_sim$scheduler)
  # wlmean <- aggregate(t[[1]]$total_wait_time, list(t[[1]]$seed), mean)
  # df = data.frame(scheduler = "fcfs",total_wait_time = wlmean$x,wl=30)
  
  # wlmean <- aggregate(t[[2]]$total_wait_time, list(t[[2]]$seed), mean)
  # df = data.frame(scheduler = "sjf",total_wait_time = wlmean$x,wl=30)

  # wlmean <- aggregate(t[[3]]$total_wait_time, list(t[[3]]$seed), mean)
  # df = data.frame(scheduler = "srtf",total_wait_time = wlmean$x,wl=30)

  wlmean <- aggregate(t[[4]]$total_wait_time, list(t[[4]]$seed), mean)
  df = data.frame(scheduler = "rr",total_wait_time = wlmean$x,wl=30)
  
  t <- split(wl2_sim,f=wl2_sim$scheduler)
  # wlmean <- aggregate(t[[1]]$total_wait_time, list(t[[1]]$seed), mean)
  # newdf = data.frame(scheduler = "fcfs",total_wait_time = wlmean$x,wl=40)
  # df= rbind(df,newdf)
  
  # wlmean <- aggregate(t[[2]]$total_wait_time, list(t[[2]]$seed), mean)
  # newdf = data.frame(scheduler = "sjf",total_wait_time = wlmean$x,wl=40)
  # df= rbind(df,newdf)

  # wlmean <- aggregate(t[[3]]$total_wait_time, list(t[[3]]$seed), mean)
  # newdf = data.frame(scheduler = "srtf",total_wait_time = wlmean$x,wl=40)
  # df= rbind(df,newdf)

  wlmean <- aggregate(t[[4]]$total_wait_time, list(t[[4]]$seed), mean)
  newdf = data.frame(scheduler = "rr",total_wait_time = wlmean$x,wl=40)
  df= rbind(df,newdf)
  
  
  t <- split(wl3_sim,f=wl3_sim$scheduler)
  # wlmean <- aggregate(t[[1]]$total_wait_time, list(t[[1]]$seed), mean)
  # newdf = data.frame(scheduler = "fcfs",total_wait_time = wlmean$x,wl=50)
  # df= rbind(df,newdf)
  
  # wlmean <- aggregate(t[[2]]$total_wait_time, list(t[[2]]$seed), mean)
  # newdf = data.frame(scheduler = "sjf",total_wait_time = wlmean$x,wl=50)
  # df= rbind(df,newdf)

  # wlmean <- aggregate(t[[3]]$total_wait_time, list(t[[3]]$seed), mean)
  # newdf = data.frame(scheduler = "srtf",total_wait_time = wlmean$x,wl=50)
  # df= rbind(df,newdf)

  wlmean <- aggregate(t[[4]]$total_wait_time, list(t[[4]]$seed), mean)
  newdf = data.frame(scheduler = "rr",total_wait_time = wlmean$x,wl=50)
  df= rbind(df,newdf)
  
  t <- split(wl4_sim,f=wl4_sim$scheduler)
  # wlmean <- aggregate(t[[1]]$total_wait_time, list(t[[1]]$seed), mean)
  # newdf = data.frame(scheduler = "fcfs",total_wait_time = wlmean$x,wl=60)
  # df= rbind(df,newdf)
  
  # wlmean <- aggregate(t[[2]]$total_wait_time, list(t[[2]]$seed), mean)
  # newdf = data.frame(scheduler = "sjf",total_wait_time = wlmean$x,wl=60)
  # df= rbind(df,newdf)

  # wlmean <- aggregate(t[[3]]$total_wait_time, list(t[[3]]$seed), mean)
  # newdf = data.frame(scheduler = "srtf",total_wait_time = wlmean$x,wl=60)
  # df= rbind(df,newdf)

  wlmean <- aggregate(t[[4]]$total_wait_time, list(t[[4]]$seed), mean)
  newdf = data.frame(scheduler = "rr",total_wait_time = wlmean$x,wl=60)
  df= rbind(df,newdf)
  
  t <- split(wl5_sim,f=wl5_sim$scheduler)
  # wlmean <- aggregate(t[[1]]$total_wait_time, list(t[[1]]$seed), mean)
  # newdf = data.frame(scheduler = "fcfs",total_wait_time = wlmean$x,wl=10)
  # df= rbind(df,newdf)
  
  # wlmean <- aggregate(t[[2]]$total_wait_time, list(t[[2]]$seed), mean)
  # newdf = data.frame(scheduler = "sjf",total_wait_time = wlmean$x,wl=10)
  # df= rbind(df,newdf)

  # wlmean <- aggregate(t[[3]]$total_wait_time, list(t[[3]]$seed), mean)
  # newdf = data.frame(scheduler = "srtf",total_wait_time = wlmean$x,wl=10)
  # df= rbind(df,newdf)

  wlmean <- aggregate(t[[4]]$total_wait_time, list(t[[4]]$seed), mean)
  newdf = data.frame(scheduler = "rr",total_wait_time = wlmean$x,wl=10)
  df= rbind(df,newdf)
  
  t <- split(wl6_sim,f=wl6_sim$scheduler)
  # wlmean <- aggregate(t[[1]]$total_wait_time, list(t[[1]]$seed), mean)
  # newdf = data.frame(scheduler = "fcfs",total_wait_time = wlmean$x,wl=150)
  # df= rbind(df,newdf)
  
  # wlmean <- aggregate(t[[2]]$total_wait_time, list(t[[2]]$seed), mean)
  # newdf = data.frame(scheduler = "sjf",total_wait_time = wlmean$x,wl=150)
  # df= rbind(df,newdf)

  # wlmean <- aggregate(t[[3]]$total_wait_time, list(t[[3]]$seed), mean)
  # newdf = data.frame(scheduler = "srtf",total_wait_time = wlmean$x,wl=150)
  # df= rbind(df,newdf)

  wlmean <- aggregate(t[[4]]$total_wait_time, list(t[[4]]$seed), mean)
  newdf = data.frame(scheduler = "rr",total_wait_time = wlmean$x,wl=150)
  df= rbind(df,newdf)
  
  t <- split(wl7_sim,f=wl7_sim$scheduler)
  # wlmean <- aggregate(t[[1]]$total_wait_time, list(t[[1]]$seed), mean)
  # newdf = data.frame(scheduler = "fcfs",total_wait_time = wlmean$x,wl=500)
  # df= rbind(df,newdf)
  
  # wlmean <- aggregate(t[[2]]$total_wait_time, list(t[[2]]$seed), mean)
  # newdf = data.frame(scheduler = "sjf",total_wait_time = wlmean$x,wl=500)
  # df= rbind(df,newdf)

  # wlmean <- aggregate(t[[3]]$total_wait_time, list(t[[3]]$seed), mean)
  # newdf = data.frame(scheduler = "srtf",total_wait_time = wlmean$x,wl=500)
  # df= rbind(df,newdf)

  wlmean <- aggregate(t[[4]]$total_wait_time, list(t[[4]]$seed), mean)
  newdf = data.frame(scheduler = "rr",total_wait_time = wlmean$x,wl=500)
  df= rbind(df,newdf)
  
  t <- split(wl8_sim,f=wl8_sim$scheduler)
  # wlmean <- aggregate(t[[1]]$total_wait_time, list(t[[1]]$seed), mean)
  # newdf = data.frame(scheduler = "fcfs",total_wait_time = wlmean$x,wl=1000)
  # df= rbind(df,newdf)
  
  # wlmean <- aggregate(t[[2]]$total_wait_time, list(t[[2]]$seed), mean)
  # newdf = data.frame(scheduler = "sjf",total_wait_time = wlmean$x,wl=1000)
  # df= rbind(df,newdf)

  # wlmean <- aggregate(t[[3]]$total_wait_time, list(t[[3]]$seed), mean)
  # newdf = data.frame(scheduler = "srtf",total_wait_time = wlmean$x,wl=1000)
  # df= rbind(df,newdf)

  wlmean <- aggregate(t[[4]]$total_wait_time, list(t[[4]]$seed), mean)
  newdf = data.frame(scheduler = "rr",total_wait_time = wlmean$x,wl=1000)
  df= rbind(df,newdf)
  
  print(cor(df$total_wait_time,df$wl))
  regression = lm(df$total_wait_time~df$wl)
  print(summary(regression))
  
  ggplot(df, aes(x=wl, y=total_wait_time)) +
          geom_point(size=2) +
            geom_smooth(method=lm, se=FALSE)+
           labs(x = "NÃºmero de Processos",y = "Total Wait Time")
    
  
  #return(df)

  
}


#
#Use this function to load all workloads and simulations to envionment
#WARNING THIS MIGHT TAKE A WHILE!!!
loadAll = function(){
  loadAllWorkloads();
  loadAllSimulations();
  print("LOAD FINISHED");
}

#
#use this function to join all data from one workload simulation in a data frame
joinSimulations = function(){
  print("Joining simulations")
  
  wl1_sim <<- rbind(sim_fcfs_wl1,sim_sjf_wl1,sim_srtf_wl1,sim_rr5_wl1)
  wl2_sim <<- rbind(sim_fcfs_wl2,sim_sjf_wl2,sim_srtf_wl2,sim_rr5_wl2)
  wl3_sim <<- rbind(sim_fcfs_wl3,sim_sjf_wl3,sim_srtf_wl3,sim_rr5_wl3)
  wl4_sim <<- rbind(sim_fcfs_wl4,sim_sjf_wl4,sim_srtf_wl4,sim_rr5_wl4)
  wl5_sim <<- rbind(sim_fcfs_wl5,sim_sjf_wl5,sim_srtf_wl5,sim_rr5_wl5)
  wl6_sim <<- rbind(sim_fcfs_wl6,sim_sjf_wl6,sim_srtf_wl6,sim_rr5_wl6)
  wl7_sim <<- rbind(sim_fcfs_wl7,sim_sjf_wl7,sim_srtf_wl7,sim_rr5_wl7)
  wl8_sim <<- rbind(sim_fcfs_wl8,sim_sjf_wl8,sim_srtf_wl8,sim_rr5_wl8)
  levels(wl1_sim$scheduler) <- c("fcfs","sjf","srtf","rr5")
  levels(wl2_sim$scheduler) <- c("fcfs","sjf","srtf","rr5")
  levels(wl3_sim$scheduler) <- c("fcfs","sjf","srtf","rr5")
  levels(wl4_sim$scheduler) <- c("fcfs","sjf","srtf","rr5")
  levels(wl5_sim$scheduler) <- c("fcfs","sjf","srtf","rr5")
  levels(wl6_sim$scheduler) <- c("fcfs","sjf","srtf","rr5")
  levels(wl7_sim$scheduler) <- c("fcfs","sjf","srtf","rr5")
  levels(wl8_sim$scheduler) <- c("fcfs","sjf","srtf","rr5")
  
}

#
#Use this function to load all workloads to environment
#WARNING: takes some time to run
loadAllWorkloads = function(){
  #
  #WORKLOADS io bursts mean, io min, io max
  print("Starting to load all workloads, might take some time!")
  wl1_procs30 <<- loadWorkload(30);
  wl2_procs40 <<- loadWorkload(40);
  wl3_procs50 <<- loadWorkload(50);
  wl4_procs60 <<- loadWorkload(60);
}


#
#Use this function to load all simulations
loadAllSimulations = function(){
  #
  #SCHEDULERS SIMULATIONS
  print("Starting to load all simulations")
  
  #First Come First Served
  sim_fcfs_wl1 <<- loadSimulation(30,'fcfs')
  sim_fcfs_wl2 <<- loadSimulation(40,'fcfs')
  sim_fcfs_wl3 <<- loadSimulation(50,'fcfs')
  sim_fcfs_wl4 <<- loadSimulation(60,'fcfs')
  sim_fcfs_wl5 <<- loadSimulation(10,'fcfs')
  sim_fcfs_wl6 <<- loadSimulation(150,'fcfs')
  sim_fcfs_wl7 <<- loadSimulation(500,'fcfs')
  sim_fcfs_wl8 <<- loadSimulation(1000,'fcfs')
  
  #Shortest Job First
  sim_sjf_wl1 <<- loadSimulation(30,'sjf')
  sim_sjf_wl2 <<- loadSimulation(40,'sjf')
  sim_sjf_wl3 <<- loadSimulation(50,'sjf')
  sim_sjf_wl4 <<- loadSimulation(60,'sjf')
  sim_sjf_wl5 <<- loadSimulation(10,'sjf')
  sim_sjf_wl6 <<- loadSimulation(150,'sjf')
  sim_sjf_wl7 <<- loadSimulation(500,'sjf')
  sim_sjf_wl8 <<- loadSimulation(1000,'sjf')
  
  #Shortest Remaining Time First
  sim_srtf_wl1 <<- loadSimulation(30,'srtf')
  sim_srtf_wl2 <<- loadSimulation(40,'srtf')
  sim_srtf_wl3 <<- loadSimulation(50,'srtf')
  sim_srtf_wl4 <<- loadSimulation(60,'srtf')
  sim_srtf_wl5 <<- loadSimulation(10,'srtf')
  sim_srtf_wl6 <<- loadSimulation(150,'srtf')
  sim_srtf_wl7 <<- loadSimulation(500,'srtf')
  sim_srtf_wl8 <<- loadSimulation(1000,'srtf')
  
  #Round Robin
  #Quantum 5
  sim_rr5_wl1 <<- loadSimulation(30,'rr')
  sim_rr5_wl2 <<- loadSimulation(40,'rr')
  sim_rr5_wl3 <<- loadSimulation(50,'rr')
  sim_rr5_wl4 <<- loadSimulation(60,'rr')
  sim_rr5_wl5 <<- loadSimulation(10,'rr')
  sim_rr5_wl6 <<- loadSimulation(150,'rr')
  sim_rr5_wl7 <<- loadSimulation(500,'rr')
  sim_rr5_wl8 <<- loadSimulation(1000,'rr')
  
  joinSimulations();
}

#
#Use this function to load a specific simulation seed
loadSimulationSeed = function(seed,num_procs,scheduler,quantum=''){
  file_name = paste("Simulations/seed",toString(seed),"_procs",toString(num_procs),"_",scheduler,quantum,".txt",sep = '');
  sim = read.table(file=file_name,header=TRUE);
  total_wait_time = sim$ready_wait_time + (sim$io_wait_time-sim$io_bursts_time)
  sim$seed = as.factor(seed);
  sim$total_wait_time = total_wait_time;
  
  testing <- sim$cpu_bursts_time/ sim$tat
  sim$testing <- testing
  scheduler = as.factor(paste(scheduler,quantum,sep=''))
  sim = cbind(scheduler,sim);
  return(sim);
}

#
#Use this function to load a batch of simulation with all seeds
loadSimulation = function(num_procs,scheduler,quantum=''){
  file_name = paste("procs",toString(num_procs),"_",scheduler,quantum,sep = '');
  print("Loading all seeds for Simulation:")
  print(file_name)
  
  for(s in 0:29){
    sim = loadSimulationSeed(s,num_procs,scheduler,quantum);
    if(s==0){
      simulation = sim;
    }else{
      simulation = rbind(simulation,sim);
    }
  }
  return(simulation);
}

#
#Use this function to load a specific workload seed
loadWorkloadSeed = function(seed,num_procs){
  file_name = paste("Workloads/seed",toString(seed),"_procs",toString(num_procs),".txt",sep = '')
  
  for(l in 1:30){
    line = read.table(file=file_name,header=FALSE,skip=7+l,nrows=1);
    
    workloadLine = data.frame(seed = seed,arrival_time = c(as.numeric(line[1])),pid=l-1,cpu_bursts = c(length(line)/2),cpu_burst_total = c(0),
                          cpu_burst_mean = c(0),cpu_max_burst = c(0),cpu_min_burst = c(1000),io_bursts = c((length(line)-2)/2), io_burst_total = c(0),
                          io_burst_mean = c(0),io_max_burst = c(0),io_min_burst = c(1000));
    
    cpu=0;
    io=0
    for(i in 2:ncol(line)){
      if(i%%2==0){
        cpu = cpu+as.numeric(line[i]);
        if(as.numeric(line[i])>workloadLine$cpu_max_burst){
          workloadLine$cpu_max_burst = as.numeric(line[i]);
        }
        if(as.numeric(line[i])<workloadLine$cpu_min_burst){
          workloadLine$cpu_min_burst = as.numeric(line[i]);
        }
      }else{
        io = io+as.numeric(line[i]);
        if(as.numeric(line[i])>workloadLine$io_max_burst){
          workloadLine$io_max_burst = as.numeric(line[i]);
        }
        if(as.numeric(line[i])<workloadLine$io_min_burst){
          workloadLine$io_min_burst = as.numeric(line[i]);
        }
      }
    }
    workloadLine$cpu_burst_total = cpu;
    workloadLine$io_burst_total = io;
    workloadLine$cpu_burst_mean = cpu/workloadLine$cpu_bursts;
    workloadLine$io_burst_mean = io/workloadLine$io_bursts;
    
    if(l==1){
      workload = workloadLine;
    }else{
      workload = rbind(workload,workloadLine);
    }
  }
  workload$seed = factor(workload$seed)
  return(workload);
}

#
#Use this function to load a batch of workloads from all seeds
loadWorkload = function(num_procs){
  file_name = paste("procs",toString(num_procs),sep = '')
  print("Loading all seeds from workload:")
  print(file_name)
  
  for(l in 0:29){
      workloadLine = loadWorkloadSeed(l,num_procs);
    if(l==0){
      workload = workloadLine;
    }else{
      workload = rbind(workload,workloadLine);
    }
  }
  
  levels(workload$seed) <- c("Seed 0","Seed 1","Seed 2","Seed 3","Seed 4","Seed 5","Seed 6","Seed 7",
                         "Seed 8","Seed 9","Seed 10","Seed 11","Seed 12","Seed 13","Seed 14","Seed 15",
                         "Seed 16","Seed 17","Seed 18","Seed 19","Seed 20","Seed 21","Seed 22","Seed 23",
                         "Seed 24","Seed 25","Seed 26","Seed 27","Seed 28","Seed 29");
  
  
  return(workload);
}

#
#Use this function to get the means of a workload
getWorkloadMeans = function(workloads){
  max_cpu_total = aggregate(list(cpu_burst_total = workloads$cpu_burst_total),list(seed = workloads$seed),max)
  min_cpu_total = aggregate(list(cpu_burst_total = workloads$cpu_burst_total),list(seed = workloads$seed),min)
  
  max_io_total = aggregate(list(io_burst_total = workloads$io_burst_total),list(seed = workloads$seed),max)
  min_io_total = aggregate(list(io_burst_total = workloads$io_burst_total),list(seed = workloads$seed),min)
  
  means = data.frame(arrival_time_mean = mean(workloads$arrival_time),cpu_bursts_mean = mean(workloads$cpu_bursts),cpu_burst_total_mean = mean(workloads$cpu_burst_total),cpu_bust_total_max_mean = mean(max_cpu_total$cpu_burst_total),
                     cpu_bust_total_min_mean = mean(min_cpu_total$cpu_burst_total),cpu_burst_mean = mean(workloads$cpu_burst_mean),cpu_max_burst_mean = mean(workloads$cpu_max_burst),cpu_min_burst_min = mean(workloads$cpu_min_burst),
                                io_bursts_mean = mean(workloads$io_bursts), io_burst_total_mean = mean(workloads$io_burst_total),io_bust_total_max_mean = mean(max_io_total$io_burst_total),
                     io_bust_total_min_mean = mean(min_io_total$io_burst_total), io_burst_mean = mean(workloads$io_burst_mean),io_max_burst_mean = mean(workloads$io_max_burst),io_min_burst_mean = mean(workloads$io_min_burst));
  return(means);
}

#
#Use this function to plot runchart plot of simulations
runchart = function(){
  
  ##SINGLE RUNCHART PLOT
  # data = subset(wl5_sim,scheduler == "rr10")
  # data = data.frame(scheduler = "rr10",arrival_time = aggregate(data$arrival_time,list(data$pid),mean),tat = aggregate(data$tat,list(data$pid),mean))
  # d1 = t(data)
  # 
  # data = subset(wl5_sim,scheduler == "rr5")
  # data = data.frame(scheduler ="rr5",arrival_time = aggregate(data$arrival_time,list(data$pid),mean),tat = aggregate(data$tat,list(data$pid),mean))
  # d2 = t(data)
  # 
  # data = subset(wl5_sim,scheduler == "rr15")
  # data = data.frame(scheduler = "rr15",arrival_time = aggregate(data$arrival_time,list(data$pid),mean),tat = aggregate(data$tat,list(data$pid),mean))
  # d3 = t(data)
  # 
  # data = subset(wl5_sim,scheduler == "fcfs")
  # data = data.frame(scheduler = "fcfs",arrival_time = aggregate(data$arrival_time,list(data$pid),mean),tat = aggregate(data$tat,list(data$pid),mean))
  # d4 = t(data)
  # 
  # data = subset(wl5_sim,scheduler == "sjf")
  # data = data.frame(scheduler = "sjf",arrival_time = aggregate(data$arrival_time,list(data$pid),mean),tat = aggregate(data$tat,list(data$pid),mean))
  # d5 = t(data)
  # 
  # data = subset(wl5_sim,scheduler == "srtf")
  # data = data.frame(scheduler = "srtf",arrival_time = aggregate(data$arrival_time,list(data$pid),mean),tat = aggregate(data$tat,list(data$pid),mean))
  # d6 = t(data)
  # 
  # data = rbind(d4,d5)
  # data = rbind(data,d6)
  # data = rbind(data,d2)
  # data = rbind(data,d3)
  # data = rbind(data,d1)
  # data$scheduler = as.factor(data$scheduler)
  # 
  # data %>%
  #   mutate(scheduler = fct_relevel(scheduler, 
  #                                  "fcfs", "sjf", "srtf", 
  #                                  "rr5", "rr10", "rr15")) %>%
  #   ggplot( aes(x=time, y=nump, colour=scheduler))+
  #   geom_line() +
  #   scale_color_viridis(discrete = TRUE, alpha=0.6)+
  #   labs(x= "Tempo", y="NÂº de processos ativos") +
  #     theme_ipsum() +
  #     theme(
  #       plot.title = element_text(size=14),
  #       legend.title = element_text(face="bold")
  #     ) +
  #   ggtitle("Runchart Workload 5")
  
  
  
  data = subset(wl1_sim,scheduler == "rr")
  data = data.frame(scheduler ="rr",arrival_time = aggregate(data$arrival_time,list(data$pid),mean),tat = aggregate(data$tat,list(data$pid),mean))
  d2 = t(data)
  
  
  data = subset(wl1_sim,scheduler == "fcfs")
  data = data.frame(scheduler = "fcfs",arrival_time = aggregate(data$arrival_time,list(data$pid),mean),tat = aggregate(data$tat,list(data$pid),mean))
  d4 = t(data)
  
  data = subset(wl1_sim,scheduler == "sjf")
  data = data.frame(scheduler = "sjf",arrival_time = aggregate(data$arrival_time,list(data$pid),mean),tat = aggregate(data$tat,list(data$pid),mean))
  d5 = t(data)
  
  data = subset(wl1_sim,scheduler == "srtf")
  data = data.frame(scheduler = "srtf",arrival_time = aggregate(data$arrival_time,list(data$pid),mean),tat = aggregate(data$tat,list(data$pid),mean))
  d6 = t(data)
  
  data = rbind(d4,d5)
  data = rbind(data,d6)
  data = rbind(data,d2)
  data$scheduler = as.factor(data$scheduler)
  
  pwl1 <- data %>%
  mutate(scheduler = fct_relevel(scheduler, 
                            "fcfs", "sjf", "srtf", 
                            "rr")) %>%
  ggplot( aes(x=time, y=nump, colour=scheduler))+
    geom_line() +
    scale_color_viridis(discrete = TRUE, alpha=0.6)+
    theme_ipsum(base_size = 6) +
    labs(x= "Tempo", y="NÂº de processos ativos") +
    theme(
      legend.position="none",
      plot.title = element_text(size=8),
      axis.title.y = element_text(size = 5),
      axis.title.x = element_text(size = 5)
    ) +
    ggtitle("Workload 1")
  
  
  grid.arrange(pwl1)
  
  #PLOT AT
  # data = data.frame(aggregate(list(arrival_time=wl1_iob30_min5_max20$arrival_time),list(pid=wl1_iob30_min5_max20$pid),mean))
  # data %>%
  #   ggplot( aes(y=pid, x=arrival_time,colour=pid)) +
  #   geom_point() +
  #   geom_rug(sides="b") +
  #   theme_ipsum() +
  #   theme(
  #     legend.position="none",
  #     plot.title = element_text(size=14)
  #   ) +
  #   ggtitle("MÃ©dia do Arrival Time") +
  #   xlab("Arrival Time") +
  #   ylab("PID")
  
}

#runchart helper function
t = function(wl){
  nump = 0;
  tstat = data.frame(scheduler=wl$scheduler,nump = 0, time = wl$tat.x+wl$arrival_time.x,type='tat');
  tsat = data.frame(scheduler=wl$scheduler,nump = 0, time = wl$arrival_time.x,type='at');
  ts = rbind(tsat,tstat)
  ts = ts[order(ts$time),]
  
  for(i in 1:60){
    if(ts$type[i] == 'at'){
      nump = nump+1
      ts$nump[i] = nump;
    }else{
      nump = nump-1;
      ts$nump[i] = nump;
    }
  }
  
  return(ts);
  
}




simPlots = function(){
  
  #Plot
  p1 <- wl1_sim %>%
    ggplot( aes(y=ready_wait_time, x=scheduler, fill=scheduler)) +
    geom_boxplot() +
    scale_fill_viridis(discrete = TRUE, alpha=0.6) +
    geom_jitter(color="black", size=0.2, alpha=0.3) +
    stat_summary(fun=mean, geom="point", shape=20, size=3, color="red", fill="red") +
    theme_ipsum() +
    theme(
      plot.margin = margin(r=2),
      legend.position="none",
      plot.title = element_text(size=14)
    ) +
    ggtitle("SimulaÃ§Ã£o Workload 1") +
    xlab("") +
    ylab("Total Wait Time ")
  
  grid.arrange(p1,nrow=1, ncol=1)
  
  # means = getWorkloadMeans(wl1_iob30_min5_max20)
  # data = wl1_sim[which(wl1_sim$cpu_bursts_time>(means$cpu_burst_total_mean+means$cpu_bust_total_max_mean)/2),]
  # 
  # data %>%
  #   ggplot( aes(y=total_wait_time, x=scheduler, fill=scheduler)) +
  #   geom_boxplot() +
  #   scale_fill_viridis(discrete = TRUE, alpha=0.6) +
  #   geom_jitter(color="black", size=0.2, alpha=0.3) +
  #   stat_summary(fun=mean, geom="point", shape=20, size=3, color="red", fill="red") +
  #   theme_ipsum() +
  #   theme(
  #     legend.position="none",
  #     plot.title = element_text(size=14)
  #   ) +
  #   ggtitle("SimulaÃ§Ã£o Workload 1 max subset") +
  #   xlab("") +
  #   ylab("Total Wait Time (Ready Wait Time + IO Ready Wait Time)")

    # data = wl1_sim[which(wl1_sim$cpu_bursts_time<(means$cpu_burst_total_mean+means$cpu_bust_total_min_mean)/2),]
    # 
    # data %>%
    #   ggplot( aes(y=total_wait_time, x=scheduler, fill=scheduler)) +
    #   geom_boxplot() +
    #   scale_fill_viridis(discrete = TRUE, alpha=0.6) +
    #   geom_jitter(color="black", size=0.2, alpha=0.3) +
    #   stat_summary(fun=mean, geom="point", shape=20, size=3, color="red", fill="red") +
    #   theme_ipsum() +
    #   theme(
    #     legend.position="none",
    #     plot.title = element_text(size=14)
    #   ) +
    #   ggtitle("SimulaÃ§Ã£o Workload 1 min subset") +
    #   xlab("") +
    #   ylab("Total Wait Time (Ready Wait Time + IO Ready Wait Time)")

  # p1<- wl1_iob30_min5_max20 %>%
  #   ggplot( aes(y=arrival_time, x=seed, colour=seed)) +
  #   geom_point() +
  #   scale_color_viridis(discrete = TRUE, alpha=0.6) +
  #   theme_ipsum() +
  #   theme(
  #     plot.margin = margin(r=1,l=1,b=1),
  #     legend.position="none",
  #     plot.title = element_text(size=14)
  #   ) +
  #   scale_x_discrete(name="Seed",labels=c("0","","","","","5","","","","","10","","","","","15","","","","","20","","","","","25","","","","29")) +
  #   ylab("Arrival Time") +
  #   ggtitle("Arrival Time por Seed")
  # 
  # p2 <- wl1_iob30_min5_max20 %>%
  #   ggplot( aes(y=cpu_burst_total, x=seed, fill=seed)) +
  #   geom_boxplot() +
  #   scale_fill_viridis(discrete = TRUE, alpha=0.6) +
  #   geom_jitter(color="black", size=0.2, alpha=0.3) +
  #   stat_summary(fun=mean, geom="point", shape=20, size=1, color="red", fill="red") +
  #   theme_ipsum() +
  #   theme(
  #     plot.margin = margin(r=10,l=10,b=1),
  #     legend.position="none",
  #     plot.title = element_text(size=14)
  #   ) +
  #   scale_x_discrete(name="Seed",labels=c("0","","","","","5","","","","","10","","","","","15","","","","","20","","","","","25","","","","29")) +
  #   ylab("Total de CPU burst") +
  #   ggtitle("Total de CPU burst por Seed")
  # 
  # p3<- wl1_iob30_min5_max20 %>%
  #   ggplot( aes(y=cpu_bursts, x=seed, fill=seed)) +
  #   geom_boxplot() +
  #   scale_fill_viridis(discrete = TRUE, alpha=0.6) +
  #   geom_jitter(color="black", size=0.2, alpha=0.3) +
  #   stat_summary(fun=mean, geom="point", shape=20, size=1, color="red", fill="red") +
  #   theme_ipsum() +
  #   theme(
  #     plot.margin = margin(r=1,l=1,b=1),
  #     legend.position="none",
  #     plot.title = element_text(size=14)
  #   ) +
  #   scale_x_discrete(name="Seed",labels=c("0","","","","","5","","","","","10","","","","","15","","","","","20","","","","","25","","","","29")) +
  #   ylab("NÃºmero de CPU bursts") +
  #   ggtitle("NÃºmero de CPU bursts por Seed")
  # 
  # grid.arrange(p1,p2,p3,nrow=1)
}


