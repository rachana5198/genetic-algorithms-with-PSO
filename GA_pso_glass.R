#GA-PSO algorithm on diabetic dataset.................

bird_setp = 100 
c2 = 1.49         #PSO parameter C1 
c1 = 1.49         # PSO parameter C2 
w  = 0.72    #PSO momentum or inertia  

PXOVER      = 0.9
PMutation   = 0.1
setwd('C:/Users/hai/Desktop/project/glass/gapso_glass')

xdat = read.table('glass3.dat')

#xdat <- xdat[sample(1:nrow(xdat),4 , replace = FALSE),]
n <- nrow(xdat)
dim <-ncol(xdat)

maxP = max(xdat)
minP = min(xdat)

source('C:/Users/hai/Desktop/project/glass/gapso_glass/Live_fn.R')
source('C:/Users/hai/Desktop/project/glass/gapso_glass/Mutation.R')
source('C:/Users/hai/Desktop/project/glass/gapso_glass/Select.R')
source('C:/Users/hai/Desktop/project/glass/gapso_glass/Xcross.R')



fitness=matrix(0,n,bird_setp)

#R1 = matrix(runif(dim*n), ncol=dim)
#R2 = matrix(runif(dim*n), ncol=dim)
R1<-runif(1)
R2<-runif(1)
current_fitness =matrix(0,n,1)

current_position = t(xdat)
velocity = matrix(0,dim,n)      #initialize initial velocity with 0's
local_best_position  = current_position 
globl_best_position =matrix(0,dim,n)
                                
for (i in 1:n){
   current_fitness[i] = Live_fn(current_position[,i])   #calculating evalj    
}

local_best_fitness  = current_fitness 
global_best_fitness <- min(local_best_fitness)   
g <- which.min(local_best_fitness)

for (i in 1:n){
    globl_best_position[,i] = local_best_position[,g] 
}

   sumfit=apply(current_fitness,2,sum)
   relativeFitness=current_fitness/sumfit
   for (i in 2:n){
      relativeFitness[i]=relativeFitness[i-1]+relativeFitness[i]
   }
   #newPop=currrent_position;
   
   
   
   newPop=Select(xdat, relativeFitness)
   
   newPop=Xcross(newPop, PXOVER)
   
   newPop=Mutation(newPop,PMutation,maxP,minP)

                                               
velocity = w*velocity + c1*(R1*(globl_best_position - current_position)) + c2*(R2*(globl_best_position-local_best_position))

current_position = current_position + velocity 

#### Main Loop ####

tic=proc.time()[1]
#iterations counter
iter = 0        
while  (iter < bird_setp)
{ 
   iter = iter + 1;
   for (i in 1:n){
      current_fitness[i] = Live_fn(current_position[,i])
   }

   for (i in 1 : n){
      if (current_fitness[i] < local_best_fitness[i]){
        local_best_fitness[i]  = current_fitness[i]  
        local_best_position[,i] = current_position[,i]   
      }
   }

current_global_best_fitness <- min(local_best_fitness) 
g <- which.min(local_best_fitness)

   if (current_global_best_fitness < global_best_fitness){
      global_best_fitness = current_global_best_fitness

      for (i in 1:n){
         globl_best_position[,i] = local_best_position[,g]
      }
   }
  
 sumfit=apply(current_fitness,2,sum)
   relativeFitness=current_fitness/sumfit
   for (i in 2:n){
      relativeFitness[i]=relativeFitness[i-1]+relativeFitness[i]
   }
   
   newPop=Select(xdat, relativeFitness)
   
   
   newPop=Xcross(newPop, PXOVER)
   
   newPop=Mutation(newPop,PMutation,maxP,minP)
   #globl_best_position[is.nan(globl_best_position)] = 0
   #current_position[is.nan(current_position)] = 0
   #R1[is.nan(R1)] = 0
   
#globl_best_position <- na.omit(globl_best_position)
#current_position <- na.omit(current_position)
   velocity = w *velocity + c1*(R1*(globl_best_position-current_position)) + c2*(R2*(globl_best_position-local_best_position))
   current_position = current_position + velocity
   x=current_position[1,]
   y=current_position[2,]
   
if (1){
   
   #plot(x, y , 'h')
  plot(x,y,type="b",col="red")  
   #plot(x,y, xlim=c(-20,20), ylim=c(-20,20)) 
   
   Sys.sleep(.1)  
}
}
#### end ####

Jbest_min = min(current_fitness) # minimum fitness
toc=proc.time()[1]-tic
 #plot(x, y , 'h')

         
