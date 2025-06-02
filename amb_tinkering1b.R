
#Boone's ABM model starting at Video Part 9 https://www.youtube.com/c/EdBoone/videos
#see abm_tinkering1.R for the first version of the ABM (without functions)

AgentGen1 <- function( nPop1, E0, I0){
  Agent1 <- data.frame( AgentNo = 1:nPop1,
                        State = "S",
                        Mixing = runif(nPop1,0,1),
                        TimeE=0,
                        TimeI=0,
                        stringsAsFactors = FALSE)
  Agent1$State[1:E0] <- "E"
  Agent1$TimeE[1:E0] <- rbinom(E0, 13, 0.5) + 1
  Agent1$State[(E0+1):(E0+I0)] <- "I"
  Agent1$TimeI[(E0+1):(E0+I0)] <- rbinom(I0,12,0.5)+1
return(Agent1)  
}
####

#Agent1 = our population of agents; par1 = parameters; and nTim1= how long of a run

ABM1 <- function( Agent1, par1, nTime1){
  nPop1 <- nrow(Agent1)
  out1 <- data.frame( S=rep(0, nTime1),
                      E=rep(0, nTime1),
                      I=rep(0, nTime1),
                      R=rep(0, nTime1),
                      D=rep(0, nTime1))
  #move the people through time
  for (k in 1:nTime1){
    #moving people through time but only S or Ewarning ppl
    StateS1 <- (1:nPop1)[Agent1$State=="S"]
    StateSE1 <- (1:nPop1)[Agent1$State=="S" | Agent1$State=="E"]
    for ( i in StateS1){
      #determine if they like to meet others
      Mix1 <- Agent1$Mixing[i]
      #then How many agents will they meet
      Meet1 <- round(Mix1*par1$MaxMix,0)
      #then who do they meet but only S or E ppl
      Meet2 <- sample (StateSE1, 
                       Meet1, 
                       replace = TRUE,
                       prob=Agent1$Mixing[StateSE1])
      for( j in 1:length(Meet2)) {
        #grab who they will meet
        Meet1a <- Agent1[Meet2[j],]
        #if exposed change State and if they meet someone else remain in state
        #the Urand1 modifies chance of exposure when an exposed person is met
        if( Meet1a$State == "E" && Meet1 > 0){
          Urand1<-runif(1,0,1)
          if(Urand1 < par1$S2E){
            Agent1$State[i] <- "E"
          }
        } else {
          Agent1$State[i]
        }
      }
    }
    #grab those been exposed and index them forward and recovered after 3-4 days
    #the 14 and 3 is a known value or constant 
    StateE1 <- (1:nPop1)[Agent1$State=="E"]
    Agent1$TimeE[StateE1] = Agent1$TimeE[StateE1] + 1
    StateE2 <- (1:nPop1)[Agent1$State=="E" & Agent1$TimeE > 14]
    Agent1$State[StateE2] <- "R"
    #grab those who could become sick then randomly assign if they get sick
    StateE3 <- (1:nPop1)[Agent1$State=="E" & Agent1$TimeE > 3]
    
    for (i in StateE3) {
      Urand1 <- runif(1,0,1)
      if( Urand1 < par1$E2I){
        Agent1$State[i] <- "I"
      }
    }
    #update how long they have been sick
    StateI1<-(1:nPop1)[Agent1$State == "I"]
    Agent1$TimeI[StateI1]=Agent1$TimeI[StateI1]+1
    StateI2 <- (1:nPop1)[Agent1$State =="I" & Agent1$TimeI > 14]
    Agent1$State[StateI2] <- "R"
    StateI3 <- (1:nPop1)[Agent1$State =="I" & Agent1$TimeI < 15]
    Agent1$State[StateI3] <- ifelse(
      runif( length(StateI3),0,1) > par1$I2D, "I", "D")

  out1$S[k]<-length(Agent1$State[Agent1$State=="S"])
  out1$E[k]<-length(Agent1$State[Agent1$State=="E"])
  out1$I[k]<-length(Agent1$State[Agent1$State=="I"])
  out1$R[k]<-length(Agent1$State[Agent1$State=="R"])
  out1$D[k]<-length(Agent1$State[Agent1$State=="D"])
  }
  return(out1)
}

#stuff to tinker with
Agent1<-AgentGen1(100, E0=5, I0=2)
par1 <- data.frame( MaxMix =10,
                    S2E = 0.25,
                    E2I = 0.1,
                    I2D = 0.05)
Model1<-ABM1(Agent1, par1, nTime1=15)
plot(1:15, Model1$S, type = "l", col = "purple",
     ylim=c(0,100))
lines(1:15, Model1$E, col="orange")
lines(1:15, Model1$I, col="red")
lines(1:15, Model1$R, col="seagreen")
lines(1:15, Model1$D, col="black")


