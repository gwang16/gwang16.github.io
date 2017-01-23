
target_selection <- function(T){
  x<- sample(1:length(T),1,replace=TRUE)
  return(x)
}

damage_selection <- function(type){
  if (type=="bomb"){
  	dmg<-sample(1:4, 1, replace=TRUE)
  } 
  
  if (type=="missle"){
  	dmg<-1
  	}
  return(dmg)
  }
  
  hit_bomb <- function(board){
  		board<-T
  		removex<-T[x]
  		T=setdiff(T, removex) 					
  		dmg<-damage_selection("bomb")			 
  		x<- target_selection(T1)			 
  		if (T[x]== "A") {
  			DMGtoFace=DMGtoFace+dmg;
  			}
  
  		if (is.element(T1[x], c("C1","D1"))) {
  		hit_bomb(T);
  		}
  return(DMGtoFace)
}

