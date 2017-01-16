#what is the probability of dealing 3 damage to  the oppponent's face 
#when both of your boards consist of Dr. Boom, and you play arcane missles


#           A
#     C     B     D
#     C1    B1    D1
#           A1

#rm(list=ls())

s <- c()

for (trial in 1:10000){

	T <-c("A", "B", "C", "D") #Define opponent's board 
	T1 <-c("A1", "B1", "C1", "D1") #Define own board

	N = 3 #Number of missles
	M = 4 #Max bomb damage

	DMGtoFace <- 0

	for (i in 1:N){
		x <- target_selection(T)
		dmg <- damage_selection("missle")
		
		if (T[x]== "A") {
			DMGtoFace = DMGtoFace+1
			}

		if (is.element(T[x], c("C","D"))) {
			DMGtoFace = hit_bomb()
			}		
	}	
	s=c(s,DMGtoFace>=3)
}

length(which(s==TRUE))/length(s)




## adding fireblast

if (DMGtoFace<=1 && (is.element("C1",T1) || is.element("D1",T1))){
	if (is.element("C1",T1)){
		TargetFB<-"C1"
		} else{
		TargetFB<-"D1"
		}
	
	if (is.element(TargetFB, c("C1","D1"))) {
		removey<-TargetFB
		T1=setdiff(T1, removey) # remove bomb C1 or D1 after it's hit
		DMGxx<-sample(1:M, 1, replace=TRUE) #select damage of bomb C1 or D1
		xx<- sample(1:length(T),1,replace=TRUE) #select target of bomb C1 or D1
		if (T[xx]== "A") {
			DMGtoFace=DMGtoFace+DMGxx;
		}
			

		if (is.element(T[xx],c("C","D"))) {
			removexx<-T[xx]
			T=setdiff(T, removexx) # remove bomb C or D after it's hit
			DMGyy<-sample(1:M, 1, replace=TRUE) #select damage of bomb C or D
			yy<- sample(1:length(T1),1,replace=TRUE) #select target of bomb C or D
			
			if (is.element(T1[yy],c("C1","D1"))) {
				removeyy<-T1[yy]
				T1=setdiff(T1, removeyy) # remove bomb C1 or D1 after it's hit
				DMGxxx<-sample(1:M, 1, replace=TRUE) #select damage of bomb C1 or D1
				xxx<- sample(1:length(T),1,replace=TRUE) #select target of bomb C1 or D1
				if (T[xxx]== "A") {
					DMGtoFace=DMGtoFace+DMGxxx;
				}	
			}
		}
	}


} else {
DMGtoFace= DMGtoFace+1
}


##
