#osgf_to_en: CONVERT OS GRID REFERENCE TO EASTINGS AND NORTHINGS
#Finds centroids of British Ordinance Survey grid references and converts them to Eastings and Northings
#Accepts a vector of hectads and returns a data frame of eastings and northings 
#NOTE: Only works with 4 characto-digit grid references (e.g., SU96) 

osgf_to_en <- function(x){

hectads <- c("HP", "HT", "HU", "HW", "HX", "HY", "HZ", "NA", "NB", "NC", "ND", "NF", "NG", "NH", "NJ", "NK", "NL", "NM", "NN", "NO", "NR", "NS", "NT", "NU", "NW", "NX", "NY", "NZ", "OV", "SC", "SD", "SE", "SH", "SJ", "SK", "SM", "SN", "SO", "SP", "SR", "SS", "ST", "SU", "SV", "SW", "SX", "SY", "SZ", "TA", "TF", "TG", "TL", "TM", "TQ", "TR", "TV")
X <- c(4, 3, 4, 1, 2, 3, 4, 0, 1, 2, 3, 0, 1, 2, 3, 4, 0, 1, 2, 3, 1, 2, 3, 4, 1, 2, 3, 4, 5, 2, 3, 4, 2, 3, 4, 1, 2, 3, 4, 1, 2, 3, 4, 0, 1, 2, 3, 4, 5, 5, 6, 5, 6, 5, 6, 5)
Y <- c(12,11,11,10,10,10,10,9, 9, 9, 9, 8, 8, 8, 8, 8, 7, 7, 7, 7, 6, 6, 6, 6, 5, 5, 5, 5, 5, 4, 4, 4, 3, 3, 3, 2, 2, 2, 2, 1, 1, 1, 1, 0, 0, 0, 0, 0, 4, 3, 3, 2, 2, 1, 1, 0)

	eastings <- rep(NA, length(x))
	northings <- rep(NA, length(x))

	for(i in 1:length(x)){
		if(is.element(substr(x[i], start = 1, stop = 2),hectads)){
			y <- match(substr(x[i], start = 1, stop = 2), hectads)
			eastings[i] <- (X[y]*100000) + (as.numeric(substr(x[i], start = 3, stop = 3)) * 10000) + 5000
			northings[i] <- (Y[y]*100000) + (as.numeric(substr(x[i], start = 4, stop = 4)) * 10000) + 5000
		}		
		else{
			print(paste("'", x, "'", " is not a hectad", sep = ""))
		}
	}
	data.frame(hectad = x, eastings, northings)
}