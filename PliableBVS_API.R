
library(plumber)
library(readr)
library(httr)
library(magrittr)
library(ggplot2)
library(gapminder)
library(aws.s3)
#* @apiTitle PliableBVS API
#* @apiDescription API for exploring PliableBVS package
#* Returns resuls and plots from PliableBVS
#* @param rds_file:file a ist containing the data, X,Z,y to passed to the Bpliable function. Always save as my_data.rds
#* @param family response type- either "gaussian", "binomial". In the binomial case, y should be 0s and 1s.
#* @param alpha:numeric  a numeric value between 0 and 1
#* @param niter:numeric a numeric value for the number of iterations
#* @param burnin:numeric a numeric value for the burnin
#* @param a_rho:numeric a numeric value for the a_rho
#* @param b_rho:numeric a numeric value for the b_rho
#* @param a_zeta:numeric a numeric value for the a_zeta
#* @param b_zeta:numeric a numeric value for the b_zeta
#* @param num_update:numeric a numeric value for number of iterations during the EM algorithm
#* @param niter_update:numeric a numeric value for the number of iterations in each num_update run during the EM algorithm
#* @param burnin_update:numeric a numeric value for the burnin in each num_update run during the EM algorithm
#* @param verbose1 a boolean value for verbose1
#* @param verbose2 a boolean value for verbose2
#* @param lam1:numeric a numeric value for lam1
#* @param lam2:numeric a numeric value for lam2
#* @param rho_prior a boolean value for rho_prior to be updated
#* @param rho:numeric a numeric value for rho
#* @param zeta:numeric a numeric value for zeta
#* @param c2:numeric a numeric value for c2
#* @param v2:numeric a numeric value for v2
#* @param update_tau a boolean value for tau to be updated
#* @param option_weight_group a boolean value for option.weight.group. Not yet implemented
#* @param option_update a string value for option.update. not yet implemented
#* @param lambda2_update:numeric a numeric value for lambda. It should be supplied if update_tau is False.
#* @parser multi
#* @parser rds
#* @post /fit
function(rds_file,
         family="gaussian",
         alpha=0.5,
         niter=500,
         burnin=200,
         a_rho=1,
         b_rho=1,
         a_zeta=1,
         b_zeta=1,
         num_update=20,
         niter_update=20,
         burnin_update=10,
         lam1=1e-1,
         lam2=1e-1,
         rho_prior="TRUE",
         rho=0.5,
         zeta=0.5,
         c2=10^2,
         v2=10^2,
         verbose1="TRUE",
         verbose2="TRUE",
         update_tau="TRUE",
         option_weight_group="TRUE",
         option_update="global",
         lambda2_update=0){
#function(rds_file, family, malpha, verbose1, verbose2){
  # Load the package
  library(PliableBVS)

  # Extract the data
  #print(data)
  X <- rds_file$my_data.rds$X
 # print(dim(X))
  Z <- rds_file$my_data.rds$Z
  #print(rds_file$my_data.rds$Z)
  y <- rds_file$my_data.rds$y
  y=matrix(y,ncol=1)
  # Call the Bpliable function
  #fit<- Bpliable(Y = y,
 #                X = X,
  #               Z = Z,
  #               family = family,
  #               alpha=as.numeric(malpha),
  #               verbose1 = verbose1,
  #               verbose2 = verbose2)
  #print(fit)

  fit<- PliableBVS(Y = y, X = X, Z = Z, alpha = as.numeric(alpha), family = family,niter = as.numeric(niter),burnin = as.numeric(burnin),a_rho = as.numeric(a_rho), b_rho = as.numeric(b_rho), a_zeta = as.numeric(a_zeta), b_zeta = as.numeric(b_zeta), num_update = as.numeric(num_update), niter.update = as.numeric(niter_update), burnin.update = as.numeric(burnin_update), verbose1 = verbose1, verbose2 = verbose2, lam1 = as.numeric(lam1), lam2 = as.numeric(lam2), rho_prior = rho_prior, rho = as.numeric(rho), zeta = as.numeric(zeta), c2 = as.numeric(c2), v2 = as.numeric(v2),update_tau = update_tau, option.weight.group = option_weight_group,option.update = option_update, lambda2_update = as.numeric(lambda2_update))

  #Bpliable(Y = y, X = X, Z = Z, alpha = as.numeric(malpha), family = family, niter = as.numeric(niter), burnin = as.numeric(burnin), a_rho = as.numeric(a_rho), b_rho = as.numeric(b_rho), a_zeta = as.numeric(a_zeta), b_zeta = as.numeric(b_zeta), num_update = as.numeric(num_update), niter.update = as.numeric(niter_update), burnin.update = as.numeric(burnin_update), verbose1 = verbose1, verbose2 = verbose2, lam1 = as.numeric(lam1), lam2 = as.numeric(lam2), rho_prior = rho_prior, rho = as.numeric(rho), zeta = as.numeric(zeta), c2 = as.numeric(c2), v2 = as.numeric(v2), update_tau = update_tau, option.weight.group = option_weight_group, option.update = option_update, lambda2_update = as.numeric(lambda2_update))

  dir.create("/data", showWarnings = FALSE, recursive = TRUE)  # Ensure /data exists
  save_path <- "/data/PliableBVS_call.rds"
  saveRDS(fit, save_path)
  print(file.exists(save_path)) # print the files in the data directory
  return(list(message = "Model saved successfully", path = "PliableBVS_call.rds"))
}


#* @get /get_results
#* @serializer contentType list(type="application/octet-stream")
function(req, res) {
  file_path <- "/data/PliableBVS_call.rds"



  if (file.exists(file_path)) {
  file_data <- readBin(file_path, "raw", file.info(file_path)$size)
  # Set the Content-Type header to 'application/rds' for RDS files
  res$setHeader("Content-Type", "application/rds")

  # Set the Content-Disposition header so that the file is downloaded with the correct name
  res$setHeader("Content-Disposition", paste("attachment; filename=", basename(file_path), sep = ""))

  # Serve the .rds file
  return(file_data)
  }else {
    # If the file does not exist, return a 404 error
    res$status <- 404
    return(list(error = "File not found"))
  }
  #raw_data <-file(file_path, "rb")# readBin(file_path, "raw", file.info(file_path)$size)
  #return(raw_data)
}

#* Returns a plots from the PliableBVS object
#* @param type a string value for the type of plot. It can be c("likelihood","dist","val","cont","ms")
#*@param rds_file:file the fitted PliableBVS object which is Bpliable_call
#*@param coef_val1:int a numeric value for the main coefficient position
#*@param coef_val2:int a numeric value for the interaction coefficient position. It should be zero if you want only main effect.
#* @parser multi
#* @parser rds
#*  @post /plot
#* @serializer contentType list(type="image/png")

function(req, res,rds_file=NULL,type="likelihood", coef_val1=1,coef_val2=1){
  # Load the package
  library(PliableBVS)
  library(graphics)
  library(plotly)



  # Default RDS file location
  default_rds <- "/data/PliableBVS_call.rds"

  # Use user-provided file_path or default file
  if (!is.null(rds_file) ) {

    rds_file=rds_file # Load user-provided file
  } else if (file.exists(default_rds)) {



    rds_file=readBin(default_rds, "raw", file.info(default_rds)$size)

      # Set the Content-Type header to 'application/rds' for RDS files
      res$setHeader("Content-Type", "application/rds")

      # Set the Content-Disposition header so that the file is downloaded with the correct name
      res$setHeader("Content-Disposition", paste("attachment; filename=", basename(default_rds), sep = ""))

      # Serve the .rds file


     # Load default file

  } else {
    res$status <- 400  # Bad Request
    print(list(error = "No valid RDS file found!"))
  }

 print(readRDS(rds_file))

  if(type=="likelihood"){
    # Define file path (use tempfile() if necessary)

  #print(rds_file$Bpliable_call.rds$coef[, as.numeric(coef_val1)])

  # Call the plot function

    file <-  "/tmp/plot.png"

    dir.create(dirname(file), showWarnings = TRUE, recursive = TRUE)

  print(paste("Saving plot to:", file))  # Debugging print

  # Open PNG graphics device with proper width/height
  png(file)

  # Try to generate the plot safely

    plot(log(rds_file$PliableBVS_call.rds$Likelihood),type = "l",ylab = "log(likelihood)",xlab = "iterations")
    #plot(type = type, x = rds_file$Bpliable_call.rds, coef_val = c(coef_val1))
    #print("Plot generated successfully!")  # Debugging message


  dev.off()
  # Check if file exists

   # Check if file exists before returning
   if (file.exists(file)) {




     return(readBin(file, "raw", n = file.info(file)$size))
   } else {
     stop("Error: Base R plot file was not created successfully.")
   }




  }else if(type=="ms"){
    # Define file path (use tempfile() if necessary)

    #print(rds_file$Bpliable_call.rds$coef[, as.numeric(coef_val1)])

    # Call the plot function

    model_size<-c()
    for (i in 1:nrow(rds_file$PliableBVS_call.rds$coef_beta_all)) {
      beta_nz<-sum(rds_file$PliableBVS_call.rds$coef_beta_all[i,]!=0)

      theta_nz<-sum(rds_file$PliableBVS_call.rds$coef_theta_all[i,,]!=0)
      model_size=c(model_size,sum(beta_nz,theta_nz))
    }

    file <-  "/tmp/plot.png"
    dir.create(dirname(file), showWarnings = TRUE, recursive = TRUE)

    print(paste("Saving plot to:", file))  # Debugging print

    # Open PNG graphics device with proper width/height
    png(file)

    # Try to generate the plot safely

      plot(model_size,type = "l",ylab = "Model size",xlab = "iterations")
      #plot(type = type, x = rds_file$Bpliable_call.rds, coef_val = c(coef_val1))


    dev.off()

    # Check if file exists before returning
    if (file.exists(file)) {
      return(readBin(file, "raw", n = file.info(file)$size))
    } else {
      stop("Error: Base R plot file was not created successfully.")
    }




  }else if(type=="val" &coef_val2 == 0){
    # Define file path (use tempfile() if necessary)

    #print(rds_file$Bpliable_call.rds$coef[, as.numeric(coef_val1)])

    # Call the plot function

    file <-  "/tmp/plot.png"
    dir.create(dirname(file), showWarnings = TRUE, recursive = TRUE)


    print(paste("Saving plot to:", file))  # Debugging print

    # Open PNG graphics device with proper width/height
    png(file)

    # Try to generate the plot safely

      plot(rds_file$PliableBVS_call.rds$pos_mpm_beta,main = "",xlab =  expression(beta) ,ylab = "coefficient")
      #plot(type = type, x = rds_file$Bpliable_call.rds, coef_val = c(coef_val1))


    dev.off()

    # Check if file exists before returning
    if (file.exists(file)) {
      return(readBin(file, "raw", n = file.info(file)$size))
    } else {
      stop("Error: Base R plot file was not created successfully.")
    }




  }else if(type=="val" &coef_val2 != 0){
    # Define file path (use tempfile() if necessary)

    #print(rds_file$Bpliable_call.rds$coef[, as.numeric(coef_val1)])

    # Call the plot function

    file <-  "/tmp/plot.png"
    dir.create(dirname(file), showWarnings = TRUE, recursive = TRUE)


    print(paste("Saving plot to:", file))  # Debugging print

    # Open PNG graphics device with proper width/height
    png(file)

    # Try to generate the plot safely

      plot(c(rds_file$PliableBVS_call.rds$pos_mpm_theta),main = "",xlab = expression(theta),ylab = "coefficient" )
      #plot(type = type, x = rds_file$Bpliable_call.rds, coef_val = c(coef_val1))


    dev.off()

    # Check if file exists before returning
    if (file.exists(file)) {
      return(readBin(file, "raw", n = file.info(file)$size))
    } else {
      stop("Error: Base R plot file was not created successfully.")
    }




  }else if (type=="dist" & coef_val2 == 0){
    coef_val1=as.numeric(coef_val1)


    # Create a ggplot2 version
    gg_hist <- ggplot(data.frame(x = rds_file$PliableBVS_call.rds$coef[,coef_val1]),
                      aes(x = x)) +
      geom_histogram(fill="#69b3a2") +
      labs(x =bquote(beta*"_"*.(coef_val1) )
, y='Frequency')+
      theme_gray()

    print(gg_hist)
    file <- "/tmp/plot.png"
    dir.create(file, showWarnings = FALSE, recursive = TRUE)  # Ensure /data exists

    ggsave(plot=gg_hist,filename=file,width = 5,       # Reduce width (in inches)
           height = 4,      # Reduce height (in inches)
           dpi = 100        # Lower DPI (Dots Per Inch) for smaller file size
    )
    readBin(file, "raw", n = file.info(file)$size)

  }else if (type=="dist" & coef_val2!= 0){

    coef_val1=as.numeric(coef_val1)
    coef_val2=as.numeric(coef_val2)

    # Create a ggplot2 version
    gg_hist <- ggplot(data.frame(x = rds_file$PliableBVS_call.rds$coef_theta[,coef_val1,coef_val2]),
                      aes(x = x)) +
      geom_histogram(fill="#69b3a2") +
      labs(x =bquote(theta*"_"*.(coef_val1)*"_"*.(coef_val2) )
           , y='Frequency')+
      theme_gray()

    print(gg_hist)
    file <- "/tmp/plot.png"
    dir.create(file, showWarnings = FALSE, recursive = TRUE)  # Ensure /data exists

    ggsave(plot=gg_hist,filename=file,width = 5,       # Reduce width (in inches)
           height = 4,      # Reduce height (in inches)
           dpi = 100        # Lower DPI (Dots Per Inch) for smaller file size
    )

    readBin(file, "raw", n = file.info(file)$size)


  }else if (type=="cont" ){

    coef_val1=as.numeric(coef_val1)
    coef_val2=as.numeric(coef_val2)

    # Create a ggplot2 version
    gg_hist <- ggplot(data.frame(x = rds_file$PliableBVS_call.rds$coef[,coef_val1],y=rds_file$PliableBVS_call.rds$coef_theta[,coef_val1,coef_val2]),
                      aes(x = x,y=y)) +


      geom_density_2d_filled()+
      geom_density_2d(colour = "black")+
      labs(y =bquote(theta*"_"*.(coef_val1)*"_"*.(coef_val2) )
           , x=bquote(beta*"_"*.(coef_val1) ))+
      theme_gray()

    print(gg_hist)
    file <- "/tmp/plot.png"
    dir.create(file, showWarnings = FALSE, recursive = TRUE)  # Ensure /data exists

    ggsave(plot=gg_hist,filename=file,width = 5,       # Reduce width (in inches)
           height = 4,      # Reduce height (in inches)
           dpi = 100        # Lower DPI (Dots Per Inch) for smaller file size
    )
    readBin(file, "raw", n = file.info(file)$size)


  }
 # plot(type = type, x = rds_file$Bpliable_call.rds, coef_val = coef_val)
}


#* Compute predicted values from a fitted PliableBVS  object
#* @param rds_file1:file the fitted PliableBVS object which is Bpliable_call
#* @param rds_file2:file a ist containing the data, X,Z to passed to the Bpliable function. Always save as my_data.rds
#* @param prob:numeric threshold for binomial prediction
#* @parser multi
#* @parser rds
#* @post /predict
 function(rds_file1,rds_file2,prob=0.5) {
  X <- rds_file2$my_data.rds$X
  # print(dim(X))
  Z <- rds_file2$my_data.rds$Z

  library(PliableBVS)



  pred_bpl<-model(beta0=rds_file1$PliableBVS_call.rds$pos_median_beta0, theta0=rds_file1$PliableBVS_call.rds$pos_median_theta0, beta= rds_file1$PliableBVS_call.rds$pos_mpm_beta, theta=rds_file1$PliableBVS_call.rds$pos_mpm_theta, X, Z)

  if(rds_file1$PliableBVS_call.rds$family=="gaussian"){
    pred_bpl<-pred_bpl}else if(rds_file1$PliableBVS_call.rds$family=="binomial"){
      pred_bpl<-ifelse(pred_bpl>prob,1,0)}

  return(pred_bpl)
}
