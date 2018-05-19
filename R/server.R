library(aws.s3)
library(EBImage)
library(jpeg)
library(methods);

process <- function(image_name, aws_bucket, vh_values_min, vh_values_max, he_values_min, he_values_max, 
                   hem_value, cws_values_min, cws_values_max) {

  Sys.setenv("AWS_ACCESS_KEY_ID" = "AKIAJF6PJGTH4ZS5Y72A",
             "AWS_SECRET_ACCESS_KEY" = "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",
             "AWS_DEFAULT_REGION" = "ap-south-1");

  save_object(object = image_name, bucket = aws_bucket, file = image_name);
  
  Input_MyImage <- readImage(paste(getwd(), "/", image_name, sep = ""));

  set.seed(42);
  # For NVD and NVE, looks like no smoothing 
  # is required, use green channel of the Input image
  BestFrame_MyImage <- getFrame(Input_MyImage,2,type='total');
  
  flagD_R <- 0;
  flagU_E <- 0;
  flagO_E <- 0;
  flagVASC_QUAL <- 0;
  flag_NV <- 0;
  
  # Convert Color image into Grayscale image
  colorMode(BestFrame_MyImage)<-Grayscale;
  dimensions_Inputimage=dim(BestFrame_MyImage);
  dimensions_Inputimage;
  BestFrame_resized=resize(BestFrame_MyImage,w = 720,h = 576);
  BestFrame_copy<-BestFrame_resized;
  dimensions_resized=dim(BestFrame_resized);
  Input_MyImage_resized=resize(Input_MyImage,w = 720,h = 576);
  report_Input_MyImage_resized <- Input_MyImage_resized;
  
  cat("finished successfully","\n");
  
  set.seed(42);
  BestFrame_copy=BestFrame_resized;
  
  ############ WhiteTop Hat transformation for edge detection ###############
  
  ### Invert the image #####
  Inverted_BestFrame=max(BestFrame_resized)-BestFrame_resized;
  #display(contrast_adjusted_BestFrame,method="raster");
  contrast_adjusted_BestFrame=Inverted_BestFrame*1.2;
  
  # Original code
  scanbot_region=round((dimensions_resized[1]/50)*(dimensions_Inputimage[1]/dimensions_resized[1]),digits = 0);
  if(scanbot_region%%2==0)
  {
    scanbot_region<-scanbot_region+1;
  }
  kern=makeBrush((scanbot_region),shape="disc");
  
  TopHat_transformed_Image=whiteTopHat(contrast_adjusted_BestFrame, kern);
  
  # Devise majority vote of pixel intensity based, normalization-segmentation method #
  
  # Devise a 80X80 pixel bot with 0s
  # Scan from the beginning of the image and traverse row wise until the end of the row,
  # check for majority pixel intensities and set all pixels scanned in the 80X80 scan region to the majority pixel intensities
  
  normalized_TopHat_transformed_Image<-normalize(TopHat_transformed_Image);
  
  Inverted_TopHat_Image=max(normalized_TopHat_transformed_Image)-normalized_TopHat_transformed_Image;
  
  ########## kmeans algorithm #################
  ### First determine, how many 'k' clusters have to be seeked
  
  pixelintensities_TopHatoutput<-vector();
  pixelintensities_TopHatoutput=sort(unique(c(Inverted_TopHat_Image)));
  
  ### Calculate absolute difference in pixel intensities from voting algorithm result
  ### Segregate them based on abs.diff of pixel intensities, and see, how many groups have similar/identical abs.diffs in pixel intensities
  
  absolute_diff_pixelintensity<-vector(); 
  eachscan=1;
  while (eachscan+1<=length(pixelintensities_TopHatoutput))
  {
    absolute_diff_pixelintensity[eachscan]=abs(pixelintensities_TopHatoutput[eachscan]-pixelintensities_TopHatoutput[eachscan+1]);
    eachscan=eachscan+1;
  }
  
  #no_of_kmeans_clusters_tobedetermined=length(unique(c(absolute_diff_pixelintensity)));
  #IMP. Testing with 50 initial clusters. Do check the vasculature quality
  no_of_kmeans_clusters_tobedetermined <- 50;
  
  ### Now, apply kmeans on normalized_BestFrame_resized with predetermined cluster no. in above step
  DEFAULT_KMEANS_ITERATION <- 20000;
  DEFAULT_NUM_OF_CLUSTERS <- 2;
  iterations_count<-10000;
  kmeans_start_time <- Sys.time();
  kmeans_failed <- FALSE;
  Inverted_TopHat_Image_kmeans=kmeans(pixelintensities_TopHatoutput,no_of_kmeans_clusters_tobedetermined,iterations_count);
  no_of_kmeans_clusters_tobedetermined<-no_of_kmeans_clusters_tobedetermined/2;
  while(Inverted_TopHat_Image_kmeans$ifault==4 | Inverted_TopHat_Image_kmeans$ifault==2)
  {
    kmeans_process_time <- Sys.time();
    kmeans_total_time <- round(difftime(time1 = kmeans_process_time, time2 = kmeans_start_time, units = "secs"));
    if (kmeans_total_time > 600) {
      kmeans_failed <- TRUE;
      break;
    }
    if(Inverted_TopHat_Image_kmeans$ifault==4)
    {
      set.seed(42);
      Inverted_TopHat_Image_kmeans=kmeans(x = pixelintensities_TopHatoutput,centers = (no_of_kmeans_clusters_tobedetermined),iter.max = 10000,nstart = no_of_kmeans_clusters_tobedetermined-1);
      no_of_kmeans_clusters_tobedetermined<-(no_of_kmeans_clusters_tobedetermined-1);
      if (no_of_kmeans_clusters_tobedetermined == 1) {
        break;
      }
    } else if(Inverted_TopHat_Image_kmeans$ifault==2) # To increase number of iterations
    {
      iterations_count<-(iterations_count+1000);
      if (iterations_count > 50000) {
        break;
      }
      set.seed(42);
      Inverted_TopHat_Image_kmeans=kmeans(x = pixelintensities_TopHatoutput,centers = (no_of_kmeans_clusters_tobedetermined),iter.max = iterations_count,nstart = no_of_kmeans_clusters_tobedetermined-1);
      
    }
  }
  
  if (kmeans_failed) {
    set.seed(42);
    Inverted_TopHat_Image_kmeans=kmeans(pixelintensities_TopHatoutput, DEFAULT_NUM_OF_CLUSTERS, DEFAULT_KMEANS_ITERATION);
  }
  
  vasculature_extracted_normalized_TopHat=(normalized_TopHat_transformed_Image>=min(Inverted_TopHat_Image_kmeans$centers));
  
  labeled_vasculature_extracted_normalized_TopHat=bwlabel(vasculature_extracted_normalized_TopHat);
  
  #max(labeled_vasculature_extracted_normalized_TopHat);
  Features_moments_labeled_normalized_TopHat=computeFeatures.moment(labeled_vasculature_extracted_normalized_TopHat);
  
  ### Eccentricity 0 is circular ##
  label_indices_circular<-vector();
  label_indices_circular=which(Features_moments_labeled_normalized_TopHat[,'m.eccentricity']==0,arr.ind=TRUE);
  ### Eccentricity 1 is linear ##
  label_indices_linear<-vector();
  label_indices_linear=which(Features_moments_labeled_normalized_TopHat[,'m.eccentricity']==1,arr.ind=TRUE);
  
  ### low area labels to be removed
  Features_shape_labeled_normalized_TopHat=computeFeatures.shape(labeled_vasculature_extracted_normalized_TopHat);
  if(length(Features_shape_labeled_normalized_TopHat[,'s.area'])>1)
  {
    set.seed(42);
    area_kmeans_vasculature=kmeans(Features_shape_labeled_normalized_TopHat[,'s.area'],2);
    labels_indices_minutearea<-vector();
    labels_indices_minutearea=which(Features_shape_labeled_normalized_TopHat[,'s.area']<=min(area_kmeans_vasculature$centers),arr.ind = TRUE);
    
  } else{
    area_kmeans_vasculature=Features_shape_labeled_normalized_TopHat[,'s.area'];
    labels_indices_minutearea<-vector();
    labels_indices_minutearea=area_kmeans_vasculature;
  }
  
  label_indices_toberemoved<-vector();
  label_indices_toberemoved=c(label_indices_circular,label_indices_linear,labels_indices_minutearea);
  final_vasculature_extracted=array(0,dim=c(dimensions_resized[1],dimensions_resized[2]));
  final_vasculature_extracted=rmObjects(labeled_vasculature_extracted_normalized_TopHat,label_indices_toberemoved);
  
  ##### narrow down to main vessels, OD region #######
  ### Step 1: get labeled image of vessels extracted after removing linear and circular and lowest area artefacts ###
  ### Step 2: remove frame around the fundus image
  ### Step 3: 'Cut', vasculature into bits to dilineate required vasculature with vertical orientation ###
  ### Step 4: implement computeFeatures to get orientation in angle in radians ####
  ### Step 5: select, linear oriented vessels from step 2 ####
  ### Step 6: From Step 3 candidates, see, which ones have bright pixel intensity regions to the 
  ####### left or right, filter only them #######
  ### Step 5: From step 4 result, if probable candidates is more than 1, identify which has more
  ####### no. of branches
  
  ## Step 1:
  labeled_final_vasculature_extracted=bwlabel(final_vasculature_extracted);
  labeled_final_vasculature_extracted_copy=labeled_final_vasculature_extracted;
  
  ## Step2:
  
  row_start=1;
  col_start=1;
  probable_counter=1;
  density_pixel<-vector();
  row_startindex_density_pixel<-vector();
  col_startindex_density_pixel<-vector();
  row_endindex_density_pixel<-vector();
  col_endindex_density_pixel<-vector();
  
  ### First, scan for few rounds about 5 iterations, record bot intensity in
  ### Inverted_TopHat_Image
  
  scanbot_region=round(dimensions_resized[1]/50,digits = 0);
  # Devise a 80X80 pixel bot with 0s
  # Scan from the beginning of the image and traverse row wise until the end of the row,
  # check for majority pixel intensities and set all pixels scanned in the 80X80 scan region to the majority pixel intensities
  row_start=1;
  col_start=1;
  scan_iter=1;
  while(row_start<dimensions_resized[1] & ((row_start-1)+scanbot_region)<dimensions_resized[1]) 
  {
    col_start=1;
    while(col_start<dimensions_resized[2] & ((col_start-1)+scanbot_region)<dimensions_resized[2]) 
    {
      majoritypixels_index <-vector();
      pixelintensitiesscan_BestFrame=labeled_final_vasculature_extracted[row_start:((row_start-1)+scanbot_region),col_start:((col_start-1)+scanbot_region)];
      unique_pixelintensities=unique(c(pixelintensitiesscan_BestFrame));
      if(length(unique_pixelintensities)==1 && unique_pixelintensities==1)
      {
        labeled_final_vasculature_extracted[row_start:((row_start-1)+scanbot_region),col_start:((col_start-1)+scanbot_region)]=0;
      }
      col_start=col_start+scanbot_region;
      
    }
    
    row_start=row_start+scanbot_region;
  }
  removedframe_labeled_final_vasculature_extracted=labeled_final_vasculature_extracted;
  background_indices=which(removedframe_labeled_final_vasculature_extracted==0,arr.ind=TRUE);
  normalized_TopHat_transformed_Image[background_indices]=0;
  
  ### select a white label from removedframe
  ### scan for white pixels top, bottom, left and right, left diagnol, right diagnol, bottom diagnol and top diagnol
  ### 
  
  row_start=1;
  col_start=1;
  
  while(row_start<dimensions_resized[1] & ((row_start-1)+scanbot_region)<dimensions_resized[1]) 
  {
    col_start=1;
    while(col_start<dimensions_resized[2] & ((col_start-1)+scanbot_region)<dimensions_resized[2]) 
    {
      majoritypixels_index <-vector();
      pixelintensitiesscan_BestFrame=normalized_TopHat_transformed_Image[row_start:((row_start-1)+scanbot_region),col_start:((col_start-1)+scanbot_region)];
      unique_pixelintensities=unique(c(pixelintensitiesscan_BestFrame));
      if(length(unique_pixelintensities)==1)
      {
        normalized_TopHat_transformed_Image[row_start:((row_start-1)+scanbot_region),col_start:((col_start-1)+scanbot_region)]=unique_pixelintensities;
      }
      else if(length(unique_pixelintensities)>1)
      {
        noofindices_withuniquepixelintensity<-vector();
        lengthof_unique_pixelintensities=length(unique_pixelintensities);
        for (eachuniquepixelintensity in 1:lengthof_unique_pixelintensities)
        {
          noofindices_withuniquepixelintensity[eachuniquepixelintensity]=length(which(pixelintensitiesscan_BestFrame==unique_pixelintensities[eachuniquepixelintensity],arr.ind=TRUE));
          
        }
        majoritypixels_index=which(noofindices_withuniquepixelintensity==max(noofindices_withuniquepixelintensity),arr.ind=TRUE);
        
        if(length(majoritypixels_index)>1) # If majority pixels are more than 1 group with different pixel intensities
        {
          normalized_TopHat_transformed_Image[row_start:((row_start-1)+scanbot_region),col_start:((col_start-1)+scanbot_region)]=mean(unique_pixelintensities[majoritypixels_index]);
          
        }
        else
        {
          normalized_TopHat_transformed_Image[row_start:((row_start-1)+scanbot_region),col_start:((col_start-1)+scanbot_region)]=unique_pixelintensities[majoritypixels_index];  
        }
      }
      col_start=col_start+scanbot_region;
    }
    
    row_start=row_start+scanbot_region;
    
  }
  
  frame_border_indices=which(normalized_TopHat_transformed_Image!=0,arr.ind=TRUE);
  # Exclusively for Macula detection script, in a different file
  
  true_frame_border_indices=which(normalized_TopHat_transformed_Image!=0,arr.ind=TRUE);
  
  removedframe_labeled_final_vasculature_extracted[frame_border_indices]=0;
  labeled_removedframe=bwlabel(removedframe_labeled_final_vasculature_extracted);
  
  ### BestFrame_resized can be used for final OD detection ####
  BestFrame_resized[frame_border_indices]=0;
  
  frameborderindices=which(BestFrame_resized==0,arr.ind=TRUE);
  
  features_shape_labeled_removedframe=computeFeatures.shape(labeled_removedframe);
  label_largestarea=which(features_shape_labeled_removedframe[,'s.area']==max(features_shape_labeled_removedframe[,'s.area']),arr.ind = TRUE);
  
  ######### Successfully removed out of frame area in a fundus image ###########
  
  main_vasculature_labeled_removedframe=(labeled_removedframe==label_largestarea);
  
  mainvessel_indices=which(main_vasculature_labeled_removedframe==1,arr.ind=TRUE);
  otherthan_mainvessel_indices=which(main_vasculature_labeled_removedframe==0,arr.ind=TRUE);
  ##### Compute vasculature quality ####
  BestFrame_copy=BestFrame_resized;
  total_areaof_image=(pi*dimensions_resized[1]*dimensions_resized[2]);
  foreground_indices_vasculature=which(removedframe_labeled_final_vasculature_extracted==1,arr.ind = TRUE);
  normalized_vasculature_imagearea=(pi*abs(max(foreground_indices_vasculature[,1])-min(foreground_indices_vasculature[,1]))*abs(max(foreground_indices_vasculature[,2])-min(foreground_indices_vasculature[,2])))/total_areaof_image;
  
  density_vasculature_imagearea=(length(foreground_indices_vasculature)/total_areaof_image)*100;
  
  flagD_R=0;
  flagU_E=0;
  flagO_E=0;
  flagRun_Status = 0;   #<=10.5 ||( >=20 && <=21)
  if( (density_vasculature_imagearea<=50&&density_vasculature_imagearea>=40))
  {
    flagD_R=1;
    flagU_E=0;
  }else if(density_vasculature_imagearea<=6.5){
    flagD_R=0;
    flagU_E=1;
  }
  
  if(flagU_E==1 && flagD_R==0)
  {
    cat("Please retake picture, Underexposed image","\n");  
  }else if(flagU_E==0 && flagD_R==1)
  {
    flagRun_Status <- 1;
    cat("probable boat hemorrhages, please validate with doctor","\n");
  }else if(flagD_R == 0 && flagU_E == 0)
  {
    cat("No boat hemorrhages found, no underexposed areas in image","\n");
    cat("finished successfully","\n");
  }
  
  # Make max.numof_slices horizontal slices of BestFrame_resized fundus image
  if(flagD_R==0)
  {
    set.seed(42);
    startx=1;
    starty=1;
    numof_slices=5;
    slice_startx<-array(data=0,dim = c(numof_slices,numof_slices));
    slice_starty<-array(data=0,dim = c(numof_slices,numof_slices));
    slice_endx<-array(data=0,dim = c(numof_slices,numof_slices));
    slice_endy<-array(data=0,dim = c(numof_slices,numof_slices));
    max_intensity_eachslice<-array(data=0,dim = c(numof_slices,numof_slices));
    
    for(eachxslice in 1:numof_slices)
    {
      for (eachyslice in 1:numof_slices)
      {
        max_intensity_eachslice[eachxslice,eachyslice]=max(BestFrame_resized[startx:(startx+(dimensions_resized[1]/numof_slices)-1),starty:(starty+(dimensions_resized[2]/numof_slices)-1)]);
        slice_startx[eachxslice,eachyslice]=startx;
        slice_starty[eachxslice,eachyslice]=starty;
        slice_endx[eachxslice,eachyslice]=(startx+(dimensions_resized[1]/numof_slices)-1);
        slice_endy[eachxslice,eachyslice]=(starty+(dimensions_resized[2]/numof_slices)-1);  
        starty=eachyslice*numof_slices+1;
        if(starty>=dimensions_resized[2])
        {
          break();
        }
      }
      starty=1;
      startx=eachxslice*numof_slices+1;
      if(startx>=dimensions_resized[1])
      {
        break();
      }
      
    }
    
    max_diff=array(data=1000,dim = c(length(max_intensity_eachslice),length(max_intensity_eachslice)));
    eachslice=1;
    while(eachslice<=((dim(max_intensity_eachslice)[1])-1))
    {
      eachnextslice=eachslice+1;
      while(eachnextslice<=(dim(max_intensity_eachslice)[2]))
      {
        max_diff[eachslice,eachnextslice]= abs(max_intensity_eachslice[eachslice,eachslice]-max_intensity_eachslice[eachnextslice]);
        eachnextslice=eachnextslice+1;
      }
      eachslice=eachslice+1; 
    }
    
    kmeans_maxdiff=kmeans(c(max_diff),round(length(unique(c(max_diff)))/2),iter.max = (length(max_intensity_eachslice)+10),nstart = (length(max_intensity_eachslice)/4));
    refindices_maxdiff=which(max_diff<=kmeans_maxdiff$centers[which(min(kmeans_maxdiff$centers)==kmeans_maxdiff$centers)],arr.ind = TRUE);
    
    startx_uniquevalues<-vector();
    startx_uniquevalues=unique(slice_startx[refindices_maxdiff[,1]]);
    startx_nonzerouniquevalues<-vector();
    startx_nonzerouniquevalues=startx_uniquevalues[startx_uniquevalues!=0];
    
    endx_uniquevalues<-vector();
    endx_uniquevalues=unique(slice_endx[refindices_maxdiff[,1]]);
    endx_nonzerouniquevalues<-vector();
    endx_nonzerouniquevalues=endx_uniquevalues[endx_uniquevalues!=0];
    
    starty_uniquevalues<-vector();
    starty_uniquevalues=unique(slice_starty[refindices_maxdiff[,2]]);
    starty_nonzerouniquevalues<-vector();
    starty_nonzerouniquevalues=starty_uniquevalues[starty_uniquevalues!=0];
    
    endy_uniquevalues<-vector();
    endy_uniquevalues=unique(slice_endy[refindices_maxdiff[,2]]);
    endy_nonzerouniquevalues<-vector();
    endy_nonzerouniquevalues=endy_uniquevalues[endy_uniquevalues!=0];
    
    flagO_E=0;
    if((max(endy_nonzerouniquevalues)-min(endy_nonzerouniquevalues)>=3*(dimensions_Inputimage[2]/dimensions_resized[2])*length(max_intensity_eachslice)))
    {
      flag_overexposed=0; 
    }else if(length(unique(endy_nonzerouniquevalues))==1 & length(startx_nonzerouniquevalues)>1)
    {
      flag_overexposed=1;
      flagO_E=1;
    }else if((max(endx_nonzerouniquevalues)-min(endx_nonzerouniquevalues)>3*(dimensions_Inputimage[1]/dimensions_resized[1])*length(max_intensity_eachslice)))
    {
      flag_overexposed=0;
      
    }else if(length(unique(endx_nonzerouniquevalues))==1 & length(starty_nonzerouniquevalues)>1)
    {
      flag_overexposed=1;
      flagO_E=1;
    }else if((max(starty_nonzerouniquevalues)-min(starty_nonzerouniquevalues)>3*(dimensions_Inputimage[2]/dimensions_resized[2])*length(max_intensity_eachslice)))
    {
      flag_overexposed=0;
      
    }else if(length(unique(starty_nonzerouniquevalues))==1 & length(unique(startx_nonzerouniquevalues))==1)
    {
      flag_overexposed=0;
      
    }else if((max(startx_nonzerouniquevalues)-min(startx_nonzerouniquevalues)>3*(dimensions_Inputimage[1]/dimensions_resized[1])*length(max_intensity_eachslice)))
    {
      flag_overexposed=0;
    }else if((max(endx_nonzerouniquevalues)-min(startx_nonzerouniquevalues))>3*(dimensions_Inputimage[1]/dimensions_resized[1])*length(max_intensity_eachslice))
    {
      flag_overexposed=0;
    }else if((max(endy_nonzerouniquevalues)-min(starty_nonzerouniquevalues))>3*(dimensions_Inputimage[2]/dimensions_resized[2])*length(max_intensity_eachslice))
    {
      flag_overexposed=0;
    }
    
    if(flagO_E==1)
    {
      cat("probable overexposed regions","\n");
      
    } else
    {
      cat("No overexposed areas found","\n");
      flagO_E=0;
    }
  }
  
  cat("finished successfully","\n");
  
  if(flagD_R==0)
  {
    set.seed(42);
    
    rows_indices_mainvessel=mainvessel_indices[,1];
    cols_indices_mainvessel=mainvessel_indices[,2];
    
    rectangular_scanbot_region=scanbot_region*8;
    backup_main_vasculature=main_vasculature_labeled_removedframe;
    
    row_start=1;
    col_start=1;
    scan_counter=1;
    row_index<-vector();
    col_index<-vector();
    density_whitepixels<-vector();
    numofconnections_circularbot<-vector();
    ### Calculate how many lines are merging into circle 
    ### circle with most number of mergers is most likely OD
    
    circumference_xcoords<-vector();
    circumference_ycoords<-vector();
    angle_values<-vector();
    noof_angles=200;
    
    angle_values=seq(0,2*pi,length.out=noof_angles);
    
    while(row_start<dimensions_resized[1] & ((row_start-1)+rectangular_scanbot_region)<dimensions_resized[1]) 
    {
      col_start=1;
      while(col_start<dimensions_resized[2] & ((col_start-1)+rectangular_scanbot_region)<dimensions_resized[2]) 
      {
        withrect_backup=main_vasculature_labeled_removedframe[row_start:((row_start-1)+rectangular_scanbot_region),col_start:((col_start-1)+rectangular_scanbot_region)];
        curr_indices=which(withrect_backup==1,arr.ind=TRUE);
        whiteindices_number=length(which(withrect_backup==1,arr.ind=TRUE));
        remove(withrect_backup);
        density_whitepixels[scan_counter]=whiteindices_number/rectangular_scanbot_region;
        
        circumference_xcoords = round(row_start + (rectangular_scanbot_region * cos(angle_values)),digits = 0);
        circumference_ycoords = round(col_start + (rectangular_scanbot_region * sin(angle_values)),digits = 0);
        xcoordstoberemoved<-vector();
        xcoordstoberemoved=which(circumference_xcoords<1,arr.ind=TRUE);
        ycoordstoberemoved<-vector();
        ycoordstoberemoved=which(circumference_ycoords<1,arr.ind=TRUE);
        coordstoberemovedlist=union(ycoordstoberemoved,xcoordstoberemoved);
        coordstoworkon=setdiff(c(1:noof_angles),coordstoberemovedlist);
        
        circumference_xcoords=circumference_xcoords[coordstoworkon];
        circumference_ycoords=circumference_ycoords[coordstoworkon];
        
        numof_conns=0;
        for (eachcoordoncircumference in 1:length(circumference_xcoords))
        {
          
          if(circumference_xcoords[eachcoordoncircumference]-1<1)
          {
            pixel_value_top=0;
            pixel_value_topright=0;
          }
          else
          {
            # Top
            pixel_value_top=main_vasculature_labeled_removedframe[circumference_xcoords[eachcoordoncircumference]-1,circumference_ycoords[eachcoordoncircumference]];
            #Top right diagnol
            pixel_value_topright=main_vasculature_labeled_removedframe[circumference_xcoords[eachcoordoncircumference]-1,circumference_ycoords[eachcoordoncircumference]+1];
          }
          
          
          if(circumference_xcoords[eachcoordoncircumference]+1<1)
          {
            pixel_value_bottom=0;
            pixel_value_bottomright=0;
          }
          else
          {
            # Bottom
            pixel_value_bottom=main_vasculature_labeled_removedframe[circumference_xcoords[eachcoordoncircumference]+1,circumference_ycoords[eachcoordoncircumference]];
            #Bottom right diagnol
            pixel_value_bottomright=main_vasculature_labeled_removedframe[circumference_xcoords[eachcoordoncircumference]+1,circumference_ycoords[eachcoordoncircumference]+1];
          }
          # Left
          if((circumference_ycoords[eachcoordoncircumference]-1)<1 || circumference_xcoords[eachcoordoncircumference]-1<1)
          {
            pixel_value_left=0;
            pixel_value_bottomleft=0;
            pixel_value_topleft=0;
          }
          else
          {
            pixel_value_left=main_vasculature_labeled_removedframe[circumference_xcoords[eachcoordoncircumference],circumference_ycoords[eachcoordoncircumference]-1];
            #Bottom left diagnol
            pixel_value_bottomleft=main_vasculature_labeled_removedframe[circumference_xcoords[eachcoordoncircumference]+1,circumference_ycoords[eachcoordoncircumference]-1];
            #Top left diagnol
            pixel_value_topleft=main_vasculature_labeled_removedframe[circumference_xcoords[eachcoordoncircumference]-1,circumference_ycoords[eachcoordoncircumference]-1];
            
          }
          
          ### Bottomleft
          
          if((circumference_ycoords[eachcoordoncircumference]-1)<1 || circumference_xcoords[eachcoordoncircumference]+1<1)
          {
            pixel_value_bottomleft=0;
          }
          else
          {
            #Bottom left diagnol
            pixel_value_bottomleft=main_vasculature_labeled_removedframe[circumference_xcoords[eachcoordoncircumference]+1,circumference_ycoords[eachcoordoncircumference]-1];
          }
          ### End Bottomleft
          
          ### Topleft
          if((circumference_ycoords[eachcoordoncircumference]-1)<1 || circumference_xcoords[eachcoordoncircumference]-1<1)
          {
            pixel_value_topleft=0;
          }
          else
          {
            #Top left diagnol
            pixel_value_topleft=main_vasculature_labeled_removedframe[circumference_xcoords[eachcoordoncircumference]-1,circumference_ycoords[eachcoordoncircumference]-1];
          }
          
          if(circumference_xcoords[eachcoordoncircumference]<1)
          {
            pixel_value_right==0;
          }
          else
          {
            # Right
            pixel_value_right=main_vasculature_labeled_removedframe[circumference_xcoords[eachcoordoncircumference],circumference_ycoords[eachcoordoncircumference]+1];
          }
          
          if (pixel_value_top==1){
            numof_conns=numof_conns+1;
          }else if (pixel_value_bottom==1){
            numof_conns=numof_conns+1;
          }else if (pixel_value_left==1){
            numof_conns=numof_conns+1;
          }else if (pixel_value_right==1){
            numof_conns=numof_conns+1;
          }else if (pixel_value_topleft==1){
            numof_conns=numof_conns+1;
          }else if (pixel_value_topright==1){
            numof_conns=numof_conns+1;
          }else if (pixel_value_bottomleft==1){
            numof_conns=numof_conns+1;
          }else if (pixel_value_bottomright==1){
            numof_conns=numof_conns+1;
          } else
            numof_conns=numof_conns+0;
          
        } # End for
        
        numofconnections_circularbot[scan_counter]=numof_conns;
        row_index[scan_counter]=row_start;
        col_index[scan_counter]=col_start;
        scan_counter=scan_counter+1;
        col_start=col_start+rectangular_scanbot_region;
      }
      
      row_start=row_start+rectangular_scanbot_region;
      
    }
    
    #### Union of "INDICES" density_whitepixels < threshold AND numberofconnections greater than threshold #########
    
    flagVASC_QUAL <- 0;
    if (length(unique(density_whitepixels)) > 2) {
      set.seed(42);
      density_whitepixelskmeans=kmeans(density_whitepixels,2);
    } else {
      #Check for this flag for the subsequent execution, in this script and the subsequent scripts
      cat("Please retake picture, Underexposed image","\n");
      flagVASC_QUAL <- 1;
    }
    
    if (flagVASC_QUAL == 0) {
      indices_greaterthan_density_threshold=which(density_whitepixels>min(density_whitepixelskmeans$centers),arr.ind=TRUE);
      indices_density_equals_zero=which(density_whitepixels==0,arr.ind=TRUE);
      indices_toberemoved_basedondensityremovalcriteria=union(indices_greaterthan_density_threshold,indices_density_equals_zero);
      indices_tobeconsidered_basedondensity=setdiff(1:length(density_whitepixels),indices_toberemoved_basedondensityremovalcriteria);
      
      ######### NEED TO REVIEW THIS HOW KMEANS WORKS NUMOFCONNECTIONS!!!! ####
      
      if (length(unique(numofconnections_circularbot)) > 2) {
        set.seed(42);
        numofconnections_circularbotkmeans=kmeans(numofconnections_circularbot,2,length(numofconnections_circularbot)/3,5);
      } else {
        #Check for this flag for the subsequent execution, in this script and the subsequent scripts
        cat("probable Macular Degeneration case","\n");
        flagVASC_QUAL <- 1;
      }
      
      if (flagVASC_QUAL == 0) {
        indices_greater_conn_threshold=which(numofconnections_circularbot>max(numofconnections_circularbotkmeans$centers),arr.ind=TRUE);
        indices_conn_equalszero=which(numofconnections_circularbot==0,arr.ind=TRUE);
        indices_toberemoved_basedonconnremovalcriteria=union(indices_greater_conn_threshold,indices_conn_equalszero);
        indices_tobeconsidered_basedonconn=setdiff(1:length(numofconnections_circularbot),indices_toberemoved_basedonconnremovalcriteria);
        
        indicestobe_processed_basedonmeanintensity=union(indices_tobeconsidered_basedondensity,indices_tobeconsidered_basedonconn);
        
        
        ############ Compute mean intensity #############
        max_intensity<-vector();
        mean_intensity<-vector();
        sd_intensity<-vector();
        length_whitepixels<-vector();
        length_blackpixels<-vector();
        
        start_x_coord_centroid<-vector();
        start_y_coord_centroid<-vector();
        
        end_x_coord_centroid<-vector();
        end_y_coord_centroid<-vector();
        col_difference<-vector();
        row_difference<-vector();
        
        for (eachindex in 1:length(indicestobe_processed_basedonmeanintensity))
        {
          row_start=row_index[indicestobe_processed_basedonmeanintensity[eachindex]];
          col_start=col_index[indicestobe_processed_basedonmeanintensity[eachindex]];
          
          circumference_xcoords = round(row_start + (rectangular_scanbot_region * cos(angle_values)),digits = 0);
          circumference_ycoords = round(col_start + (rectangular_scanbot_region * sin(angle_values)),digits = 0);
          xcoordstoberemoved<-vector();
          xcoordstoberemoved=which(circumference_xcoords<1,arr.ind=TRUE);
          ycoordstoberemoved<-vector();
          ycoordstoberemoved=which(circumference_ycoords<1,arr.ind=TRUE);
          coordstoberemovedlist=union(ycoordstoberemoved,xcoordstoberemoved);
          coordstoworkon=setdiff(c(1:noof_angles),coordstoberemovedlist);
          
          circumference_xcoords=circumference_xcoords[coordstoworkon];
          circumference_ycoords=circumference_ycoords[coordstoworkon];
          
          image_fordensitycomputation=main_vasculature_labeled_removedframe[min(circumference_xcoords):max(circumference_xcoords),min(circumference_ycoords):max(circumference_ycoords)];
          image_forODsegmentation=BestFrame_copy[min(circumference_xcoords):max(circumference_xcoords),min(circumference_ycoords):max(circumference_ycoords)];
          length_whitepixels[eachindex]=length(which(image_fordensitycomputation==1,arr.ind=TRUE));
          length_blackpixels[eachindex]=length(which(image_fordensitycomputation==0,arr.ind=TRUE));
          max_intensity[eachindex]=max(c(BestFrame_copy[min(circumference_xcoords):max(circumference_xcoords),min(circumference_ycoords):max(circumference_ycoords)]));
          mean_intensity[eachindex]=mean(c(BestFrame_copy[min(circumference_xcoords):max(circumference_xcoords),min(circumference_ycoords):max(circumference_ycoords)]));
          sd_intensity[eachindex]=sd(c(BestFrame_copy[min(circumference_xcoords):max(circumference_xcoords),min(circumference_ycoords):max(circumference_ycoords)]));
          col_difference[eachindex]=max(circumference_ycoords)-min(circumference_ycoords);
          row_difference[eachindex]=max(circumference_xcoords)-min(circumference_xcoords);
          
          start_x_coord_centroid[eachindex]=min(circumference_xcoords);
          end_x_coord_centroid[eachindex]=max(circumference_xcoords);
          
          start_y_coord_centroid[eachindex]=min(circumference_ycoords);
          end_y_coord_centroid[eachindex]=max(circumference_ycoords);
        } # End for
        
        summary_max_intensity=summary(max_intensity);
        
        indices_all_tobe_considered_finally=unique(c(which(length_blackpixels==min(length_blackpixels)),c(which(length_whitepixels==max(length_whitepixels))),c(which(max_intensity>=summary_max_intensity[2]))));
        start_xcoords_vasculature<-vector();
        end_xcoords_vasculature<-vector();
        start_ycoords_vasculature<-vector();
        end_ycoords_vasculature<-vector();
        diff_ycoords<-vector();
        diff_xcoords<-vector();
        true_OD_index_counter=1;
        finalOD_region_indices<-vector();
        
        for (each_shortlist_index in 1:length(indices_all_tobe_considered_finally))
        {
          vasculature_current_image=main_vasculature_labeled_removedframe[start_x_coord_centroid[indices_all_tobe_considered_finally[each_shortlist_index]]:end_x_coord_centroid[indices_all_tobe_considered_finally[each_shortlist_index]],start_y_coord_centroid[indices_all_tobe_considered_finally[each_shortlist_index]]:end_y_coord_centroid[indices_all_tobe_considered_finally[each_shortlist_index]]];
          vasculature_current_coords=(which(vasculature_current_image==1,arr.ind=TRUE));
          start_xcoords_vasculature[each_shortlist_index]=min(vasculature_current_coords[,1]);
          end_xcoords_vasculature[each_shortlist_index]=max(vasculature_current_coords[,1]);
          start_ycoords_vasculature[each_shortlist_index]=min(vasculature_current_coords[,2]);
          end_ycoords_vasculature[each_shortlist_index]=max(vasculature_current_coords[,2]);
          diff_ycoords[each_shortlist_index]=end_ycoords_vasculature[each_shortlist_index]-start_ycoords_vasculature[each_shortlist_index];
          diff_xcoords[each_shortlist_index]=end_xcoords_vasculature[each_shortlist_index]-start_xcoords_vasculature[each_shortlist_index];
          
          image_forODsegmentation=BestFrame_copy[start_x_coord_centroid[indices_all_tobe_considered_finally[each_shortlist_index]]:end_x_coord_centroid[indices_all_tobe_considered_finally[each_shortlist_index]],start_y_coord_centroid[indices_all_tobe_considered_finally[each_shortlist_index]]:end_y_coord_centroid[indices_all_tobe_considered_finally[each_shortlist_index]]];
          segmented_OD_region=(image_forODsegmentation>=mean_intensity[indices_all_tobe_considered_finally[each_shortlist_index]]+sd_intensity[indices_all_tobe_considered_finally[each_shortlist_index]]);
        }
        # Edited line
        finalOD_region_index_basedonvessels=indices_all_tobe_considered_finally[which(diff_ycoords==max(diff_ycoords),arr.ind=TRUE)];
        finalOD_region_index_basedonvessels <- finalOD_region_index_basedonvessels[!is.na(finalOD_region_index_basedonvessels)];
        
        pointers_maxintensity_indicestobeconsidered<-vector();
        pointers_maxintensity_indicestobeconsidered=which(max_intensity[indices_all_tobe_considered_finally]>=summary_max_intensity[5]); # >=3rd Quartile max_intensity
        finalOD_region_index_basedonintensity<-vector();
        finalOD_region_index_basedonintensity=indices_all_tobe_considered_finally[pointers_maxintensity_indicestobeconsidered];
        finalOD_region_index_basedonintensity <- finalOD_region_index_basedonintensity[!is.na(finalOD_region_index_basedonintensity)];
        final_OD_indices<-vector();
        
        if(length(finalOD_region_index_basedonvessels)==0 && length(finalOD_region_index_basedonintensity)>=1)
        {
          final_OD_indices=finalOD_region_index_basedonintensity; 
        } else if(length(finalOD_region_index_basedonvessels)>=1 && length(finalOD_region_index_basedonintensity)==0)
        {
          final_OD_indices=finalOD_region_index_basedonvessels;
        } else if(length(finalOD_region_index_basedonvessels)>=1 && length(finalOD_region_index_basedonintensity)>=1 && length(intersect(finalOD_region_index_basedonvessels,finalOD_region_index_basedonintensity))==0)
        {
          final_OD_indices=union(finalOD_region_index_basedonvessels,finalOD_region_index_basedonintensity);
          
        } else if(length(finalOD_region_index_basedonvessels)>=1 && length(finalOD_region_index_basedonintensity)>=1 && length(intersect(finalOD_region_index_basedonvessels,finalOD_region_index_basedonintensity))>=1)
        { 
          final_OD_indices=union(finalOD_region_index_basedonvessels,finalOD_region_index_basedonintensity);
          
        } else if(length(finalOD_region_index_basedonvessels)==1 && length(finalOD_region_index_basedonintensity)==1 && finalOD_region_index_basedonvessels==finalOD_region_index_basedonintensity)
        {
          final_OD_indices=finalOD_region_index_basedonvessels;
        }
        
        centroid_x_currframe<-vector();
        centroid_y_currframe<-vector();
        num_of_labels_OD_region<-vector();
        centroid_OD_xcoordinate<-vector();
        centroid_OD_ycoordinate<-vector();
        dist_between_centroids_frameandOD<-vector();
        OD_max_intensity<-vector();
        
        if(length(final_OD_indices)>1)
        {
          for(eachfinalOD_index in 1:length(final_OD_indices))
          {  
            currOD_region_index=final_OD_indices[eachfinalOD_index];
            image_forODsegmentation=BestFrame_copy[start_x_coord_centroid[currOD_region_index]:end_x_coord_centroid[currOD_region_index],start_y_coord_centroid[currOD_region_index]:end_y_coord_centroid[currOD_region_index]];
            segmented_OD_region=(image_forODsegmentation>=mean_intensity[currOD_region_index]+sd_intensity[currOD_region_index]);
            centroid_x_currframe[eachfinalOD_index]=start_x_coord_centroid[currOD_region_index]+(end_x_coord_centroid[currOD_region_index]-start_x_coord_centroid[currOD_region_index])/2;
            centroid_y_currframe[eachfinalOD_index]=start_y_coord_centroid[currOD_region_index]+(end_y_coord_centroid[currOD_region_index]-start_y_coord_centroid[currOD_region_index])/2;
            labeled_segmented_OD_region=bwlabel(segmented_OD_region);
            num_of_labels_OD_region[eachfinalOD_index]=max(labeled_segmented_OD_region);
            segmented_OD_region_indices=which(segmented_OD_region==1,arr.ind=TRUE);
            final_OD_rowindices=intersect(segmented_OD_region_indices[,1],mainvessel_indices[,1]);
            final_OD_colindices=intersect(segmented_OD_region_indices[,2],mainvessel_indices[,2]);
            segmented_OD_region[final_OD_rowindices,final_OD_colindices]=1;
            OD_indices=which(segmented_OD_region==1,arr.ind=TRUE);
            
            if(abs(round(max_intensity[final_OD_indices[eachfinalOD_index]])-summary_max_intensity[5])>=0.008) # 3rd quartile of max-intensity scan values
            {
              OD_max_intensity[eachfinalOD_index]=max_intensity[final_OD_indices[eachfinalOD_index]];
              
            }else
            {
              OD_max_intensity[eachfinalOD_index]=0;
            }
            # Compute Centroid coordinates of OD "bounding area"
            # An error range in radius from these actual centroid coordinates
            # will be useful for detecting Macula in the next script
            
            centroid_OD_xcoordinate[eachfinalOD_index]=min(OD_indices[,1])+round(max(OD_indices[,1])-min(OD_indices[,1]),digits = 0)/2;
            centroid_OD_ycoordinate[eachfinalOD_index]=min(OD_indices[,2])+round(max(OD_indices[,2])-min(OD_indices[,2]),digits = 0)/2;
            dist_between_centroids_frameandOD[eachfinalOD_index]=sqrt((centroid_x_currframe[eachfinalOD_index]-centroid_OD_xcoordinate[eachfinalOD_index])^2+((centroid_y_currframe[eachfinalOD_index]-centroid_OD_ycoordinate[eachfinalOD_index])^2));
            
          } # End for
          
          index_ge_thirdquartilemaxintensity<-vector();  
          index_ge_thirdquartilemaxintensity=which(OD_max_intensity!=0,arr.ind=TRUE);
          if(length(index_ge_thirdquartilemaxintensity)!=0)
          {
            final_OD_indices=final_OD_indices[index_ge_thirdquartilemaxintensity];
            OD_max_intensity=OD_max_intensity[index_ge_thirdquartilemaxintensity];
            dist_between_centroids_frameandOD=dist_between_centroids_frameandOD[index_ge_thirdquartilemaxintensity];
          }
          
          # choosing the best OD region as belonging to centre of current frame
          summary_dist_bwcentroids_frameOD<-vector();
          summary_dist_bwcentroids_frameOD=summary(dist_between_centroids_frameandOD);
          # 1st Quartile
          if(length(final_OD_indices)>1){
            final_OD_indices=final_OD_indices[which(dist_between_centroids_frameandOD>summary_dist_bwcentroids_frameOD[2],arr.ind=TRUE)];
          } else{
            final_OD_indices<-final_OD_indices;
          }
          
          dist_between_vasculatureandOD<-vector();
          
          for (eachfinalOD_index in 1:length(final_OD_indices))
          {
            final_OD_region=array(0,dim=c(dimensions_resized[1],dimensions_resized[2]));
            final_OD_region[start_x_coord_centroid[final_OD_indices[eachfinalOD_index]]:end_x_coord_centroid[final_OD_indices[eachfinalOD_index]],start_y_coord_centroid[final_OD_indices[eachfinalOD_index]]:end_y_coord_centroid[final_OD_indices[eachfinalOD_index]]]<-BestFrame_copy[start_x_coord_centroid[final_OD_indices[eachfinalOD_index]]:end_x_coord_centroid[final_OD_indices[eachfinalOD_index]],start_y_coord_centroid[final_OD_indices[eachfinalOD_index]]:end_y_coord_centroid[final_OD_indices[eachfinalOD_index]]];
            final_OD_region[otherthan_mainvessel_indices]=0;
            segmented_OD_region=(final_OD_region!=0);
            labeled_segmented_OD_region=bwlabel(segmented_OD_region);  
            curr_segmented_OD_region_indices=which(segmented_OD_region==1,arr.ind = TRUE);
            row_seg_indices=curr_segmented_OD_region_indices[,1];
            col_seg_indices=curr_segmented_OD_region_indices[,2];
            
            curr_finalODindex_centroidx=round((end_x_coord_centroid[final_OD_indices[eachfinalOD_index]]-start_x_coord_centroid[final_OD_indices[eachfinalOD_index]])/2+start_x_coord_centroid[final_OD_indices[eachfinalOD_index]],digits = 0);   
            curr_finalODindex_centroidy=round((end_y_coord_centroid[final_OD_indices[eachfinalOD_index]]-start_y_coord_centroid[final_OD_indices[eachfinalOD_index]])/2+start_y_coord_centroid[final_OD_indices[eachfinalOD_index]],digits = 0);
            dist_between_vasculatureandOD[eachfinalOD_index]=unique(min(sqrt((curr_finalODindex_centroidx-row_seg_indices)^2+(curr_finalODindex_centroidy-col_seg_indices)^2)));
          }
          
          finalOD_index_basedondist=final_OD_indices[which(dist_between_vasculatureandOD==min(dist_between_vasculatureandOD),arr.ind=TRUE)];
          finalOD_index_basedonintensity=final_OD_indices[which(max(max_intensity)==max_intensity[final_OD_indices],arr.ind=TRUE)];
          
          allfinalOD_indices<-vector();
          allfinalOD_indices=union(finalOD_index_basedondist,finalOD_index_basedonintensity)
          
          centroid_xfinalframe<-vector();
          centroid_yfinalframe<-vector();
          eachof_finalODindices=1;
          while(eachof_finalODindices <= length(allfinalOD_indices))
          {
            centroid_xfinalframe[eachof_finalODindices]=start_x_coord_centroid[allfinalOD_indices[eachof_finalODindices]]+(rectangular_scanbot_region/2);
            centroid_yfinalframe[eachof_finalODindices]=start_y_coord_centroid[allfinalOD_indices[eachof_finalODindices]]+(rectangular_scanbot_region/2);
            eachof_finalODindices=eachof_finalODindices+1;
          }
          
          eachindex_centroid=1;
          neighbour_count=1;
          separate_count=1;
          dist_between_centroids<-vector();
          neighbours_frames<-vector();
          separate_frames<-vector();
          if(length(centroid_xfinalframe)>1)
          {
            while(eachindex_centroid+1<=length(centroid_xfinalframe))
            {
              dist_between_centroids[eachindex_centroid]=(abs(abs(centroid_xfinalframe[eachindex_centroid]-centroid_xfinalframe[eachindex_centroid+1])-abs(centroid_yfinalframe[eachindex_centroid]-centroid_yfinalframe[eachindex_centroid+1])));
              eachindex_centroid=eachindex_centroid+1;
              if(dist_between_centroids[eachindex_centroid-1]<=rectangular_scanbot_region)
              {
                neighbours_frames[neighbour_count]=allfinalOD_indices[eachindex_centroid];
                neighbours_frames[neighbour_count+1]=allfinalOD_indices[eachindex_centroid-1];
                neighbour_count=neighbour_count+1;
              }else {
                separate_frames[separate_count]=allfinalOD_indices[eachindex_centroid];
                separate_frames[separate_count+1]=allfinalOD_indices[eachindex_centroid-1];
                separate_count=separate_count+1;
              }
            }
          } else
          {
            neighbours_frames=allfinalOD_indices;
          }
          
          frames_vector<-vector();
          frames_vector=neighbours_frames;
          
          if(length(unique(frames_vector))==length(neighbours_frames) && length(unique(neighbours_frames))>1)
          {
            circlecheck<-vector();
            for (eachfinalODindex in 1:length(frames_vector))
            {
              final_OD_region=array(0,dim=c(dimensions_resized[1],dimensions_resized[2]));
              final_OD_region[start_x_coord_centroid[frames_vector[eachfinalODindex]]:end_x_coord_centroid[frames_vector[eachfinalODindex]],start_y_coord_centroid[frames_vector[eachfinalODindex]]:end_y_coord_centroid[frames_vector[eachfinalODindex]]]<-BestFrame_resized[start_x_coord_centroid[frames_vector[eachfinalODindex]]:end_x_coord_centroid[frames_vector[eachfinalODindex]],start_y_coord_centroid[frames_vector[eachfinalODindex]]:end_y_coord_centroid[frames_vector[eachfinalODindex]]];
              segmented_OD_region=(final_OD_region>=mean_intensity[frames_vector[eachfinalODindex]]+sd_intensity[frames_vector[eachfinalODindex]]);
              segmented_OD_region=bwlabel(segmented_OD_region);
              areaforcirclecheck=computeFeatures.shape(segmented_OD_region);
              largestarea_index=which(max(areaforcirclecheck[,'s.area'])==areaforcirclecheck[,'s.area'],arr.ind = TRUE);
              circularshapecheck_eachfinalODindex=computeFeatures.moment(segmented_OD_region);
              circlecheck[eachfinalODindex]=circularshapecheck_eachfinalODindex[,'m.eccentricity'][largestarea_index];
              finalOD_index=frames_vector[which(circlecheck[which(min(circlecheck)==circlecheck,arr.ind=TRUE)]==circlecheck,arr.ind = TRUE)];
            }
          } else  if(length(unique(frames_vector))==length(neighbours_frames) && length(unique(neighbours_frames))==1)
          {
            finalOD_index=neighbours_frames;
          } else if(length(unique(neighbours_frames))==0)
          {
            separateframe_index_maxintensity=which(max(max_intensity)==max_intensity,arr.ind = TRUE);
            finalOD_index=separateframe_index_maxintensity;
          } else if(sort(frames_vector)==sort(neighbours_frames))
          {
            
            circlecheck<-vector();
            for (eachfinalODindex in 1:length(unique(frames_vector)))
            {
              final_OD_region=array(0,dim=c(dimensions_resized[1],dimensions_resized[2]));
              final_OD_region[start_x_coord_centroid[frames_vector[eachfinalODindex]]:end_x_coord_centroid[frames_vector[eachfinalODindex]],start_y_coord_centroid[frames_vector[eachfinalODindex]]:end_y_coord_centroid[frames_vector[eachfinalODindex]]]<-BestFrame_resized[start_x_coord_centroid[frames_vector[eachfinalODindex]]:end_x_coord_centroid[frames_vector[eachfinalODindex]],start_y_coord_centroid[frames_vector[eachfinalODindex]]:end_y_coord_centroid[frames_vector[eachfinalODindex]]];
              segmented_OD_region=(final_OD_region>=mean_intensity[frames_vector[eachfinalODindex]]+sd_intensity[frames_vector[eachfinalODindex]]);
              segmented_OD_region=bwlabel(segmented_OD_region);
              areaforcirclecheck=computeFeatures.shape(segmented_OD_region);
              largestarea_index=which(max(areaforcirclecheck[,'s.area'])==areaforcirclecheck[,'s.area'],arr.ind = TRUE);
              circularshapecheck_eachfinalODindex=computeFeatures.moment(segmented_OD_region);
              circlecheck[eachfinalODindex]=circularshapecheck_eachfinalODindex[,'m.eccentricity'][largestarea_index];
              finalOD_index=frames_vector[which(circlecheck[which(min(circlecheck)==circlecheck,arr.ind=TRUE)]==circlecheck,arr.ind = TRUE)];
              
            }
          }
          final_OD_region=array(0,dim=c(dimensions_resized[1],dimensions_resized[2]));
          final_OD_region[start_x_coord_centroid[finalOD_index]:end_x_coord_centroid[finalOD_index],start_y_coord_centroid[finalOD_index]:end_y_coord_centroid[finalOD_index]]<-BestFrame_copy[start_x_coord_centroid[finalOD_index]:end_x_coord_centroid[finalOD_index],start_y_coord_centroid[finalOD_index]:end_y_coord_centroid[finalOD_index]];
          segmented_OD_region=(final_OD_region>=mean_intensity[finalOD_index]+sd_intensity[finalOD_index]);
          OD_indices=which(segmented_OD_region==1,arr.ind=TRUE);
          # Compute Centroid coordinates of OD "bounding area"
          # An error range in radius from these actual centroid coordinates
          # will be useful for detecting Macula in the next script
          
          centroid_OD_xcoordinate=round(((max(OD_indices[,1])-min(OD_indices[,1]))/2)+min(OD_indices[,1]));
          centroid_OD_ycoordinate=round(((max(OD_indices[,2])-min(OD_indices[,2]))/2)+min(OD_indices[,2]));
          radius_OD=abs(max(OD_indices[,2])-centroid_OD_ycoordinate)/2;
        } else # When final_OD_indices vector has only one elements, in other words if it is a scalar
        {
          finalOD_index <- final_OD_indices;
          final_OD_region=array(0,dim=c(dimensions_resized[1],dimensions_resized[2]));
          final_OD_region[start_x_coord_centroid[finalOD_index]:end_x_coord_centroid[finalOD_index],start_y_coord_centroid[finalOD_index]:end_y_coord_centroid[finalOD_index]]<-BestFrame_copy[start_x_coord_centroid[finalOD_index]:end_x_coord_centroid[finalOD_index],start_y_coord_centroid[finalOD_index]:end_y_coord_centroid[finalOD_index]];
          segmented_OD_region=(final_OD_region>=mean_intensity[finalOD_index]+sd_intensity[finalOD_index]);
          OD_indices=which(segmented_OD_region==1,arr.ind=TRUE);
          # Compute Centroid coordinates of OD "bounding area"
          # An error range in radius from these actual centroid coordinates
          # will be useful for detecting Macula in the next script
          
          centroid_OD_xcoordinate=round(((max(OD_indices[,1])-min(OD_indices[,1]))/2)+min(OD_indices[,1]));
          centroid_OD_ycoordinate=round(((max(OD_indices[,2])-min(OD_indices[,2]))/2)+min(OD_indices[,2]));
          radius_OD=abs(max(OD_indices[,2])-centroid_OD_ycoordinate)/2;
        }
      }
    }
    
    if (flagVASC_QUAL == 1) {
      flagRun_Status <- 1;
    }
  }
  cat("finished successfully","\n");
  
  if (flagD_R == 0 && flagVASC_QUAL == 0) {
    set.seed(42);
    macula_leftOD_flag=0;
    macula_rightOD_flag=0;
    
    centroid_row=centroid_OD_xcoordinate;
    centroid_col=centroid_OD_ycoordinate;
    radius_opticdisc=radius_OD;
    macula_leftOD_flag=0;
    macula_rightOD_flag=0;
    if(abs(1-centroid_row)>abs(dimensions_resized[1]-centroid_row)){
      cat("Macula left to OD","\n");
      macula_leftOD_flag=1;
    } else
    {
      cat("Macula right to OD","\n");
      macula_rightOD_flag=1;
    }
    
    if(macula_leftOD_flag==1)
    {
      macula_candidate_region=array(0,dim=c(dimensions_resized[1],dimensions_resized[2]));
      rowstart_formacula=1+(3*radius_opticdisc);
      rowend_formacula=centroid_row-(2*radius_opticdisc);
      colstart_formacula=centroid_col-(2*radius_opticdisc);
      colend_formacula=dimensions_resized[2]-(2.5*radius_opticdisc);
      macula_candidate_region[rowstart_formacula:rowend_formacula,colstart_formacula:colend_formacula]=BestFrame_copy[rowstart_formacula:rowend_formacula,colstart_formacula:colend_formacula];
    } else
    {
      macula_candidate_region=array(0,dim=c(dimensions_resized[1],dimensions_resized[2]));
      rowstart_formacula=centroid_row+(3*radius_opticdisc);
      rowend_formacula=dimensions_resized[1]-(3*radius_opticdisc);
      colstart_formacula=centroid_col-(2*radius_opticdisc);
      colend_formacula=dimensions_resized[2]-(2.5*radius_opticdisc);
      macula_candidate_region[rowstart_formacula:rowend_formacula,colstart_formacula:colend_formacula]=BestFrame_resized[rowstart_formacula:rowend_formacula,colstart_formacula:colend_formacula];
    }
  }
  
  cat("finished successfully","\n");
  
  if (flagD_R == 0 && flagVASC_QUAL == 0) {
    set.seed(42);
    macula_kern_size=radius_opticdisc;
    
    rowcoords_macula<-vector();
    colcoords_macula<-vector();
    
    rowcoords_macula<-seq(rowstart_formacula,rowend_formacula,macula_kern_size);
    colcoords_macula<-seq(colstart_formacula,colend_formacula,macula_kern_size);
    meanof<-matrix(data = 0,nrow = length(rowcoords_macula),ncol = length(colcoords_macula));
    startrow_macula<-matrix(data = 0,nrow = length(rowcoords_macula),ncol = length(colcoords_macula));
    endrow_macula<-matrix(data = 0,nrow = length(rowcoords_macula),ncol = length(colcoords_macula));
    startcol_macula<-matrix(data = 0,nrow = length(rowcoords_macula),ncol = length(colcoords_macula));
    endcol_macula<-matrix(data = 0,nrow = length(rowcoords_macula),ncol = length(colcoords_macula));
    
    for(eachrowcoord in 1:length(rowcoords_macula))
    {
      for (eachcolcoord in 1:length(colcoords_macula))
      {
        fromrow<-rowcoords_macula[eachrowcoord]-macula_kern_size;
        torow<-rowcoords_macula[eachrowcoord]+macula_kern_size;
        fromcol<-colcoords_macula[eachcolcoord]-macula_kern_size;
        tocol<-colcoords_macula[eachcolcoord]+macula_kern_size;
        
        if(fromrow<=0){
          fromrow<-1;
        }
        if(torow>=dimensions_resized[1])
        {
          torow<-dimensions_resized[1];
        }
        if(fromcol<=0){
          fromcol<-1;
        }
        
        if(tocol>=dimensions_resized[2])
        {
          tocol<-dimensions_resized[2];
        }
        
        BestFrame_resized_withoutOD<-BestFrame_resized;
        BestFrame_resized_withoutOD[min(OD_indices[,1]):max(OD_indices[,1]),min(OD_indices[,2]):max(OD_indices[,2])]<-0;
        meanof[eachrowcoord,eachcolcoord]<-mean(c(BestFrame_resized_withoutOD[fromrow:torow,fromcol:tocol]));
        circle_scanformacula<-drawCircle(Input_MyImage_resized,rowcoords_macula[eachrowcoord],colcoords_macula[eachcolcoord],radius = macula_kern_size,col = 'white',fill = TRUE);    
        startrow_macula[eachrowcoord,eachcolcoord]<-fromrow;
        endrow_macula[eachrowcoord,eachcolcoord]<-torow;
        startcol_macula[eachrowcoord,eachcolcoord]<-fromcol;
        endcol_macula[eachrowcoord,eachcolcoord]<-tocol;
      }
      
    }
    
    dist_criterion_lessthanthreshold<-(abs(startrow_macula-centroid_row)/radius_opticdisc)>3;
    indices_lessthan_distthreshold<-which(dist_criterion_lessthanthreshold==1,arr.ind=TRUE);
    
    dist_criterion_greaterthanthreshold<-(abs(startrow_macula-centroid_row)/radius_opticdisc)<6;
    indices_greaterthan_distthreshold<-which(dist_criterion_greaterthanthreshold==1,arr.ind=TRUE);
    
    verticaldist_criterion_greaterthanthreshold<-(abs(startcol_macula-centroid_col)/radius_opticdisc)>7;
    verticalindices_greaterthan_distthreshold<-which(verticaldist_criterion_greaterthanthreshold==1,arr.ind=TRUE);
    
    verticaldist_criterion_lesserthanthreshold<-(abs(endcol_macula-centroid_col)/radius_opticdisc)<=10;
    verticalindices_lesserthan_distthreshold<-which(verticaldist_criterion_lesserthanthreshold==1,arr.ind=TRUE);
    
    
    
    matrixA<-indices_lessthan_distthreshold;
    matrixB<-indices_greaterthan_distthreshold;
    matrixC<-verticalindices_greaterthan_distthreshold;
    matrixD<-verticalindices_lesserthan_distthreshold;
    
    flagNull_Matrix_Check <- 0;
    
    if ((nrow(matrixA) == 0 && nrow(matrixB) == 0) || (nrow(matrixC) == 0 && nrow(matrixD) == 0)) {
      cat("Logical error in Fovea detection, please change distance from centre of fovea to radius of optic disc ratio value","\n");
      flagNull_Matrix_Check <- 1;
    }
    
    if (flagNull_Matrix_Check == 0) {
      if (nrow(matrixA) == 0) {
        horizontal_indices_truematrix <- matrixB
      } else if (nrow(matrixB) == 0) {
        horizontal_indices_truematrix <- matrixA
      } else {
        #### Decide which matrix is big/small
        bigmatrix<-matrixA;
        smallmatrix<-matrixB;
        
        if(nrow(matrixA)<nrow(matrixB))
        {
          smallmatrix<-matrixA;
          bigmatrix<-matrixB;
        }
        
        ################ New patch starts ##############
        final_ans <- matrix(NA, nrow=nrow(bigmatrix), ncol=ncol(bigmatrix))
        ind <- 1
        for(i in 1:nrow(bigmatrix)) {
          rowA <- bigmatrix[i,]
          for(j in 1:nrow(smallmatrix)) {
            rowB <- smallmatrix[j,]
            comp_row <- rowA == rowB
            oper <- TRUE
            for(k in 1:length(comp_row)) {
              oper <- oper && comp_row[k]
            }
            if(oper) {
              final_ans[ind, ] <- bigmatrix[i, ]
              ind <- ind+1
            }
          }
        }
        final_ans <-unique(na.omit(final_ans));
        
        ######## New patch ends ##############
        horizontal_indices_truematrix<-final_ans;
      }
      
      if (nrow(matrixC) == 0) {
        vertical_indices_truematrix <- matrixD;
      } else if (nrow(matrixD) == 0) {
        vertical_indices_truematrix <- matrixC;
      } else {
        #### Vertical criterion ###
        #### Decide which matrix is big/small
        bigmatrix<-matrixC;
        smallmatrix<-matrixD;
        
        if(nrow(matrixC)<nrow(matrixD))
        {
          smallmatrix<-matrixC;
          bigmatrix<-matrixD;
        }
        ############## New patch starts ###############
        final_ans <- matrix(NA, nrow=nrow(bigmatrix), ncol=ncol(bigmatrix))
        ind <- 1
        for(i in 1:nrow(bigmatrix)) {
          rowA <- bigmatrix[i,]
          for(j in 1:nrow(smallmatrix)) {
            rowB <- smallmatrix[j,]
            comp_row <- rowA == rowB
            oper <- TRUE
            for(k in 1:length(comp_row)) {
              oper <- oper && comp_row[k]
            }
            if(oper) {
              final_ans[ind, ] <- bigmatrix[i, ]
              ind <- ind+1
            }
          }
        }
        final_ans <-unique(na.omit(final_ans));
        ################# New patch Ends ###########
        vertical_indices_truematrix<-final_ans;
      }
      
      if (nrow(horizontal_indices_truematrix) == 0 && nrow(vertical_indices_truematrix) == 0) {
        cat("Logical error in Fovea detection, please change distance from centre of fovea to radius of optic disc ratio value","\n");
        flagNull_Matrix_Check <- 1;
      }
      
      if (flagNull_Matrix_Check == 0) {
        if (nrow(horizontal_indices_truematrix) == 0) {
          indices_truematrix <- vertical_indices_truematrix;
        } else if (nrow(vertical_indices_truematrix) == 0) {
          indices_truematrix <- horizontal_indices_truematrix;
        } else {
          ############ Combine Horizontal and Vertical ########
          #### Decide which matrix is big/small
          bigmatrix<-horizontal_indices_truematrix;
          smallmatrix<-vertical_indices_truematrix;
          
          if(nrow(horizontal_indices_truematrix)<nrow(vertical_indices_truematrix))
          {
            smallmatrix<-horizontal_indices_truematrix;
            bigmatrix<-vertical_indices_truematrix;
          }
          
          
          ########## New patch starts ############
          
          final_ans <- matrix(NA, nrow=nrow(bigmatrix), ncol=ncol(bigmatrix))
          ind <- 1
          for(i in 1:nrow(bigmatrix)) {
            rowA <- bigmatrix[i,]
            for(j in 1:nrow(smallmatrix)) {
              rowB <- smallmatrix[j,]
              comp_row <- rowA == rowB
              oper <- TRUE
              for(k in 1:length(comp_row)) {
                oper <- oper && comp_row[k]
              }
              if(oper) {
                final_ans[ind, ] <- bigmatrix[i, ]
                ind <- ind+1
              }
            }
          }
          final_ans <-unique(na.omit(final_ans));
          
          ############### New patch Ends ########
          indices_truematrix<-final_ans;
        }
        ################# Code needs to be reviewed ###############
        if(length(indices_truematrix)==2)
        {
          macula_index<-indices_truematrix;
          circle_scanformacula<-drawCircle(Input_MyImage_resized,startrow_macula[macula_index[1]],startcol_macula[macula_index[2]],radius = macula_kern_size,col = 'white',fill = TRUE);  
        }else
        {
          macula_index<-which(min(meanof[indices_truematrix[,1],indices_truematrix[,2]])==meanof,arr.ind=TRUE);
          circle_scanformacula<-drawCircle(Input_MyImage_resized,startrow_macula[macula_index[,1]],startcol_macula[macula_index[,2]],radius = macula_kern_size,col = 'white',fill = TRUE);    
        }
      }    
    }
    
    if (flagNull_Matrix_Check == 1) {
      flagRun_Status <- 1;
    }
  }
  
  cat("finished successfully","\n");
  
  if (flagD_R == 0 && flagVASC_QUAL == 0 && flagNull_Matrix_Check == 0) {
    set.seed(42);
    #### First, hard exudates identification based on pixel intensity ####
    dist_frommaculacentre=10;
    flag_exudates<-0;
    mean_BestFrameresized=mean(c(c(BestFrame_resized)));
    sd_BestFrameresized=sd(c(c(BestFrame_resized)));
    exudates_image_basedonintensity<-BestFrame_resized>(mean_BestFrameresized+(1.2*sd_BestFrameresized));
    reference_exudates_region=array(data = 0,dim=c(dimensions_resized[1],dimensions_resized[2]));
    if(length(macula_index)==2)
    {
      centroid_macula_row<-startrow_macula[macula_index[1]];
      centroid_macula_col<-startcol_macula[macula_index[2]];  
    }else
    {
      centroid_macula_row<-startrow_macula[macula_index[,1]];
      centroid_macula_col<-startcol_macula[macula_index[,2]];
    }  
    
    rowstart_referenceexudates_region<-(centroid_macula_row-(dist_frommaculacentre*radius_opticdisc));
    rowend_referenceexudates_region<-(centroid_macula_row+(dist_frommaculacentre*radius_opticdisc));
    colstart_referenceexudates_region<-(centroid_macula_col-(dist_frommaculacentre*radius_opticdisc));
    colend_referenceexudates_region<-(centroid_macula_col+(dist_frommaculacentre*radius_opticdisc));
    
    if(rowstart_referenceexudates_region<=0)
    {
      rowstart_referenceexudates_region<-1;
    }
    
    if(colstart_referenceexudates_region<=0)
    {
      colstart_referenceexudates_region<-1;
    }
    
    if(rowend_referenceexudates_region>dimensions_resized[1])
    {
      rowend_referenceexudates_region<-dimensions_resized[1];
    }
    if(colend_referenceexudates_region>dimensions_resized[2])
    {
      colend_referenceexudates_region<-dimensions_resized[2];
    }
    
    reference_exudates_region[rowstart_referenceexudates_region:rowend_referenceexudates_region,colstart_referenceexudates_region:colend_referenceexudates_region]=exudates_image_basedonintensity[rowstart_referenceexudates_region:rowend_referenceexudates_region,colstart_referenceexudates_region:colend_referenceexudates_region];
    
    ref_exudatesrefregion_lowerbound<-(min(OD_indices[,1])-radius_opticdisc);
    ref_exudatesrefregion_upperbound<-(max(OD_indices[,1])+radius_opticdisc);
    
    if(ref_exudatesrefregion_lowerbound<=0)
    {
      ref_exudatesrefregion_lowerbound<-min(OD_indices[,1]);
    }
    
    if(ref_exudatesrefregion_upperbound>dimensions_resized[1])
    {
      ref_exudatesrefregion_upperbound<-dimensions_resized[1];
    }
    if(macula_rightOD_flag==1)
    {
      reference_exudates_region[min(OD_indices[,1]):ref_exudatesrefregion_upperbound,min(OD_indices[,2]):max(OD_indices[,2])]=0;
    }else
    {
      reference_exudates_region[ref_exudatesrefregion_lowerbound:ref_exudatesrefregion_upperbound,min(OD_indices[,2]):max(OD_indices[,2])]=0;
    }
    reference_exudates_region<-bwlabel(reference_exudates_region);
    area_computation_reference_exudates_region<-computeFeatures.shape(reference_exudates_region);
    lowarea_candidates_exudates<-unique(which(area_computation_reference_exudates_region[,1]<=6,arr.ind = TRUE));
    hiarea_candidates_exudates<-unique(which(area_computation_reference_exudates_region[,1]>=10*radius_OD,arr.ind = TRUE));
    
    
    if(length(lowarea_candidates_exudates)!=0 && length(hiarea_candidates_exudates)!=0){
      reference_exudates_region<-rmObjects(reference_exudates_region,union(lowarea_candidates_exudates,hiarea_candidates_exudates));
    } else if(length(lowarea_candidates_exudates)!=0 && length(hiarea_candidates_exudates)==0){
      reference_exudates_region<-rmObjects(reference_exudates_region,lowarea_candidates_exudates);  
    } else if(length(lowarea_candidates_exudates)==0 && length(hiarea_candidates_exudates)!=0){
      reference_exudates_region<-rmObjects(reference_exudates_region,hiarea_candidates_exudates);  
    }
    
    red_channel=channel(Input_MyImage,mode ='asred');
    blue_channel=channel(Input_MyImage,mode ='asblue');
    green_channel=channel(Input_MyImage,mode ='asgreen');
    
    resizedred_channel=resize(red_channel,w = dimensions_resized[1],h = dimensions_resized[2]);
    resizedblue_channel=resize(blue_channel,w = dimensions_resized[1],h = dimensions_resized[2]);
    resizedgreen_channel=resize(green_channel,w = dimensions_resized[1],h = dimensions_resized[2]);
    
    finalHue_candidates<-vector();
    if(length(unique(c(c(reference_exudates_region))))!=1)
    {
      reference_exudates_region=bwlabel(reference_exudates_region);
      start_row_exudate_candidate_basedon_intensity<-vector();
      end_row_exudate_candidate_basedon_intensity<-vector();
      start_col_exudate_candidate_basedon_intensity<-vector();
      end_col_exudate_candidate_basedon_intensity<-vector();
      
      Hue_calculated<-vector();
      
      for (eachexudatecandidate in 1:max(reference_exudates_region))
      {
        curr_exudatecandidate_indices=which((reference_exudates_region==eachexudatecandidate)==1,arr.ind=TRUE);
        start_row_exudate_candidate_basedon_intensity[eachexudatecandidate]=min(curr_exudatecandidate_indices[,1]);
        end_row_exudate_candidate_basedon_intensity[eachexudatecandidate]=max(curr_exudatecandidate_indices[,1]);
        start_col_exudate_candidate_basedon_intensity[eachexudatecandidate]=min(curr_exudatecandidate_indices[,2]);
        end_col_exudate_candidate_basedon_intensity[eachexudatecandidate]=max(curr_exudatecandidate_indices[,2]);
        
        kern_size=(dimensions_resized[1]/(dimensions_resized[1]/10));
        
        # Find the minimum and maximum values of R, G and B.
        # Depending on what RGB color channel is the max value. The three different formulas are:
        #   If Red is max, then Hue = (G-B)/(max-min)
        # If Green is max, then Hue = 2.0 + (B-R)/(max-min)
        # If Blue is max, then Hue = 4.0 + (R-G)/(max-min)
        # 
        # The Hue value you get needs to be multiplied by 60 to convert it to degrees on the color circle. If Hue becomes negative you need to add 360 to, because a circle has 360 degrees.
        
        
        max_red=max(resizedred_channel[start_row_exudate_candidate_basedon_intensity[eachexudatecandidate]:end_row_exudate_candidate_basedon_intensity[eachexudatecandidate],start_col_exudate_candidate_basedon_intensity[eachexudatecandidate]:end_col_exudate_candidate_basedon_intensity[eachexudatecandidate]]);
        
        max_blue=max(resizedblue_channel[start_row_exudate_candidate_basedon_intensity[eachexudatecandidate]:end_row_exudate_candidate_basedon_intensity[eachexudatecandidate],start_col_exudate_candidate_basedon_intensity[eachexudatecandidate]:end_col_exudate_candidate_basedon_intensity[eachexudatecandidate]]);
        
        max_green=max(resizedgreen_channel[start_row_exudate_candidate_basedon_intensity[eachexudatecandidate]:end_row_exudate_candidate_basedon_intensity[eachexudatecandidate],start_col_exudate_candidate_basedon_intensity[eachexudatecandidate]:end_col_exudate_candidate_basedon_intensity[eachexudatecandidate]]);
        
        min_red=min(resizedred_channel[start_row_exudate_candidate_basedon_intensity[eachexudatecandidate]:end_row_exudate_candidate_basedon_intensity[eachexudatecandidate],start_col_exudate_candidate_basedon_intensity[eachexudatecandidate]:end_col_exudate_candidate_basedon_intensity[eachexudatecandidate]]);
        
        min_blue=min(resizedblue_channel[start_row_exudate_candidate_basedon_intensity[eachexudatecandidate]:end_row_exudate_candidate_basedon_intensity[eachexudatecandidate],start_col_exudate_candidate_basedon_intensity[eachexudatecandidate]:end_col_exudate_candidate_basedon_intensity[eachexudatecandidate]]);
        
        min_green=min(resizedgreen_channel[start_row_exudate_candidate_basedon_intensity[eachexudatecandidate]:end_row_exudate_candidate_basedon_intensity[eachexudatecandidate],start_col_exudate_candidate_basedon_intensity[eachexudatecandidate]:end_col_exudate_candidate_basedon_intensity[eachexudatecandidate]]);
        
        max_allchannels=c(max_red,max_green,max_blue);
        min_allchannels=c(min_red,min_green,min_blue);
        
        max_channel_index=which(max_allchannels==max(max_allchannels));
        min_channel_index=which(min_allchannels==min(min_allchannels));
        
        # If Red is max, then Hue = (G-B)/(max-min)
        # If Green is max, then Hue = 2.0 + (B-R)/(max-min)
        # If Blue is max, then Hue = 4.0 + (R-G)/(max-min)
        
        if(max_channel_index==1){ # Max. Red channel
          
          Hue_calculated[eachexudatecandidate]=(max_green-max_blue)/(max_red-min_allchannels[min_channel_index]);
          Hue_calculated[eachexudatecandidate]=Hue_calculated[eachexudatecandidate]*60;
          if(is.na(Hue_calculated[eachexudatecandidate])){
            Hue_calculated[eachexudatecandidate]=0;  
          } else if(Hue_calculated[eachexudatecandidate]<0){
            
            Hue_calculated[eachexudatecandidate]=360;  
            
          }
        } else if(max_channel_index==2){ # Max. Green channel
          
          Hue_calculated[eachexudatecandidate] = 2.0 + (max_blue-max_red)/(max_green-min_allchannels[min_channel_index]);
          
          if(is.na(Hue_calculated[eachexudatecandidate])){
            Hue_calculated[eachexudatecandidate]=0;  
          } else if(Hue_calculated[eachexudatecandidate]<0){
            
            Hue_calculated[eachexudatecandidate]=360;  
            
          }
        } else if(max_channel_index==3){ # Max. Blue channel
          Hue_calculated[eachexudatecandidate] = 4.0 + (max_red-max_green)/(max_blue-min_allchannels[min_channel_index]);
          if(is.na(Hue_calculated[eachexudatecandidate])){
            Hue_calculated[eachexudatecandidate]=0;  
          } else if(Hue_calculated[eachexudatecandidate]<0){
            
            Hue_calculated[eachexudatecandidate]=360;  
            
          }
        } 
        
      }
      
      Hue_calculated=Hue_calculated[!is.na(Hue_calculated)];
      Hue_criterion_greaterthan<-which(Hue_calculated>=he_values_min,arr.ind=TRUE);
      Hue_criterion_lesserthan<-which(Hue_calculated<=he_values_max,arr.ind=TRUE);
      finalHue_candidates<-intersect(Hue_criterion_greaterthan,Hue_criterion_lesserthan);
      
      rowstart_curr_exudate_found<-vector();
      rowend_curr_exudate_found<-vector();
      colstart_curr_exudate_found<-vector();
      colend_curr_exudate_found<-vector();
      if(length(finalHue_candidates)!=0)
      {
        flag_exudates=1;
        cat("Exudates found","\n");
        for (eachidentifiedexudate in 1:length(finalHue_candidates))
        {
          curr_exudate_found<-matrix(data = 0,nrow = dimensions_resized[1],ncol = dimensions_resized[2]);
          curr_exudate_found<-(reference_exudates_region==finalHue_candidates[eachidentifiedexudate]);
          curr_exudate_found_indices<-which(curr_exudate_found==1,arr.ind = TRUE);
          rowstart_curr_exudate_found[eachidentifiedexudate]<-min(curr_exudate_found_indices[,1]);
          colstart_curr_exudate_found[eachidentifiedexudate]<-min(curr_exudate_found_indices[,2]);
          rowend_curr_exudate_found[eachidentifiedexudate]<-max(curr_exudate_found_indices[,1]);
          colend_curr_exudate_found[eachidentifiedexudate]<-max(curr_exudate_found_indices[,2]);
        } 
      } else {
        cat("Exudates not found","\n");
        flag_exudates=0;
      } 
    } else{
      cat("Exudates not found","\n");
      flag_exudates=0;
    }
  }
  
  cat("finished successfully","\n");
  
  if (flagD_R == 0 && flagVASC_QUAL == 0 && flagNull_Matrix_Check == 0) {
    set.seed(42);
    removedframe_finalvasculature_hemorrhages<-removedframe_labeled_final_vasculature_extracted;
    ### Mask hard Exudates 
    if(flag_exudates==1)
    {
      for (eachidentifiedexudate in 1:length(finalHue_candidates))
      {
        
        
        removedframe_finalvasculature_hemorrhages[rowstart_curr_exudate_found[eachidentifiedexudate]:rowend_curr_exudate_found[eachidentifiedexudate],colstart_curr_exudate_found[eachidentifiedexudate]:colend_curr_exudate_found[eachidentifiedexudate]]<-0;
      }
    }
    
    if(macula_rightOD_flag==1)
    {
      removedframe_finalvasculature_hemorrhages[min(OD_indices[,1]):ref_exudatesrefregion_upperbound,min(OD_indices[,2]):max(OD_indices[,2])]=0;
    }else
    {
      removedframe_finalvasculature_hemorrhages[ref_exudatesrefregion_lowerbound:ref_exudatesrefregion_upperbound,min(OD_indices[,2]):max(OD_indices[,2])]=0;
    }
    
    threshold_hemcandidates<-hem_value;
    binary_topleft_quadrant<-matrix(data = 0,nrow = dimensions_resized[1],ncol = dimensions_resized[2]);
    binary_topleft_quadrant[1:centroid_OD_xcoordinate,1:centroid_OD_ycoordinate]=removedframe_finalvasculature_hemorrhages[1:centroid_OD_xcoordinate,1:centroid_OD_ycoordinate];
    binary_topleft_quadrant=bwlabel(binary_topleft_quadrant);
    
    if(length(unique(c(c(binary_topleft_quadrant))))!=1)
    {
      flag_topleftquadrant_hemorrhages<-0;
      moments_topleft_quadrant<-computeFeatures.moment(binary_topleft_quadrant);
      objs_vessel_candidates<-which(moments_topleft_quadrant[,'m.eccentricity']>=threshold_hemcandidates,arr.ind = TRUE);
      binary_topleft_quadrant<-rmObjects(binary_topleft_quadrant,objs_vessel_candidates);
      
      labeled_binary_topleft_quadrant<-bwlabel(binary_topleft_quadrant);
      
      intensities_topleft_quadrant<-computeFeatures.basic(labeled_binary_topleft_quadrant,ref = Input_MyImage_resized,properties = FALSE);
      summary_objsintensities_topleft_quadrant<-summary(intensities_topleft_quadrant[,'b.mean']);
      
      brightobs_nothemorrhages<-which(intensities_topleft_quadrant[,'b.mean']>=summary_objsintensities_topleft_quadrant[2],arr.ind = TRUE);
      resultant_binary_topleftquadrant<-rmObjects(labeled_binary_topleft_quadrant,brightobs_nothemorrhages);
    } else{
      flag_topleftquadrant_hemorrhages<-1;
    }
    
    binary_topright_quadrant<-matrix(data = 0,nrow = dimensions_resized[1],ncol = dimensions_resized[2]);
    binary_topright_quadrant[centroid_OD_xcoordinate:dimensions_resized[2],1:centroid_OD_ycoordinate]=removedframe_finalvasculature_hemorrhages[centroid_OD_xcoordinate:dimensions_resized[2],1:centroid_OD_ycoordinate];
    if(length(unique(c(c(binary_topright_quadrant))))!=1)
    {
      flag_toprightquadrant_hemorrhages<-0;
      binary_topright_quadrant=bwlabel(binary_topright_quadrant);
      moments_topright_quadrant<-computeFeatures.moment(binary_topright_quadrant);
      objs_vessel_candidates<-which(moments_topright_quadrant[,'m.eccentricity']>=threshold_hemcandidates,arr.ind = TRUE);
      binary_topright_quadrant<-rmObjects(binary_topright_quadrant,objs_vessel_candidates);
      labeled_binary_topright_quadrant<-bwlabel(binary_topright_quadrant);
      intensities_topright_quadrant<-computeFeatures.basic(labeled_binary_topright_quadrant,ref = Input_MyImage_resized,properties = FALSE);
      summary_objsintensities_topright_quadrant<-summary(intensities_topright_quadrant[,'b.mean']);
      brightobs_nothemorrhages<-which(intensities_topright_quadrant[,'b.mean']>=summary_objsintensities_topright_quadrant[2],arr.ind = TRUE);
      resultant_binary_toprightquadrant<-rmObjects(labeled_binary_topright_quadrant,brightobs_nothemorrhages);
    } else{
      flag_toprightquadrant_hemorrhages<-1;
    }
    
    binary_bottomleft_quadrant<-matrix(data = 0,nrow = dimensions_resized[1],ncol = dimensions_resized[2]);
    binary_bottomleft_quadrant[1:centroid_OD_xcoordinate,centroid_OD_ycoordinate:dimensions_resized[2]]=removedframe_finalvasculature_hemorrhages[1:centroid_OD_xcoordinate,centroid_OD_ycoordinate:dimensions_resized[2]];
    if(length(unique(c(c(binary_bottomleft_quadrant))))!=1)
    {
      flag_bottomleftquadrant_hemorrhages<-0;
      binary_bottomleft_quadrant=bwlabel(binary_bottomleft_quadrant);
      moments_bottomleft_quadrant<-computeFeatures.moment(binary_bottomleft_quadrant);
      objs_vessel_candidates<-which(round(moments_bottomleft_quadrant[,'m.eccentricity'])>=threshold_hemcandidates,arr.ind = TRUE);
      binary_bottomleft_quadrant<-rmObjects(binary_bottomleft_quadrant,objs_vessel_candidates);
      labeled_binary_bottomleft_quadrant<-bwlabel(binary_bottomleft_quadrant);
      intensities_bottomleft_quadrant<-computeFeatures.basic(labeled_binary_bottomleft_quadrant,ref = Input_MyImage_resized,properties = FALSE);
      summary_objsintensities_bottomleft_quadrant<-summary(intensities_bottomleft_quadrant[,'b.mean']);
      brightobs_nothemorrhages<-which(intensities_bottomleft_quadrant[,'b.mean']>=summary_objsintensities_bottomleft_quadrant[2],arr.ind = TRUE);
      resultant_binary_bottomleftquadrant<-rmObjects(labeled_binary_bottomleft_quadrant,brightobs_nothemorrhages);
    } else{
      flag_bottomleftquadrant_hemorrhages<-1;
    }
    
    binary_bottomright_quadrant<-matrix(data = 0,nrow = dimensions_resized[1],ncol = dimensions_resized[2]);
    binary_bottomright_quadrant[centroid_OD_xcoordinate:dimensions_resized[1],centroid_OD_ycoordinate:dimensions_resized[2]]=removedframe_finalvasculature_hemorrhages[centroid_OD_xcoordinate:dimensions_resized[1],centroid_OD_ycoordinate:dimensions_resized[2]];
    
    if(length(unique(c(c(binary_bottomright_quadrant))))!=1)
    {
      flag_bottomrightquadrant_hemorrhages<-0;
      binary_bottomright_quadrant=bwlabel(binary_bottomright_quadrant);
      
      moments_bottomright_quadrant<-computeFeatures.moment(binary_bottomright_quadrant);
      objs_vessel_candidates<-which(round(moments_bottomright_quadrant[,'m.eccentricity'])>=threshold_hemcandidates,arr.ind = TRUE);
      binary_bottomright_quadrant<-rmObjects(binary_bottomright_quadrant,objs_vessel_candidates);
      labeled_binary_bottomright_quadrant<-bwlabel(binary_bottomright_quadrant);
      intensities_bottomright_quadrant<-computeFeatures.basic(labeled_binary_bottomright_quadrant,ref = Input_MyImage_resized,properties = FALSE);
      summary_objsintensities_bottomright_quadrant<-summary(intensities_bottomright_quadrant[,'b.mean']);
      brightobs_nothemorrhages<-which(intensities_bottomright_quadrant[,'b.mean']>=summary_objsintensities_bottomright_quadrant[2],arr.ind = TRUE);
      resultant_binary_bottomrightquadrant<-rmObjects(labeled_binary_bottomright_quadrant,brightobs_nothemorrhages);
    } else{
      flag_bottomrightquadrant_hemorrhages<-1;
    }
    
    hemorrhages_Input_resized=Input_MyImage_resized;
    
    numofquadrants=1;
    while(numofquadrants<=4)
    {
      
      if(numofquadrants==1 & flag_topleftquadrant_hemorrhages==0)
      {
        curr_quadrant=resultant_binary_topleftquadrant;
      }else if(numofquadrants==2 & flag_toprightquadrant_hemorrhages==0)
      {
        curr_quadrant=resultant_binary_toprightquadrant;
        
      }else if(numofquadrants==3 & flag_bottomleftquadrant_hemorrhages==0)
      {
        curr_quadrant=resultant_binary_bottomleftquadrant;
        
      }else if(numofquadrants==4 & flag_bottomrightquadrant_hemorrhages==0)
      {
        curr_quadrant=resultant_binary_bottomrightquadrant;
      }
      
      forradius_hemcandidate=computeFeatures.shape(curr_quadrant);
      
      numofhem_candidates=1;
      while(numofhem_candidates<=max(curr_quadrant))
      {
        curr_hem_candidate=which((curr_quadrant==numofhem_candidates)==1,arr.ind=TRUE);
        
        if(forradius_hemcandidate[,'s.radius.mean'][numofhem_candidates]==0)
        {
          numofhem_candidates=numofhem_candidates+1;
        }else
        {
          
          if(round(max(curr_hem_candidate[,1])-min(curr_hem_candidate[,1]))/2<1&&round(max(curr_hem_candidate[,1])-min(curr_hem_candidate[,1]))/2>0)
          {
            radius_currhem_candidate=1;
          }
          else
          {
            radius_currhem_candidate=round(max(curr_hem_candidate[,1])-min(curr_hem_candidate[,1]))/2;
          }
          final_hemorrhages_Input_resized<-drawCircle(hemorrhages_Input_resized,x = min(curr_hem_candidate[,1]),y = min(curr_hem_candidate[,2]),radius = radius_currhem_candidate,col = 'green',fill = FALSE); 
        }  
        numofhem_candidates=numofhem_candidates+1;  
      }
      
      cat("Next quadrant","\n");  
      numofquadrants=numofquadrants+1;
    }
    
    #######################
    numofquadrants=1;
    numofhem_candidates=1;
    final_hemorrhages_rowstart_indices<-vector();
    final_hemorrhages_colstart_indices<-vector();
    final_hemorrhages_rowend_indices<-vector();
    final_hemorrhages_colend_indices<-vector();
    
    while(numofquadrants<=4)
    {
      
      if(numofquadrants==1 & flag_topleftquadrant_hemorrhages==0)
      {
        curr_quadrant=resultant_binary_topleftquadrant;
      }else if(numofquadrants==2 & flag_toprightquadrant_hemorrhages==0)
      {
        curr_quadrant=resultant_binary_toprightquadrant;
        
      }else if(numofquadrants==3 & flag_bottomleftquadrant_hemorrhages==0)
      {
        curr_quadrant=resultant_binary_bottomleftquadrant;
        
      }else if(numofquadrants==4 & flag_bottomrightquadrant_hemorrhages==0)
      {
        curr_quadrant=resultant_binary_bottomrightquadrant;
      }
      
      numofhem_candidates_currquadrant<-1;
      while(numofhem_candidates_currquadrant<=max(curr_quadrant))
      {
        curr_hem_candidate=which((curr_quadrant==numofhem_candidates_currquadrant)==1,arr.ind=TRUE);
        final_hemorrhages_rowstart_indices[numofhem_candidates]<-min(curr_hem_candidate[,1]);
        final_hemorrhages_colstart_indices[numofhem_candidates]<-min(curr_hem_candidate[,2]);
        final_hemorrhages_rowend_indices[numofhem_candidates]<-max(curr_hem_candidate[,1]);
        final_hemorrhages_colend_indices[numofhem_candidates]<-max(curr_hem_candidate[,2]);
        numofhem_candidates_currquadrant=numofhem_candidates_currquadrant+1;
        numofhem_candidates=numofhem_candidates+1;
      }
      numofquadrants<-numofquadrants+1;
    }
    
    if(numofhem_candidates==1)
    {
      cat("hemorrhages not found","\n");
      flag_hem=0;
    }else
    {
      cat("hemorrhages found","\n");
      flag_hem=1;
    }
  }
  
  cat("finished successfully","\n");
  
  if (flagD_R == 0 && flagVASC_QUAL == 0 && flagNull_Matrix_Check == 0) {
    set.seed(42);
    #forcws_colorImage=Input_MyImage_resized;
    BestFrame_resized_cws=BestFrame_resized;
    if(macula_rightOD_flag==1)
    {
      BestFrame_resized_cws[min(OD_indices[,1]):ref_exudatesrefregion_upperbound,min(OD_indices[,2]):max(OD_indices[,2])]=0;
    }else
    {
      BestFrame_resized_cws[ref_exudatesrefregion_lowerbound:ref_exudatesrefregion_upperbound,min(OD_indices[,2]):max(OD_indices[,2])]=0;
    }
    
    mean_BestFrameresized=mean(c(c(BestFrame_resized_cws)));
    sd_BestFrameresized=sd(c(c(BestFrame_resized_cws)));
    
    segmented_cws_regions=(BestFrame_resized_cws>(mean_BestFrameresized+(1.1*sd_BestFrameresized)));
    segmented_cws_regions=bwlabel(segmented_cws_regions);
    
    
    areacheck_probablecws=computeFeatures.shape(segmented_cws_regions);
    false_cwscandidates_higharea=which(areacheck_probablecws[,'s.area']>=(3*radius_opticdisc),arr.ind=TRUE);
    false_cwscandidates_lowarea=which(areacheck_probablecws[,'s.area']<=10,arr.ind=TRUE);
    false_cwscandidates_lowarea<-unique(false_cwscandidates_lowarea[!is.na(false_cwscandidates_lowarea)]);
    false_cwscandidates_higharea<-unique(false_cwscandidates_higharea[!is.na(false_cwscandidates_higharea)]);
    
    if(length(false_cwscandidates_lowarea)!=0 && length(false_cwscandidates_higharea)!=0){
      false_cwscandidates<-union(unique(false_cwscandidates_higharea),unique(false_cwscandidates_lowarea));
    } else if(length(false_cwscandidates_lowarea)==0 && length(false_cwscandidates_higharea)!=0){
      false_cwscandidates<-false_cwscandidates_higharea;
    } else if(length(false_cwscandidates_higharea)==0 && length(false_cwscandidates_lowarea)!=0){
      false_cwscandidates<-false_cwscandidates_lowarea;
    }
    
    segmented_cws_regions<-rmObjects(segmented_cws_regions,false_cwscandidates);
    
    if(length(unique(c(c(segmented_cws_regions))))==1)
    {
      cat("No Cotton wool spots found","\n");
      flag_cws=0;
    } else {
      ### Mask Hard Exudates
      
      if(flag_exudates==1)
      {
        for (eachidentifiedexudate in 1:length(finalHue_candidates))
        {
          segmented_cws_regions[rowstart_curr_exudate_found[eachidentifiedexudate]:rowend_curr_exudate_found[eachidentifiedexudate],colstart_curr_exudate_found[eachidentifiedexudate]:colend_curr_exudate_found[eachidentifiedexudate]]<-0;
        }
      }
      
      #### Mask Hemorrhages
      if(flag_hem==1)
      {
        numofhem_candidates_allquadrants=1;
        while(numofhem_candidates_allquadrants<=length(final_hemorrhages_rowstart_indices))
        {
          segmented_cws_regions[final_hemorrhages_rowstart_indices[numofhem_candidates_allquadrants]:final_hemorrhages_rowend_indices[numofhem_candidates_allquadrants],final_hemorrhages_colstart_indices[numofhem_candidates_allquadrants]:final_hemorrhages_colend_indices[numofhem_candidates_allquadrants]]<-0;
          numofhem_candidates_allquadrants<-numofhem_candidates_allquadrants+1;
        }
      }
      
    }
    
    if (max(segmented_cws_regions) > 0) {
      Hue_candidatecws<-vector();
      
      for(eachprob_cwsregion in 1:max(segmented_cws_regions))
      {
        curr_cwsregion=array(0,dim=c(dimensions_resized[1],dimensions_resized[2]));
        curr_cwsregion=(segmented_cws_regions==eachprob_cwsregion);
        
        curr_cwsregion_indices=which(curr_cwsregion==1,arr.ind = TRUE);
        if(length(curr_cwsregion_indices)!=0)
        {
          max_red=max(resizedred_channel[min(curr_cwsregion_indices[,1]):max(curr_cwsregion_indices[,1]),min(curr_cwsregion_indices[,2]):max(curr_cwsregion_indices[,2])]);
          max_blue=max(resizedblue_channel[min(curr_cwsregion_indices[,1]):max(curr_cwsregion_indices[,1]),min(curr_cwsregion_indices[,2]):max(curr_cwsregion_indices[,2])]);
          
          max_green=max(resizedgreen_channel[min(curr_cwsregion_indices[,1]):max(curr_cwsregion_indices[,1]),min(curr_cwsregion_indices[,2]):max(curr_cwsregion_indices[,2])]);
          
          min_red=min(resizedred_channel[min(curr_cwsregion_indices[,1]):max(curr_cwsregion_indices[,1]),min(curr_cwsregion_indices[,2]):max(curr_cwsregion_indices[,2])]);
          
          min_blue=min(resizedblue_channel[min(curr_cwsregion_indices[,1]):max(curr_cwsregion_indices[,1]),min(curr_cwsregion_indices[,2]):max(curr_cwsregion_indices[,2])]);
          
          min_green=min(resizedgreen_channel[min(curr_cwsregion_indices[,1]):max(curr_cwsregion_indices[,1]),min(curr_cwsregion_indices[,2]):max(curr_cwsregion_indices[,2])]);
          
          max_allchannels=c(max_red,max_green,max_blue);
          min_allchannels=c(min_red,min_green,min_blue);
          
          max_channel_index=which(max_allchannels==max(max_allchannels));
          min_channel_index=which(min_allchannels==min(min_allchannels));
          
          
          # If Red is max, then Hue = (G-B)/(max-min)
          # If Green is max, then Hue = 2.0 + (B-R)/(max-min)
          # If Blue is max, then Hue = 4.0 + (R-G)/(max-min)
          
          if(max_channel_index[1]==1){ # Max. Red channel # Take only the first element incase max_channel_index is a vector and not a scalar
            
            Hue_candidatecws[eachprob_cwsregion]=(max_green-max_blue)/(max_red-min_allchannels[min_channel_index]);
            Hue_candidatecws[eachprob_cwsregion]=  Hue_candidatecws[eachprob_cwsregion]*60;
            if(is.na(Hue_candidatecws[eachprob_cwsregion]))
            {
              Hue_candidatecws[eachprob_cwsregion]= 0;
            } else if(Hue_candidatecws[eachprob_cwsregion]<0){
              Hue_candidatecws[eachprob_cwsregion]=  360;  
            }
            
          }else if(max_channel_index[1]==2){ # Max. Green channel
            
            Hue_candidatecws[eachprob_cwsregion] = 2.0 + (max_blue-max_red)/(max_green-min_allchannels[min_channel_index]);
            
            if(is.na(Hue_candidatecws[eachprob_cwsregion]))
            {
              Hue_candidatecws[eachprob_cwsregion]= 0;
            } else if(  Hue_candidatecws[eachprob_cwsregion]<0){
              Hue_candidatecws[eachprob_cwsregion]=  360;  
            }
            
          }else if(max_channel_index[1]==3){ # Max. Blue channel
            Hue_candidatecws[eachprob_cwsregion] = 4.0 + (max_red-max_green)/(max_blue-min_allchannels[min_channel_index]);
            if(is.na(Hue_candidatecws[eachprob_cwsregion]))
            {
              Hue_candidatecws[eachprob_cwsregion]= 0;
            } else if(  Hue_candidatecws[eachprob_cwsregion]<0){
              Hue_candidatecws[eachprob_cwsregion]=  360;  
            }
            
          } 
          
          remove(curr_cwsregion_indices);
        }
        else{
          Hue_candidatecws[eachprob_cwsregion]<-0;
        }
      }
      Hue_candidatecws[is.na(Hue_candidatecws)]=0;
      Hue_candidatecws <- Hue_candidatecws[ Hue_candidatecws != 0 ];
      
      if(length(Hue_candidatecws)!=0){
        probable_cwscandidate_indices_lessthanthreshold=which(Hue_candidatecws<=cws_values_max,arr.ind = TRUE);
        probable_cwscandidate_indices_greaterthanthreshold=which(Hue_candidatecws>=cws_values_min,arr.ind = TRUE);
        probable_cwscandidate_indices<-intersect(probable_cwscandidate_indices_lessthanthreshold,probable_cwscandidate_indices_greaterthanthreshold);
        flag_cwsHuecheck<-1;
      } else{
        flag_cwsHuecheck<-0;
      }
      
      segmented_cws_regions<-bwlabel(segmented_cws_regions);
    }
    
    if(max(segmented_cws_regions)==0 || length(probable_cwscandidate_indices)==0 || flag_cwsHuecheck==0)
    {
      cat("No Cotton wool spots found","\n");
      flag_cws=0;
    } else
    {
      
      cat("Cotton wool spots found","\n");
      flag_cws=1;
      rowstart_cws<-vector();
      rowend_cws<-vector();
      colstart_cws<-vector();
      colend_cws<-vector();
      eachcws_found=1;
      while(eachcws_found <= length(probable_cwscandidate_indices))
      {
        cws_indices<-which(segmented_cws_regions==probable_cwscandidate_indices[eachcws_found],arr.ind = TRUE);
        rowstart_cws[eachcws_found]<-min(cws_indices[,1]);
        rowend_cws[eachcws_found]<-max(cws_indices[,1]);
        colstart_cws[eachcws_found]<-min(cws_indices[,2]);
        colend_cws[eachcws_found]<-max(cws_indices[,2]);
        eachcws_found=eachcws_found+1;
      }
    } 
  }
  
  cat("finished successfully","\n");
  
  if(flagD_R==1 || flagVASC_QUAL==1) # Image with Vitreous Hemorrhage(s) 
  {
    if (flagD_R == 1) {
      notificationId <- showNotification("Probable Vitreous Hemorrhage", duration = NA, type = "error")
    } else if (flagVASC_QUAL == 1) {
      showNotification("Please retake picture, Underexposed image", duration = NA, type = "error")
    }
  } else
  {
    report_Input_MyImage_resized<-Input_MyImage_resized;
    
    if(flag_exudates==1){
      for (eachidentifiedexudate in 1:length(finalHue_candidates))
      {
        report_Input_MyImage_resized<-drawCircle(report_Input_MyImage_resized,x = rowstart_curr_exudate_found[eachidentifiedexudate],y = colstart_curr_exudate_found[eachidentifiedexudate],radius = 10,col = "green");
      }
    }
    if(flag_hem==1){
      for (eachidentifiedhemorrhage in 1:(numofhem_candidates-1))
      {
        report_Input_MyImage_resized<-drawCircle(report_Input_MyImage_resized,x = final_hemorrhages_rowstart_indices[eachidentifiedhemorrhage],y = final_hemorrhages_colstart_indices[eachidentifiedhemorrhage],radius = 10,col = "blue");
      }  
    }
    if(flag_cws==1){
      for (eachidentifiedcws in 1:length(probable_cwscandidate_indices))
      {
        report_Input_MyImage_resized<-drawCircle(report_Input_MyImage_resized,x = rowstart_cws[eachidentifiedcws],y = colstart_cws[eachidentifiedcws],radius = 10,col = "violet");
      }
    }
    
    # if (flagU_E == 1) {
    #   showNotification("Probable Under Exposed Image", duration = NA, type = "error")
    # } else if (flagO_E == 1) {
    #   showNotification("Probable Over Exposed Image", duration = NA, type = "error")
    # } else if (flag_NV == 1) {
    #   showNotification("Probable Neo Vascularization", duration = NA, type = "error")
    # }
  }
  
  
  split_filename<-strsplit(image_name,'.',TRUE);
  file_ext <- paste(".", split_filename[[1]][2], sep = "");
  curr_filename<-split_filename[[1]][1];
  
  reportfile <- tempfile(fileext = file_ext)
  writeJPEG(transpose(report_Input_MyImage_resized), reportfile)
  
  revisedfile <- tempfile(fileext = file_ext)
  writeJPEG(transpose(Input_MyImage_resized), revisedfile)
  
  put_object(file = revisedfile, object = paste(curr_filename, "_revised", file_ext, sep = ""), bucket = "okio")
  put_object(file = reportfile, object = paste(curr_filename, "_report", file_ext, sep = ""), bucket = "okio")
  
  file.remove(paste(getwd(), "/", image_name, sep = ""))

}