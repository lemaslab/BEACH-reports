#' dietrecord2redcap Function
#' Description: Formats the diet-record data for import to redcap

dietrecord2redcap <- function(diet_wide, redcap_event_name) {
  data_long=diet_wide %>%
    rename( wgt_g="Wgt (g)",
            cals_kcal="Cals (kcal)",
            fatcals_kcal="FatCals (kcal)",
            satcals_kcal="SatCals (kcal)",
            prot_g="Prot (g)",
            carb_g="Carb (g)",
            totfib_g="TotFib (g)",
            totsolfib_g="TotSolFib (g)",
            fib16_g="Fib(16) (g)",
            solfib16_g="SolFib(16) (g)",
            sugar_g="Sugar (g)",
            sugadd_g="SugAdd (g)",
            monsac_g="MonSac (g)",
            disacc_g="Disacc (g)",
            ocarb_g="OCarb (g)",
            fat_g="Fat (g)",
            satfat_g="SatFat (g)",
            monofat_g="MonoFat (g)",
            polyfat_g="PolyFat (g)",
            transfat_g="TransFat (g)",
            chol_mg="Chol (mg)",
            water_g="Water (g)",
            vita_iu="Vit A-IU (IU)",
            vita_mcg="Vit A-RAE (mcg)",
            caroten_mcg="Caroten (mcg)",
            retinol_mcg="Retinol (mcg)",
            betacaro_mcg="BetaCaro (mcg)",
            vitb1_mg="Vit B1 (mg)",
            vitb2_mg="Vit B2 (mg)",
            vitb3_mg="Vit B3 (mg)",
            vitb3_ne_mg="Vit B3-NE (mg)",
            vitb6_mg="Vit B6 (mg)",
            vitb12_mcg="Vit B12 (mcg)",
            biot_mcg="Biot (mcg)",
            vitc_mg="Vit C (mg)",
            vitd_iu="Vit D-IU (IU)",
            vitd_mcg="Vit D-mcg (mcg)",
            vite_mg="Vit E-a-Toco (mg)",
            folate_mcg="Folate (mcg)",
            foldfe_mcg="Fol-DFE (mcg DFE)",
            vitk_mcg="Vit K (mcg)",
            panto_mg="Panto (mg)",
            calc_mg="Calc (mg)",
            chrom_mcg="Chrom (mcg)",
            copp_mg="Copp (mg)",
            flour_mg="Fluor (mg)",
            iodine_mcg="Iodine (mcg)",
            iron_mg="Iron (mg)",
            magn_mg="Magn (mg)",
            mang_mg="Mang (mg)",
            moly_mcg="Moly (mcg)",
            phos_mg="Phos (mg)",
            pot_mg="Pot (mg)",
            sel_mcg="Sel (mcg)",
            sod_mg="Sod (mg)",
            zinc_mg="Zinc (mg)",
            omega3_g="Omega3 (g)",
            omega6_g="Omega6 (g)",
            alc_g="Alc (g)",
            caff_mg="Caff (mg)",
            chln_mg="Chln (mg)") %>%
    gather(key="food_record_nutrient", value="food_record_value", wgt_g:chln_mg) %>%
    mutate(food_record_value = replace(food_record_value, food_record_value == "--", NA),
           redcap_event_name=redcap_event_name,
           redcap_repeat_instrument="diet_record_processed",
           redcap_repeat_instance=row_number(),
           test_id=food_record_test_id) %>%
    select(test_id,redcap_event_name,redcap_repeat_instrument,redcap_repeat_instance,everything())
  
  return(data_long)
  
} # END FUNCTION



#' Get RedCap API Token
#' Description: Return string with api-token.
#' @return api-token
get_API_token <- function(credential_label){
  # Get Emoncms API Token
    credential_path <- paste(Sys.getenv("USERPROFILE"), '\\DPAPI\\passwords\\', Sys.info()["nodename"], '\\', credential_label, '.txt', sep="")
    token<-decrypt_dpapi_pw(credential_path)
  return(token)
  } # END FUNCTION

#' Outersect Function
#' Description: To find the non-duplicated elements between two or more vector

outersect <- function(x, y) {
  sort(c(setdiff(x, y),
         setdiff(y, x)))
} # END FUNCTION



#' Get RedCap Data
#' Description: Return tibble with ehr data.
#' @return tibble containing: part_id + "variable-of-interest"
getData_redcap <- function(api_token,uri,records,variables,col_types){
  
  # parameters to troubleshoot
  # records=mom_list[1:1500]
  # event_list=c("2011_arm_1")
  # uri='https://redcap.ctsi.ufl.edu/redcap/api/'
  # api_token
  
  # create part_id batch
    batchSize=100
    chunks=split(records, floor(1:(length(records))/batchSize))
    
  # START LOOP  
    pages <- list()
    
    for(i in 1:length(chunks)){
      redcap_data=redcap_read(batch_size=200, 
                          redcap_uri=uri, 
                          token=api_token,
                          records=chunks[[i]],
                          fields=variables,
                          col_types=col_types)$data
      pages[[i]] <- redcap_data
      } # END LOOP
    redcap_final=bind_rows(pages)
    return(redcap_final)
    } # end function