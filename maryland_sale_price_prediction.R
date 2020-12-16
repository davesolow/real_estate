#load packages
library(RSocrata)
library(dplyr)
library(ggplot2)
library(scales)
library(caTools)

#Get raw dataset
df <- read.socrata(
"https://opendata.maryland.gov/resource/ed4q-f8tm.json?$query=SELECT county_name_mdp_field_cntyname,real_property_search_link,record_key_county_code_sdat_field_1,record_key_district_ward_sdat_field_2,mdp_street_address_mdp_field_address,mdp_street_address_city_mdp_field_city,mdp_street_address_zip_code_mdp_field_zipcode,zoning_code_mdp_field_zoning_sdat_field_45,sales_segment_1_transfer_number_mdp_field_transno1_sdat_field_79,sales_segment_1_transfer_date_yyyy_mm_dd_mdp_field_tradate_sdat_field_89,sales_segment_1_consideration_mdp_field_considr1_sdat_field_90,sales_segment_1_mkt_land_value_sdat_field_95,base_cycle_data_date_assessed_yyyy_mm_sdat_field_158,sales_segment_1_mkt_improvement_value_sdat_field_96,prior_assessment_year_total_assessment_sdat_field_161,c_a_m_a_system_data_number_of_dwelling_units_mdp_field_bldg_units_sdat_field_239,c_a_m_a_system_data_structure_area_sq_ft_mdp_field_sqftstrc_sdat_field_241 WHERE sales_segment_1_transfer_date_yyyy_mm_dd_mdp_field_tradate_sdat_field_89 between '2017-03-01' and '2020-02-29' AND c_a_m_a_system_data_number_of_dwelling_units_mdp_field_bldg_units_sdat_field_239 > 1 AND premise_address_zip_code_mdp_field_premzip_sdat_field_26 = 21222 OR premise_address_zip_code_mdp_field_premzip_sdat_field_26 = 21224"
)
# Local
#df <- read.csv("Maryland_Real_Property_Assessments__Hidden_Property_Owner_Names.csv", stringsAsFactors = FALSE)
#df$SALES.SEGMENT.1..Transfer.Date..YYYY.MM.DD...MDP.Field..TRADATE..SDAT.Field..89. <- df$SALES.SEGMENT.1..Transfer.Date..YYYY.MM.DD...MDP.Field..TRADATE..SDAT.Field..89. %>% as.Date(format = "%Y.%m.%d")

#create model dataset
model_dataset <- subset(df, 
                       select = c(County.Name..MDP.Field..CNTYNAME.,
                                  Account.ID..MDP.Field..ACCTID.,
                                  Real.Property.Search.Link,
                                  MDP.Street.Address..MDP.Field..ADDRESS.,
                                  MDP.Street.Address.City..MDP.Field..CITY.,
                                  MDP.Street.Address.Zip.Code..MDP.Field..ZIPCODE.,
                                  Zoning.Code..MDP.Field..ZONING..SDAT.Field..45.,
                                  Land.Use.Code..MDP.Field..LU.DESCLU..SDAT.Field..50.,
                                  SALES.SEGMENT.1..Transfer.Date..YYYY.MM.DD...MDP.Field..TRADATE..SDAT.Field..89.,
                                  SALES.SEGMENT.1..Consideration..MDP.Field..CONSIDR1..SDAT.Field..90.,
                                  CURRENT.CYCLE.DATA..Date.Assessed..YYYY.MM...MDP.Field..LASTASSD..SDAT.Field..169.,
                                  CURRENT.ASSESSMENT.YEAR..Total.Assessment..SDAT.Field..172.,
                                  C.A.M.A..SYSTEM.DATA..Number.of.Dwelling.Units..MDP.Field..BLDG_UNITS..SDAT.Field..239.,
                                  C.A.M.A..SYSTEM.DATA..Structure.Area..Sq.Ft....MDP.Field..SQFTSTRC..SDAT.Field..241.))
colnames(model_dataset) <- c("County",
                            "Account_ID",
                            "SDAT_Link",
                            "Street_Address",
                            "City",
                            "Zip",
                            "Zoning_Code",
                            "Land_Use_Code",
                            "Sale_Date",
                            "Sale_Amount",
                            "Assessment_Date",
                            "Assessment_Amount",
                            "Number_of_Units",
                            "Square_Feet")
predict_dataset <- model_dataset[model_dataset$Street_Address == "1109 S CURLEY ST",]
model_dataset <- model_dataset[model_dataset$Street_Address != "1109 S CURLEY ST",]
model_dataset <- model_dataset[model_dataset$Land_Use_Code == "Residential (R)" | model_dataset$Land_Use_Code == "Apartments (M)",]
model_dataset <- model_dataset[model_dataset$Sale_Date >= "2017-03-01" & model_dataset$Sale_Date <= "2020-02-29",]
model_dataset <- model_dataset[model_dataset$Sale_Amount > 0,]
model_dataset <- model_dataset[model_dataset$Square_Feet > 0,]
model_dataset <- model_dataset[!is.na(model_dataset$Account_ID),]
model_dataset <- subset(model_dataset, 
                        select = c(
                          Zip,
                          Zoning_Code,
                          Land_Use_Code,
                          Number_of_Units,
                          Assessment_Amount,
                          Square_Feet,
                          Sale_Amount))
model_dataset$Zip <- factor(model_dataset$Zip,
                            levels = unique(model_dataset$Zip),
                            labels = 1:length(unique(model_dataset$Zip)))
model_dataset$Zoning_Code <- factor(model_dataset$Zoning_Code,
                            levels = unique(model_dataset$Zoning_Code),
                            labels = 1:length(unique(model_dataset$Zoning_Code)))
model_dataset$Land_Use_Code <- factor(model_dataset$Land_Use_Code,
                            levels = unique(model_dataset$Land_Use_Code),
                            labels = 1:length(unique(model_dataset$Land_Use_Code)))
predict_dataset <- predict_dataset[1,]
predict_dataset <- subset(predict_dataset,
                          select = c(
                            Zip,
                            Zoning_Code,
                            Land_Use_Code,
                            Number_of_Units,
                            Assessment_Amount,
                            Square_Feet,
                            Sale_Amount))
predict_dataset$Zip <- factor(predict_dataset$Zip,
                            levels = unique(predict_dataset$Zip),
                            labels = 1:length(unique(predict_dataset$Zip)))
predict_dataset$Zoning_Code <- factor(predict_dataset$Zoning_Code,
                                    levels = unique(predict_dataset$Zoning_Code),
                                    labels = 1:length(unique(predict_dataset$Zoning_Code)))
predict_dataset$Land_Use_Code <- factor(predict_dataset$Land_Use_Code,
                                      levels = unique(predict_dataset$Land_Use_Code),
                                      labels = 1:length(unique(predict_dataset$Land_Use_Code)))

#fit model
regressor = lm(Sale_Amount ~ .,
               data = model_dataset)
summary(regressor)

#predict sale price for one property
prediction = predict(regressor, newdata = predict_dataset)
print(paste("PREDICTED SALE PRICE: ", prediction))