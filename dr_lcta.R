  
  # Load libraries
  libraries <- c(
    "tidyverse", "knitr", "kableExtra", "ggplot2", "ggtext", 
    "stringr", "forcats", "Cairo", "extrafont", "hrbrthemes", 
    "directlabels", "ggrepel", "readxl", "extrafont", "scales", 
    "ggrepel", "ggsci", "lcmm", "MixAll", "kml", "traj", "lcmm", 
    "lmerTest", "plyr", "tidyr", "psych", "fpc", "mclust", 
    "rcompanion", "gridExtra", "tidyLPA", "MASS", "broom", 
    "skimr", "gtExtras", "psych", "pander", "tibble", 
    "BayesFactor", "car", "elrm", "epiDisplay", "lessR", "stargazer", "patchwork",
    "openxlsx", "ROSE")
  
  # Load libraries
  lapply(libraries, library, character.only = TRUE)
  
  # EXPERIMENT 1 ------------------------------------------------------------
  
  
  ## All series --------------------------------------------------------------
  
  
  coverage_2011_2023 <- read.csv("all_series_diabetes.csv", sep=",")

  coverage_2011_2023 <- coverage_2011_2023 %>% 
    mutate(zona = ifelse(region %in% c("De Arica y Parinacota", "De Tarapacá", "De Antofagasta", "De Atacama", "De Coquimbo"), 1, 
                  ifelse(region %in% c("De Valparaíso", "Metropolitana de Santiago", "Del Libertador B. O'Higgins", "Del Maule"), 2, 3)))
  

  ## coverage ---------------------------------------------------------------
  
  coverage_2011_2023 <- coverage_2011_2023 %>% 
    filter(codigo_prestacion %in% c("P4150602","P4190950","P4190400", "P4180300")) %>% 
    dplyr::group_by(ano, comuna, id_servicio, id_region, zona, codigo_prestacion) %>% 
    dplyr::summarise(cantidad = round(sum(col01))) %>% 
    tidyr::spread(codigo_prestacion, cantidad) %>% 
    dplyr::rename(dm = P4150602,
                  dm_fo = P4190950,
                  dm_fo_2 =P4190400,
                  dm_hg_menor7 = P4180300) %>% 
    dplyr::mutate(dm_fo= coalesce(dm_fo, dm_fo_2)) %>% 
    dplyr::select(-dm_fo_2) %>% 
    dplyr::mutate(drs_coverage = dm_fo/dm,
                  dm_coverage = dm_hg_menor7/dm)
  
  
  # Check for duplicated communes ----------------------------------------------------
  
  

  coverage_2011_2023 %>% 
    group_by(ano) %>% 
    select(comuna) %>% 
    table() %>% 
    as.data.frame() %>% 
    arrange(-Freq)
  
  
  
  # fix duplicated comunas and unify in one only ------------------------------------------------
  
  
  pac <- coverage_2011_2023 %>% 
    ungroup() %>% 
    filter(str_detect(comuna, "Cerda")) %>% 
    select(-id_region, -id_servicio, -drs_coverage, -dm_coverage) %>% 
    group_by(ano, comuna) %>%
    summarise_all(~ if(is.numeric(.)) sum(., na.rm = TRUE) else first(.)) %>% 
    mutate(drs_coverage = dm_fo/dm,
           dm_coverage = dm_hg_menor7/dm,
           id_servicio = 13,
           id_region =13) %>% 
    select(ano,comuna,id_servicio,id_region,dm,dm_hg_menor7,dm_fo,drs_coverage, dm_coverage )
    
  
  santiago <- coverage_2011_2023 %>% 
    ungroup() %>% 
    filter(comuna== "Santiago") %>% 
    select(-id_region, -id_servicio, -drs_coverage, -dm_coverage) %>% 
    group_by(ano, comuna) %>%
    summarise_all(~ if(is.numeric(.)) sum(., na.rm = TRUE) else first(.)) %>%
    mutate(drs_coverage = dm_fo/dm,
           dm_coverage = dm_hg_menor7/dm,
           id_servicio = 11,
           id_region =13) %>% 
    select(ano,comuna,id_servicio,id_region,dm,dm_hg_menor7,dm_fo,drs_coverage, dm_coverage )
  
  la_granja <- coverage_2011_2023 %>% 
    ungroup() %>% 
    filter(comuna== "La Granja") %>% 
    select(-id_region, -id_servicio, -drs_coverage, -dm_coverage) %>% 
    group_by(ano, comuna) %>%
    summarise_all(~ if(is.numeric(.)) sum(., na.rm = TRUE) else first(.)) %>% 
    mutate(drs_coverage = dm_fo/dm,
           dm_coverage = dm_hg_menor7/dm,
           id_servicio = 14,
           id_region =13)%>% 
    select(ano,comuna,id_servicio,id_region,dm,dm_hg_menor7,dm_fo,drs_coverage, dm_coverage )
  
  
  # Remove duplicated comunes -----------------------------------------------
  
  
  coverage_2011_2023 <- coverage_2011_2023 %>% 
            filter(!comuna %in% c("Santiago", "La Granja"),  
            !str_detect(comuna, "Cerda")) 
  
  #Bind fixed comunes with the main dataset
  
  coverage_2011_2023 <- bind_rows(coverage_2011_2023, pac, santiago, la_granja)
  
  
  
  coverage_2011_2023 %>% 
    filter(grepl("La Granja", comuna))
  
  
  
  coverage_2011_2023 <- coverage_2011_2023 %>% 
    mutate(zona = ifelse(id_region %in% 13, 2, zona)) 
  
  

  # Check for duplicated communes ----------------------------------------------------
  
  
  coverage_2011_2023 %>%
    ungroup() %>%
    mutate(ano = as.character(as.numeric(ano))) %>% 
    group_by(ano) %>%
    summarise(cobertura_menor7 = sum(dm_hg_menor7, na.rm = TRUE) / sum(dm, na.rm = TRUE))
  
  coverage_2011_2023 %>%
    ungroup() %>%
    mutate(ano = factor(ano)) %>%
    filter(ano=="2023") %>% 
    group_by(ano) %>%
    summarise(total_dm = sum(dm, na.rm = TRUE), 
              total_dm_hg_menor7 = sum(dm_hg_menor7, na.rm = TRUE)) %>%
    mutate(cobertura_menor7 = total_dm_hg_menor7 / total_dm)
  
  
  #Categorise dm quintiles -------------------------------------------------
  
  coverage_2011_2023_noq1 <- coverage_2011_2023 %>%
    ungroup() %>% 
    mutate(quintil_dm_category = cut(dm,
                                      breaks = quantile(dm, probs = c(0, 0.2, 0.4, 0.6, 0.8, 1), na.rm = TRUE),
                                      labels = c("Q1", "Q2", "Q3", "Q4", "Q5"),
                                      include.lowest = TRUE))
  
  
  coverage_2011_2023 %>%
    ungroup() %>% 
    mutate(quintil_dm_category = cut(dm,
                                     breaks = quantile(dm, probs = c(0, 0.2, 0.4, 0.6, 0.8, 1), na.rm = TRUE),
                                     labels = c("Q1", "Q2", "Q3", "Q4", "Q5"),
                                     include.lowest = TRUE)) %>% 
    dplyr::filter(quintil_dm_category!= "Q1") %>%
    dplyr::mutate(comuna2 = as.integer(factor(comuna, levels = unique(comuna)))) %>% 
    dplyr::mutate(ano2 = ano - 2011) %>% 
    arrange(-drs_coverage) %>% 
    mutate(drs_coverage=replace(drs_coverage, drs_coverage>1, 1)) %>% 
    arrange(ano, comuna) 
  
  
  
  #%>% 
   # group_by(ano) %>% 
    #select(comuna) %>% 
    #table() %>% 
    #as.data.frame() %>% 
    #arrange(-Freq) 
  ## Save coverage.csv -------------------------------------------------------
  
  
  ## Delete Q1 category ------------------------------------------------------
  
  coverage_2011_2023_noq1 <- coverage_2011_2023_noq1 %>% 
    dplyr::filter(quintil_dm_category!= "Q1")
  
  # Add a simple indentifier to comuna --------------------------------------
  
  coverage_2011_2023_noq1$comuna2 <- match(coverage_2011_2023_noq1$comuna, unique(coverage_2011_2023_noq1$comuna))  ##creating a new comuna variable as a numeric variable (comuna2)
  
  coverage_2011_2023_noq1 <- coverage_2011_2023_noq1 %>% 
    dplyr::mutate(ano2 = ano - 2011)
  
  coverage_2011_2023_noq1 <- coverage_2011_2023_noq1 %>% 
    arrange(-drs_coverage) %>% 
    mutate(drs_coverage=replace(drs_coverage, drs_coverage>1, 1)) %>% 
    arrange(ano, comuna)
  
  max(coverage_2011_2023_noq1$ano2)
  ##View(coverage_2011_2023)
  

    
  
  ## Save coverage.csv -------------------------------------------------------
  
  coverage_2011_2023_noq1 %>% 
    group_by(ano) %>% 
    select(comuna) %>% 
    table() %>% 
    as.data.frame() %>% 
    arrange(-Freq) 
  
  write.csv(coverage_2011_2023_noq1, "coverage_2011_2023_noq1.csv")
  
  ggplot(coverage_2011_2023_noq1, aes(x = drs_coverage)) +
    geom_density() +
    labs(title = "Density Plot", x = "DRSC", y = "Density")
  

  
  
  ##  LCMM for DM coverage2 2011-2019 (excluding Q1) -----------------------------------------
  coverage_2011_2023_noq1 %>% 
    select(comuna) %>% 
    table() %>% 
    as.data.frame() %>% 
    arrange(-Freq)
  
  m1dm_noq1 <- lcmm::hlme(dm_coverage ~ ano2, random = ~ano2, subject = "comuna2", ng = 1, data = coverage_2011_2023_noq1)
  m2dm_noq1 <- lcmm::hlme(dm_coverage ~ ano2, mixture = ~ano2, random = ~ano2, subject='comuna2', ng = 2, data = coverage_2011_2023_noq1, B=m1dm_noq1)
  m3dm_noq1 <- lcmm::hlme(dm_coverage ~ ano2, mixture = ~ano2, random = ~ano2, subject='comuna2', ng = 3, data = coverage_2011_2023_noq1, B=m1dm_noq1)
  m4dm_noq1 <- lcmm::hlme(dm_coverage ~ ano2, mixture = ~ano2, random = ~ano2, subject='comuna2', ng = 4, data = coverage_2011_2023_noq1, B=m1dm_noq1)
  m5dm_noq1 <- gridsearch(rep = 500, maxiter = 10, minit = m1dm_noq1, m= hlme(dm_coverage ~ ano2, mixture = ~ano2, random = ~ano2, subject='comuna2', ng = 5, data = coverage_2011_2023_noq1, B=m1dm_noq1))
#m5dm_noq1 <- lcmm::hlme(dm_coverage ~ ano2, mixture = ~ano2, random = ~ano2, subject='comuna2', ng = 5, data = coverage_2011_2023_noq1, B=m1dm_noq1)
m6dm_noq1 <- lcmm::hlme(dm_coverage ~ ano2, mixture = ~ano2, random = ~ano2, subject='comuna2', ng = 6, data = coverage_2011_2023_noq1, B=m1dm_noq1)



## Sensitivity analysis table for DGCC 2011-2023-----------------------------


tab_dm_2011_2023_noq1 <- as.data.frame(lcmm::summarytable(m1dm_noq1, m2dm_noq1, m3dm_noq1, m4dm_noq1, m5dm_noq1, m6dm_noq1, 
                                                          which=c("G", 
                                                                  "loglik", 
                                                                  "conv", 
                                                                  "npm", 
                                                                  "AIC", 
                                                                  "BIC", 
                                                                  "SABIC", 
                                                                  "entropy", 
                                                                  "ICL", 
                                                                  "ICL1", 
                                                                  "ICL2", 
                                                                  "%class")))

LCTMtoolkit(m2dm_noq1)

LCTMcompare(m2dm_noq1, m3dm_noq1)
## Merge classification of size DM trajectory ------------------------------
dm_2011_2023_noq1 <- merge(coverage_2011_2023_noq1, round(m1dm_noq1$pprob,3), by = "comuna2") ##one trajectories
dm_2011_2023_noq1$class <- as.factor(dm_2011_2023_noq1$class)
write.csv(dm_2011_2023_noq1, "dm_2011_2023_noq1.csv")

a <- m1dm_noq1$pprob[1]

b <- dm_2011_2023_noq1 %>% 
  select(comuna, comuna2, id_servicio, id_region, ano, dm_coverage, class) %>% 
  mutate(id_region = ifelse(id_region==16, 8, id_region),
         id_region2 = match(id_region, unique(id_region))) %>%  #Tratar a Ñuble como si hubiera siempre pertencido a una misma region  
  spread(ano, dm_coverage) %>% 
  select(comuna, comuna2, "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018", "2019", "2020", "2021", "2023")

ab <- merge(a, b, by= "comuna2")


#supplementary_data_DGCC_2011_2023 <- ab[1:nrow(a), -1] 
supplementary_data_DGCC_2011_2023 <- ab %>% select(-comuna2)


supplementary_data_DGCC_2011_2023 <- supplementary_data_DGCC_2011_2023 %>% 
  mutate(completeness_2011_2023 = rowMeans(!is.na(select(., -comuna))),
         completeness_2011_2023 = round(completeness_2011_2023*100, 1),
         average_years = rowSums(!is.na(supplementary_data_DGCC_2011_2023[,2:13])))

supplementary_data_DGCC_2011_2023 %>% 
  select(completeness_2011_2023) %>% 
  summary() # 91.77 


write.csv(supplementary_data_DGCC_2011_2023, "supplementary_data_DGCC_2011_2023.csv")

dm2_2011_2023_noq1 <- merge(coverage_2011_2023_noq1, round(m2dm_noq1$pprob,3), by = "comuna2") ##two trajectories
dm2_2011_2023_noq1$class <- as.factor(dm2_2011_2023_noq1$class)


dm3_2011_2023_noq1 <- merge(coverage_2011_2023_noq1, round(m3dm_noq1$pprob,3), by = "comuna2") ##three trajectories
dm3_2011_2023_noq1$class <- as.factor(dm3_2011_2023_noq1$class)

dm4_2011_2023_noq1 <- merge(coverage_2011_2023_noq1, round(m4dm_noq1$pprob,3), by = "comuna2") ##four trajectories
dm4_2011_2023_noq1$class <- as.factor(dm4_2011_2023_noq1$class)

dm5_2011_2023_noq1 <- merge(coverage_2011_2023_noq1, round(m5dm_noq1$pprob,3), by = "comuna2") ##five trajectories
dm5_2011_2023_noq1$class <- as.factor(dm5_2011_2023_noq1$class)

dm6_2011_2023_noq1 <- merge(coverage_2011_2023_noq1, round(m6dm_noq1$pprob,3), by = "comuna2") ##five trajectories
dm6_2011_2023_noq1$class <- as.factor(dm6_2011_2023_noq1$class)




# MODEL ADEQUACY ----------------------------------------------------------



## Model fit criteria ------------------------------------------------------

model_fit_criteria_dm_2011_2023_noq1 <- tab_dm_2011_2023_noq1 %>% 
  dplyr::select(G,loglik, npm, AIC, BIC, SABIC) %>% 
  dplyr::mutate(sample_size= c(m1dm_noq1$ns, m2dm_noq1$ns, m3dm_noq1$ns, m4dm_noq1$ns, m5dm_noq1$ns, m6dm_noq1$ns),
                CAIC = -2*loglik + 2*(log(sample_size)+1),##𝐶𝐴𝐼𝐶 = −2(𝐿𝐿) + 𝑑[𝑙𝑜𝑔(𝑛) + 1]
                AWE= -2*loglik + 2*(log(sample_size)+1.5),##𝐴𝑊𝐸 = −2(𝐿𝐿) +𝑑[𝑙𝑜𝑔(𝑛)+ 1.5]
                SIC = -0.05*BIC,
                BF = exp(lead(SIC)-SIC),##BF = exp[SICa − SICb], where Schwartz Information Criterion, is defined as SIC=-0.05*BIC
                cmP = exp((SIC - max(SIC))/sum(exp(SIC - max(SIC))))) %>%  
  tibble::as.tibble() %>% 
  tibble::rownames_to_column() %>% 
  dplyr::select(-rowname) 


write.csv(model_fit_criteria_dm_2011_2023_noq1, "model_fit_criteria_dm_2011_2023_noq1.csv", row.names = F)



## Diagnostic criteria -----------------------------------------------------

## average latent class posterior probability ------------------------------

postprob(m1dm_noq1)
postprob(m2dm_noq1)
postprob(m3dm_noq1)
postprob(m4dm_noq1)
postprob(m5dm_noq1)
postprob(m6dm_noq1)

# Assuming postprob() returns a structured list
posterior_probsm2dm_noq1 <- postprob(m2dm_noq1)  
posterior_probsm3dm_noq1 <- postprob(m3dm_noq1)  
posterior_probsm4dm_noq1 <- postprob(m4dm_noq1)  
posterior_probsm5dm_noq1 <- postprob(m5dm_noq1)  
posterior_probsm6dm_noq1 <- postprob(m6dm_noq1)  


LCTMtoolkit(m2dm_noq1)

# OCC
# Assuming you have a list of your model objects
models_list <- list(m2dm_noq1, m3dm_noq1, m4dm_noq1, m5dm_noq1, m6dm_noq1)  # Replace with your actual model objects

# Extract the lower OCC values for each model
lower_occ_values <- sapply(models_list, function(model) {
  occ_values <- LCTMtoolkit(model)$occ
  min(as.numeric(occ_values[1,]))  # Assuming OCC is in the first row of the OCC matrix
})

# Print the lower OCC values
print(lower_occ_values)



# Mismatch
# Extract the lower OCC values for each model
highest_mismatch_values <- sapply(models_list, function(model) {
  mismatch_values <- LCTMtoolkit(model)$mismatch
  max(as.numeric(mismatch_values[1,]))  # Assuming OCC is in the first row of the OCC matrix
})

# Print the lower OCC values
print(highest_mismatch_values)



# Define a function to extract p-value from calc_lrt output
extract_p_value_from_lrt <- function(ns, loglik1, npm1, G1, loglik2, npm2, G2) {
  # Capture the output of calc_lrt
  output <- capture.output(tidyLPA::calc_lrt(ns, loglik1, npm1, G1, loglik2, npm2, G2))
  
  # Combine output into a single string
  output_text <- paste(output, collapse = " ")
  
  # Extract the p-value part from the output
  p_value <- str_extract(output_text, "(?<=p\\s).*")
  return(str_trim(p_value))
}

# Sample inputs for calc_lrt function calls Lo–Mendell–Rubin adjusted LRT  ---------------
outputs_vllrt <- list(
  extract_p_value_from_lrt(m1dm_noq1$ns, tab_dm_2011_2023_noq1$loglik[1], tab_dm_2011_2023_noq1$npm[1], tab_dm_2011_2023_noq1$G[1], tab_dm_2011_2023_noq1$loglik[2], tab_dm_2011_2023_noq1$npm[2], tab_dm_2011_2023_noq1$G[2]),
  extract_p_value_from_lrt(m2dm_noq1$ns, tab_dm_2011_2023_noq1$loglik[2], tab_dm_2011_2023_noq1$npm[2], tab_dm_2011_2023_noq1$G[2], tab_dm_2011_2023_noq1$loglik[3], tab_dm_2011_2023_noq1$npm[3], tab_dm_2011_2023_noq1$G[3]),
  extract_p_value_from_lrt(m3dm_noq1$ns, tab_dm_2011_2023_noq1$loglik[3], tab_dm_2011_2023_noq1$npm[3], tab_dm_2011_2023_noq1$G[3], tab_dm_2011_2023_noq1$loglik[4], tab_dm_2011_2023_noq1$npm[4], tab_dm_2011_2023_noq1$G[4]),
  extract_p_value_from_lrt(m4dm_noq1$ns, tab_dm_2011_2023_noq1$loglik[4], tab_dm_2011_2023_noq1$npm[4], tab_dm_2011_2023_noq1$G[4], tab_dm_2011_2023_noq1$loglik[5], tab_dm_2011_2023_noq1$npm[5], tab_dm_2011_2023_noq1$G[5]),
  extract_p_value_from_lrt(m5dm_noq1$ns, tab_dm_2011_2023_noq1$loglik[5], tab_dm_2011_2023_noq1$npm[5], tab_dm_2011_2023_noq1$G[5], tab_dm_2011_2023_noq1$loglik[6], tab_dm_2011_2023_noq1$npm[6], tab_dm_2011_2023_noq1$G[6])
)

# Assuming outputs is already defined as your object
values_vllrt <- sapply(outputs_vllrt, function(x) gsub("^= ", "", x))

# Print the vector
print(values_vllrt)

#Diagnistic criteria

diagnostic_criteria_dm_2011_2023_noq1 <- tab_dm_2011_2023_noq1 %>% 
  select(G, entropy) %>% 
  mutate(smallest_class_count = c(m1dm_noq1$ns, min(posterior_probsm2dm_noq1[[1]]), min(posterior_probsm3dm_noq1[[1]]), min(posterior_probsm4dm_noq1[[1]]), min(posterior_probsm5dm_noq1[[1]]), min(posterior_probsm6dm_noq1[[1]])),
         smallest_class_size_perc = c(1, min(posterior_probsm2dm_noq1[[1]])/m2dm_noq1$ns, min(posterior_probsm3dm_noq1[[1]])/m3dm_noq1$ns, min(posterior_probsm4dm_noq1[[1]])/m4dm_noq1$ns, min(posterior_probsm5dm_noq1[[1]])/m5dm_noq1$ns, min(posterior_probsm6dm_noq1[[1]])/m6dm_noq1$ns),
         smallest_class_size_perc = percent(round(smallest_class_size_perc, 4)),
         ALCPP = c(NA, min(diag(posterior_probsm2dm_noq1[[2]])), min(diag(posterior_probsm3dm_noq1[[2]])), min(diag(posterior_probsm4dm_noq1[[2]])), min(diag(posterior_probsm5dm_noq1[[2]])), min(diag(posterior_probsm6dm_noq1[[2]]))),
         "Highest MMV" =c(NA, highest_mismatch_values),
         "Lowest OCC" = c(NA, lower_occ_values),
         VLMRLRT = c(NA, values_vllrt)
         ) %>% 
  tibble::as.tibble() %>% 
  tibble::rownames_to_column() %>% 
  dplyr::select(-rowname) 

write.csv(diagnostic_criteria_dm_2011_2023_noq1, "diagnostic_criteria_dm_2011_2023_noq1.csv", row.names = F)


## Plot trajectories -------------------------------------------------------

pdm_2011_2023_noq1 <- ggplot(dm_2011_2023_noq1, aes(x=ano2, y=dm_coverage, colour = class, group = comuna2)) +
  ##geom_point(size = .1) + 
  ##geom_line(size = .05) +
  scale_x_continuous(breaks = c(0:12), labels=2011:2023) +
  xlab("Year") +
  ylab("DGCC")+
  theme(legend.position = "none") +
  geom_smooth(se = T, method = "loess", aes(group = class))

pdm2_2011_2023_noq1 <- ggplot(dm2_2011_2023_noq1, aes(x=ano2, y=dm_coverage, colour = class, group = comuna2)) +
  ##geom_point(size = .1) + 
  ##geom_line(size = .05) +
  scale_x_continuous(breaks = c(0:12), labels=2011:2023) +
  xlab("Year") +
  ylab("DGCC")+
  theme(legend.position = "none") +
  geom_smooth(se = T, method = "loess", aes(group = class))

pdm3_2011_2023_noq1 <- ggplot(dm3_2011_2023_noq1, aes(x=ano2, y=dm_coverage, colour = class, group = comuna2)) +
  ##geom_point(size = .1) + 
  ##geom_line(size = .05) +
  scale_x_continuous(breaks = c(0:12), labels=2011:2023) +
  xlab("Year") +
  ylab("DGCC")+
  theme(legend.position = "none") +
  geom_smooth(se = T, method = "loess", aes(group = class))

pdm4_2011_2023_noq1 <- ggplot(dm4_2011_2023_noq1, aes(x=ano2, y=dm_coverage, colour = class, group = comuna2)) +
  ##geom_point(size = .1) + 
  ##geom_line(size = .05) +
  scale_x_continuous(breaks = c(0:12), labels=2011:2023) +
  xlab("Year") +
  ylab("DGCC")+
  theme(legend.position = "none") +
  geom_smooth(se = T, method = "loess", aes(group = class))

pdm5_2011_2023_noq1 <- ggplot(dm5_2011_2023_noq1, aes(x=ano2, y=dm_coverage, colour = class, group = comuna2)) +
  ##geom_point(size = .1) + 
  ##geom_line(size = .05) +
  scale_x_continuous(breaks = c(0:12), labels=2011:2023) +
  xlab("Year") +
  ylab("DGCC")+
  theme(legend.position = "none") +
  geom_smooth(se = T, method = "loess", aes(group = class))

str(dm5_2011_2023_noq1)

pdm6_2011_2023_noq1 <- ggplot(dm6_2011_2023_noq1, aes(x=ano2, y=dm_coverage, colour = class, group = comuna2)) +
  ##geom_point(size = .1) + 
  ##geom_line(size = .05) +
  scale_x_continuous(breaks = c(0:12), labels=2011:2023) +
  xlab("Year") +
  ylab("DGCC")+
  theme(legend.position = "none") +
  geom_smooth(se = T, method = "loess", aes(group = class))

gridExtra::grid.arrange(pdm_2011_2023_noq1, pdm2_2011_2023_noq1, pdm3_2011_2023_noq1, pdm4_2011_2023_noq1, pdm5_2011_2023_noq1, pdm6_2011_2023_noq1)





##  LCMM for DM coverage2 2011-2023-----------------------------------------
coverage_2011_2023_noq1 %>% 
  select(comuna) %>% 
  table() %>% 
  as.data.frame() %>% 
  arrange(-Freq)

m1drs_noq1 <- lcmm::hlme(drs_coverage ~ ano2, random = ~ano2, subject = "comuna2", ng = 1, data = coverage_2011_2023_noq1)
m2drs_noq1 <- lcmm::hlme(drs_coverage ~ ano2, mixture = ~ano2, random = ~ano2, subject='comuna2', ng = 2, data = coverage_2011_2023_noq1, B=m1drs_noq1)
m3drs_noq1 <- lcmm::hlme(drs_coverage ~ ano2, mixture = ~ano2, random = ~ano2, subject='comuna2', ng = 3, data = coverage_2011_2023_noq1, B=m1drs_noq1)
##m3drs <- gridsearch(rep = 100, maxiter = 10, minit = m1drs, m= hlme(drs_coverage2 ~ ano2, mixture = ~ano2, random = ~ano2, subject='comuna2', ng = 3, data = coverage2, B=m1drs))
m4drs_noq1 <- lcmm::hlme(drs_coverage ~ ano2, mixture = ~ano2, random = ~ano2, subject='comuna2', ng = 4, data = coverage_2011_2023_noq1, B=m1drs_noq1)
m5drs_noq1 <- lcmm::hlme(drs_coverage ~ ano2, mixture = ~ano2, random = ~ano2, subject='comuna2', ng = 5, data = coverage_2011_2023_noq1, B=m1drs_noq1)
m6drs_noq1 <- lcmm::hlme(drs_coverage ~ ano2, mixture = ~ano2, random = ~ano2, subject='comuna2', ng = 6, data = coverage_2011_2023_noq1, B=m1drs_noq1)

##m6dm_noq1 <- gridsearch(rep = 500, maxiter = 10, minit = m1dm, m= hlme(dm_coverage2 ~ ano2, mixture = ~ano2, random = ~ano2, subject='comuna2', ng = 6, data = coverage2, B=m1dm))
summary(m2drs_noq1)


## Sensitivity analysis table for DGCC 2011-2023-----------------------------


tab_drs_2011_2023_noq1 <- as.data.frame(lcmm::summarytable(m1drs_noq1, m2drs_noq1, m3drs_noq1, m4drs_noq1, m5drs_noq1, m6drs_noq1, 
                                                           which=c("G", 
                                                                   "loglik", 
                                                                   "conv", 
                                                                   "npm", 
                                                                   "AIC", 
                                                                   "BIC", 
                                                                   "SABIC", 
                                                                   "entropy", 
                                                                   "ICL", 
                                                                   "ICL1", 
                                                                   "ICL2", 
                                                                   "%class")))


## Merge classification of size drs trajectory ------------------------------
drs_2011_2023_noq1 <- left_join(coverage_2011_2023_noq1, round(m1drs_noq1$pprob,3), by = "comuna2") ##one trajectories
drs_2011_2023_noq1$class <- as.factor(drs_2011_2023_noq1$class)
View(drs_2011_2023_noq1)

drs2_2011_2023_noq1 <- left_join(coverage_2011_2023_noq1, round(m2drs_noq1$pprob,3), by = "comuna2") ##one trajectories
drs2_2011_2023_noq1$class <- as.factor(drs2_2011_2023_noq1$class)


write.csv(drs2_2011_2023_noq1, "drs2_2011_2023_noq1.csv", row.names = F )

c <- m2drs_noq1$pprob

d <- drs2_2011_2023_noq1 %>% 
  select(comuna, comuna2, id_servicio, id_region, ano, drs_coverage, class) %>% 
  mutate(id_region = ifelse(id_region==16, 8, id_region),
         id_region2 = match(id_region, unique(id_region))) %>%  #Tratar a Ñuble como si hubiera siempre pertencido a una misma region  
  spread(ano, drs_coverage) %>% 
  select(comuna, comuna2, "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018", "2019", "2020", "2021", "2023")


cd <- merge(d, c, by= "comuna2")


#supplementary_data_DGCC_2011_2023 <- ab[1:nrow(a), -1] 
supplementary_data_DRSC_2011_2023 <- cd %>% select(-comuna2)


supplementary_data_DRSC_2011_2023 <- supplementary_data_DRSC_2011_2023 %>% 
  mutate(completeness_2011_2023 = rowMeans(!is.na(select(., -comuna, - class, - prob1, -prob2))),
         completeness_2011_2023 = round(completeness_2011_2023*100, 1),
         average_years = rowSums(!is.na(supplementary_data_DRSC_2011_2023[,2:13])))

supplementary_data_DRSC_2011_2023 %>% 
  arrange(average_years)
# Average completeness ----------------------------------------------------
supplementary_data_DRSC_2011_2023 %>% 
  select(completeness_2011_2023) %>% 
  summary() #  87.39 
0.8739  *12

# Average completeness for classes ----------------------------------------
supplementary_data_DRSC_2011_2023 %>% 
  filter(class==1) %>% 
  select(completeness_2011_2023) %>% 
  summary() #91.16  

supplementary_data_DRSC_2011_2023 %>% 
  filter(class==2) %>% 
  select(completeness_2011_2023) %>% 
  summary() #87.16


# checkear variances ------------------------------------------------------

e <- supplementary_data_DRSC_2011_2023 %>% 
  filter(class==1)  %>% 
  select(completeness_2011_2023)

f <- supplementary_data_DRSC_2011_2023 %>% 
  filter(class==2) %>% 
  select(completeness_2011_2023)

var.test(e$completeness_2011_2023, f$completeness_2011_2023) # las varianzas son distintas


# t-test para varianza distinta -----------------------------------------------


t.test(e$completeness_2011_2023, f$completeness_2011_2023, var.equal = F) # No Hay DES para completeness between trajectories


write.csv(supplementary_data_DRSC_2011_2023, "supplementary_data_DRSC_2011_2023.csv")


drs3_2011_2023_noq1 <- merge(coverage_2011_2023_noq1, round(m3drs_noq1$pprob,3), by = "comuna2") ##three trajectories
drs3_2011_2023_noq1$class <- as.factor(drs3_2011_2023_noq1$class)

drs4_2011_2023_noq1 <- merge(coverage_2011_2023_noq1, round(m4drs_noq1$pprob,3), by = "comuna2") ##four trajectories
drs4_2011_2023_noq1$class <- as.factor(drs4_2011_2023_noq1$class)

drs5_2011_2023_noq1 <- merge(coverage_2011_2023_noq1, round(m5drs_noq1$pprob,3), by = "comuna2") ##five trajectories
drs5_2011_2023_noq1$class <- as.factor(drs5_2011_2023_noq1$class)

drs6_2011_2023_noq1 <- merge(coverage_2011_2023_noq1, round(m6drs_noq1$pprob,3), by = "comuna2") ##five trajectories
drs6_2011_2023_noq1$class <- as.factor(drs6_2011_2023_noq1$class)


# MODEL ADEQUACY ----------------------------------------------------------



## Model fit criteria ------------------------------------------------------

model_fit_criteria_drs_2011_2023_noq1 <- tab_drs_2011_2023_noq1 %>% 
  dplyr::select(G,loglik, npm, AIC, BIC, SABIC) %>% 
  dplyr::mutate(sample_size= c(m1drs_noq1$ns, m2drs_noq1$ns, m3drs_noq1$ns, m4drs_noq1$ns, m5drs_noq1$ns, m6drs_noq1$ns),
                CAIC = -2*loglik + 2*(log(sample_size)+1),##𝐶𝐴𝐼𝐶 = −2(𝐿𝐿) + 𝑑[𝑙𝑜𝑔(𝑛) + 1]
                AWE= -2*loglik + 2*(log(sample_size)+1.5),##𝐴𝑊𝐸 = −2(𝐿𝐿) +𝑑[𝑙𝑜𝑔(𝑛)+ 1.5]
                SIC = -0.05*BIC,
                BF = exp(lead(SIC)-SIC),##BF = exp[SICa − SICb], where Schwartz Information Criterion, is defined as SIC=-0.05*BIC
                cmP = exp((SIC - max(SIC))/sum(exp(SIC - max(SIC))))) %>%  
  tibble::as.tibble() %>% 
  tibble::rownames_to_column() %>% 
  dplyr::select(-rowname) 


write.csv(model_fit_criteria_drs_2011_2023_noq1, "model_fit_criteria_drs_2011_2023_noq1.csv", row.names = F)



## Diagnostic criteria -----------------------------------------------------

## average latent class posterior probability ------------------------------

postprob(m1drs_noq1)
postprob(m2drs_noq1)
postprob(m3drs_noq1)
postprob(m4drs_noq1)
postprob(m5drs_noq1)
postprob(m6drs_noq1)

# Assuming postprob() returns a structured list
posterior_probsm2drs_noq1 <- postprob(m2drs_noq1)  
posterior_probsm3drs_noq1 <- postprob(m3drs_noq1)  
posterior_probsm4drs_noq1 <- postprob(m4drs_noq1)  
posterior_probsm5drs_noq1 <- postprob(m5drs_noq1)  
posterior_probsm6drs_noq1 <- postprob(m6drs_noq1)  


# OCC
# Assuming you have a list of your model objects
models_list2 <- list(m2drs_noq1, m3drs_noq1, m4drs_noq1, m5drs_noq1, m6drs_noq1)  # Replace with your actual model objects

# Extract the lower OCC values for each model
lower_occ_values2 <- sapply(models_list2, function(model) {
  occ_values2 <- LCTMtoolkit(model)$occ
  min(as.numeric(occ_values2[1,]))  # Assuming OCC is in the first row of the OCC matrix
})

# Print the lower OCC values
print(lower_occ_values2)



# Mismatch
# Extract the lower OCC values for each model
highest_mismatch_values2 <- sapply(models_list2, function(model) {
  mismatch_values2 <- LCTMtoolkit(model)$mismatch
  max(as.numeric(mismatch_values2[1,]))  # Assuming OCC is in the first row of the OCC matrix
})

# Print the lower OCC values
print(highest_mismatch_values2)



# Sample inputs for calc_lrt function calls Lo–Mendell–Rubin adjusted LRT  ---------------
outputs_vllrt2 <- list(
  extract_p_value_from_lrt(m1drs_noq1$ns, tab_drs_2011_2023_noq1$loglik[1], tab_drs_2011_2023_noq1$npm[1], tab_drs_2011_2023_noq1$G[1], tab_drs_2011_2023_noq1$loglik[2], tab_drs_2011_2023_noq1$npm[2], tab_drs_2011_2023_noq1$G[2]),
  extract_p_value_from_lrt(m2drs_noq1$ns, tab_drs_2011_2023_noq1$loglik[2], tab_drs_2011_2023_noq1$npm[2], tab_drs_2011_2023_noq1$G[2], tab_drs_2011_2023_noq1$loglik[3], tab_drs_2011_2023_noq1$npm[3], tab_drs_2011_2023_noq1$G[3]),
  extract_p_value_from_lrt(m3drs_noq1$ns, tab_drs_2011_2023_noq1$loglik[3], tab_drs_2011_2023_noq1$npm[3], tab_drs_2011_2023_noq1$G[3], tab_drs_2011_2023_noq1$loglik[4], tab_drs_2011_2023_noq1$npm[4], tab_drs_2011_2023_noq1$G[4]),
  extract_p_value_from_lrt(m4drs_noq1$ns, tab_drs_2011_2023_noq1$loglik[4], tab_drs_2011_2023_noq1$npm[4], tab_drs_2011_2023_noq1$G[4], tab_drs_2011_2023_noq1$loglik[5], tab_drs_2011_2023_noq1$npm[5], tab_drs_2011_2023_noq1$G[5]),
  extract_p_value_from_lrt(m5drs_noq1$ns, tab_drs_2011_2023_noq1$loglik[5], tab_drs_2011_2023_noq1$npm[5], tab_drs_2011_2023_noq1$G[5], tab_drs_2011_2023_noq1$loglik[6], tab_drs_2011_2023_noq1$npm[6], tab_drs_2011_2023_noq1$G[6])
)

# Assuming outputs is already defined as your object
values_vllrt2 <- sapply(outputs_vllrt2, function(x) gsub("^= ", "", x))

# Print the vector
print(values_vllrt2)

#Diagnistic criteria

diagnostic_criteria_drs_2011_2023_noq1 <- tab_drs_2011_2023_noq1 %>% 
  select(G, entropy) %>% 
  mutate(smallest_class_count = c(m1drs_noq1$ns, min(posterior_probsm2drs_noq1[[1]]), min(posterior_probsm3drs_noq1[[1]]), min(posterior_probsm4drs_noq1[[1]]), min(posterior_probsm5drs_noq1[[1]]), min(posterior_probsm6drs_noq1[[1]])),
         smallest_class_size_perc = c(1, min(posterior_probsm2drs_noq1[[1]])/m2drs_noq1$ns, min(posterior_probsm3drs_noq1[[1]])/m3drs_noq1$ns, min(posterior_probsm4drs_noq1[[1]])/m4drs_noq1$ns, min(posterior_probsm5drs_noq1[[1]])/m5drs_noq1$ns, min(posterior_probsm6drs_noq1[[1]])/m6drs_noq1$ns),
         smallest_class_size_perc = percent(round(smallest_class_size_perc, 4)),
         ALCPP = c(NA, min(diag(posterior_probsm2drs_noq1[[2]])), min(diag(posterior_probsm3drs_noq1[[2]])), min(diag(posterior_probsm4drs_noq1[[2]])), min(diag(posterior_probsm5drs_noq1[[2]])), min(diag(posterior_probsm6drs_noq1[[2]]))),
         "Highest MMV" =c(NA, highest_mismatch_values2),
         "Lowest OCC" = c(NA, lower_occ_values2),
         VLMRLRT = c(NA, values_vllrt2)
  ) %>% 
  tibble::as.tibble() %>% 
  tibble::rownames_to_column() %>% 
  dplyr::select(-rowname) 

write.csv(diagnostic_criteria_drs_2011_2023_noq1, "diagnostic_criteria_drs_2011_2023_noq1.csv", row.names = F)



## Plot trajectories -------------------------------------------------------

pdrs_2011_2023_noq1 <- ggplot(drs_2011_2023_noq1, aes(x=ano2, y=drs_coverage, colour = class, group = comuna2)) +
  ##geom_point(size = .1) + 
  ##geom_line(size = .05) +
  scale_x_continuous(breaks = c(0:12), labels=2011:2023) +
  xlab("Year") +
  ylab("DRSC")+
  theme(legend.position = "none") +
  geom_smooth(se = T, method = "loess", aes(group = class))

pdrs2_2011_2023_noq1 <- ggplot(drs2_2011_2023_noq1, aes(x=ano2, y=drs_coverage, colour = class, group = comuna2)) +
  ##geom_point(size = .1) + 
  ##geom_line(size = .05) +
  scale_x_continuous(breaks = c(0:12), labels=2011:2023) +
  xlab("Year") +
  ylab("DRSC")+
  theme(legend.position = "none") +
  geom_smooth(se = T, method = "loess", aes(group = class))

pdrs3_2011_2023_noq1 <- ggplot(drs3_2011_2023_noq1, aes(x=ano2, y=drs_coverage, colour = class, group = comuna2)) +
  ##geom_point(size = .1) + 
  ##geom_line(size = .05) +
  scale_x_continuous(breaks = c(0:12), labels=2011:2023) +
  xlab("Year") +
  ylab("DRSC")+
  theme(legend.position = "none") +
  geom_smooth(se = T, method = "loess", aes(group = class))

pdrs4_2011_2023_noq1 <- ggplot(drs4_2011_2023_noq1, aes(x=ano2, y=drs_coverage, colour = class, group = comuna2)) +
  ##geom_point(size = .1) + 
  ##geom_line(size = .05) +
  scale_x_continuous(breaks = c(0:12), labels=2011:2023) +
  xlab("Year") +
  ylab("DRSC")+
  theme(legend.position = "none") +
  geom_smooth(se = T, method = "loess", aes(group = class))

pdrs5_2011_2023_noq1 <- ggplot(drs5_2011_2023_noq1, aes(x=ano2, y=drs_coverage, colour = class, group = comuna2)) +
  ##geom_point(size = .1) + 
  ##geom_line(size = .05) +
  scale_x_continuous(breaks = c(0:12), labels=2011:2023) +
  xlab("Year") +
  ylab("DRSC")+
  theme(legend.position = "none") +
  geom_smooth(se = T, method = "loess", aes(group = class))

pdrs6_2011_2023_noq1 <- ggplot(drs6_2011_2023_noq1, aes(x=ano2, y=drs_coverage, colour = class, group = comuna2)) +
  ##geom_point(size = .1) + 
  ##geom_line(size = .05) +
  scale_x_continuous(breaks = c(0:12), labels=2011:2023) +
  xlab("Year") +
  ylab("DRSC")+
  theme(legend.position = "none") +
  geom_smooth(se = T, method = "loess", aes(group = class))

grid.arrange(pdrs_2011_2023_noq1, pdrs2_2011_2023_noq1, pdrs3_2011_2023_noq1, pdrs4_2011_2023_noq1, pdrs5_2011_2023_noq1, pdrs6_2011_2023_noq1)



## Índice sociodemográfico -------------------------------------------------
isde <- read_excel("SocioEconominoSaludComunas.xlsx")

isde <- isde[,c(2,4)]

isde <- isde %>% 
  dplyr::mutate(comuna = ...2 ) %>% 
  dplyr::select(comuna, index) %>% 
  arrange(comuna)


#class <- drs2_2011_2023_noq1 %>% 
#  mutate(ano2=match(ano, unique(ano)),
#         id_region = ifelse(id_region==16, 8, id_region), #Tratar a Ñuble como si hubiera siempre pertencido a una misma region  
#         id_region2 = match(id_region, unique(id_region)),
#         zona = factor(zona),
#         id_servicio2 = match(id_servicio, unique(id_servicio))) %>% 
#  dplyr::group_by(comuna2,
#                  class,
#                    comuna,
#                    id_servicio,
#                    id_region,
#                  zona,
#                  quintil_dm_category
#                  ) %>% 
#  dplyr::summarise(mean_drs_coverage = mean(drs_coverage, na.rm=T),
#                   mean_dm_coverage = mean(dm_coverage, na.rm=T)) %>% 
#  distinct(comuna2, .keep_all = TRUE) 


class <- drs2_2011_2023_noq1 %>% 
  mutate(ano2=match(ano, unique(ano)),
         id_region = ifelse(id_region==16, 8, id_region), #Tratar a Ñuble como si hubiera siempre pertencido a una misma region  
         id_region2 = match(id_region, unique(id_region)),
         zona = factor(zona),
         id_servicio2 = match(id_servicio, unique(id_servicio))) %>% 
  dplyr::group_by(comuna, comuna2, class,id_servicio, id_region, zona) %>% 
  dplyr::summarise(mean_drs_coverage = mean(drs_coverage, na.rm=T),
                   mean_dm_coverage = mean(dm_coverage, na.rm=T)) %>% 
  distinct(comuna, .keep_all = TRUE) %>% 
  mutate(comuna2 = cur_group_id()) 






#class <- drs2_2011_2023_noq1 %>% 
#  dplyr::select(ano, ano2, comuna, comuna2, id_region, id_servicio, class) %>% 
 # dplyr::group_by(class) %>% 
#  distinct(comuna, .keep_all = TRUE)
#summary(class)

class[!(class$comuna %in% isde$comuna), ] 

isde$comuna[isde$comuna == "Aisen"] <- "Aisén"
isde$comuna[isde$comuna =="Padre las Casas"] <- "Padre Las Casas"
class$comuna[174] = isde$comuna[192] ## esto arregla PAC
isde$comuna[isde$comuna =="San Juan de La Costa"] <- "San Juan de la Costa" 


class[!(class$comuna %in% isde$comuna), ] 

#class <- class %>% 
 # mutate(zona = ifelse(zona %in% NA, 3, zona)) 


data_reglog <- right_join(class, isde) 

##View(data_reglog)

data_reglog <- data_reglog %>% 
  mutate(class_membership = ifelse(class==2, 1, 0)) 


data_reglog$index_standardized <- scale(data_reglog$index)

rurality <- read_excel("Clasificacion-comunas-PNDR.xlsx")
rurality <- janitor::clean_names(rurality) %>% 
  select(comuna, tipo_com, clasificacion) %>% 
  mutate(urbanisation = tipo_com,
         comuna = str_to_title(comuna),
         classification= clasificacion) %>% 
  select(-tipo_com)

rurality$comuna <- gsub("\\bDe\\b", "de", rurality$comuna, ignore.case = TRUE)
rurality$comuna <- gsub("\\bDel\\b", "del", rurality$comuna, ignore.case = TRUE)
rurality$comuna <- gsub("\\b La\\b", " la", rurality$comuna, ignore.case = TRUE)
rurality$comuna[rurality$comuna == "Aysén"] <- "Aisén"
rurality$comuna[rurality$comuna == "Coyhaique"] <- "Coihaique"
rurality$comuna[rurality$comuna == "Alto Biobío"] <- "Alto Bío-Bío"

rurality[!(rurality$comuna %in% data_reglog$comuna), ] 

data_reglog[!(data_reglog$comuna %in% rurality$comuna), ] 

data_reglog <- right_join(data_reglog, rurality)


data_reglog <- data_reglog %>%
  mutate(zona = ifelse(zona == 1, 'norte', 
                       ifelse(zona == 2, 'centro', 'sur'))) 

write.csv(data_reglog, "data_reglog.csv")




# EXPERIMENT 2 ------------------------------------------------------------


## coverage ---------------------------------------------------------------


coverage_2011_2019 <- coverage_2011_2023 %>% 
  filter(ano %in% c(2011:2019))

coverage_2011_2019$comuna2 <- match(coverage_2011_2019$comuna, unique(coverage_2011_2019$comuna))  ##creating a new comuna variable as a numeric variable (comuna2)

coverage_2011_2019 <- coverage_2011_2019 %>% 
  dplyr::mutate(ano2 = ano - 2011)

coverage_2011_2019 <- coverage_2011_2019 %>% 
  arrange(-drs_coverage) %>% 
  mutate(drs_coverage=replace(drs_coverage, drs_coverage>1, 1)) %>% 
  arrange(ano, comuna)

##View(coverage_2011_2019)

## Categorise dm quintiles -------------------------------------------------

coverage_2011_2019 <- coverage_2011_2019 %>%
  ungroup() %>% 
  mutate( quintil_dm_category = cut(dm,
                                    breaks = quantile(dm, probs = c(0, 0.2, 0.4, 0.6, 0.8, 1), na.rm = TRUE),
                                    labels = c("Q1", "Q2", "Q3", "Q4", "Q5"),
                                    include.lowest = TRUE))

## Save coverage.csv -------------------------------------------------------


## Delete Q1 category ------------------------------------------------------

coverage_2011_2019_noq1 <- coverage_2011_2019 %>% 
  dplyr::filter(quintil_dm_category!= "Q1")


## Save coverage.csv -------------------------------------------------------

write.csv(coverage_2011_2019_noq1, "coverage_2011_2019_noq1.csv")



##  LCMM for DM coverage2 2011-2019 (excluding Q1) -----------------------------------------
coverage_2011_2019_noq1 %>% 
  select(comuna) %>% 
  table() %>% 
  as.data.frame() %>% 
  arrange(-Freq)

m7dm_noq1 <- lcmm::hlme(dm_coverage ~ ano2, random = ~ano2, subject = "comuna2", ng = 1, data = coverage_2011_2019_noq1)
m8dm_noq1 <- lcmm::hlme(dm_coverage ~ ano2, mixture = ~ano2, random = ~ano2, subject='comuna2', ng = 2, data = coverage_2011_2019_noq1, B=m7dm_noq1)
m9dm_noq1 <- lcmm::hlme(dm_coverage ~ ano2, mixture = ~ano2, random = ~ano2, subject='comuna2', ng = 3, data = coverage_2011_2019_noq1, B=m7dm_noq1)
##m3dm <- gridsearch(rep = 100, maxiter = 10, minit = m1dm, m= hlme(dm_coverage2 ~ ano2, mixture = ~ano2, random = ~ano2, subject='comuna2', ng = 3, data = coverage2, B=m1dm))
m10dm_noq1 <- lcmm::hlme(dm_coverage ~ ano2, mixture = ~ano2, random = ~ano2, subject='comuna2', ng = 4, data = coverage_2011_2019_noq1, B=m7dm_noq1)
#m5dm_noq1 <- gridsearch(rep = 200, maxiter = 10, minit = m1dm_noq1, m= hlme(dm_coverage ~ ano2, mixture = ~ano2, random = ~ano2, subject='comuna2', ng = 5, data = coverage_2011_2019_noq1, B=m1dm_noq1))
m11dm_noq1 <- lcmm::hlme(dm_coverage ~ ano2, mixture = ~ano2, random = ~ano2, subject='comuna2', ng = 5, data = coverage_2011_2019_noq1, B=m7dm_noq1)
m12dm_noq1 <- lcmm::hlme(dm_coverage ~ ano2, mixture = ~ano2, random = ~ano2, subject='comuna2', ng = 6, data = coverage_2011_2019_noq1, B=m7dm_noq1)

##m6dm_noq1 <- gridsearch(rep = 500, maxiter = 10, minit = m1dm, m= hlme(dm_coverage2 ~ ano2, mixture = ~ano2, random = ~ano2, subject='comuna2', ng = 6, data = coverage2, B=m1dm))
summary(m7dm_noq1)
## Sensitivity analysis table for DGCC 2011-2023-----------------------------


tab_dm_2011_2019_noq1 <- as.data.frame(lcmm::summarytable(m7dm_noq1, m8dm_noq1, m9dm_noq1, m10dm_noq1, m11dm_noq1, m12dm_noq1, 
                                                          which=c("G", 
                                                                  "loglik", 
                                                                  "conv", 
                                                                  "npm", 
                                                                  "AIC", 
                                                                  "BIC", 
                                                                  "SABIC", 
                                                                  "entropy", 
                                                                  "ICL", 
                                                                  "ICL1", 
                                                                  "ICL2", 
                                                                  "%class")))

## Merge classification of size DM trajectory ------------------------------
dm_2011_2019_noq1 <- merge(coverage_2011_2019_noq1, round(m7dm_noq1$pprob,3), by = "comuna2") ##one trajectories
dm_2011_2019_noq1$class <- as.factor(dm_2011_2019_noq1$class)
write.csv(dm_2011_2019_noq1, "dm_2011_2019_noq1.csv")


g <- m7dm_noq1$pprob[1]

h <- dm_2011_2019_noq1 %>% 
  select(comuna, comuna2, zona, id_servicio, id_region, ano, dm_coverage, class) %>% 
  mutate(id_region = ifelse(id_region==16, 8, id_region),
         id_region2 = match(id_region, unique(id_region))) %>%  #Tratar a Ñuble como si hubiera siempre pertencido a una misma region  
  spread(ano, dm_coverage) %>% 
  select(comuna, comuna2, "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018", "2019")




gh <- merge(h, g, by= "comuna2")


supplementary_data_DGCC_2011_2019 <- gh %>% 
                                        select(-comuna2)
#supplementary_data_DGCC_2011_2019 <- select(-comuna2)


supplementary_data_DGCC_2011_2019 <- supplementary_data_DGCC_2011_2019 %>% 
  mutate(completeness_2011_2019 = rowMeans(!is.na(select(., -comuna))),
         completeness_2011_2019 = round(completeness_2011_2019*100, 2),
         average_years = rowSums(!is.na(supplementary_data_DGCC_2011_2019[,2:10])))

supplementary_data_DGCC_2011_2019 %>% 
  select(completeness_2011_2019) %>% 
  summary() # 93.46

mean(supplementary_data_DGCC_2011_2019$completeness_2011_2019)
sd(supplementary_data_DGCC_2011_2019$completeness_2011_2019)



write.csv(supplementary_data_DGCC_2011_2019, "supplementary_data_DGCC_2011_2019.csv")

dm2_2011_2019_noq1 <- merge(coverage_2011_2019_noq1, round(m8dm_noq1$pprob,3), by = "comuna2") ##two trajectories
dm2_2011_2019_noq1$class <- as.factor(dm2_2011_2019_noq1$class)

dm3_2011_2019_noq1 <- merge(coverage_2011_2019_noq1, round(m9dm_noq1$pprob,3), by = "comuna2") ##three trajectories
dm3_2011_2019_noq1$class <- as.factor(dm3_2011_2019_noq1$class)

dm4_2011_2019_noq1 <- merge(coverage_2011_2019_noq1, round(m10dm_noq1$pprob,3), by = "comuna2") ##four trajectories
dm4_2011_2019_noq1$class <- as.factor(dm4_2011_2019_noq1$class)

dm5_2011_2019_noq1 <- merge(coverage_2011_2019_noq1, round(m11dm_noq1$pprob,3), by = "comuna2") ##five trajectories
dm5_2011_2019_noq1$class <- as.factor(dm5_2011_2019_noq1$class)

dm6_2011_2019_noq1 <- merge(coverage_2011_2019_noq1, round(m12dm_noq1$pprob,3), by = "comuna2") ##five trajectories
dm6_2011_2019_noq1$class <- as.factor(dm6_2011_2019_noq1$class)


## Model fit criteria ------------------------------------------------------

model_fit_criteria_dm_2011_2019_noq1 <- tab_dm_2011_2019_noq1 %>% 
  dplyr::select(G,loglik, npm, AIC, BIC, SABIC) %>% 
  dplyr::mutate(sample_size= c(m7dm_noq1$ns, m8dm_noq1$ns, m9dm_noq1$ns, m10dm_noq1$ns, m11dm_noq1$ns, m12dm_noq1$ns),
                CAIC = -2*loglik + 2*(log(sample_size)+1),##𝐶𝐴𝐼𝐶 = −2(𝐿𝐿) + 𝑑[𝑙𝑜𝑔(𝑛) + 1]
                AWE= -2*loglik + 2*(log(sample_size)+1.5),##𝐴𝑊𝐸 = −2(𝐿𝐿) +𝑑[𝑙𝑜𝑔(𝑛)+ 1.5]
                SIC = -0.05*BIC,
                BF = exp(lead(SIC)-SIC),##BF = exp[SICa − SICb], where Schwartz Information Criterion, is defined as SIC=-0.05*BIC
                cmP = exp((SIC - max(SIC))/sum(exp(SIC - max(SIC))))) %>%  
  tibble::as.tibble() %>% 
  tibble::rownames_to_column() %>% 
  dplyr::select(-rowname) 

write.csv(model_fit_criteria_dm_2011_2019_noq1, "model_fit_dm_criteria_2011_2019_noq1.csv", row.names = F)



## Diagnostic criteria -----------------------------------------------------

## average latent class posterior probability ------------------------------

postprob(m7dm_noq1)
postprob(m8dm_noq1)
postprob(m9dm_noq1)
postprob(m10dm_noq1)
postprob(m11dm_noq1)
postprob(m12dm_noq1)

# Assuming postprob() returns a structured list
posterior_probsm8dm_noq1 <- postprob(m8dm_noq1)  
posterior_probsm9dm_noq1 <- postprob(m9dm_noq1)  
posterior_probsm10dm_noq1 <- postprob(m10dm_noq1)  
posterior_probsm11dm_noq1 <- postprob(m11dm_noq1)  
posterior_probsm12dm_noq1 <- postprob(m12dm_noq1)  


# OCC
# Assuming you have a list of your model objects
models_list3 <- list(m8dm_noq1, m9dm_noq1, m10dm_noq1, m11dm_noq1, m12dm_noq1)  # Replace with your actual model objects

# Extract the lower OCC values for each model
lower_occ_values3 <- sapply(models_list3, function(model) {
  occ_values3 <- LCTMtoolkit(model)$occ
  min(as.numeric(occ_values3[1,]))  # Assuming OCC is in the first row of the OCC matrix
})

# Print the lower OCC values
print(lower_occ_values3)



# Mismatch
# Extract the lower OCC values for each model
highest_mismatch_values3 <- sapply(models_list3, function(model) {
  mismatch_values3 <- LCTMtoolkit(model)$mismatch
  max(as.numeric(mismatch_values3[1,]))  # Assuming OCC is in the first row of the OCC matrix
})

# Print the lower OCC values
print(highest_mismatch_values3)



# Sample inputs for calc_lrt function calls Lo–Mendell–Rubin adjusted LRT  ---------------
outputs_vllrt3 <- list(
  extract_p_value_from_lrt(m7dm_noq1$ns, tab_dm_2011_2023_noq1$loglik[1], tab_dm_2011_2023_noq1$npm[1], tab_dm_2011_2023_noq1$G[1], tab_dm_2011_2023_noq1$loglik[2], tab_dm_2011_2023_noq1$npm[2], tab_dm_2011_2023_noq1$G[2]),
  extract_p_value_from_lrt(m8dm_noq1$ns, tab_dm_2011_2023_noq1$loglik[2], tab_dm_2011_2023_noq1$npm[2], tab_dm_2011_2023_noq1$G[2], tab_dm_2011_2023_noq1$loglik[3], tab_dm_2011_2023_noq1$npm[3], tab_dm_2011_2023_noq1$G[3]),
  extract_p_value_from_lrt(m9dm_noq1$ns, tab_dm_2011_2023_noq1$loglik[3], tab_dm_2011_2023_noq1$npm[3], tab_dm_2011_2023_noq1$G[3], tab_dm_2011_2023_noq1$loglik[4], tab_dm_2011_2023_noq1$npm[4], tab_dm_2011_2023_noq1$G[4]),
  extract_p_value_from_lrt(m10dm_noq1$ns, tab_dm_2011_2023_noq1$loglik[4], tab_dm_2011_2023_noq1$npm[4], tab_dm_2011_2023_noq1$G[4], tab_dm_2011_2023_noq1$loglik[5], tab_dm_2011_2023_noq1$npm[5], tab_dm_2011_2023_noq1$G[5]),
  extract_p_value_from_lrt(m11dm_noq1$ns, tab_dm_2011_2023_noq1$loglik[5], tab_dm_2011_2023_noq1$npm[5], tab_dm_2011_2023_noq1$G[5], tab_dm_2011_2023_noq1$loglik[6], tab_dm_2011_2023_noq1$npm[6], tab_dm_2011_2023_noq1$G[6])
)

# Assuming outputs is already defined as your object
values_vllrt3 <- sapply(outputs_vllrt3, function(x) gsub("^= ", "", x))

# Print the vector
print(values_vllrt3)

#Diagnistic criteria

diagnostic_criteria_dm_2011_2023_noq1 <- tab_dm_2011_2023_noq1 %>% 
  select(G, entropy) %>% 
  mutate(smallest_class_count = c(m1dm_noq1$ns, min(posterior_probsm2dm_noq1[[1]]), min(posterior_probsm3dm_noq1[[1]]), min(posterior_probsm4dm_noq1[[1]]), min(posterior_probsm5dm_noq1[[1]]), min(posterior_probsm6dm_noq1[[1]])),
         smallest_class_size_perc = c(1, min(posterior_probsm2dm_noq1[[1]])/m2dm_noq1$ns, min(posterior_probsm3dm_noq1[[1]])/m3dm_noq1$ns, min(posterior_probsm4dm_noq1[[1]])/m4dm_noq1$ns, min(posterior_probsm5dm_noq1[[1]])/m5dm_noq1$ns, min(posterior_probsm6dm_noq1[[1]])/m6dm_noq1$ns),
         smallest_class_size_perc = percent(round(smallest_class_size_perc, 4)),
         ALCPP = c(NA, min(diag(posterior_probsm2dm_noq1[[2]])), min(diag(posterior_probsm3dm_noq1[[2]])), min(diag(posterior_probsm4dm_noq1[[2]])), min(diag(posterior_probsm5dm_noq1[[2]])), min(diag(posterior_probsm6dm_noq1[[2]]))),
         "Highest MMV" =c(NA, highest_mismatch_values3),
         "Lowest OCC" = c(NA, lower_occ_values3),
         VLMRLRT = c(NA, values_vllrt3)
  ) %>% 
  tibble::as.tibble() %>% 
  tibble::rownames_to_column() %>% 
  dplyr::select(-rowname) 

write.csv(diagnostic_criteria_dm_2011_2023_noq1, "diagnostic_criteria_dm_2011_2023_noq1.csv", row.names = F)


## Plot trajectories -------------------------------------------------------

pdm_2011_2019_noq1 <- ggplot(dm_2011_2019_noq1, aes(x=ano2, y=dm_coverage, colour = class, group = comuna2)) +
  ##geom_point(size = .1) + 
  ##geom_line(size = .05) +
  scale_x_continuous(breaks = c(0:8), labels=2011:2019) +
  xlab("Year") +
  ylab("DGCC")+
  theme(legend.position = "none") +
  geom_smooth(se = T, method = "loess", aes(group = class))

pdm2_2011_2019_noq1 <- ggplot(dm2_2011_2019_noq1, aes(x=ano2, y=dm_coverage, colour = class, group = comuna2)) +
  ##geom_point(size = .1) + 
  ##geom_line(size = .05) +
  scale_x_continuous(breaks = c(0:8), labels=2011:2019) +
  xlab("Year") +
  ylab("DGCC")+
  theme(legend.position = "none") +
  geom_smooth(se = T, method = "loess", aes(group = class))

pdm3_2011_2019_noq1 <- ggplot(dm3_2011_2019_noq1, aes(x=ano2, y=dm_coverage, colour = class, group = comuna2)) +
  ##geom_point(size = .1) + 
  ##geom_line(size = .05) +
  scale_x_continuous(breaks = c(0:8), labels=2011:2019) +
  xlab("Year") +
  ylab("DGCC")+
  theme(legend.position = "none") +
  geom_smooth(se = T, method = "loess", aes(group = class))

pdm4_2011_2019_noq1 <- ggplot(dm4_2011_2019_noq1, aes(x=ano2, y=dm_coverage, colour = class, group = comuna2)) +
  ##geom_point(size = .1) + 
  ##geom_line(size = .05) +
  scale_x_continuous(breaks = c(0:8), labels=2011:2019) +
  xlab("Year") +
  ylab("DGCC")+
  theme(legend.position = "none") +
  geom_smooth(se = T, method = "loess", aes(group = class))

pdm5_2011_2019_noq1 <- ggplot(dm5_2011_2019_noq1, aes(x=ano2, y=dm_coverage, colour = class, group = comuna2)) +
  ##geom_point(size = .1) + 
  ##geom_line(size = .05) +
  scale_x_continuous(breaks = c(0:8), labels=2011:2019) +
  xlab("Year") +
  ylab("DGCC")+
  theme(legend.position = "none") +
  geom_smooth(se = T, method = "loess", aes(group = class))

pdm6_2011_2019_noq1 <- ggplot(dm6_2011_2019_noq1, aes(x=ano2, y=dm_coverage, colour = class, group = comuna2)) +
  ##geom_point(size = .1) + 
  ##geom_line(size = .05) +
  scale_x_continuous(breaks = c(0:8), labels=2011:2019) +
  xlab("Year") +
  ylab("DGCC")+
  theme(legend.position = "none") +
  geom_smooth(se = T, method = "loess", aes(group = class))

gridExtra::grid.arrange(pdm_2011_2019_noq1, pdm2_2011_2019_noq1, pdm3_2011_2019_noq1, pdm4_2011_2019_noq1, pdm5_2011_2019_noq1, pdm6_2011_2019_noq1  )


##  LCMM for DM coverage2 2011-2019-----------------------------------------


m7drs_noq1 <- lcmm::hlme(drs_coverage ~ ano2, random = ~ano2, subject = "comuna2", ng = 1, data = coverage_2011_2019_noq1)
m8drs_noq1 <- lcmm::hlme(drs_coverage ~ ano2, mixture = ~ano2, random = ~ano2, subject='comuna2', ng = 2, data = coverage_2011_2019_noq1, B=m7drs_noq1)
m9drs_noq1 <- lcmm::hlme(drs_coverage ~ ano2, mixture = ~ano2, random = ~ano2, subject='comuna2', ng = 3, data = coverage_2011_2019_noq1, B=m7drs_noq1)
##m3drs <- gridsearch(rep = 100, maxiter = 10, minit = m1drs, m= hlme(drs_coverage2 ~ ano2, mixture = ~ano2, random = ~ano2, subject='comuna2', ng = 3, data = coverage2, B=m1drs))
m10drs_noq1 <- lcmm::hlme(drs_coverage ~ ano2, mixture = ~ano2, random = ~ano2, subject='comuna2', ng = 4, data = coverage_2011_2019_noq1, B=m7drs_noq1)
m11drs_noq1 <- lcmm::hlme(drs_coverage ~ ano2, mixture = ~ano2, random = ~ano2, subject='comuna2', ng = 5, data = coverage_2011_2019_noq1, B=m7drs_noq1)
m12drs_noq1 <- lcmm::hlme(drs_coverage ~ ano2, mixture = ~ano2, random = ~ano2, subject='comuna2', ng = 6, data = coverage_2011_2019_noq1, B=m7drs_noq1)

##m6dm_noq1 <- gridsearch(rep = 500, maxiter = 10, minit = m1dm, m= hlme(dm_coverage2 ~ ano2, mixture = ~ano2, random = ~ano2, subject='comuna2', ng = 6, data = coverage2, B=m1dm))
summary(m8drs_noq1)
## Sensitivity analysis table for DGCC 2011-2023-----------------------------


tab_drs_2011_2019_noq1 <- as.data.frame(lcmm::summarytable(m7drs_noq1, m8drs_noq1, m9drs_noq1, m10drs_noq1, m11drs_noq1, m12drs_noq1, 
                                                           which=c("G", 
                                                                   "loglik", 
                                                                   "conv", 
                                                                   "npm", 
                                                                   "AIC", 
                                                                   "BIC", 
                                                                   "SABIC", 
                                                                   "entropy", 
                                                                   "ICL", 
                                                                   "ICL1", 
                                                                   "ICL2", 
                                                                   "%class")))


## Merge classification of size drs trajectory ------------------------------
drs_2011_2019_noq1 <- left_join(coverage_2011_2019_noq1, round(m7drs_noq1$pprob,3), by = "comuna2") ##one trajectories
drs_2011_2019_noq1$class <- as.factor(drs_2011_2019_noq1$class)

drs2_2011_2019_noq1 <- left_join(coverage_2011_2019_noq1, round(m8drs_noq1$pprob,3), by = "comuna2") %>% 
  mutate(class = factor(class)) ##one trajectories

#drs2_2011_2019_noq1$class <- as.factor(drs_2011_2019_noq1$class)

write.csv(drs2_2011_2019_noq1, "drs2_2011_2019_noq1.csv", row.names = F)

i <- m8drs_noq1$pprob

j <- drs2_2011_2019_noq1 %>% 
  select(comuna, comuna2, zona, id_servicio, id_region, ano, drs_coverage, class) %>% 
  mutate(id_region = ifelse(id_region==16, 8, id_region),
         id_region2 = match(id_region, unique(id_region))) %>%  #Tratar a Ñuble como si hubiera siempre pertencido a una misma region  
  spread(ano, drs_coverage) %>% 
  select(comuna, comuna2, "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018", "2019")


ij <- merge(j, i, by= "comuna2")


supplementary_data_DRSC_2011_2019 <- ij %>% 
  select(-comuna2)


supplementary_data_DRSC_2011_2019 <- supplementary_data_DRSC_2011_2019 %>% 
  mutate(completeness_2011_2019 = rowMeans(!is.na(select(., -comuna, -class, -prob1, -prob2))),
         completeness_2011_2019 = round(completeness_2011_2019*100, 1),
         average_years = rowSums(!is.na(supplementary_data_DRSC_2011_2019[,2:10])))
supplementary_data_DRSC_2011_2019

write.csv(supplementary_data_DRSC_2011_2019, "supplementary_data_DRSC_2011_2019.csv")
# Average completeness ----------------------------------------------------


supplementary_data_DRSC_2011_2019 %>% 
  select(completeness_2011_2019) %>% 
  summary() # 89.62  
0.8962  *9


# Average completeness for classes ----------------------------------------
supplementary_data_DRSC_2011_2019 %>% 
  filter(class==1) %>% 
  select(completeness_2011_2019) %>% 
  summary() # 90.07  
0.9007  *9

supplementary_data_DRSC_2011_2019 %>% 
  filter(class==2) %>% 
  select(completeness_2011_2019) %>% 
  summary() # 88.3% 
0.883*9

# checkear variances ------------------------------------------------------

k <- supplementary_data_DRSC_2011_2019 %>% 
  filter(class==1)  %>% 
  select(completeness_2011_2019)

l <- supplementary_data_DRSC_2011_2019 %>% 
  filter(class==2) %>% 
  select(completeness_2011_2019)

var.test(k$completeness_2011_2019, l$completeness_2011_2019) # las varianzas son distintas


# t-test para varianza distinta -----------------------------------------------


t.test(k$completeness_2011_2019, l$completeness_2011_2019, var.equal = F) # Hay DES para completeness between trajectories



drs3_2011_2019_noq1 <- merge(coverage_2011_2019_noq1, round(m9drs_noq1$pprob,3), by = "comuna2") ##three trajectories
drs3_2011_2019_noq1$class <- as.factor(drs3_2011_2019_noq1$class)

drs4_2011_2019_noq1 <- merge(coverage_2011_2019_noq1, round(m10drs_noq1$pprob,3), by = "comuna2") ##four trajectories
drs4_2011_2019_noq1$class <- as.factor(drs4_2011_2019_noq1$class)

drs5_2011_2019_noq1 <- merge(coverage_2011_2019_noq1, round(m11drs_noq1$pprob,3), by = "comuna2") ##five trajectories
drs5_2011_2019_noq1$class <- as.factor(drs5_2011_2019_noq1$class)

drs6_2011_2019_noq1 <- merge(coverage_2011_2019_noq1, round(m12drs_noq1$pprob,3), by = "comuna2") ##five trajectories
drs6_2011_2019_noq1$class <- as.factor(drs6_2011_2019_noq1$class)

## Model fit criteria ------------------------------------------------------

model_fit_criteria_drs_2011_2019_noq1 <- tab_drs_2011_2019_noq1 %>% 
  dplyr::select(G,loglik, npm, AIC, BIC, SABIC) %>% 
  dplyr::mutate(sample_size= c(m7drs_noq1$ns, m8drs_noq1$ns, m9drs_noq1$ns, m10drs_noq1$ns, m11drs_noq1$ns, m12drs_noq1$ns),
                CAIC = -2*loglik + 2*(log(sample_size)+1),##𝐶𝐴𝐼𝐶 = −2(𝐿𝐿) + 𝑑[𝑙𝑜𝑔(𝑛) + 1]
                AWE= -2*loglik + 2*(log(sample_size)+1.5),##𝐴𝑊𝐸 = −2(𝐿𝐿) +𝑑[𝑙𝑜𝑔(𝑛)+ 1.5]
                SIC = -0.05*BIC,
                BF = exp(lead(SIC)-SIC),##BF = exp[SICa − SICb], where Schwartz Information Criterion, is defined as SIC=-0.05*BIC
                cmP = exp((SIC - max(SIC))/sum(exp(SIC - max(SIC))))) %>%  
  tibble::as.tibble() %>% 
  tibble::rownames_to_column() %>% 
  dplyr::select(-rowname) 

write.csv(model_fit_criteria_drs_2011_2019_noq1, "model_fit_criteria_drs_2011_2019_noq1.csv", row.names = F)


## Diagnostic criteria -----------------------------------------------------

## average latent class posterior probability ------------------------------

postprob(m7drs_noq1)
postprob(m8drs_noq1)
postprob(m9drs_noq1)
postprob(m10drs_noq1)
postprob(m11drs_noq1)
postprob(m12drs_noq1)

# Assuming postprob() returns a structured list
posterior_probsm8drs_noq1 <- postprob(m8drs_noq1)  
posterior_probsm9drs_noq1 <- postprob(m9drs_noq1)  
posterior_probsm10drs_noq1 <- postprob(m10drs_noq1)  
posterior_probsm11drs_noq1 <- postprob(m11drs_noq1)  
posterior_probsm12drs_noq1 <- postprob(m12drs_noq1)  


# OCC
# Assuming you have a list of your model objects
models_list4 <- list(m8drs_noq1, m9drs_noq1, m10drs_noq1, m11drs_noq1, m12drs_noq1)  # Replace with your actual model objects

# Extract the lower OCC values for each model
lower_occ_values4 <- sapply(models_list4, function(model) {
  occ_values4 <- LCTMtoolkit(model)$occ
  min(as.numeric(occ_values4[1,]))  # Assuming OCC is in the first row of the OCC matrix
})

# Print the lower OCC values
print(lower_occ_values3)



# Mismatch
# Extract the lower OCC values for each model
highest_mismatch_values4 <- sapply(models_list4, function(model) {
  mismatch_values4 <- LCTMtoolkit(model)$mismatch
  max(as.numeric(mismatch_values4[1,]))  # Assuming OCC is in the first row of the OCC matrix
})

# Print the lower OCC values
print(highest_mismatch_values4)



# Sample inputs for calc_lrt function calls Lo–Mendell–Rubin adjusted LRT  ---------------
outputs_vllrt4 <- list(
  extract_p_value_from_lrt(m7drs_noq1$ns, tab_drs_2011_2023_noq1$loglik[1], tab_drs_2011_2023_noq1$npm[1], tab_drs_2011_2023_noq1$G[1], tab_drs_2011_2023_noq1$loglik[2], tab_drs_2011_2023_noq1$npm[2], tab_drs_2011_2023_noq1$G[2]),
  extract_p_value_from_lrt(m8drs_noq1$ns, tab_drs_2011_2023_noq1$loglik[2], tab_drs_2011_2023_noq1$npm[2], tab_drs_2011_2023_noq1$G[2], tab_drs_2011_2023_noq1$loglik[3], tab_drs_2011_2023_noq1$npm[3], tab_drs_2011_2023_noq1$G[3]),
  extract_p_value_from_lrt(m9drs_noq1$ns, tab_drs_2011_2023_noq1$loglik[3], tab_drs_2011_2023_noq1$npm[3], tab_drs_2011_2023_noq1$G[3], tab_drs_2011_2023_noq1$loglik[4], tab_drs_2011_2023_noq1$npm[4], tab_drs_2011_2023_noq1$G[4]),
  extract_p_value_from_lrt(m10drs_noq1$ns, tab_drs_2011_2023_noq1$loglik[4], tab_drs_2011_2023_noq1$npm[4], tab_drs_2011_2023_noq1$G[4], tab_drs_2011_2023_noq1$loglik[5], tab_drs_2011_2023_noq1$npm[5], tab_drs_2011_2023_noq1$G[5]),
  extract_p_value_from_lrt(m11drs_noq1$ns, tab_drs_2011_2023_noq1$loglik[5], tab_drs_2011_2023_noq1$npm[5], tab_drs_2011_2023_noq1$G[5], tab_drs_2011_2023_noq1$loglik[6], tab_drs_2011_2023_noq1$npm[6], tab_drs_2011_2023_noq1$G[6])
)

# Assuming outputs is already defined as your object
values_vllrt4 <- sapply(outputs_vllrt4, function(x) gsub("^= ", "", x))

# Print the vector
print(values_vllrt4)

#Diagnistic criteria

diagnostic_criteria_drs_2011_2023_noq1 <- tab_drs_2011_2023_noq1 %>% 
  select(G, entropy) %>% 
  mutate(smallest_class_count = c(m1drs_noq1$ns, min(posterior_probsm2drs_noq1[[1]]), min(posterior_probsm3drs_noq1[[1]]), min(posterior_probsm4drs_noq1[[1]]), min(posterior_probsm5drs_noq1[[1]]), min(posterior_probsm6drs_noq1[[1]])),
         smallest_class_size_perc = c(1, min(posterior_probsm2drs_noq1[[1]])/m2drs_noq1$ns, min(posterior_probsm3drs_noq1[[1]])/m3drs_noq1$ns, min(posterior_probsm4drs_noq1[[1]])/m4drs_noq1$ns, min(posterior_probsm5drs_noq1[[1]])/m5drs_noq1$ns, min(posterior_probsm6drs_noq1[[1]])/m6drs_noq1$ns),
         smallest_class_size_perc = percent(round(smallest_class_size_perc, 4)),
         ALCPP = c(NA, min(diag(posterior_probsm2drs_noq1[[2]])), min(diag(posterior_probsm3drs_noq1[[2]])), min(diag(posterior_probsm4drs_noq1[[2]])), min(diag(posterior_probsm5drs_noq1[[2]])), min(diag(posterior_probsm6drs_noq1[[2]]))),
         "Highest MMV" =c(NA, highest_mismatch_values4),
         "Lowest OCC" = c(NA, lower_occ_values4),
         VLMRLRT = c(NA, values_vllrt4)
  ) %>% 
  tibble::as.tibble() %>% 
  tibble::rownames_to_column() %>% 
  dplyr::select(-rowname) 

write.csv(diagnostic_criteria_drs_2011_2023_noq1, "diagnostic_criteria_drs_2011_2023_noq1.csv", row.names = F)

## Plot trajectories -------------------------------------------------------

pdrs_2011_2019_noq1 <- ggplot(drs_2011_2019_noq1, aes(x=ano2, y=drs_coverage, colour = class, group = comuna2)) +
  ##geom_point(size = .1) + 
  ##geom_line(size = .05) +
  scale_x_continuous(breaks = c(0:8), labels=2011:2019) +
  xlab("Year") +
  ylab("DRSC")+
  theme(legend.position = "none") +
  geom_smooth(se = T, method = "loess", aes(group = class))

pdrs2_2011_2019_noq1 <- ggplot(drs2_2011_2019_noq1, aes(x=ano2, y=drs_coverage, colour = class, group = comuna2)) +
  ##geom_point(size = .1) + 
  ##geom_line(size = .05) +
  scale_x_continuous(breaks = c(0:8), labels=2011:2019) +
  xlab("Year") +
  ylab("DRSC")+
  theme(legend.position = "none") +
  geom_smooth(se = T, method = "loess", aes(group = class))

pdrs3_2011_2019_noq1 <- ggplot(drs3_2011_2019_noq1, aes(x=ano2, y=drs_coverage, colour = class, group = comuna2)) +
  ##geom_point(size = .1) + 
  ##geom_line(size = .05) +
  scale_x_continuous(breaks = c(0:8), labels=2011:2019) +
  xlab("Year") +
  ylab("DRSC")+
  theme(legend.position = "none") +
  geom_smooth(se = T, method = "loess", aes(group = class))

pdrs4_2011_2019_noq1 <- ggplot(drs4_2011_2019_noq1, aes(x=ano2, y=drs_coverage, colour = class, group = comuna2)) +
  ##geom_point(size = .1) + 
  ##geom_line(size = .05) +
  scale_x_continuous(breaks = c(0:8), labels=2011:2019) +
  xlab("Year") +
  ylab("DRSC")+
  theme(legend.position = "none") +
  geom_smooth(se = T, method = "loess", aes(group = class))

pdrs5_2011_2019_noq1 <- ggplot(drs5_2011_2019_noq1, aes(x=ano2, y=drs_coverage, colour = class, group = comuna2)) +
  ##geom_point(size = .1) + 
  ##geom_line(size = .05) +
  scale_x_continuous(breaks = c(0:8), labels=2011:2019) +
  xlab("Year") +
  ylab("DRSC")+
  theme(legend.position = "none") +
  geom_smooth(se = T, method = "loess", aes(group = class))

pdrs6_2011_2019_noq1 <- ggplot(drs6_2011_2019_noq1, aes(x=ano2, y=drs_coverage, colour = class, group = comuna2)) +
  ##geom_point(size = .1) + 
  ##geom_line(size = .05) +
  scale_x_continuous(breaks = c(0:8), labels=2011:2019) +
  xlab("Year") +
  ylab("DRSC")+
  theme(legend.position = "none") +
  geom_smooth(se = T, method = "loess", aes(group = class))

grid.arrange(pdrs_2011_2019_noq1, pdrs2_2011_2019_noq1, pdrs3_2011_2019_noq1, pdrs4_2011_2019_noq1, pdrs5_2011_2019_noq1, pdrs6_2011_2019_noq1)


## Índice sociodemográfico -------------------------------------------------
isde2 <- read_excel("SocioEconominoSaludComunas.xlsx")

isde2 <- isde2[,c(2,4)]

isde2 <- isde2 %>% 
  dplyr::mutate(comuna = ...2 ) %>% 
  dplyr::select(comuna, index) %>% 
  arrange(comuna)


class2 <- drs2_2011_2019_noq1 %>% 
  mutate(ano2=match(ano, unique(ano)),
         id_region = ifelse(id_region==16, 8, id_region), #Tratar a Ñuble como si hubiera siempre pertencido a una misma region  
         id_region2 = match(id_region, unique(id_region)),
         zona = factor(zona),
         id_servicio2 = match(id_servicio, unique(id_servicio))) %>% 
  dplyr::group_by(comuna, comuna2, class,id_servicio, id_region, zona) %>% 
  dplyr::summarise(mean_drs_coverage = mean(drs_coverage, na.rm=T),
                   mean_dm_coverage = mean(dm_coverage, na.rm=T)) %>% 
  distinct(comuna, .keep_all = TRUE) %>% 
  mutate(comuna2 = cur_group_id()) 




class2[!(class2$comuna %in% isde2$comuna), ] 

isde2$comuna[isde2$comuna == "Aisen"] <- "Aisén"
isde2$comuna[isde2$comuna =="Padre las Casas"] <- "Padre Las Casas"
class2$comuna[170] = isde2$comuna[192] ## esto arregla PAC
isde2$comuna[isde2$comuna =="San Juan de La Costa"] <- "San Juan de la Costa" 


class2[!(class2$comuna %in% isde2$comuna), ] 


data_reglog2 <- right_join(class2, isde2) 

##View(data_reglog)

data_reglog2 <- data_reglog2 %>% 
  mutate(class_membership = ifelse(class==2, 1, 0)) 


data_reglog2$index_standardized <- scale(data_reglog2$index)


rurality <- read_excel("Clasificacion-comunas-PNDR.xlsx")
rurality <- janitor::clean_names(rurality) %>% 
  select(comuna, tipo_com, clasificacion) %>% 
  mutate(urbanisation = tipo_com,
         comuna = str_to_title(comuna),
         classification= clasificacion) %>% 
  select(-tipo_com)

rurality$comuna <- gsub("\\bDe\\b", "de", rurality$comuna, ignore.case = TRUE)
rurality$comuna <- gsub("\\bDel\\b", "del", rurality$comuna, ignore.case = TRUE)
rurality$comuna <- gsub("\\b La\\b", " la", rurality$comuna, ignore.case = TRUE)
rurality$comuna[rurality$comuna == "Aysén"] <- "Aisén"
rurality$comuna[rurality$comuna == "Coyhaique"] <- "Coihaique"
rurality$comuna[rurality$comuna == "Alto Biobío"] <- "Alto Bío-Bío"

rurality[!(rurality$comuna %in% data_reglog2$comuna), ] 

data_reglog2[!(data_reglog2$comuna %in% rurality$comuna), ] 

data_reglog2 <- right_join(data_reglog2, rurality) 

data_reglog2 <- data_reglog2 %>%
  mutate(zona = ifelse(zona == 1, 'norte', 
                       ifelse(zona == 2, 'centro', 'sur')))


write.csv(data_reglog2, "data_reglog2.csv")






# Supplementary material --------------------------------------------------



sm_drsc <- full_join(supplementary_data_DRSC_2011_2023, supplementary_data_DRSC_2011_2019, by= "comuna") %>% 
  select(-"2011.y", - "2012.y", -"2013.y", -"2014.y", -"2015.y", -"2016.y", -"2017.y", -"2018.y", -"2019.y") %>% 
  dplyr::rename("2011" = "2011.x",
                "2012" = "2012.x",
                "2013" = "2013.x",
                "2014" = "2014.x",
                "2015" = "2015.x",
                "2016" = "2016.x",
                "2017" = "2017.x",
                "2018" = "2018.x",
                "2019" = "2019.x") %>% 
  dplyr::rename_with(~str_replace(., "\\.x$", "_2011_2023"), contains(".x")) %>% 
  dplyr::rename_with(~str_replace(., "\\.y$", "_2011_2019"), contains(".y")) 

supplementary_data_DRSC <- sm_drsc %>%
  mutate(across(starts_with("prob") | starts_with("completeness"), ~ round(., 1))) %>% 
  mutate(across(starts_with("20"), ~ round(.*100, 1))) %>% 
  arrange(comuna)

write.csv(supplementary_data_DRSC, "supplementary_data_DRSC.csv")


sm_dgcc <- full_join(supplementary_data_DGCC_2011_2023, supplementary_data_DGCC_2011_2019, by= "comuna") %>% 
  select(-"2011.y", - "2012.y", -"2013.y", -"2014.y", -"2015.y", -"2016.y", -"2017.y", -"2018.y", -"2019.y") %>% 
  dplyr::rename("2011" = "2011.x",
                "2012" = "2012.x",
                "2013" = "2013.x",
                "2014" = "2014.x",
                "2015" = "2015.x",
                "2016" = "2016.x",
                "2017" = "2017.x",
                "2018" = "2018.x",
                "2019" = "2019.x") %>% 
  dplyr::rename_with(~str_replace(., "\\.x$", "_2011_2023"), contains(".x")) %>% 
  dplyr::rename_with(~str_replace(., "\\.y$", "_2011_2019"), contains(".y")) 

supplementary_data_DGCC <- sm_dgcc %>%
  mutate(across(starts_with("completeness"), ~ round(., 1))) %>% 
  mutate(across(starts_with("20"), ~ round(.*100, 1))) %>% 
  arrange(comuna)

write.csv(supplementary_data_DGCC, "supplementary_data_DGCC.csv")


write.xlsx(supplementary_data_DGCC, "Supplementary material DGCC.xlsx", sheetName = "DGCC", append = TRUE, row.names = FALSE)
write.xlsx(supplementary_data_DRSC, "Supplementary material DRSC.xlsx", sheetName = "DRSC", append = TRUE, row.names = FALSE)


#Completeness DGCC
skimr::skim(supplementary_data_DGCC_2011_2023) 

supplementary_data_DGCC_2011_2023 %>% 
  select(1:13) %>% 
  gather(key, value,2:13) %>% 
  summarise(mean = mean(value, na.rm=T))
  
  
  
mean(supplementary_data_DGCC_2011_2023$completeness_2011_2023)
sd(supplementary_data_DGCC_2011_2023$completeness_2011_2023)
mean(supplementary_data_DGCC_2011_2023$average_years)
sd(supplementary_data_DGCC_2011_2023$average_years)
skimr::skim(supplementary_data_DGCC_2011_2019)
mean(supplementary_data_DGCC_2011_2019$completeness_2011_2019)
sd(supplementary_data_DGCC_2011_2019$completeness_2011_2019)
mean(supplementary_data_DGCC_2011_2019$average_years)
sd(supplementary_data_DGCC_2011_2019$average_years)

#Completeness DRSC
skimr::skim(supplementary_data_DRSC_2011_2023)
mean(supplementary_data_DRSC_2011_2023$completeness_2011_2023)
sd(supplementary_data_DRSC_2011_2023$completeness_2011_2023)
mean(supplementary_data_DRSC_2011_2023$average_years)
sd(supplementary_data_DRSC_2011_2023$average_years)
skimr::skim(supplementary_data_DRSC_2011_2019)
mean(supplementary_data_DRSC_2011_2019$completeness_2011_2019)
sd(supplementary_data_DRSC_2011_2019$completeness_2011_2019)
mean(supplementary_data_DRSC_2011_2019$average_years)
sd(supplementary_data_DRSC_2011_2019$average_years)

#Completeness DRSC 2011-2023
skimr::skim(supplementary_data_DRSC_2011_2023 %>% 
              filter(class==2))

supplementary_data_DRSC_2011_2023 %>% 
  filter(class==2) %>% 
  select(3) %>% 
 drop_na()

modelsummary::datasummary_skim(supplementary_data_DRSC_2011_2023 %>% 
                                 filter(class==2),
                               fmt = "%.4f")

modelsummary::datasummary_skim(supplementary_data_DRSC_2011_2023)

supplementary_data_DRSC_2011_2023 %>% 
  filter(class==1)%>%
  summarise(mean_percentage = mean(completeness_2011_2023),
            sd_percentage =sd(completeness_2011_2023),
            mean_years = mean(average_years),
            sd_years = sd(average_years))

supplementary_data_DRSC_2011_2023 %>% 
  filter(class==2)%>%
  summarise(mean_percentage = mean(completeness_2011_2023),
            sd_percentage =sd(completeness_2011_2023),
            mean_years = mean(average_years),
            sd_years = sd(average_years))

#Completeness DRSC 2011-2019
skimr::skim(supplementary_data_DRSC_2011_2019 %>% 
              filter(class==1))

supplementary_data_DRSC_2011_2019 %>% 
  filter(class==1)%>%
  summarise(mean_percentage = mean(completeness_2011_2019),
            sd_percentage =sd(completeness_2011_2019),
            mean_years = mean(average_years),
            sd_years = sd(average_years))

supplementary_data_DRSC_2011_2019 %>% 
  filter(class==2)%>%
  summarise(mean_percentage = mean(completeness_2011_2019),
            sd_percentage =sd(completeness_2011_2019),
            mean_years = mean(average_years),
            sd_years = sd(average_years))



sd(supplementary_data_DRSC_2011_2023 %>% 
  filter(class==1) %>%
  select(completeness_2011_2023))









data_summary1 <- modelsummary::datasummary_skim(data_reglog2[1:300,] %>% 
                                                  select("dm_coverage", 
                                                         "drs_coverage", 
                                                         "index_standardized")%>% 
                                                  mutate_if(is.character, as.factor),
                                                type="numeric",
                                                histogram=T,
                                                fmt = "%.4f",
                                                output="gt") %>% 
  as.data.frame() 

data_summary2 <- modelsummary::datasummary_skim(data_reglog2[1:292,] %>% 
                                                  select("dm_coverage", 
                                                         "drs_coverage", 
                                                         "index_standardized")%>% 
                                                  mutate_if(is.character, as.factor),
                                                type="numeric",
                                                histogram=T,
                                                fmt = "%.4f",
                                                output="gt") %>% 
  as.data.frame()

data_summary <- rbind(data_summary2, data_summary1)


data_summary <- data_summary %>% 
  mutate_at(vars(-1, -ncol(.)), as.numeric) %>% 
  mutate(across(4:8, ~ if_else(row_number() %in% c(3, 6), .x, round(.x * 100, 2)))) %>% 
  mutate(Metric= rep(c("DGCC", "DRSC", "ISDE"), 2)) %>% 
  select(Metric, 2:9)

data_summary %>% 
  gt::gt() %>%
  #tab_spanner(label = 'Model fit and diagnostic criteria', columns = 2:16) %>%
  tab_options(
    table.font.size = 10,
    data_row.padding = px(1),
    table.border.top.color = "black",
    heading.border.bottom.color = "black",
    row_group.border.top.color = "black",
    row_group.border.bottom.color = "white",
    table.border.bottom.color = "white",
    column_labels.border.top.color = "black",
    column_labels.border.bottom.color = "black",
    table_body.border.bottom.color = "black",
    table_body.hlines.color = "white") %>% 
  tab_row_group(group = "2011-2023",
                rows = c(4:6)
  ) %>%
  tab_row_group(
    group = "2011-2019",
    rows = c(1:3)
  )



# Load necessary library
library(tibble)

# Create dataframe
df <- tibble::tibble(
  Step = 1:8,
  `Step description` = c(
    "Scope model by provisionally selecting a plausible number of classes based on available literature and the structure based on plausible clinical patterns.",
    "Refine the model from step 1 to confirm the optimal number of classes, typically testing K=1–7 classes.",
    "Refine optimal model structure from fixed through to unrestricted random effects of the model using the favoured K derived in step 2.",
    "Run model adequacy assessments as described in online supplementary table S3 including posterior probability of assignments (APPA), odds of correct classification (OCC) and relative entropy.",
    "Investigate graphical presentation",
    "Run additional tools to assess discrimination including Degrees of separation (DoS) and Elsensohn’s envelope of residuals",
    "Assess for clinical characterisation and plausibility.",
    "Conduct sensitivity analyses, for example, testing models without complete data at all time points."
  ),
  `Criteria for selection` = c(
    "Examine linearity of the shape of standardised residual plots for each of the classes in a model with no random effects.",
    "Lowest Bayesian information criteria value.",
    "",
    "APPA: average of maximum probabilities should be greater than 70% for all classes.\nOCC values greater than 5.0.\nRelative entropy values greater than 0.5.",
    "Plot mean trajectories across time for each class in a single graph.\nPlot mean trajectories with 95% predictive intervals for each class (one class per graph).\nPlot individual class ‘spaghetti plots’ across time for a random sample.",
    "DoS greater than zero.\nEnvelope of residuals is assessed in plots by observing clear separations between classes.",
    "Tabulation of characteristics by latent classes. Are the trajectory patterns clinically meaningful? Perhaps, consider classes with a minimum percentage of the population.\nAre the trajectory patterns clinically plausible?\nConcordance of class characteristics with those for other well-established variables.",
    "General assessment of patterns of trajectories compared with main model."
  )
)

# Display dataframe
print(df)




# Load necessary library
library(dplyr)

# Assuming your dataset is named 'dataset' and has columns: 'Comunes', 'drs_coverage', and 'Years'

# Calculate the overall mean of all drs_coverage values, ignoring NA values
overall_mean <- mean(dataset$drs_coverage, na.rm = TRUE)

# Calculate the mean drs_coverage for each commune
longitudinal_means <- dataset %>%
  group_by(Comunes) %>%
  summarize(mean_drs_coverage = mean(drs_coverage, na.rm = TRUE))

# Calculate the difference between each longitudinal mean and the overall mean
longitudinal_means <- longitudinal_means %>%
  mutate(difference = mean_drs_coverage - overall_mean)

# Print the results
print(longitudinal_means)








# Summarize counts for columns 9 to 10
drs2_2011_2023_noq1 %>%
  dplyr::summarize(
    across(9:10,
           list(
             observations = ~ length(.),
             non_na_observations = ~ sum(!is.na(.)),
             `Missing (n)` = ~ sum(is.na(.))
           ),
           .names = "{.col}_{.fn}"
    )
  ) %>%
  pivot_longer(
    cols = everything(),
    names_to = c("Metric", ".value"),
    names_pattern = "(.*)_(.*)"
  ) %>%
  slice(3 ,1) %>% 
  mutate(Metric = dplyr::recode(Metric,
                         "dm_coverage" = "DGCC",
                         "drs_coverage" = "DRSC")) 



drs2_2011_2019_noq1 %>%
  dplyr::summarize(
    across(9:10,
           list(
             observations = ~ length(.),
             non_na_observations = ~ sum(!is.na(.)),
             `Missing (n)` = ~ sum(is.na(.))
           ),
           .names = "{.col}_{.fn}"
    )
  ) %>%
  pivot_longer(
    cols = everything(),
    names_to = c("Metric", ".value"),
    names_pattern = "(.*)_(.*)"
  ) %>%
  slice(3 ,1) %>% 
  mutate(Metric = dplyr::recode(Metric,
                                "dm_coverage" = "DGCC",
                                "drs_coverage" = "DRSC")) 




summary(model5)
