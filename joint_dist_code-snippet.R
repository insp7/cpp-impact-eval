test <- data %>% 
  select(B12A_OTHER_LANG, B12B_SPEAK_ENGLISH, 
         spoken_english_proficiency_very_well_0, spoken_english_proficiency_well_0, 
         spoken_english_proficiency_not_well_0, spoken_english_proficiency_not_at_all_0)

joint_dist <- table(test)
jd <- as.data.frame(joint_dist)
write_tsv(jd, "sample_jd.tsv")