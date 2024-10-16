library(readr)
library(dplyr)
library(ggplot2)

#urban adaptations from Santini et al., 2018
urban_species <- read_csv("C:/Users/tizge/Documents/HW project/urban_species_and_their_adaptations_from_Santini_2018.csv")%>%
  dplyr::select(-Order, -Family) %>%
  rename(species=Species)
urb_spp <- urban_species$Species


#host-disease associations from Albery et al., 2022
url = "https://raw.githubusercontent.com/viralemergence/UrbanOutputters/refs/heads/main/EskewFiles/res.imp.FIXED.csv"
d <- read_csv(url(url))
d <- d %>% mutate(urban_reservoir = ifelse(species %in% urb_spp, 1, 0))

lept <- d %>% dplyr::filter(grepl("lepto", pathogen, ignore.case=TRUE)) 
unique(lept$species)
#add urban adaptation
d<-left_join(d, urban_species)


#urban hosts in north america
d_urb <- d %>% dplyr::filter(urban_reservoir==1) %>% dplyr::filter(n.america==1 | worldwide ==1) 
d_urb_hosts <- unique(d_urb$species)
d_urb_pathogens <- unique(d_urb$pathogen)
#note some may be missing like white-tailed deer

#group by type and by pathogen
host_communities <- d_urb %>% group_by(type,  transmission, vector.id, pathogen,target, MSW05_Order, species, Urban) %>% count()
pathos <- as.data.frame(unique(host_communities$pathogen))
names(pathos)<- c("pathogen")
pathos$id <- c(1:nrow(pathos))

host_communities <- left_join(host_communities, pathos)
# write.csv(host_communities, "C:/Users/tizge/Documents/HW project/urban_host_communities2.csv")


#add pathogen community id to the hosts, to see to how many pathogen communities they belong
hosts_sp <- host_communities %>% group_by(MSW05_Order,species, pathogen, type, vector.id) %>% count()
hosts_sp_simple <- host_communities %>% group_by(MSW05_Order,species, vector.id) %>% count()
# write.csv(hosts_sp, "C:/Users/tizge/Documents/HW project/urban_host_communities_hostSP.csv")


#separate host (reservoir) communities into three groups: 
#1. direct contact, 2. vector-borne (ticks) 3. Helminths
regrouped_host_communities <- host_communities %>% mutate(group = 
                                                            ifelse(type == "helminth"| transmission == "T", "Endoparasite", 
                                                                   ifelse(grepl("DC",transmission), "Direct-contact",
                                                                          ifelse(grepl("IC",transmission), "Environmental",
                                                                                 ifelse(vector.id == "ticks", "Tick-borne", "flying-bug-borne"))))
)  %>% dplyr::filter(!(target %in% c("cattle","deer","wild felids", "wild canids")))
regrouped_host_communities1 <- regrouped_host_communities %>% group_by(group, type,  transmission, vector.id, pathogen,target, MSW05_Order, species, Urban, id) %>% count()
write.csv(regrouped_host_communities1, "C:/Users/tizge/Documents/HW project/urban_host_communities_regrouped.csv")

regrouped_host_communities2 <- regrouped_host_communities %>% group_by(group, MSW05_Order, species) %>% count()
groups <- as.data.frame(unique(regrouped_host_communities2$group))
names(groups)<- c("group")
groups$id <- c(1:nrow(groups))

regrouped_host_communities2 <- left_join(regrouped_host_communities2, groups)

View(regrouped_host_communities)
regrouped_host_communities3 <- regrouped_host_communities2 %>% dplyr::filter(!(MSW05_Order %in% c("Rodentia", "Soricomorpha", "Chiroptera")))
ggplot(regrouped_host_communities3, aes(x=group, y=species, color=MSW05_Order))+
  geom_boxplot()

#clean columns
d <- d %>% dplyr::select("pathogen", "type", "target", "transmission", "vector.id", "reservoir.type", "species", "MSW05_Order", "AdultMass_g")

#similarity matrix
#make correlation matrix
groups_spp <- regrouped_host_communities1 %>% group_by(group, MSW05_Order, species) %>% count() 
summary_all_species <- regrouped_host_communities1 %>% group_by(MSW05_Order, species) %>% count() 
all_spp <- as.data.frame(summary_all_species)
DC_spp <-groups_spp[which(groups_spp$group=="Direct-contact"),]$species
EP_spp <-groups_spp[which(groups_spp$group=="Endoparasite"),]$species
EN_spp <-groups_spp[which(groups_spp$group=="Environmental"),]$species
TB_spp <-groups_spp[which(groups_spp$group=="Tick-borne"),]$species
FB_spp <-groups_spp[which(groups_spp$group=="flying-bug-borne"),]$species


host_matrix <- all_spp %>% dplyr::mutate(
  DC = ifelse(species %in% DC_spp, 1, 0),
  EP = ifelse(species %in% EP_spp, 1, 0),
  EN = ifelse(species %in% EN_spp, 1, 0),
  TB = ifelse(species %in% TB_spp, 1, 0),
  FB = ifelse(species %in% FB_spp, 1, 0))

host_matrix <-  host_matrix %>% dplyr::filter(MSW05_Order != "Rodentia")%>% 
  dplyr::filter(species != "Canis lupus")%>% 
  dplyr::filter(species != "Felis catus")


library(corrplot)
corrplot(as.matrix(host_matrix[,c(4:8)]), method = 'shade') # colorful number

#host matrix can be used to prune species based on pathogen groups. 

#add to database missing interactions

