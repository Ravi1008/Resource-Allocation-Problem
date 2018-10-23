# Importing Packages
library(rio)
library(data.table)
library(tidyverse)
library(matchingR)
library(mosaic)

# Mocked_data
tidyData <- import("Simulated_Data.xlsx")
account_slots <- import("Account_Slots.xlsx")

# Resource Indexation to get Resource_Id
resourceData <- tidyData %>% 
  select(CEC_ID,Grade,Hours) %>%
  group_by(CEC_ID,Grade) %>% 
  summarise(Total_Hours = sum(Hours)) %>% 
  arrange(CEC_ID) %>% 
  rownames_to_column("Resource_Id") %>% 
  mutate(Resource_Id = as.integer(Resource_Id))

# Account Constraint : Find no of accounts per resource
setDT(tidyData)[,Account_Count := n_distinct(Account),
                by = .(CEC_ID)][,c("Account_Hours") := (sum(Hours, na.rm = TRUE)),
                by = .(CEC_ID,Account)][,c("Primary_Account_Hours") := max(Account_Hours, na.rm = TRUE),
                by = .(CEC_ID)]


##%######################################################%##
#                                                          #
####         Phase 1: Resource Account Mapping          ####
#                                                          #
##%######################################################%##

Resource_Account_Mapping <- tidyData %>%
                            select(CEC_ID, Grade ,Account, Hours) %>% 
                            group_by(CEC_ID, Grade, Account) %>% 
                            summarise(Resource_Hours = sum(Hours),
                                      Account_Hours = sum(Hours, na.rm = TRUE)) %>% 
                            ungroup() %>% 
                            group_by(CEC_ID) %>% 
                            arrange(desc(Grade),desc(Resource_Hours)) %>% 
                            mutate(Resource_Priority = ave(Resource_Hours,CEC_ID,FUN=sum)) %>% 
                            ungroup() %>% 
                            group_by(Account) %>% 
                            arrange(Account,desc(Account_Hours)) %>% 
                            mutate(Account_Priority = ave(Account_Hours,Account,FUN=sum)) %>% 
                            arrange(CEC_ID, Resource_Priority)

# Payoff Matrix : Defining for Resource_Account_Mapping
Resource_Account_Mapping$Resource_Priority <- 
  Resource_Account_Mapping$Resource_Hours * (Resource_Account_Mapping$Resource_Hours/Resource_Account_Mapping$Resource_Priority)

Resource_Account_Mapping$Account_Priority  <- 
  Resource_Account_Mapping$Account_Hours * (Resource_Account_Mapping$Account_Hours/Resource_Account_Mapping$Account_Priority)

# Account Indexation to get Account_Id
accountData  <- Resource_Account_Mapping %>% 
  select(Account) %>% 
  distinct() %>% 
  arrange(Account) %>% 
  rownames_to_column("Account_Id") %>% 
  mutate(Account_Id = as.integer(Account_Id))

accountData[is.na(accountData)] <- 0

##%######################################################%##
#                                                          #
####          Phase 1: Gale Shapley Algorithm           ####
#                                                          #
##%######################################################%##

# set seed for reproducibility
set.seed(1)

# set number of Resources
nResources <- nrow(resourceData)

# set number of Accounts
nAccounts <- nrow(accountData)

a <-  Resource_Account_Mapping %>% 
  ungroup() %>% 
  select(CEC_ID,Account,Account_Priority) %>% 
  spread(key = CEC_ID, value = Account_Priority) %>% 
  select(-Account)

a <- as.matrix(a, nrow = nAccounts, ncol = nResources, byrow = FALSE)
colnames(a) <- NULL

a[is.na(a)] <- rep(0,sum(is.na(a)))

uResources <- a

b <-  Resource_Account_Mapping %>% 
  ungroup() %>% 
  select(CEC_ID,Account,Resource_Priority) %>% 
  spread(key = Account, value = as.numeric(Resource_Priority)) %>% 
  select(-CEC_ID)

b <- as.matrix(b, nrow = nResources, nrow = nAccounts, byrow = FALSE)
colnames(b) <- NULL

b[is.na(b)] <- rep(0,sum(is.na(b)))

uAccounts <- b

slot_values <- rep(ceiling(nResources/nAccounts) * 100, nrow(accountData))
#slot_values <- as.vector(account_slots$Demand)

# Resource Optimal Matching : Preferences must be complete
phase_1_results = galeShapley.collegeAdmissions(studentUtils =  uResources, collegeUtils =  uAccounts, 
                                                slots = slot_values ,studentOptimal = TRUE)
str(phase_1_results)

# Optional Stability Check : TRUE
#galeShapley.checkStability(uResources, uAccounts, phase_1_results$matched.students, phase_1_results$matched.colleges)

# Stitching things back together
resourceData$Account_Id <- as.vector(phase_1_results$matched.students)
final <- left_join(resourceData,accountData,by = c("Account_Id"))

phase_1_output  <-  final %>% 
  mutate(counter = 1) %>% ungroup() %>% 
  group_by(Account) %>% 
  arrange(Account,desc(Grade), desc(Total_Hours)) %>% 
  mutate(LT_Priority = ave(counter,Account, FUN = cumsum),
         Resource_Count = ave(counter,Account,FUN=sum)) %>% 
  mutate(Team_Position = ifelse(LT_Priority == 1, "Coach","Member"))

##%######################################################%##
#                                                          #
####  Phase 1: Diagnostics for Resource_Account Combo   ####
#                                                          #
##%######################################################%##

# Greedy Accounts
greedyAccount_set  <- phase_1_output %>% ungroup() %>%  filter(Resource_Count > 8)
length(unique(greedyAccount_set$Account))
length(unique(greedyAccount_set$CEC_ID))

# Liberal Accounts
liberaAccount_set <- phase_1_output %>% ungroup() %>%  filter(Resource_Count < 4)
length(unique(liberaAccount_set$Account))
length(unique(liberaAccount_set$CEC_ID))

normalAccout_set <- phase_1_output %>% ungroup() %>% filter(Resource_Count <= 8 & Resource_Count >= 4)
length(unique(normalAccout_set$Account))
length(unique(normalAccout_set$CEC_ID))

# Unrelated Resources
unrelatedResource_set <-  Resource_Account_Mapping %>% 
  select(CEC_ID,Grade,Account,Resource_Priority,Account_Priority) %>% 
  right_join(phase_1_output) %>% 
  filter(is.na(Resource_Priority))

nrow(unrelatedResource_set)

# Assertion: It is not just the quantum of unrelated Resource but quality of them.

length(unique(phase_1_output$Account))

length(unique(Resource_Account_Mapping$Account))

Resource_Account_Mapping %>% 
  filter(! Account %in% phase_1_output$Account) %>% distinct(Account)

# Stitching Back
stitch <- phase_1_output %>% 
          ungroup() %>% 
          select(CEC_ID,Account,Account_Id,Team_Position) %>% 
          rename(Assigned_Account = Account,
                 Assigned_Account_Id = Account_Id)


summaryData <- tidyData %>% 
  left_join(stitch, by = "CEC_ID") %>% 
  mutate(Assigned_Hours = ifelse(Account == Assigned_Account,Hours,0))

resource_summary <- summaryData %>% 
  ungroup() %>% 
  select(CEC_ID,Grade,Hours,Assigned_Account,Assigned_Hours) %>% 
  group_by(CEC_ID,Grade,Assigned_Account) %>% 
  summarise(Hours = sum(Hours, na.rm = T),
            Assigned_Hours = sum(Assigned_Hours, na.rm = T))

# Result 0.2934646
print(sum(resource_summary$Assigned_Hours) / sum(resource_summary$Hours))

account_summary <- summaryData %>% 
  ungroup() %>% 
  select(Account, Hours) %>% 
  group_by(Account) %>% 
  summarise(Hours = sum(Hours, na.rm = T)) %>% 
  left_join(
    summaryData %>% 
      ungroup() %>% 
      select(Assigned_Account,Assigned_Hours) %>% 
      group_by(Assigned_Account) %>% 
      summarise(Assigned_Hours = sum(Assigned_Hours,na.rm = T)),
    by = c("Account" = "Assigned_Account")) %>% 
  na.omit()

# Result 0.3359316
print(sum(account_summary$Assigned_Hours) / sum(account_summary$Hours))


# empty_df <- data.frame(Iteration,Resource_Metric,Account_Metric)
empty_df <- setNames(data.frame(matrix(ncol = 3, nrow = 0)), c("Iteration", "Resource_Metric", "Account_Metric"))

i <- 1
for(i in 1:5000){
  
empty_df[i,1] <- i

stitch_2 <- phase_1_output %>% 
            ungroup() %>%
            select(CEC_ID) %>% 
            mutate(Account_Id = sample(1:207, 638, replace = T)) %>% 
            left_join(accountData,by = "Account_Id") %>% 
            rename(Assigned_Account = Account,
            Assigned_Account_Id = Account_Id)

summaryData <- tidyData %>% 
               left_join(stitch_2, by = "CEC_ID") %>% 
               mutate(Assigned_Hours = ifelse(Account == Assigned_Account,Hours,0))

resource_summary <- summaryData %>% 
                    ungroup() %>% 
                    select(CEC_ID,Grade,Hours,Assigned_Account,Assigned_Hours) %>% 
                    group_by(CEC_ID,Grade,Assigned_Account) %>% 
                    summarise(Hours = sum(Hours, na.rm = T),
                              Assigned_Hours = sum(Assigned_Hours, na.rm = T))

# Result 0.2934646
print(sum(resource_summary$Assigned_Hours) / sum(resource_summary$Hours))
empty_df[i,2] <- sum(resource_summary$Assigned_Hours) / sum(resource_summary$Hours)

account_summary <- summaryData %>% 
                   ungroup() %>% 
                   select(Account, Hours) %>% 
                   group_by(Account) %>% 
                   summarise(Hours = sum(Hours, na.rm = T)) %>% 
                   left_join(
                     summaryData %>% 
                     ungroup() %>% 
                     select(Assigned_Account,Assigned_Hours) %>% 
                     group_by(Assigned_Account) %>% 
                     summarise(Assigned_Hours = sum(Assigned_Hours,na.rm = T)),
                     by = c("Account" = "Assigned_Account")) %>% 
                   na.omit()

# Result 0.3359316
print(sum(account_summary$Assigned_Hours) / sum(account_summary$Hours))
empty_df[i,3] <- sum(account_summary$Assigned_Hours) / sum(account_summary$Hours)
i <- i+1
}

ggplot(data = empty_df) +
  aes(x = Iteration, y = Resource_Metric, fill = Resource_Metric) +
  geom_line(color = "#0c4c8a") +
  # geom_hline(yintercept = 0.33) +
  theme_minimal()

export(empty_df,"simulation_results.xlsx")
empty_df <- import("simulation_results.xlsx")

table <- empty_df %>% top_n(n = 100)
