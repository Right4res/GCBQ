library(readxl)
library(corrplot)
#library(factoextra)
library(caret)
library(tidyverse)

Data_location <- 'C:/Users/phili/OneDrive/UNI/CBQ/GCBQ/BF&BD/BD Assessment/'


Shooting <- readxl::read_excel(path = file.path(Data_location, "FBRef_player_stats_all_PL21-22.xlsx"), 
                   sheet = "Shooting") %>%
  filter(!is.na(ID))


Shooting %>%
  filter(Pos != "GK") %>%
  ggplot(aes(x = `Standard - Sh`, fill = Pos_primary)) +
  geom_density(position = "dodge", alpha = 0.5)
                   
               

Shooting %>%
  filter(Pos != "GK") %>%
  ggplot(aes(x = `Expected - xG`, fill = Pos_primary)) +
  geom_density(position = "dodge", alpha = 0.5)    

Shooting_numerical <- Shooting %>% select(Age:`Expected - np:G-xG`, -Born) %>%
  na.omit()

# calculate correlation matrix
Cor_matrix <- cor(Shooting_numerical)

# Calculate the significant values
# mat : is a matrix of data
# ... : further arguments to pass to the native R cor.test function
cor.mtest <- function(mat, ...) {
  mat <- as.matrix(mat)
  n <- ncol(mat)
  p.mat<- matrix(NA, n, n)
  diag(p.mat) <- 0
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      tmp <- cor.test(mat[, i], mat[, j], ...)
      p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
    }
  }
  colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
  p.mat
}
# matrix of the p-value of the correlation
p.mat <- cor.mtest(Shooting_numerical)

# Leave blank on no significant coefficient
corrplot(Cor_matrix, type="upper", order="hclust", 
         p.mat = p.mat, sig.level = 0.01, insig = "blank")


# K means using caret package
set.seed(123)

km.shooting <- kmeans(Shooting_numerical, centers = 3, nstart = 25, iter.max = 20)

Shooting_cluster <- Shooting_numerical %>%
  mutate(category = km.shooting$cluster) %>%
  left_join(Shooting %>% select(Pos_primary, Age, `90s`, `Gls`, `Standard - Sh`, `Standard - Dist`, `Expected - xG`), 
            by = c("Age", "90s", "Gls", "Standard - Sh", "Expected - xG", "Standard - Dist"))

Shooting_cluster %>% 
  mutate(category = factor(category)) %>%
  ggplot(aes(x = Pos_primary, y = category, fill = category)) +
  geom_col()




# Adding other data

Possession <- readxl::read_excel(path = file.path(Data_location, "FBRef_player_stats_all_PL21-22.xlsx"), 
                               sheet = "Possession") %>%
  filter(!is.na(ID))

DefensiveActions <- readxl::read_excel(path = file.path(Data_location, "FBRef_player_stats_all_PL21-22.xlsx"), 
                                 sheet = "DefensiveActions") %>%
  filter(!is.na(ID))

# Combining

PossessionID <- Possession %>%
  select(`Touches - Touches`:ID)

DefensiveActionsID <- DefensiveActions %>%
  select(`Tackles - Tkl`:ID)


All_data <- Shooting %>%
  inner_join(PossessionID, by = c("ID")) %>%
  inner_join(DefensiveActionsID, by = c("ID")) %>%
  distinct(ID, .keep_all = TRUE)


All_data_numerical_withID <- All_data %>%
  select(Age:last_col(), -Born,) %>%
  na.omit()

#k means
km.all <- kmeans(All_data_numerical_withID %>% select(-ID), centers = 3, nstart = 25, iter.max = 20)

# results evaluation
All_cluster <- All_data_numerical_withID %>%
  mutate(category = km.all$cluster) %>%
  left_join(All_data %>% select(Pos_primary, Age, `90s`, `Gls`, `Standard - Sh`, `Standard - Dist`, `Expected - xG`, `Tackles - Tkl`, `Touches - Touches`), 
            by = c("Age", "90s", "Gls", "Standard - Sh", "Expected - xG", "Standard - Dist", "Tackles - Tkl","Touches - Touches"))

All_cluster %>% 
  mutate(category = factor(category)) %>%
  ggplot(aes(x = Pos_primary, y = category, fill = category)) +
  geom_col()
