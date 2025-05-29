library(readxl)
df <- read_excel("C:/Users/bravo/Downloads/data.xlsx", sheet = "Company C")
b <- df$resTime_CompanyB
c <- df$resTime_CompanyC
ci <- t.test(b, c, paired = TRUE, conf.level = 0.95)$conf.int
cat(sprintf("95%% CI for μ_B – μ_C: [%.1f, %.1f]\n", ci[1], ci[2]))

