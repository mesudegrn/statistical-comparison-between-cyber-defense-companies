library(readxl)
library(dplyr)

path <- "C:/Users/bravo/Downloads/data.xlsx"
data <- read_excel(path, sheet = "data") %>% filter(Company == "A")

det_cols <- grep("^DetectionTime_",  names(data), value = TRUE)
res_cols <- grep("^ResolutionTime_", names(data), value = TRUE)

det_tot <- rowSums(data[det_cols])
res_tot <- rowSums(data[res_cols])
ratio   <- det_tot / (det_tot + res_tot)

successes <- sum(ratio < 0.4, na.rm = TRUE)
trials    <- length(ratio)

ci <- binom.test(successes, trials, conf.level = 0.95)$conf.int
cat(sprintf("p̂ = %.3f, 95%% CI: [%.3f, %.3f]\n", successes / trials, ci[1], ci[2]))
