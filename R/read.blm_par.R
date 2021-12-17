
read.blm_par <- function(file)
{
  dados <-  read.fwf(file,
                     widths=c(8,-4,-24,10,10,10,10,-20,10,10),
                     skip=4,
                     col.names=c("nomeblg","aorig","epa","borig","epb","c","epc"),
                     colClasses=c("character"),
                     header = FALSE, flush = TRUE)
  for(i in 2:7) dados[,i] <- as.numeric(dados[,i])
  dados
}
# 
# 
# Columns Format Description
# 001 – 008 A8    item name
# 009 – 016 A8    subtest name
# 017 – 026 F10.5 intercept parameter
# 027 – 036 F10.5 intercept s. e.
# 037 – 046 F10.5 slope parameter
# 047 – 056 F10.5 slope s. e.
# 057 – 066 F10.5 threshold parameter
# 067 – 076 F10.5 threshold s. e.
# 077 – 086 F10.5 dispersion parameter (reciprocal of slope)
# 087 – 096 F10.5 dispersion s. e.
# 097 – 106 F10.5 lower asymptote parameter
# 107 – 116 F10.5 lower asymptote s.e.
# 117 – 126 F10.5 DRIFT parameter
# 127 – 136 F10.5 DRIFT s. e.
# 137 – 146 F10.5 unused columns
# 147 – 150 I4    location of item in input stream
# 151 - 151 A1    answer key
# 152 - 152 I1    dummy values
# 
# 8
# 8
# 10
# 10
# 
# 
# dados <-  read.fwf(file,
#                    widths=c(rep(8,2), rep(10, 13),
#                    skip=4,
#                    col.names=c("nomeblg","aorig","epa","borig","epb","c","epc"),
#                    colClasses=c("character"),
#                    header = FALSE, flush = TRUE)
# for(i in 2:7) dados[,i] <- as.numeric(dados[,i])
# dados
