FDIR.factors.VIII.2022 <- data.table::data.table(Description=character(), Species=character(), ProductType=character(), WeightFactor=numeric())

#bromse
FDIR.factors.VIII.2022 <- rbind(FDIR.factors.VIII.2022, data.table::data.table("USK GWOH (Brosme brosme)", "164740", "4", 1.40), use.names=F)
FDIR.factors.VIII.2022 <- rbind(FDIR.factors.VIII.2022, data.table::data.table("USK GWH (Brosme brosme)", "164740", "3", 1.20), use.names=F)

#torsk
FDIR.factors.VIII.2022 <- rbind(FDIR.factors.VIII.2022, data.table::data.table("COD GWOH (Gadus morhua)", "164712", "4", 1.50), use.names=F)
FDIR.factors.VIII.2022 <- rbind(FDIR.factors.VIII.2022, data.table::data.table("COD GWH (Gadus morhua)", "164712", "3", 1.18), use.names=F)

#lange
FDIR.factors.VIII.2022 <- rbind(FDIR.factors.VIII.2022, data.table::data.table("LIN GWOH (Molva molva)", "164760", "4", 1.40), use.names=F)
FDIR.factors.VIII.2022 <- rbind(FDIR.factors.VIII.2022, data.table::data.table("LIN GWH (Molva molva)", "164760", "3", 1.20), use.names=F)

#blålange
FDIR.factors.VIII.2022 <- rbind(FDIR.factors.VIII.2022, data.table::data.table("BLI GWOH (Molva dypterygia)", "164761", "4", 1.40), use.names=F)
FDIR.factors.VIII.2022 <- rbind(FDIR.factors.VIII.2022, data.table::data.table("BLI GWH (Molva dypterygia)", "164761", "3", 1.20), use.names=F)

#skjellbrosme
FDIR.factors.VIII.2022 <- rbind(FDIR.factors.VIII.2022, data.table::data.table("GFB GWOH (Phycis blennoides)", "164751", "4", 1.40), use.names=F)
FDIR.factors.VIII.2022 <- rbind(FDIR.factors.VIII.2022, data.table::data.table("GFB GWH (Phycis blennoides)", "164751", "3", 1.20), use.names=F)

#hyse
FDIR.factors.VIII.2022 <- rbind(FDIR.factors.VIII.2022, data.table::data.table("HAD GWOH (Melanogrammus aeglefinus)", "164744", "4", 1.40), use.names=F)
FDIR.factors.VIII.2022 <- rbind(FDIR.factors.VIII.2022, data.table::data.table("HAD GWH (Melanogrammus aeglefinus)", "164744", "3", 1.14), use.names=F)

#sei
FDIR.factors.VIII.2022 <- rbind(FDIR.factors.VIII.2022, data.table::data.table("POK GWOH (Pollachius virens)", "164727", "4", 1.35), use.names=F)
FDIR.factors.VIII.2022 <- rbind(FDIR.factors.VIII.2022, data.table::data.table("POK GWH (Pollachius virens)", "164727", "3", 1.20), use.names=F)

#lyr
FDIR.factors.VIII.2022 <- rbind(FDIR.factors.VIII.2022, data.table::data.table("POL GWOH (Pollachius pollachius)", "164728", "4", 1.30), use.names=F)
FDIR.factors.VIII.2022 <- rbind(FDIR.factors.VIII.2022, data.table::data.table("POL GWH (Pollachius pollachius)", "164728", "3", 1.15), use.names=F)

#hvitting
FDIR.factors.VIII.2022 <- rbind(FDIR.factors.VIII.2022, data.table::data.table("WHG GWOH (Merlangius merlangus)", "164758", "4", 1.40), use.names=F)
FDIR.factors.VIII.2022 <- rbind(FDIR.factors.VIII.2022, data.table::data.table("WHG GWH (Merlangius merlangus)", "164758", "3", 1.20), use.names=F)

#lysing
FDIR.factors.VIII.2022 <- rbind(FDIR.factors.VIII.2022, data.table::data.table("HKE GWOH (Merluccius merluccius)", "164795", "4", 1.40), use.names=F)
FDIR.factors.VIII.2022 <- rbind(FDIR.factors.VIII.2022, data.table::data.table("HKE GWH (Merluccius merluccius)", "164795", "3", 1.20), use.names=F)

#isgalt
FDIR.factors.VIII.2022 <- rbind(FDIR.factors.VIII.2022, data.table::data.table("RHG GWOH (Macrourus berglax)", "165421", "4", 1.40), use.names=F)
FDIR.factors.VIII.2022 <- rbind(FDIR.factors.VIII.2022, data.table::data.table("RHG GWH (Macrourus berglax)", "165421", "3", 1.20), use.names=F)

#skolest
FDIR.factors.VIII.2022 <- rbind(FDIR.factors.VIII.2022, data.table::data.table("RNG GWOH (Coryphaenoides rupestris)", "165350", "4", 1.40), use.names=F)
FDIR.factors.VIII.2022 <- rbind(FDIR.factors.VIII.2022, data.table::data.table("RNG GWH (Coryphaenoides rupestris)", "165350", "3", 1.20), use.names=F)

#gråsteinbit
FDIR.factors.VIII.2022 <- rbind(FDIR.factors.VIII.2022, data.table::data.table("CAA GWOH (Anarhichas lupus)", "171341", "4", 1.65), use.names=F)
FDIR.factors.VIII.2022 <- rbind(FDIR.factors.VIII.2022, data.table::data.table("CAA GWH (Anarhichas lupus)", "171341", "3", 1.10), use.names=F)

#Flekksteinbit
FDIR.factors.VIII.2022 <- rbind(FDIR.factors.VIII.2022, data.table::data.table("CAS GWOH (Anarhichas minor)", "171342", "4", 1.65), use.names=F)
FDIR.factors.VIII.2022 <- rbind(FDIR.factors.VIII.2022, data.table::data.table("CAS GWH (Anarhichas minor)", "171342", "3", 1.10), use.names=F)

#Blåsteinbit
FDIR.factors.VIII.2022 <- rbind(FDIR.factors.VIII.2022, data.table::data.table("CAB GWOH (Anarhichas denticulatus)", "550561", "4", 1.65), use.names=F)
FDIR.factors.VIII.2022 <- rbind(FDIR.factors.VIII.2022, data.table::data.table("CAB GWH (Anarhichas denticulatus)", "550561", "3", 1.10), use.names=F)

#uspesifisert uer
FDIR.factors.VIII.2022 <- rbind(FDIR.factors.VIII.2022, data.table::data.table("RED GWOH (Sebastes sp)", "166705", "4", 1.65), use.names=F)
FDIR.factors.VIII.2022 <- rbind(FDIR.factors.VIII.2022, data.table::data.table("RED GWH (Sebastes sp)", "166705", "3", 1.20), use.names=F)

#vanlig uer
FDIR.factors.VIII.2022 <- rbind(FDIR.factors.VIII.2022, data.table::data.table("REG GWOH (Sebastes norvegicus)", "166781", "4", 1.65), use.names=F)
FDIR.factors.VIII.2022 <- rbind(FDIR.factors.VIII.2022, data.table::data.table("REG GWH (Sebastes norvegicus)", "166781", "3", 1.20), use.names=F)

#snabeluer
FDIR.factors.VIII.2022 <- rbind(FDIR.factors.VIII.2022, data.table::data.table("REB GWOH (Sebastes mentella)", "166756", "4", 1.50), use.names=F)
FDIR.factors.VIII.2022 <- rbind(FDIR.factors.VIII.2022, data.table::data.table("REB GWH (Sebastes mentella)", "166756", "3", 1.08), use.names=F)

#kveite
FDIR.factors.VIII.2022 <- rbind(FDIR.factors.VIII.2022, data.table::data.table("HAL GWOH (Hippoglossus hippoglossus)", "172933", "4", 1.35), use.names=F)
FDIR.factors.VIII.2022 <- rbind(FDIR.factors.VIII.2022, data.table::data.table("HAL GWH (Hippoglossus hippoglossus)", "172933", "3", 1.10), use.names=F)

#rødspette
FDIR.factors.VIII.2022 <- rbind(FDIR.factors.VIII.2022, data.table::data.table("PLE GWOH (Pleuronectes platessa)", "172902", "4", 1.20), use.names=F)
FDIR.factors.VIII.2022 <- rbind(FDIR.factors.VIII.2022, data.table::data.table("PLE GWH (Pleuronectes platessa)", "172902", "3", 1.10), use.names=F)

#blåkveite
FDIR.factors.VIII.2022 <- rbind(FDIR.factors.VIII.2022, data.table::data.table("GHL GWOH (Reinhardtius hippoglossoides)", "172930", "4", 1.32), use.names=F)
FDIR.factors.VIII.2022 <- rbind(FDIR.factors.VIII.2022, data.table::data.table("GHL GWH (Reinhardtius hippoglossoides)", "172930", "3", 1.12), use.names=F)

#smørflyndre
FDIR.factors.VIII.2022 <- rbind(FDIR.factors.VIII.2022, data.table::data.table("WIT GWOH (Glyptocephalus cynoglossus)", "172873", "4", 1.20), use.names=F)
FDIR.factors.VIII.2022 <- rbind(FDIR.factors.VIII.2022, data.table::data.table("WIT GWH (Glyptocephalus cynoglossus)", "172873", "3", 1.10), use.names=F)

#gapeflyndre
FDIR.factors.VIII.2022 <- rbind(FDIR.factors.VIII.2022, data.table::data.table("PLA GWOH (Hippoglossoides platessoides)", "172877", "4", 1.20), use.names=F)
FDIR.factors.VIII.2022 <- rbind(FDIR.factors.VIII.2022, data.table::data.table("PLA GWH (Hippoglossoides platessoides)", "172877", "3", 1.10), use.names=F)

#lomre
FDIR.factors.VIII.2022 <- rbind(FDIR.factors.VIII.2022, data.table::data.table("LEM GWOH (Microstomus kitt)", "172888", "4", 1.20), use.names=F)
FDIR.factors.VIII.2022 <- rbind(FDIR.factors.VIII.2022, data.table::data.table("LEM GWH (Microstomus kitt)", "172888", "3", 1.10), use.names=F)

#tunge
FDIR.factors.VIII.2022 <- rbind(FDIR.factors.VIII.2022, data.table::data.table("SOL GWOH (Solea Solea)", "173002", "4", 1.20), use.names=F)
FDIR.factors.VIII.2022 <- rbind(FDIR.factors.VIII.2022, data.table::data.table("SOL GWH (Solea Solea)", "173002", "3", 1.10), use.names=F)

#Slettvar
FDIR.factors.VIII.2022 <- rbind(FDIR.factors.VIII.2022, data.table::data.table("BLL GWOH (Scophthalmus rhombus)", "172749", "4", 1.20), use.names=F)
FDIR.factors.VIII.2022 <- rbind(FDIR.factors.VIII.2022, data.table::data.table("BLL GWH (Scophthalmus rhombus)", "172749", "3", 1.10), use.names=F)

#Piggvar
FDIR.factors.VIII.2022 <- rbind(FDIR.factors.VIII.2022, data.table::data.table("TUR GWOH (Scophthalmus maximus)", "172748", "4", 1.20), use.names=F)
FDIR.factors.VIII.2022 <- rbind(FDIR.factors.VIII.2022, data.table::data.table("TUR GWH (Scophthalmus maximus)", "172748", "3", 1.10), use.names=F)

#Breiflabb
FDIR.factors.VIII.2022 <- rbind(FDIR.factors.VIII.2022, data.table::data.table("MON GWOH (Lophius piscatorius)", "164501", "4", 2.80), use.names=F)
FDIR.factors.VIII.2022 <- rbind(FDIR.factors.VIII.2022, data.table::data.table("MON GWH (Lophius piscatorius)", "164501", "3", 1.20), use.names=F)


usethis::use_data(FDIR.factors.VIII.2022, overwrite = T)
