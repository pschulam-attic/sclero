data: data/pft.rdata data/clinic.rdata data/patient.rdata

data/pft.rdata: inst/R/create-pft-data.r
	Rscript $<

data/clinic.rdata: inst/R/create-clinic-data.r
	Rscript $<

data/patient.rdata: inst/R/create-patient-data.r
	Rscript $<
