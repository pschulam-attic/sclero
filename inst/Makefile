data: data/patientsplit.rdata data/pft.rdata data/clinic.rdata data/patient.rdata

data/patientsplit.rdata: inst/R/create-patientsplit-data.r
	Rscript $<

data/pft.rdata: inst/R/create-pft-data.r
	Rscript $<

data/clinic.rdata: inst/R/create-clinic-data.r
	Rscript $<

data/patient.rdata: inst/R/create-patient-data.r
	Rscript $<
