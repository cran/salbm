all : get Res Main Samps
.PHONY : get Res Main Samps

Main :	
	R --no-save --no-restore <  Main.R >  Main.Rout &

Samps :	
	R --no-save --no-restore <  samp_parts.R --args ${part} ${NB} ${st} >  samp_parts${part}.Rout &

get :	
	R --no-save --no-restore <  get.R >  get.Rout &

