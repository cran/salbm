all : Preliminaries Plots InfAss
.PHONY : Preliminaries Plots InfAss

Preliminaries :	
	cd Preliminaries        && R --no-save --no-restore <  DescTrt.R      >  DescTrt.Rout
	cd Preliminaries        && R --no-save --no-restore <  Patterns.R     >  Patterns.Rout

Plots :	
	cd Plots                && R --no-save --no-restore <  plots.R        >  plots.Rout
	cd Plots                && R --no-save --no-restore <  EYplot.R       >  EYplot.Rout
	cd Plots                && R --no-save --no-restore <  contourPlot.R  >  contourPlot.Rout

InfAss :	
	cd InferenceAssumptions && R --no-save --no-restore <  InfAss.R       >  InfAss.Rout



