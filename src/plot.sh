#!/bin/bash

clear &&
echo -e "summarize.R:\n" &&
Rscript src/plot/summarize.R &&
clear &&
echo -e "intervention_input_summary.R:\n" &&
Rscript src/plot/intervention_input_summary.R &&
clear &&
echo -e "interventions.R:\n" &&
Rscript src/plot/interventions.R &&
clear &&
echo -e "phase_years_from_phases.R:\n" &&
Rscript src/plot/phase_years_from_phases.R &&
clear &&
echo -e "phase_years_from_years.R:\n" &&
Rscript src/plot/phase_years_from_years.R &&
clear &&
echo -e "phase_years.R:\n" &&
Rscript src/plot/phase_years.R &&
clear &&
echo -e "phases_from_phases.R:\n" &&
Rscript src/plot/phases_from_phases.R &&
clear &&
echo -e "DONE"