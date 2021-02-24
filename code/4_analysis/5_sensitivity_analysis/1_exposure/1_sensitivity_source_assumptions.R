
## sensitivity analysis - different assumptions of source contributions

## simple contribution

res_glmer_adj_mcl <- res_table_primary(sibs_crossover_mcl, glmer_co_adj)
View(res_glmer_adj_mcl)

write_output(res_glmer_adj_mcl, "output/results/sensitivity/1_exp_assumptions/")
