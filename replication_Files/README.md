# Gov2001 replication paper

## To Replicate Extensions

### Topography and Autonomy Extensions

Run `autonomy_extension.Rmd` file. This markdown document uses data created in the following scripts:
* rep_05_include_terrain.R (this can be slow if run completely)
* rep_06_claim_type.R

Both of these scripts build upon the authors' original datasets, which we would not recommend replicating due to the computational expense - it took us a few hours to re-create their full sets of data using their scripts. The instructions for re-running their original analysis are in `replicationPaper/redemption-through-rebellion-dataverse_files/README.txt`.

### Instigator Extension

Run `instigator_tfrac_extension.Rmd` in `replicationPaper/replication_Files/`. This markdown uses data created in `rep_04_include_instigator.R`, which it is not possible to entirely replicate because it involved manual intervention to generate the `acd_dyads_cows_years_rough_matched.csv` file, which is used to combine the Correlates of War and ACD conflict data: we manually checked each of the contents of `acd_dyads_cows_years_rough_gb` to identify whether the conflict records from CoW and ACD were referring to the same conflict. 
 
