pacman::p_load(raster)

# merge files created in Script_input_landcover_split.R and Slurm_input_global_landcover.sh

print('Script to merge 60x60deg land cover tiles globally. NOTE: tiles are removed')
year    <- 2000

outputfilename = paste0('ESA_stack_',year,'.gri')
print(paste('Output file (note year!!):',outputfilename))

A <- stack('ESA_stack_2000_A.gri')
B <- stack('ESA_stack_2000_B.gri')
C <- stack('ESA_stack_2000_C.gri')
D <- stack('ESA_stack_2000_D.gri')
E <- stack('ESA_stack_2000_E.gri')
F <- stack('ESA_stack_2000_F.gri')
G <- stack('ESA_stack_2000_G.gri')
H <- stack('ESA_stack_2000_H.gri')
I <- stack('ESA_stack_2000_I.gri')
J <- stack('ESA_stack_2000_J.gri')
K <- stack('ESA_stack_2000_K.gri')
L <- stack('ESA_stack_2000_L.gri')
M <- stack('ESA_stack_2000_M.gri')
N <- stack('ESA_stack_2000_N.gri')
O <- stack('ESA_stack_2000_O.gri')
P <- stack('ESA_stack_2000_P.gri')
Q <- stack('ESA_stack_2000_Q.gri')
R <- stack('ESA_stack_2000_R.gri')

print('tiles loaded, now merge')
pred_stack_global <- merge(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R)
print('   and save')
writeRaster(pred_stack_global,filename='ESA_stack_2000.gri',overwrite=TRUE,format='raster')

# remove tiles
system('rm ESA_stack_2000_*')
