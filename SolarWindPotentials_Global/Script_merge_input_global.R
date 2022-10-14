pacman::p_load(raster)

# merge files created in Script_input_global_split.R and Slurm_input_global.sh

print('Script to merge 60x60deg tiles globally. NOTE: tiles are removed')

A <- stack('predictor_stack_A.gri')
B <- stack('predictor_stack_B.gri')
C <- stack('predictor_stack_C.gri')
D <- stack('predictor_stack_D.gri')
E <- stack('predictor_stack_E.gri')
F <- stack('predictor_stack_F.gri')
G <- stack('predictor_stack_G.gri')
H <- stack('predictor_stack_H.gri')
I <- stack('predictor_stack_I.gri')
J <- stack('predictor_stack_J.gri')
K <- stack('predictor_stack_K.gri')
L <- stack('predictor_stack_L.gri')
M <- stack('predictor_stack_M.gri')
N <- stack('predictor_stack_N.gri')
O <- stack('predictor_stack_O.gri')
P <- stack('predictor_stack_P.gri')
Q <- stack('predictor_stack_Q.gri')
R <- stack('predictor_stack_R.gri')

print('tiles loaded, now merge')
pred_stack_global <- merge(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R)
print('   and save')
names(pred_stack_global) <- c('road_dist','urban_dist','grid','access','slope','elev','protect','rsds','wind')
writeRaster(pred_stack_global,filename='pred_stack_global.gri',overwrite=TRUE,format='raster')

# remove tiles
system('rm predictor_stack*')
