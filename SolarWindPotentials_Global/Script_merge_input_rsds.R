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
names(pred_stack_global) <- c('rsds')
writeRaster(pred_stack_global,filename='pred_stack_rsds.gri',overwrite=TRUE,format='raster')

# remove tiles
system('rm predictor_stack*')

# get the original predictor stack, delete 'original access' and replace
pred_stack_orig  <- stack('pred_stack_global_orig.gri')
names(pred_stack_global) <- c('road_dist','urban_dist','grid','slope','elev','protect','rsds','wind','access')

pred_stack_orig  <- dropLayer(pred_stack_orig,'rsds')
pred_stack       <- stack(pred_stack_orig,pred_stack_global)		#merge original and new stack (which contains only access)

print('CHECK order of raster layers and names!')
names(pred_stack) <- c('road_dist','urban_dist','grid','access','slope','elev','protect','wind','rsds')
writeRaster(pred_stack,filename='pred_stack_global.gri',overwrite=TRUE,format='raster')
