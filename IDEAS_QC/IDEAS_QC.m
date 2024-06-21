%% Brain_QC is designed to generate slices 
% to perform quality control of amyloid PET scans 
% collected in Imaging Dementia-Evidence for Amyloid Scanning
% (https://www.ideas-study.org).
% Image processing is not part of the current script. 
%
% PET output files processed with rPOP (MRI-free pipeline) are 100%
% compatible with this script. 
%
% If need to modify slice selection please see run/get_plane_parameters

% Prepare MATLAB space
clear; % Clear Workspace

% Set defaults
overwrite = false;
brainqc_path = fileparts(mfilename('fullpath'));

% Add BrainQC to MATLAB path
addpath(genpath(brainqc_path));
cd(brainqc_path);

fprintf('- Please provide path to input files in .nii format\n');
input_dir = uigetdir(pwd,'Input file directory');

fprintf('- Please provide path to ROI files in .nii format\n');
roi_dir = uigetdir(pwd,'Input file directory');

output_dir = fullfile(brainqc_path,'output_slices');
if ~exist(output_dir,'dir')
   mkdir(output_dir)
end
fprintf('- Output slices will be saved at: %s\n', output_dir);

setup_multislices(input_dir, roi_dir, output_dir, overwrite);

fprintf('- All done!\n');