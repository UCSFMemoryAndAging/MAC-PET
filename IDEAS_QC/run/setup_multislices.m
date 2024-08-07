function setup_multislices(input_dir, roi_dir, output_dir, overwrite)
    % Function to setup pipeline to create multislices for image quality control
    %
    % Requires SPM12 to run.
    %
    % Parameters 
    % --------------------------------------------------------
    % input_dir: char or str
    %   Path to scans to be reviewed. 
    %   Input files need to be in .nii format.
    %
    % roi_dir: char or str
    %   Path to regions of interest to be overlayed on top of input image.
    %   Images and regions should be in the voxel/image space.
    %
    % output_dir: char or str
    %   Path to desired output directory
    %
    % overwrite: logical, optional
    %   If true, overwrite existing files

    arguments
        input_dir {mustBeFolder}
        roi_dir {mustBeFolder} = pwd; 
        output_dir {mustBeFolder} = pwd;
        overwrite logical = false;
    end
    
    %% Setup
    if isempty(fileparts(which('spm')))
        fprintf('SPM is required to run this pipeline and do not appear to be on the path. Please add path to SPM directory.\n');
        spm_dir = uigetdir(pwd,'Select SPM directory');
        if isempty(spm_dir)
            error('ERROR: It seems that SPM is not installed in this computer. Please install it and run this again');
        else 
            addpath(genpath(spm_dir));
        end
    end

    % Define defaults
    default.img_extension = 'tif';
    default.final_width = 396;
    default.final_height = 696;
    default.planes = {'axial','coronal','sagittal'};
    default.rangecolorscale = 1;
    overwrite = prompt_bool('Overwrite files?', false);
    if overwrite
        fprintf('Will overwrite existing files.\n\n');
        confirm_response = prompt_bool('Are you sure you want to overwrite files?', false, true);
            if ~confirm_response
                fprintf('OK--let''s exit the program and try again...\n');
                return;
            end
    end
            
    %% Create the output folder directory (if it does not exist yet)

    % Get scan names and count
    input_files = dir(fullfile(input_dir, '*.nii'));
    n_input_files = length(input_files);

    fprintf('\nSearching input directory: %s\n-------------------\n', input_dir);
    fprintf('Found %d files to process\n', n_input_files);

    roi_files = select_roi_files(roi_dir);

    %% Start processing
    % Process one scan
    if n_input_files == 1

       input_file=[input_files(1).folder filesep input_files(1).name];
       create_multislice(input_file, roi_files, output_dir, default, overwrite);

    % Process multiple scans in parallel
    else
        % Check if Parallel Computing Toolbox is installed
        toolboxes = ver;
        isInstalled = any(strcmp({toolboxes.Name}, 'Parallel Computing Toolbox'));
        if isInstalled
            % Start a parallel pool
            % Create a cluster object
            c = parcluster; 
            n_workers = min(n_input_files, c.NumWorkers);
            poolobj = parpool(n_workers);
    
            % Assign a worker to each scan
            parfor ii = 1:n_input_files
                try
                    input_file=[input_files(ii).folder filesep input_files(ii).name];
                    create_multislice(input_file, roi_files, output_dir, default, overwrite);
    
                catch ME
                    warning( ...
                        '\n\n\nERROR processing %s: %s\n\n\n', input_file, ...
                        getReport(ME, 'extended', 'hyperlinks', 'off') ...
                    );
                end
            end
             % Close the parallel pool
            if ~isempty(poolobj)
                delete(poolobj);
            end
        else
            for ii = 1:n_input_files
                try
                    input_file=[input_files(ii).folder filesep input_files(ii).name];
                    create_multislice(input_file, roi_files, output_dir, default, overwrite);
    
                catch ME
                    warning( ...
                        '\n\n\nERROR processing %s: %s\n\n\n', input_file, ...
                        getReport(ME, 'extended', 'hyperlinks', 'off') ...
                    );
                end
            end
        end
       
    end
    delete('i2s*.mat'); delete('yxratio_*.mat');
end