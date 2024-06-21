function roi_files = select_roi_files(roi_dir)
    % Check for .nii files in the selected directory
    if ~isempty(dir(fullfile(roi_dir, '*.nii')))
        fprintf('\nPlease select ROI of interest. \n');
        while true
            % Prompt user to select .nii files
            roi_files = uigetfile('*.nii', 'Select ROIs of interest', roi_dir, 'MultiSelect', 'on');
            roi_files = fullfile(roi_dir, roi_files);
            
            % Check if user selected files
            if iscell(roi_files) || ischar(roi_files)
                % If multiple files selected, roi_files is a cell array
                % If single file selected, roi_files is a char array
                if iscell(roi_files)
                    roi_files = roi_files';
                end
                return; % Exit function with selected files
            else
                fprintf('\nNo files selected. Please select ROI of interest. \n');
            end
        end
    else
        fprintf('Could not find .nii files in the selected directory. Please select a different directory.\n\n');
        roi_files = []; % Return empty if no files found
    end
end
